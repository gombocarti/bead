{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Markdown (
    headersToDiv
  , minHeaderLevel
  , markdownToHtml
  ) where

{- A markdown to HTML conversion. -}

import           Bead.View.Pagelets (copyToClipboardButton)
import           Bead.View.Translation (I18N)

import           Control.Monad ((<=<))
import           Control.Monad.State (State, evalState, state)
import           Data.Char (intToDigit)
import           Data.Foldable (foldrM)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath (FilePath)

import           Text.Pandoc.Class (PandocPure, runPure)
import           Text.Pandoc.Definition (Pandoc, Block(Header, Div, Plain, CodeBlock, RawBlock))
import           Text.Pandoc.Extensions (pandocExtensions)
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown (readMarkdown)
import           Text.Pandoc.Walk (walkM)
import           Text.Pandoc.Writers.HTML (writeHtml5)
import           Text.Blaze (string)
import           Text.Blaze.Html5 (Html)
import           Text.Blaze.Internal (MarkupM(..), getText, ChoiceString(Static))
import           Text.Blaze.Renderer.String (renderMarkup)

-- Produces HTML from the given markdown formatted string what
-- comes from a text area field.
-- It also assigns unique identifiers and Copy to Clipboard buttons to code blocks.
-- Identifiers are unique in context of single markdown document but not
-- over multiple documents.
markdownToHtml :: I18N -> String -> Html
markdownToHtml msg = either (string . show) id . runPure . (wrt . transform <=< rd)
  where
    wrt :: Pandoc -> PandocPure Html
    wrt = writeHtml5 writerOpts

    rd :: String -> PandocPure Pandoc
    rd = readMarkdown readerOpts . T.pack

    transform :: Pandoc -> Pandoc
    transform p = evalState (walkM (copyToClipboardForCodeBlocks msg) p) 0

    readerOpts :: ReaderOptions
    readerOpts = def { readerExtensions = enableExtension Ext_tex_math_single_backslash pandocExtensions }

    writerOpts :: WriterOptions
    writerOpts = def { writerHTMLMathMethod = KaTeX "/katex" }

copyToClipboardForCodeBlocks :: I18N -> [Block] -> State Int [Block]
copyToClipboardForCodeBlocks msg bs = foldrM addCopyButton [] bs
  where
    addCopyButton :: Block -> [Block] -> State Int [Block]
    addCopyButton (CodeBlock (_, classes, kv) code) blocks =
      state (\n ->
               let ident = "code-" ++ show n
               in ((RawBlock "html" (renderMarkup $ copyToClipboardButton msg ident) : CodeBlock (ident, classes, kv) code : blocks), n + 1))
    addCopyButton b blocks = return (b : blocks)

headersToDiv :: MarkupM a -> MarkupM a
headersToDiv (Parent tag open close contents)
  | getText tag `elem` headers = AddAttribute "class" " class=\"" (Static tag) (Parent "div" "<div" "</div>" (headersToDiv contents))
  where
    headers :: [Text]
    headers = map (\n -> T.snoc "h" (intToDigit n)) [1..6]
headersToDiv (CustomParent tag contents) = CustomParent tag $ headersToDiv contents
headersToDiv (Append h1 h2) = Append (headersToDiv h1) (headersToDiv h2)
headersToDiv (AddAttribute a b c contents) = AddAttribute a b c (headersToDiv contents)
headersToDiv (AddCustomAttribute a b contents) = AddCustomAttribute a b (headersToDiv contents)
headersToDiv h = h

minHeaderLevel :: Int -> MarkupM a -> MarkupM a
minHeaderLevel n (Parent tag open close contents) =
  case lookup (getText tag) replacement of
    Just t -> Parent (fromString t) (fromString $ '<' : t) (fromString $ "</" ++ t ++ ">") (minHeaderLevel n contents)
    Nothing -> Parent tag open close (minHeaderLevel n contents)
  where
    replacement :: [(Text, String)]
    replacement = zip headers headers'

    headers :: [Text]
    headers = map (\n -> T.snoc "h" (intToDigit n)) [1..6]

    headers' :: [String]
    headers' = map (\n -> "h" ++ show n) ([n..6] ++ repeat 6)

minHeaderLevel n (CustomParent tag contents) = CustomParent tag $ minHeaderLevel n contents
minHeaderLevel n (Append h1 h2) = Append (minHeaderLevel n h1) (minHeaderLevel n h2)
minHeaderLevel n (AddAttribute a b c contents) = AddAttribute a b c (minHeaderLevel n contents)
minHeaderLevel n (AddCustomAttribute a b contents) = AddCustomAttribute a b (minHeaderLevel n contents)
minHeaderLevel _ h = h
