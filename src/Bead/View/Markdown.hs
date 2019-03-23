module Bead.View.Markdown (
    markdownToHtml
  , syntaxHighlightCss
  ) where

{- A markdown to HTML conversion. -}

import           Control.Monad ((<=<))
import           Data.String.Utils (replace)
import qualified Data.Text as T
import           System.FilePath (FilePath)

import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Extensions (pandocExtensions)
import           Text.Pandoc.Highlighting (styleToCss, pygments)
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown (readMarkdown)
import           Text.Pandoc.Writers.HTML (writeHtml5)
import           Text.Blaze (string)
import           Text.Blaze.Html5 (Html)

-- Produces an HTML value from the given markdown formatted strings what
-- comes from a text area field, crlf endings must be replaced with lf in the string
markdownToHtml :: String -> Html
markdownToHtml = either (string . show) id . runPure .
                   (writeHtml5 writerOpts <=< readMarkdown readerOpts) . T.pack . replaceCrlf
  where
    readerOpts :: ReaderOptions
    readerOpts = def { readerExtensions = enableExtension Ext_tex_math_single_backslash pandocExtensions }

    writerOpts :: WriterOptions
    writerOpts = def { writerHTMLMathMethod = KaTeX "/katex" }

replaceCrlf :: String -> String
replaceCrlf = replace "\r\n" "\n"

syntaxHighlightCss :: (String, FilePath)
syntaxHighlightCss = (styleToCss pygments, "syntax-highlight.css")
