module Bead.View.Markdown (
    markdownToHtml
  , serveMarkdown
  ) where

{- A markdown to HTML conversion. -}

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BS
import           Data.Either
import           Data.String
import           Data.String.Utils (replace)
import qualified Data.Text as T
import           System.Directory
import           System.FilePath

import           Snap.Core
import           Snap.Blaze (blaze)
import           Text.Pandoc.Class (runPure)
import           Text.Pandoc.Extensions (pandocExtensions)
import           Text.Pandoc.Options
import           Text.Pandoc.Readers.Markdown (readMarkdown)
import           Text.Pandoc.Writers.HTML (writeHtml5)
import           Text.Blaze.Html5

import           Bead.Domain.Entities (PageSettings(PageSettings), needsLatex)
import           Bead.View.BeadContext
import           Bead.View.ContentHandler
import           Bead.View.I18N
import           Bead.View.Pagelets
import           Bead.View.Translation

-- Produces an HTML value from the given markdown formatted strings what
-- comes from a text area field, crlf endings must be replaced with lf in the string
markdownToHtml :: String -> Html
markdownToHtml = either (fromString . show) id . runPure .
                   (writeHtml5 writerOpts <=< readMarkdown readerOpts) . T.pack . replaceCrlf
  where
    readerOpts :: ReaderOptions
    readerOpts = def { readerExtensions = pandocExtensions }

    writerOpts :: WriterOptions
    writerOpts = def { writerHTMLMathMethod = KaTeX "/katex" }

replaceCrlf :: String -> String
replaceCrlf = replace "\r\n" "\n"

serveMarkdown :: BeadHandler ()
serveMarkdown = do
  rq <- getRequest
  let path = "markdown" </> (BS.unpack $ rqPathInfo rq)
  exists <- liftIO $ doesFileExist path
  let serve settings = bootstrapPublicPage settings >=> blaze
      withLatex = PageSettings { needsLatex = True }
      withoutLatex = PageSettings { needsLatex = False }
  if exists
    then do
      contents <- liftIO $ readFile path
      serve withLatex $ return $ markdownToHtml contents
    else do
      serve withoutLatex $ do
        msg <- getI18N
        return $ do
          p $ fromString . msg $
            msg_Markdown_NotFound "Sorry, but the requested page could not be found."
