{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Public.Index (
    index
  ) where

import           Data.ByteString.Char8 (unpack)
import           Data.String (fromString)

import           Bead.Domain.Entities (Language)
import           Bead.View.BeadContext (DictionaryInfos)
import           Bead.View.Common
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.I18N (IHtml, getI18N)
import           Bead.View.Markdown
import           Bead.View.RouteOf
import           Bead.View.Translation

import qualified Text.Blaze.Html5 as H

fromMarkdown :: String -> H.Html
fromMarkdown = markdownToHtml

index :: DictionaryInfos -> IHtml
index languages = do
  msg <- getI18N
  return $ do
    Bootstrap.rowCol4Offset4 $ Bootstrap.pageHeader $ H.h2 $
      fromString $ msg $ msg_Index_Header "Welcome"
    Bootstrap.rowCol4Offset4 $ do
      fromMarkdown $ fromString $
        msg $ msg_Index_Body $ unlines
          [ "This page can be only used with an Active Directory account associated with the hosting institute."
          , ""
          , "With such an account available, please proceed with clicking on the **Proceed** button."
          , ""
          , "*Note:* Don't forget to log out on public computers (e.g. computer labs)."
          ]

      Bootstrap.blockButtonLink (unpack loginPath) (msg $ msg_Index_Proceed "Proceed")
    languageMenu msg languages
