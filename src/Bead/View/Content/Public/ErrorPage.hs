{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE CPP #-}
module Bead.View.Content.Public.ErrorPage (
    template
  ) where

import           Data.ByteString.Char8 (unpack)
import           Data.Monoid (mempty)
import           Data.String (fromString)

import qualified Text.Blaze.Html5 as H

import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.ContentHandler (HtmlPage(HtmlPage), pageTitle, pageBody)
import           Bead.View.I18N (IHtml, getI18N)
import           Bead.View.RouteOf (indexPath)
import           Bead.View.Translation

template :: (e -> H.Html) -> Translation String -> Maybe e -> HtmlPage
template content title errorMsg =
  HtmlPage {
      pageTitle = do
        msg <- getI18N
        return $ Bootstrap.rowCol4Offset4 $ Bootstrap.pageHeader (msg title) Nothing
    , pageBody = do
        msg <- getI18N
        return $ do
          Bootstrap.rowCol4Offset4 $
            H.p $ maybe (defaultErrorMsg msg) content errorMsg
          Bootstrap.rowCol4Offset4 $
            Bootstrap.buttonLink loginLink (msg $ msg_ErrorPage_GoBackToHome "Back to the home page")
    }

  where
    defaultErrorMsg :: I18N -> H.Html
    defaultErrorMsg msg =
      fromString $ msg $ msg_ErrorPage_DefaultMsg "Some error happened... :("

    loginLink = unpack indexPath
