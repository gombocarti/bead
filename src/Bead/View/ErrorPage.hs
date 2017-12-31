{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Bead.View.ErrorPage (
    ErrorPage(..)
  , defErrorPage
  , msgErrorPage
  , translationErrorPage
  ) where

import           Data.String
import qualified Text.Blaze.Html5 as H

import           Snap

import           Bead.Domain.Entities (PageSettings(PageSettings), needsLatex)
import           Bead.View.BeadContext (BeadHandler')
import qualified Bead.View.Content.Public.ErrorPage as View
import           Bead.View.ContentHandler
import           Bead.View.I18N (IHtml, getI18N)
import           Bead.View.Translation

class ErrorPage e where
  errorPage :: Translation String -> e -> BeadHandler' b H.Html

instance ErrorPage String where
  errorPage title msg = publicErrorPage title msg

instance ErrorPage (Translation String) where
  errorPage title msg = publicPage . (pageTranslation title) $ Just msg

instance ErrorPage TransMsg where
  errorPage title msg = do
    i18n <- i18nH
    publicPage . (page title) $ Just (translateMessage i18n msg)

instance ErrorPage ContentError where
  errorPage title msg = contentHandlerErrorMap (publicPage . (page title)) msg

msgErrorPage :: String -> BeadHandler' b H.Html
msgErrorPage = defErrorPage

defErrorPage :: (ErrorPage e) => e -> BeadHandler' b H.Html
defErrorPage = errorPage (msg_ErrorPage_Title "Oh snap!")

-- Produces a handler that renders the error page, with the
-- given title and message for the user
translationErrorPage :: Translation String -> Translation String -> BeadHandler' b H.Html
translationErrorPage = errorPage

page :: Translation String -> (Maybe String) -> IHtml
page = View.template fromString

pageTranslation :: Translation String -> (Maybe (Translation String)) -> IHtml
pageTranslation title err = do
  msg <- getI18N
  View.template (fromString . msg) title err

publicErrorPage :: Translation String -> String -> BeadHandler' b H.Html
publicErrorPage title msg = publicPage $ page title (Just msg)

publicPage :: IHtml -> BeadHandler' b H.Html
publicPage = bootstrapPublicPage (PageSettings { needsLatex = False })
