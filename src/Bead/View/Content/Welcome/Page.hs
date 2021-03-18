{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Welcome.Page (
    welcome
  ) where

import           Bead.View.Content (setPageContents, htmlPage, ViewHandler(ViewHandler), GETContentHandler)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.I18N (getI18N)
import           Bead.View.Translation (msg_LinkText_Welcome, msg_Welcome_ClickNavigation)

import qualified Text.Blaze.Html5 as H

welcome :: ViewHandler
welcome = ViewHandler welcomePage

welcomePage :: GETContentHandler
welcomePage = setPageContents $ htmlPage (msg_LinkText_Welcome "Welcome") $ do
  msg <- getI18N
  return $ Bootstrap.alert Bootstrap.Info $ do
    Bootstrap.infoIcon
    H.toMarkup (msg $ msg_Welcome_ClickNavigation "Click an item from the navigation menu.")

