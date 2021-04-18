{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Bead.View.Content.Public.Index (
    index
  ) where

import qualified Bead.Controller.Pages as P
import           Bead.Domain.Entities (Language, AuthFailure(..))
import qualified Bead.View.BeadContext as BeadContext (getDictionaryInfos, withDictionary)
import           Bead.View.Common
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.I18N (IHtml, getI18N)
import           Bead.View.RouteOf
import           Bead.View.Translation

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Blaze as B
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

index :: Maybe AuthFailure -> BeadHandler HtmlPage
index authError = do
  languages <- BeadContext.withDictionary BeadContext.getDictionaryInfos
  return $ HtmlPage {
      pageTitle = do
        msg <- getI18N
        return $ Bootstrap.rowCol4Offset4 $
          Bootstrap.pageHeader (msg $ msg_Index_Header "Welcome") Nothing
    , pageBody = do
        msg <- getI18N
        return $ pageContent authError languages msg
    }

pageContent :: Maybe AuthFailure -> DictionaryInfos -> I18N -> H.Html
pageContent authError languages msg = Bootstrap.rowCol4Offset4 $ do
  H.p $ B.toMarkup $
    msg $ msg_Index_Body "This page can be only used with an account registered in the Active Directory of the hosting institute."

  postForm (routeOf $ P.login ()) $ do
    Bootstrap.textInputWithAttr (fieldName loginUsername :: Text) (msg $ msg_Login_Username "Username:") "" (A.autofocus "")
    Bootstrap.passwordInput (fieldName loginPassword :: Text) (msg $ msg_Login_Password "Password:")
    Bootstrap.submitButtonColorful  (fieldName loginSubmitBtn :: Text) (msg $ msg_Login_Submit "Log in")

  maybe mempty (\err -> H.br >> (Bootstrap.alert Bootstrap.Danger .(H.p ! A.class_ "text-center") . B.toMarkup . visibleFailure msg $ err)) authError

#ifndef SSO
  Bootstrap.rowCol4Offset4 $ Bootstrap.buttonGroupJustified $ do
    Bootstrap.buttonLink "/reg_request" (msg $ msg_Login_Registration "Registration")
    Bootstrap.buttonLink "/reset_pwd"   (msg $ msg_Login_Forgotten_Password "Forgotten password")
#endif

  H.hr

  languageMenu msg languages

visibleFailure :: I18N -> AuthFailure -> Text
visibleFailure msg IncorrectUserOrPassword = msg $ msg_Login_InvalidPasswordOrUser "Incorrect username or password."
visibleFailure msg UserNotFound            = msg $ msg_Login_InvalidPasswordOrUser "Incorrect username or password."
