{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Bead.View.Content.Public.Index (
    index
  ) where

import           Data.ByteString.Char8 (unpack)
import           Data.String (fromString)

import qualified Bead.Controller.Pages as P
import           Bead.Domain.Entities (Language, AuthFailure(..))
import qualified Bead.View.BeadContext as BeadContext (getDictionaryInfos)
import           Bead.View.Common
import           Bead.View.Content
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.I18N (IHtml, getI18N)
import           Bead.View.Markdown
import           Bead.View.RouteOf
import           Bead.View.Translation

import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

fromMarkdown :: String -> H.Html
fromMarkdown = markdownToHtml

index :: Maybe AuthFailure -> BeadHandler IHtml
index authError = do
  languages <- BeadContext.getDictionaryInfos
  return $ do
    msg <- getI18N
    return $
      Bootstrap.rowCol4Offset4 $ do
        Bootstrap.pageHeader $ H.h2 $
          fromString $ msg $ msg_Index_Header "Welcome"

        fromMarkdown $ fromString $
          msg $ msg_Index_Body "This page can be only used with an account registered in the Active Directory of the hosting institute."

        postForm (routeOf $ P.login ()) $ do
          Bootstrap.textInputWithAttr (fieldName loginUsername) (msg $ msg_Login_Username "Username:") (A.autofocus "")
          Bootstrap.passwordInput (fieldName loginPassword) (msg $ msg_Login_Password "Password:")
          Bootstrap.submitButtonColorful  (fieldName loginSubmitBtn) (msg $ msg_Login_Submit "Log in")

        maybe mempty (\err -> H.br >> (Bootstrap.alert Bootstrap.Danger .(H.p ! A.class_ "text-center") . fromString . visibleFailure msg $ err)) authError

#ifndef SSO
        Bootstrap.rowCol4Offset4 $ Bootstrap.buttonGroupJustified $ do
          Bootstrap.buttonLink "/reg_request" (msg $ msg_Login_Registration "Registration")
          Bootstrap.buttonLink "/reset_pwd"   (msg $ msg_Login_Forgotten_Password "Forgotten password")
#endif

        H.hr

        languageMenu msg languages

visibleFailure :: I18N -> AuthFailure -> String
visibleFailure msg IncorrectUserOrPassword = msg $ msg_Login_InvalidPasswordOrUser "Incorrect username or password."
visibleFailure msg UserNotFound            = msg $ msg_Login_InvalidPasswordOrUser "Incorrect username or password."
