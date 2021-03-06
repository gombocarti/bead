{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Public.Registration (
    registrationFirstStep
  , registrationFirstStepEmailSent
  , registrationPasswordStep
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Text.Blaze as B
import qualified Text.Blaze.Html5.Attributes as A hiding (title, rows, accept)
import           Text.Blaze.Html5 as H

import qualified Bead.Controller.Pages as P
import           Bead.View.Content hiding (BlazeTemplate, template)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.DataBridge as DataBridge

backToLogin msg =
  Bootstrap.rowCol4Offset4 $ Bootstrap.buttonGroupJustified $
    Bootstrap.buttonLink (routeOf $ P.index ()) $ msg $ msg_Registration_GoBackToLogin "Back to login"

registrationTitle msg =
  Bootstrap.rowCol4Offset4 $ h2 $
    B.toMarkup $ msg $ msg_Registration_Title "Registration"

registrationFirstStep :: IHtml
registrationFirstStep = do
  msg <- getI18N
  return $ do
    registrationTitle msg
    Bootstrap.rowCol4Offset4 $ postForm "/reg_request" $ do
      Bootstrap.textInput (DataBridge.name regUsernamePrm) (msg $ msg_Registration_Username "Username:") ""
      Bootstrap.textInput (DataBridge.name regEmailPrm)    (msg $ msg_Registration_Email "Email:") ""
      Bootstrap.textInput (DataBridge.name regFullNamePrm) (msg $ msg_Registration_FullName "Full name:") ""
      Bootstrap.submitButton (fieldName regSubmitBtn :: Text) (msg $ msg_Registration_SubmitButton "Registration")
    backToLogin msg

registrationFirstStepEmailSent :: IHtml
registrationFirstStepEmailSent = do
  msg <- getI18N
  return $ do
    registrationTitle msg
    Bootstrap.rowCol4Offset4 $ p $
      B.toMarkup $ msg $ msg_RegistrationTokenSend_Title "The registration token has been sent in email, it shall arrive soon."
    backToLogin msg

-- The second step f the registration
-- registrationPasswordStep :: IHtml
registrationPasswordStep utcZoneInfo timeZones key language username token = do
  msg <- getI18N
  return $ do
    registrationTitle msg
    Bootstrap.rowCol4Offset4 $ postForm "/reg_final" $ do
      Bootstrap.passwordInput (DataBridge.name regPasswordPrm)      (msg $ msg_RegistrationFinalize_Password "Password:")
      Bootstrap.passwordInput (DataBridge.name regPasswordAgainPrm) (msg $ msg_RegistrationFinalize_PwdAgain "Password (again):")
      Bootstrap.selectionWithLabel
        (DataBridge.name regTimeZonePrm)
        (msg $ msg_RegistrationFinalize_Timezone "Time zone:")
        (==utcZoneInfo)
        timeZones
      hiddenParam regUserRegKeyPrm key
      hiddenParam regTokenPrm      token
      hiddenParam regUsernamePrm   username
      hiddenParam regLanguagePrm   language
      Bootstrap.submitButton (fieldName regSubmitBtn :: Text) (msg $ msg_RegistrationFinalize_SubmitButton "Register")
    backToLogin msg

  where
    hiddenParam parameter value = hiddenInput (DataBridge.name parameter) (DataBridge.encode parameter value)
