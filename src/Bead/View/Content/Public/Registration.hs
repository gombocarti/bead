{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Public.Registration (
    registrationFirstStep
  , registrationFirstStepEmailSent
  , registrationPasswordStep
  ) where

import           Data.String (fromString)
import qualified Data.Text as T

import qualified Text.Blaze.Html5.Attributes as A hiding (title, rows, accept)
import           Text.Blaze.Html5 as H

import           Bead.View.Content hiding (BlazeTemplate, template)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.DataBridge as DataBridge

backToLogin msg =
  Bootstrap.rowCol4Offset4 $ Bootstrap.buttonGroupJustified $
    Bootstrap.buttonLink "/" $ msg $ msg_Registration_GoBackToLogin "Back to login"

registrationTitle msg =
  Bootstrap.rowCol4Offset4 $ h2 $
    fromString $ msg $ msg_Registration_Title "Registration"

registrationFirstStep :: IHtml
registrationFirstStep = do
  msg <- getI18N
  return $ do
    registrationTitle msg
    Bootstrap.rowCol4Offset4 $ postForm "/reg_request" ! (A.id . formId $ regForm) $ do
      Bootstrap.formGroup
        (T.pack . msg $ msg_Registration_Username "Username")
        (Bootstrap.textInput (T.pack $ DataBridge.name regUsernamePrm) Bootstrap.PlainText Nothing)
        []
      Bootstrap.formGroup
        (T.pack . msg $ msg_Registration_Email "Email")
        (Bootstrap.textInput (T.pack $ DataBridge.name regEmailPrm) Bootstrap.PlainText Nothing)
        []
      Bootstrap.formGroup
        (T.pack $ msg $ msg_Registration_FullName "Full name")
        (Bootstrap.textInput (T.pack $ DataBridge.name regFullNamePrm) Bootstrap.PlainText Nothing)
        []
      Bootstrap.submitButton (fieldName regSubmitBtn) (msg $ msg_Registration_SubmitButton "Registration")
    backToLogin msg

registrationFirstStepEmailSent :: IHtml
registrationFirstStepEmailSent = do
  msg <- getI18N
  return $ do
    registrationTitle msg
    Bootstrap.rowCol4Offset4 $ p $
      fromString $ msg $ msg_RegistrationTokenSend_Title "The registration token has been sent in email, it shall arrive soon."
    backToLogin msg

-- The second step f the registration
-- registrationPasswordStep :: IHtml
registrationPasswordStep utcZoneInfo timeZones key language username token = do
  msg <- getI18N
  return $ do
    registrationTitle msg
    Bootstrap.rowCol4Offset4 $ postForm "/reg_final" ! (A.id . formId $ regFinalForm) $ do
      Bootstrap.formGroup
        (T.pack . msg $ msg_RegistrationFinalize_Password "Password")
        (Bootstrap.textInput (T.pack $ DataBridge.name regPasswordPrm) Bootstrap.Password Nothing)
        []
      Bootstrap.formGroup
        (T.pack . msg $ msg_RegistrationFinalize_PwdAgain "Password again")
        (Bootstrap.textInput (T.pack $ DataBridge.name regPasswordAgainPrm) Bootstrap.Password Nothing)
        []
      Bootstrap.selectionWithLabel'
        (DataBridge.name regTimeZonePrm)
        (msg $ msg_RegistrationFinalize_Timezone "Time zone")
        (==utcZoneInfo)
        timeZones
      hiddenParam regUserRegKeyPrm key
      hiddenParam regTokenPrm      token
      hiddenParam regUsernamePrm   username
      hiddenParam regLanguagePrm   language
      Bootstrap.submitButton (fieldName regSubmitBtn) (msg $ msg_RegistrationFinalize_SubmitButton "Register")
    backToLogin msg

  where
    hiddenParam parameter value = hiddenInput (DataBridge.name parameter) (DataBridge.encode parameter value)
