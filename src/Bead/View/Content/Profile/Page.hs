{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Profile.Page (
    profile
  , changePassword
  ) where

import           Control.Monad.Trans.Class
import           Control.Arrow ((&&&))
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as T

import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (currentUser)
import qualified Bead.Controller.UserStories as Story
import           Bead.Domain.Entities hiding (name)
import           Bead.View.Content hiding (name, option)
import           Bead.View.ContentHandler (setUserLanguage)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.View.DataBridge as B
import           Bead.View.Dictionary

#ifndef SSO
import           Bead.View.ResetPassword
#endif

profile = ViewModifyHandler profilePage changeUserDetails

profilePage :: GETContentHandler
profilePage = do
  user <- userStory currentUser
  languages <- getDictionaryInfos
  ts <- beadHandler foundTimeZones
  setPageContents $ htmlPage (msg_LinkText_Profile "Profile") $
    profileContent ts user languages

changeUserDetails :: POSTContentHandler
changeUserDetails = do
  name <- getParameter regFullNamePrm
  tz <- getParameter userTimeZonePrm
  lang <- getParameter userLanguagePrm
  return $ Action $ do
    Story.changeUserDetails name tz lang
    return $ redirection $ Pages.profile ()

#ifdef SSO
changePassword = ModifyHandler $
  return $ Action $ do
    Story.logErrorMessage "With single sign-on, one cannot change password from the Profile page."
    return $ redirection $ Pages.profile ()
#else
changePassword = ModifyHandler $ do
  oldPwd <- getParameter oldPasswordPrm
  newPwd <- getParameter newPasswordPrm
  checkCurrentAuthPassword oldPwd
  updateCurrentAuthPassword newPwd
  return Action $ do
    Story.putStatusMessage $ msg_Profile_PasswordHasBeenChanged "The password has been changed."
    return $ redirection $ Pages.profile ()
#endif

profileContent :: [TimeZoneName] -> User -> DictionaryInfos -> IHtml
profileContent ts user ls = do
  msg <- getI18N
  return $ do
    Bootstrap.row $ do
      -- User Details
      userDetailsCol $ postForm (routeOf profile) $ do
        profileFields msg
        Bootstrap.submitButton (fieldName changeProfileBtn) (msg $ msg_Profile_SaveButton "Save")
      passwordSection msg

  where
    regFullNameField  = B.name regFullNamePrm
    userLanguageField = B.name userLanguagePrm
    userTimeZoneField = B.name userTimeZonePrm
    fullName          = T.pack $ u_name user

    userDetailsCol =
#ifdef SSO
      Bootstrap.colMd12
#else
      Bootstrap.colMd6
#endif

    passwordSection msg = do
#ifdef SSO
      return ()
#else
      let oldPasswordField = fromString $ B.name oldPasswordPrm
      let newPasswordField = fromString $ B.name newPasswordPrm
      let newPasswordAgain = fromString $ B.name newPasswordAgainPrm
      Bootstrap.colMd6 $ postForm (routeOf changePassword) `withId` (rFormId changePwdForm) $ do
        Bootstrap.passwordInput oldPasswordField (msg $ msg_Profile_OldPassword "Old password: ")
        Bootstrap.passwordInput newPasswordField (msg $ msg_Profile_NewPassword "New password: ")
        Bootstrap.passwordInput newPasswordAgain (msg $ msg_Profile_NewPasswordAgain "New password (again): ")
        Bootstrap.submitButton (fieldName changePasswordBtn) (msg $ msg_Profile_ChangePwdButton "Update")
#endif

    profileFields msg = do
#ifdef SSO
      Bootstrap.readOnlyTextInputWithDefault "" (msg $ msg_Profile_User "Username") (usernameCata fromString $ u_username user)
      Bootstrap.readOnlyTextInputWithDefault "" (msg $ msg_Profile_Email "Email") (emailCata fromString $ u_email user)
      Bootstrap.readOnlyTextInputWithDefault "" (msg $ msg_Profile_FullName "Full name") fullName
      hiddenInput regFullNameField (u_name user)
      Bootstrap.selectionWithLabel userLanguageField (msg $ msg_Profile_Language "Language") (== u_language user) languages
      Bootstrap.selectionWithLabel userTimeZoneField (msg $ msg_Profile_TimeZone "Time zone") (== u_timezone user) timeZones
#else
      Bootstrap.labeledText (msg $ msg_Profile_User "Username") (usernameCata fromString $ u_username user)
      Bootstrap.labeledText (msg $ msg_Profile_Email "Email") (emailCata fromString $ u_email user)
      Bootstrap.textInputWithDefault regFullNameField (msg $ msg_Profile_FullName "Full name") fullName
      Bootstrap.selectionWithLabel userLanguageField (msg $ msg_Profile_Language "Language") (== u_language user) languages
      Bootstrap.selectionWithLabel userTimeZoneField (msg $ msg_Profile_TimeZone "Time zone") (== u_timezone user) timeZones
#endif

    timeZones = map (Prelude.id &&& timeZoneName T.pack) ts
    languages = map langValue ls
    profile = Pages.profile ()
    changePassword = Pages.changePassword ()

    langValue (lang,info)  = (lang, T.pack $ languageName info)
