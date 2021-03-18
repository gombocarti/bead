{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Login (
    loginSubmit
  ) where

import qualified Control.Exception as E
import           Data.ByteString.Char8 as B hiding (index, putStrLn)
import           Data.Char
import           Data.Either (isLeft, isRight)
import           Data.Maybe
import           Data.String (fromString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Typeable as Type

#ifdef SSO
import           Network.Security.Kerberos (krb5Login, KrbException(KrbException))
#endif
import qualified Text.Blaze.Html5 as H
import           Prelude as P
import           Text.Printf

import qualified Bead.Config as Config
import           Bead.Controller.Logging as L
import qualified Bead.Controller.Pages as P
import           Bead.Controller.ServiceContext hiding (serviceContext)
import qualified Bead.Controller.UserStories as Story
#ifdef SSO
import           Bead.Daemon.LDAP.Result
#endif
import           Bead.Domain.Entities (u_role)
import           Bead.Domain.Relationships (HomePageContents)
import qualified Bead.Domain.Relationships as R
import           Bead.View.BeadContext
import           Bead.View.Common
import           Bead.View.Content hiding (BlazeTemplate, template)
import qualified Bead.View.Content.Public.Index as I
import           Bead.View.ContentHandler
import           Bead.View.ErrorPage

-- * Login handler

loginSubmit :: ContentHandler PageContents
loginSubmit = do
  cfg <- beadHandler getConfiguration
  principal <- getParameter principalPrm
  correctCredentials <-
    if (Config.sSODeveloperMode $ Config.loginConfig cfg)
      then return True
      else do
        password <- getParameter loginPasswordPrm
        let encodeUtf8 = TE.encodeUtf8 . T.pack
            login = krb5Login (encodeUtf8 principal) (encodeUtf8 password)
        liftIO (E.try login) >>= either (\exception -> logLoginError principal exception >> return False) (const $ return True) 
  if correctCredentials
    then do
      let username = usernameFromPrincipal principal
      lResult <- beadHandler $ ldapQuery username
      ldapResult
        (ldapError username)
        (ldapInvalidUser username)
        (ldapAttrMapError username)
        (ldapUser username)
        lResult
    else
      beadHandler (I.index (Just IncorrectUserOrPassword)) >>= setPageContents
  where
    usernameFromPrincipal :: String -> Username
    usernameFromPrincipal = Username . nameComponents
     where
       nameComponents :: String -> String
       nameComponents "" = ""
       nameComponents ('\\':c:cs) = '\\' : c : nameComponents cs
       nameComponents ('@':_) = ""
       nameComponents (c:cs) = c : nameComponents cs

    logLoginError :: String -> KrbException -> ContentHandler ()
    logLoginError principal (KrbException code errorMessage) = beadHandler $
      logMessage INFO $ join ["[Login] Login with principal ", principal, " failed. Code: ", show (fromIntegral code :: Int), ", reason: ", B.unpack errorMessage]

    -- Falls back to local credentials
    ldapError :: Username -> String -> ContentHandler PageContents
    ldapError username msg = do
      beadHandler $ logMessage ERROR $ join ["[LDAP] Query failed, falling back to normal login for ", usernameCata id username, ", reason: ", msg]
      beadLogin username

    ldapInvalidUser :: Username -> ContentHandler PageContents
    ldapInvalidUser username = beadHandler $ do
      logMessage ERROR $ join ["[LDAP] Invalid user: ", usernameCata id username]
      Right <$> I.index (Just IncorrectUserOrPassword)

    -- Logs error and authenticates with the fallback
    ldapAttrMapError :: Username -> ContentHandler PageContents
    ldapAttrMapError username = do
      beadHandler $ logMessage ERROR $ join ["[LDAP] Attributes cannot be mapped, falling back to normal login for ", usernameCata id username]
      beadLogin username

    ldapUser :: Username -> (Uid, Email, String) -> ContentHandler PageContents
    ldapUser ldapUsername (uid,email,name) = do
      -- Check if the user exist
      let username = ldapUsername
      let usernameStr = usernameCata id username
      let user role timezone lang = User role ldapUsername email name timezone lang uid
      existInDB <- registrationStory (Story.doesUserExist username)
      if existInDB
        then do
          beadHandler $ logMessage INFO $ join [usernameStr, " has registration in DB."]
          -- If the user exists then update its profile
          i18n <- i18nE
          beadUser <- registrationStory (Story.loadUser username)
          registrationStory $ Story.updateUser $ user (u_role beadUser) (u_timezone beadUser) (u_language beadUser)
        else do
          -- If the user does not exist, create a user with the given profile related informations
          beadHandler $ logMessage INFO $ join [usernameStr, " has no registration in DB. Registration started."]
          createUserInPersist user
      beadLogin username

    -- Tries to log in the user
    beadLogin :: Username -> ContentHandler PageContents
    beadLogin username = do
      -- Force login on the user
        user <- userStory $ Story.login username
        beadHandler $
          logMessage INFO $ show username ++ " successfully logged in"
        redirectTo $ P.homePageToPageDesc (defaultHomePage $ u_role user)

    -- Creates user in persistent with the default time zone and language
    -- from the session
    createUserInPersist :: (Role -> TimeZoneName -> Language -> User) -> ContentHandler ()
    createUserInPersist user = do
      timezone <- fmap getTimeZone $ beadHandler getConfiguration
      lang <- userLanguage
      let userToCreate = user Student timezone lang
      registrationStory (Story.createUser $ user Student timezone lang)
      let usernameStr = usernameCata id $ u_username userToCreate
      beadHandler $ logMessage INFO $ join [usernameStr, " is registered in persistent."]
      where
        getTimeZone = TimeZoneName . Config.defaultRegistrationTimezone
