{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings  #-}
module Bead.View.Login (
    login
  , loginSubmit
  ) where

import           Data.ByteString.Char8 as B hiding (index, putStrLn)
import           Data.Char
import           Data.Either (isLeft)
import           Data.Maybe
import           Data.String (fromString)
import qualified Data.Text as Text
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
import           Bead.View.BeadContext
import           Bead.View.Common
import           Bead.View.RouteOf (homePath)
import           Bead.View.Content hiding (BlazeTemplate, template)
import qualified Bead.View.Content.Public.Login as View
import           Bead.View.ContentHandler
import           Bead.View.Headers (getHeaders)
import           Bead.View.Headers.AcceptLanguage
import           Bead.View.ErrorPage

-- * Login and Logout handlers

login :: Maybe AuthFailure -> BeadHandler' b H.Html
#ifdef SSO
login authError = do
  bootstrapPublicPage defaultPageSettings . publicFrame $ do
    msg <- getI18N
    View.login authError
#else
login authError = do
  languages <- setDefaultLanguage
  bootstrapPublicPage defaultPageSettings . publicFrame $ do
    msg <- getI18N
    View.login authError languages
#endif

loginSubmit :: ContentHandler (PageContents H.Html)
#ifdef SSO
loginSubmit = do
  cfg <- beadHandler getConfiguration
  username <-
    if (Config.sSODeveloperMode $ Config.loginConfig cfg)
      then getParameter loginUsernamePrm
      else do
        headers <- beadHandler $ getHeaders "X-Forwarded-User" <$> getRequest
        case headers of
          [hs] -> return (Username $ toUpper <$> unpack hs)
          other     -> do
            beadHandler $ logMessage ERROR $ join ["[FORWARDED USER] Forwarded user is not unique, but: ", reason]
            i18n <- i18nE
            throwError $ contentHandlerError $ i18n $ msg_Login_Error_NoUser "User is unknown"
            where
              reason = if P.null other then "nothing found" else show other

  lResult <- beadHandler $ ldapQuery username
  ldapResult
    (ldapError username)
    (ldapInvalidUser username)
    (ldapAttrMapError username)
    (ldapUser username)
    lResult
  where
    -- Falls back to local credentials
    ldapError :: Username -> String -> ContentHandler (PageContents H.Html)
    ldapError username msg = do
      beadHandler $ logMessage ERROR $ join ["[LDAP] Query failed, falling back to normal login for ", usernameCata id username, ", reason: ", msg]
      beadLogin username

    ldapInvalidUser :: Username -> ContentHandler (PageContents H.Html)
    ldapInvalidUser username = beadHandler $ do
      logMessage ERROR $ join ["[LDAP] Invalid user: ", usernameCata id username]
      Right <$> login (Just IncorrectUserOrPassword)

    -- Logs error and authenticates with the fallback
    ldapAttrMapError :: Username -> ContentHandler (PageContents H.Html)
    ldapAttrMapError username = do
      beadHandler $ logMessage ERROR $ join ["[LDAP] Attributes cannot be mapped, falling back to normal login for ", usernameCata id username]
      beadLogin username

    ldapUser :: Username -> (Uid, Email, String) -> ContentHandler (PageContents H.Html)
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
    beadLogin :: Username -> ContentHandler (PageContents H.Html)
    beadLogin username = do
      -- Force login on the user
        userStory $ Story.login username
        beadHandler $
          logMessage INFO $ show username ++ " successfully logged in"
        redirectTo $ P.home ()

    -- Creates user in persistent with the default timezone and language
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
#else
loginSubmit = withTop auth $ handleError $ runErrorT $ do
  user <- getParameter loginUsernamePrm
  pwd  <- getParameter loginPasswordPrm
  loggedIn <- lift $ loginByUsername
    (usernameCata Text.pack user)
    (ClearText . pack $ pwd)
    False
  case loggedIn of
    Left failure -> lift . login $ Just failure
    Right authUser -> lift $ do
      context <- getServiceContext
      token   <- sessionToken
      let unameFromAuth = usernameFromAuthUser authUser
          mpasswFromAuth = passwordFromAuthUser authUser
      case mpasswFromAuth of
        Nothing -> do logMessage ERROR "No password was given"
                      Auth.logout
        Just _passwFromAuth -> do
          i18n <- i18nH
          result <- liftIO $ Story.runUserStory context i18n UserNotLoggedIn $ do
            Story.login unameFromAuth token
            Story.currentUser
          case result of
            Left err -> do
              logMessage ERROR $ "Error happened processing user story: " ++ Story.translateUserError trans err
              -- Service context authentication
              liftIO $ (userContainer context) `userLogsOut` (userToken (unameFromAuth, token))
              Auth.logout
              commitSessionTop
              translationErrorPage
                (msg_Login_PageTitle "Login")
                (msg_Login_InternalError
                   "Some internal error happened, please contact the administrators.")
            Right (user,userState) -> do
              initSessionValues (page userState) unameFromAuth (u_language user)
              commitSessionTop
              redirect "/"
  return ()
  where
    handleError m =
      m >>= (either (login . Just . AuthError . contentHandlerErrorMsg) (const $ return ()))

    initSessionValues :: P.PageDesc -> Username -> Language -> BeadHandler' b ()
    initSessionValues page username language = do
        setSessionVersion
        setLanguageInSession language
        setUsernameInSession username

        logMessage DEBUG $ "User's actual page is set in session to: " ++ show page
#endif
