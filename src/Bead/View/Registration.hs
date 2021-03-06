{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Registration (
    createAdminUser
  , createStudentUser
  , doesUserExist
  , changeUserPassword
#ifndef SSO
  , registrationRequest
  , finalizeRegistration
#endif
  ) where

{-
This module represents the registration pages. The registration process has two stages.
1) The user tries to register on a form, with the future username, email address and full name.
2) The user opens the recieved mail and sets her password.
-}

import qualified Data.ByteString.Char8 as B
import           Data.Either (isLeft)
import           Data.Maybe (fromJust, isJust, isNothing)
import           Data.String (fromString)
import           Data.Time hiding (TimeZone, timeZoneName)
import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8, decodeUtf8)

import           Snap hiding (Config(..), get)
import           Snap.Snaplet.Auth as A hiding (createUser)
import           Snap.Snaplet.Auth.Backends.JsonFile (mkJsonAuthMgr)
import           Snap.Snaplet.Session

import           Bead.Controller.Logging
import qualified Bead.Controller.UserStories as S
import           Bead.Config
import           Bead.Domain.TimeZone (utcZoneInfo)
import qualified Bead.Persistence.Persist as Persist
import           Bead.View.BeadContext
import           Bead.View.Content
import qualified Bead.View.Content.Public.Registration as View
import           Bead.View.ContentHandler
import           Bead.View.DataBridge as DataBridge
#ifdef EmailEnabled
import           Bead.View.EmailTemplate
#endif
import           Bead.View.ErrorPage
import           Bead.View.RouteOf (queryString)

createUser :: Persist.Interpreter -> User -> String -> IO ()
createUser persist user _password = do
  let name = usernameCata id $ u_username user
  Persist.runPersist persist $ Persist.saveUser user
  return ()

-- Checks if the user exists in the Snap Auth module
doesUserExist :: Persist.Interpreter -> Username -> IO Bool
doesUserExist persist username =
  isLeft <$> Persist.runPersist persist (Persist.doesUserExist username)

changeUserPassword :: Username -> String -> IO ()
changeUserPassword _username _password = return ()

createUserWithRole :: Bead.View.Content.Role -> Persist.Interpreter -> UserRegInfo -> IO ()
createUserWithRole role persist = userRegInfoCata $
  \name uid password email fullName timeZone ->
    let usr = User {
        u_role = role
      , u_username = Username name
      , u_email = Email email
      , u_name = fullName
      , u_timezone = timeZone
      , u_language = Language "hu" -- TODO: I18N
      , u_uid = Uid uid
      }
    in createUser persist usr password

createAdminUser   = createUserWithRole Admin
createStudentUser = createUserWithRole Student

-- * User registration handler

data RegError
  = RegError LogLevel String
  | RegErrorUserExist Username

readParameter :: (MonadSnap m) => Parameter a -> m (Maybe a)
readParameter param = do
  reqParam <- getParam . encodeUtf8 . DataBridge.name $ param
  return (reqParam >>= decode param . T.unpack . decodeUtf8)

#ifndef SSO
{-
User registration request
- On GET request it renders the HTML registration form with
  username, email address, and full name input fields, that
  has the proper JavaScript validation method.
- On POST request it validates the input fields.
  It the input field values are incorrect renders the error page, otherwise
  runs the User story to create a UserRegistration
  data in the persistence layer, after send the information via email
-}
registrationRequest :: Config -> BeadHandler ()
registrationRequest config = method GET renderForm <|> method POST saveUserRegData where

  -- Creates a timeout days later than the given time
  timeout :: Integer -> UTCTime -> UTCTime
  timeout days = addUTCTime (fromInteger (60 * 60 * 24 * days))

  createUserRegData :: Username -> Email -> String -> IO UserRegistration
  createUserRegData user email name = do
    now <- getCurrentTime
    -- TODO random token
    return $ UserRegistration {
      reg_username = usernameCata id user
    , reg_email    = emailFold    id email
    , reg_name     = name
    , reg_token    = "token"
    , reg_timeout  = timeout 2 now
    }

  renderForm = renderBootstrapPublicPage View.registrationFirstStep

  saveUserRegData = do
    u <- checkUsernamePrm
    e <- readParameter regEmailPrm
    f <- readParameter regFullNamePrm

    renderPage $ do
      i18n <- lift i18nH
      case (u,e,f) of
        (Left l, _, _) -> throwSError (translateMessage i18n l)
        (Right username, Just email, Just fullname) -> do
            exist <- lift $ registrationStory (S.doesUserExist username)
            when (isLeft exist) . throwSError . i18n $
              msg_Registration_HasNoUserAccess "No access allowed to the user's data."
            when (fromRight exist) . throwSError . i18n $
              msg_Registration_UserAlreadyExists "User already exists."
            userRegData <- liftIO $ createUserRegData username email fullname
            result <- lift $ registrationStory (S.createUserReg userRegData)
            when (isLeft result) . throwSError . i18n $
              msg_Registration_RegistrationNotSaved "The registration has not been saved."
            lang <- lift languageFromSession
            let language = maybe (Language "en") id lang
            let key = fromRight result
            lift $
              sendEmail
                email
                (i18n $ msg_Registration_EmailSubject "BE-AD: Registration")
                (i18n $ msg_Registration_EmailBody registrationEmailTemplate)
                RegTemplate {
                    regUsername = reg_username userRegData
                  , regUrl = createUserRegAddress key language userRegData
                  }
            lift $ renderBootstrapPublicPage View.registrationFirstStepEmailSent
        _ -> throwSError . i18n $
               msg_Registration_RequestParameterIsMissing "Some request parameter is missing."

      where
        throwSError :: (Monad m) => String -> ErrorT String m a
        throwSError = throwError

  createUserRegAddress :: UserRegKey -> Language -> UserRegistration -> Text
  createUserRegAddress key (Language language) reg =
    -- TODO: Add the correct address of the server
    queryString (join [emailHostname config, "/reg_final"])
                 [ requestParameter regUserRegKeyPrm key
                 , requestParameter regTokenPrm      (reg_token reg)
                 , requestParameter regUsernamePrm   (Username . reg_username $ reg)
                 , requestParameter regLanguagePrm   language
                 ]

  -- Calculates the result of an (ErrorT String ...) transformator and
  -- returns the (Right x) or renders the error page with the given error
  -- message in (Left x)
  renderPage :: (Error e, ErrorPage e) => ErrorT e (BeadHandler' b) () -> BeadHandler' b ()
  renderPage m = do
    x <- runErrorT m
    either registrationErrorPage return x

-- Checks the username given in the registration form, and validate it against
-- the required form.
checkUsernamePrm = do
  username <- readParameter regUsernamePrm
  case username of
    Nothing -> return . Left . TransMsg $ msg_RegistrationFinalize_NoRegistrationParametersAreFound "No registration parameters found."
    Just u -> do
      isValid <- checkUsername $ usernameCata id u
      if isValid
        then return $ Right u
        else do
          config <- getConfiguration
          return . Left $ TransPrmMsg
            (msg_Registration_InvalidUsername "The username is not valid. Try something similar: %s")
            (usernameRegExpExample $ loginConfig config)

{-
Registration finalization
- On GET request: The user gets and email from the BE-AD this email contains
  the necessary code and token in for to finalize the registration
  The system reads the UserREgistration data and decides that the registration
  can go on, this depends on the factor, the first, that the user is not registered yet
  and the registration time limit has not passed yet.
  If the registration is not permitted the system renders the error page, otherwise
  a page where the user can enter the desired password. The password field is validated
  by JavaScript.
- On POST request the desired password is validated by server side too, if the validation
  is passed than the user registration happens. If any error occurs during the registration
  an error page is shown, otherwise the page is redirected to "/"
-}
finalizeRegistration :: BeadHandler ()
finalizeRegistration = method GET renderForm <|> method POST createStudent where

  readRegParameters = do
    username <- checkUsernamePrm
    key      <- readParameter regUserRegKeyPrm
    token    <- readParameter regTokenPrm
    language <- readParameter regLanguagePrm
    case (key, token, username, language) of
      (Just k, Just t, Right u, Just l) -> return $ Right (k,t,u,l)
      (Just _, Just _, Left e , Just _) -> return $ Left e
      _ -> return . Left . TransMsg $ msg_RegistrationFinalize_NoRegistrationParametersAreFound "No registration parameters found."

  renderForm = do
    values <- readRegParameters
    case values of
      Left err -> registrationErrorPage err
      Right (key, token, username, language) -> do
        result <- registrationStory $ do
                    userReg   <- S.loadUserReg key
                    existence <- S.doesUserExist username
                    return (userReg, existence)
        setLanguageInSession (Language language)
        commitSessionTop
        i18n <- i18nH
        case result of
          Left e -> registrationErrorPage $ S.translateUserError i18n e
          Right (userRegData,exist) -> do
            -- TODO: Check username and token values
            now <- liftIO $ getCurrentTime
            case (reg_timeout userRegData < now, exist) of
              (True , _) -> errorPage
                (msg_Registration_Title "Registration")
                (i18n $ msg_RegistrationFinalize_InvalidToken "The registration token has expired, start the registration over.")
              (False, True) -> errorPage
                (msg_Registration_Title "Registration")
                (i18n $ msg_RegistrationFinalize_UserAlreadyExist "This user already exists.")
              (False, False) -> do
                timeZones <- map (\t -> (t, timeZoneName id t)) <$> foundTimeZones
                renderBootstrapPublicPage $
                  View.registrationPasswordStep utcZoneInfo timeZones key token username language

  hiddenParam parameter value = hiddenInput (DataBridge.name parameter) (DataBridge.encode parameter value)

  createStudent = do
    values <- readRegParameters
    pwd    <- readParameter regPasswordPrm
    tz     <- readParameter regTimeZonePrm
    msg <- i18nH
    case (values, pwd, tz) of
      (Left err,_,_) -> registrationErrorPage err
      (Right (key, _token, _username, language), Just _password, Just timezone) -> do
        result <- registrationStory (S.loadUserReg key)
        case result of
          Left _e -> errorPage (msg_Registration_Title "Registration") $ msg $
            msg_RegistrationCreateStudent_InternalError "Some internal error happened."
          Right userRegData -> do
            now <- liftIO getCurrentTime
            -- TODO: Check username and token values (are the same as in the persistence)
            case (reg_timeout userRegData < now) of
              True -> errorPage (msg_Registration_Title "Registration") $ msg $
                msg_RegistrationCreateStudent_InvalidToken "The registration token has expired, start the registration over."
              False -> do
                withTop auth $
                  createNewUser userRegData timezone (Language language)
                redirect "/"
      (Right _, _, _) -> registrationErrorPage ("Password or timezone is not defined" :: String)

createNewUser :: UserRegistration -> TimeZoneName -> Language -> BeadHandler' (AuthManager BeadContext) (Either RegError ())
createNewUser reg timezone language = runErrorT $ do
  -- Check if the user is exist already
  userExistence <- checkFailure =<< lift (registrationStory (S.doesUserExist username))
  when userExistence . throwError $ (RegErrorUserExist username)

  -- Registers the user in the Snap authentication module
  lift $ registerUser (B.pack $ DataBridge.name regUsernamePrm) (B.pack $ DataBridge.name regPasswordPrm)
  let user = User {
      u_role = Student
    , u_username = username
    , u_email = email
    , u_name = fullname
    , u_timezone = timezone
    , u_language = language
    , u_uid = usernameCata Uid username
    }

  -- Check if the Snap Auth registration went fine
  createdUser <- lift $ withBackend $ \r -> liftIO $ lookupByLogin r (usernameCata T.pack username)
  when (isNothing createdUser) . throwError . RegError ERROR $ "User was not created in the Snap Auth module"
  let snapAuthUser = fromJust createdUser
  when (isNothing . passwordFromAuthUser $ snapAuthUser) . throwError . RegError ERROR $ "Snap Auth: no password is created"
  let snapAuthPwd = fromJust . passwordFromAuthUser $ snapAuthUser

  -- Creates the user in the persistence layer
  checkFailure =<< lift (registrationStory (S.createUser user))
  return ()

  where
    username = Username . reg_username $ reg
    email    = Email . reg_email $ reg
    fullname = reg_name reg

    -- Checks if the result of a story is failure, in the case of failure
    -- it throws an exception, otherwise lift's the result into the monadic
    -- calculation
    checkFailure (Left _)  = throwError . RegError ERROR $ "User story failed"

    checkFailure (Right x) = return x

registrationErrorPage :: (ErrorPage e) => e -> BeadHandler' b ()
registrationErrorPage = errorPage (msg_Registration_Title "Registration")

-- * Tools

-- Return the value from Right x otherwise throws a runtime error
fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight (Left _)  = error "fromRight: left found"


-- * Email template

registrationEmailTemplate :: String
registrationEmailTemplate = unlines
  [ "Dear future user,"
  , ""
  , "This email was sent to you, because somebody has tried register to our BE-AD"
  , "service with your email address as \"{{regUsername}}\".  Please confirm this"
  , "intention by following the link below:"
  , ""
  , "{{regUrl}}"
  , ""
  , "Thanks,"
  , "The administrators"
  , ""
  , "PS: If you received this email by accident, or you do not want to register"
  , "    any more, feel free to ignore this message."
  ]

#endif
