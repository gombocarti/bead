{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.View.BeadContext where

import           Prelude hiding (log)
import           Control.Applicative
import           Control.Monad.IO.Class
import           Control.Monad.State

import           Control.Lens.TH
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Char (isAlphaNum, toUpper)
import           Data.IORef
import qualified Data.HashMap.Strict as Map

import           Data.Maybe (maybe)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Time (UTCTime)
import qualified Data.Time as Time
import qualified Data.Time.Calendar as Cal
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Network.Mail.Mime
import           System.Random
import           Text.Regex.TDFA

import           Snap hiding (Config(..), getCookie)
import qualified Snap
import           Snap.Snaplet.Fay

import           Bead.Config as Config
import           Bead.Controller.Logging
import           Bead.Controller.ServiceContext (ServiceContext, UserState)
import qualified Bead.Controller.ServiceContext as SC
#ifdef EmailEnabled
import           Bead.Daemon.Email as EmailDaemon
#endif
#ifdef SSO
import           Bead.Daemon.LDAP as LDAPDaemon
#endif
import           Bead.Domain.Entities
import           Bead.Domain.TimeZone
import qualified Bead.View.AuthToken as Auth
import           Bead.View.Dictionary
#ifdef EmailEnabled
import           Bead.View.EmailTemplate
#endif
import           Bead.View.Logger (SnapLogger)
import qualified Bead.View.Logger as SnapLogger

-- * Mini snaplet : Service context

newtype SnapContext c = SnapContext (IORef c)

makeSnapContext :: String -> String -> c -> SnapletInit b (SnapContext c)
makeSnapContext name desc c = makeSnaplet
  (fromString name)
  (fromString desc)
  Nothing $
  liftIO $ do
    ref <- newIORef c
    return $! SnapContext ref

snapContextCata :: (c -> a) -> Handler b (SnapContext c) a
snapContextCata f = do
  SnapContext ref <- get
  f <$> (liftIO $ readIORef ref)

snapContextHandlerCata :: (c -> Handler b (SnapContext c) a) -> Handler b (SnapContext c) a
snapContextHandlerCata k = do
  SnapContext ref <- get
  (liftIO $ readIORef ref) >>= k

type ConfigServiceContext = SnapContext Config

configurationServiceContext :: Config -> SnapletInit b ConfigServiceContext
configurationServiceContext = makeSnapContext
  "Configuration"
  "A snaplet providin the service context of the configuration"


type SnapletServiceContext = SnapContext ServiceContext

contextSnaplet :: ServiceContext -> SnapletInit b SnapletServiceContext
contextSnaplet = makeSnapContext
  "ServiceContext"
  "A snaplet providing the service context of the user stories"

-- * Mini snaplet : Dictionary snaplet

-- | Available dictionaries and the default configured language
type DictionaryContext = SnapContext (Dictionaries, Language)

-- Create a Dictionary context from the given dictionaries and a defualt language
dictionarySnaplet :: Dictionaries -> Language -> SnapletInit a DictionaryContext
dictionarySnaplet d l = makeSnapContext
  "Dictionaries"
  "A snaplet providing the i18 dictionary context"
  ((addDefault d), l)
  where
    -- The source code contains english comments by default
    addDefault = Map.insertWith (\_new old -> old) (Language defaultLanguage)
      (idDictionary, defaultDictionaryInfo)

-- A dictionary infos is a list that contains the language of and information
-- about the dictionaries contained by the DictionarySnaplet
type DictionaryInfos = [(Language, DictionaryInfo)]

dictionaryInfosCata list item d = list $ map item d

#ifdef EmailEnabled
-- * Email sending snaplet

type Subject = String -- The subject of an email message
type Message = String -- The content of an email message

-- Email sender function get a string and en email address
-- and sends the email to the address
type EmailSender = Email -> Subject -> Message -> IO ()

-- SendEmailContext is a reference to the email sender function, we keep only
-- one of the email senders.
type SendEmailContext = SnapContext EmailSender

verySimpleMail :: Address -> Address -> T.Text -> TL.Text -> IO Mail
verySimpleMail to from subject plainBody = do
  return Mail
        { mailFrom = from
        , mailTo   = [to]
        , mailCc   = []
        , mailBcc  = []
        , mailHeaders = [ ("Subject", subject) ]
        , mailParts =
            [[ Part "text/plain; charset=UTF-8" Base64 Nothing []
             $ BL.pack . TL.unpack $ plainBody
            ]]
        }

emailSenderSnaplet :: Config -> EmailDaemon -> SnapletInit a SendEmailContext
emailSenderSnaplet config daemon = makeSnapContext
  "Email sending"
  "A snaplet providing email sender functionality"
  sender
  where
    sender :: Email -> Subject -> Message -> IO ()
    sender address sub msg = do
      let from = Address Nothing (fromString $ emailFromAddress config)
          to   = Address Nothing (emailFold fromString address)
          subject = fromString sub
          plain   = fromString msg
      mail <- verySimpleMail to from subject plain
      EmailDaemon.sendEmail daemon mail
#endif

-- * Bead's temp directory

-- Bead temp directory holds a reference to the created temp directory
-- where the handlers can place temporary files
type TempDirectoryContext = SnapContext FilePath

tempDirectorySnaplet :: FilePath -> SnapletInit a TempDirectoryContext
tempDirectorySnaplet = makeSnapContext
  "Template directory"
  "A snaplet holding a reference to the temporary directory"

-- * Username check

-- Checks the given username against some IO computation based checker
type CheckUsernameContext = SnapContext (String -> IO Bool)

regexpUsernameChecker :: Config -> SnapletInit a CheckUsernameContext
regexpUsernameChecker cfg = makeSnaplet
  "Regexp username checker"
  "A snaplet providing checks against the regular expression defined in the configuration file"
  Nothing $ liftIO $ checkUsername
    where
      checkUsername = do
#ifdef SSO
        ref <- newIORef (\username -> return $ and [all isAlphaNum username, length username > 0])
        return $! SnapContext ref
#else
        let pattern = usernameRegExp $ loginConfig cfg
        ref <- newIORef (\username -> return $ check username pattern)
        return $! SnapContext ref
          where
            check :: String -> String -> Bool
            check usr ptn = usr =~ ptn
#endif

-- * Password generation

-- PasswordGeneratorContext is a reference to the password generator computation,
-- we keep only one of it
type PasswordGeneratorContext = SnapContext (IO String)

passwordGeneratorSnaplet :: SnapletInit a PasswordGeneratorContext
passwordGeneratorSnaplet = makeSnaplet
  "Password generation"
  "A snaplet providing password generation functionality"
  Nothing $ liftIO $ do
    pwdGen <- createPasswordGenerator
    ref <- newIORef pwdGen
    return $! SnapContext ref

-- Creates a password generator that generates 12 length passwords containing
-- upper, lowercase letters, and digits.
createPasswordGenerator :: IO (IO String)
createPasswordGenerator = do
  std <- newStdGen
  stdRef <- newIORef std
  let nextValue = do
        s <- readIORef stdRef
        let (x,s') = random s
        writeIORef stdRef s'
        return x

  let passwordGenerator = replicateM 12 $ do
        type_ <- fmap (`mod` 3) nextValue
        fmap ([lowerCase, upperCase, digit] !! type_) nextValue

  return passwordGenerator

  where
    lowerCase :: Int -> Char
    lowerCase n = ['a'..'z'] !! (mod n 26)

    upperCase :: Int -> Char
    upperCase n = ['A'..'Z'] !! (mod n 26)

    digit :: Int -> Char
    digit n = ['0'..'9'] !! (mod n 10)


-- | Represents an application context that has a debugging
-- log capacibility
type DebugLoggerContext = SnapContext SnapLogger

-- | Creates a debug logger for pointing the "./log/debug.log" file
createDebugLogger :: SnapletInit a DebugLoggerContext
createDebugLogger = makeSnaplet
  "Debug logger"
  "A snaplet holding a debug logger output"
  Nothing $ liftIO $ do
    logger <- SnapLogger.createSnapLogger "./log/debug.log"
    ref <- newIORef logger
    return $! SnapContext ref

-- * Timezone

type TimeZoneContext = SnapContext TimeZoneConverter

createTimeZoneContext :: TimeZoneConverter -> SnapletInit a TimeZoneContext
createTimeZoneContext = makeSnapContext
  "Timezone converter"
  "A snaplet holding a reference to the time zone converter functionality"

#ifdef SSO
-- * LDAP Context

newtype LDAP = LDAP {
    ldapDaemon :: LDAPDaemon  -- ^ Queries LDAP users
  }

ldap f (LDAP x) = f x

type LDAPContext = SnapContext LDAP

createLDAPContext :: LDAP -> SnapletInit a LDAPContext
createLDAPContext = makeSnapContext
  "LDAP configuration"
  "A snaplet holding a reference to the ldap configuration"
#endif

-- * Authentication

type AuthTokenContext = SnapContext Auth.AuthTokenManager

createAuthTokenContext :: Auth.AuthTokenManager -> SnapletInit a AuthTokenContext
createAuthTokenContext = makeSnapContext
  "Authentication"
  "A snaplet that manages token-based authentication"

-- * Application

data BeadContext = BeadContext {
    _authContext :: Snaplet AuthTokenContext
  , _serviceContext :: Snaplet SnapletServiceContext
  , _dictionaryContext :: Snaplet DictionaryContext
#ifdef EmailEnabled
  , _sendEmailContext   :: Snaplet SendEmailContext
#endif
  , _randomPasswordContext :: Snaplet PasswordGeneratorContext
  , _fayContext     :: Snaplet Fay
  , _tempDirContext :: Snaplet TempDirectoryContext
  , _configContext  :: Snaplet ConfigServiceContext
#ifndef SSO
  , _checkUsernameContext :: Snaplet CheckUsernameContext
#endif
  , _timeZoneContext :: Snaplet TimeZoneContext
  , _debugLoggerContext :: Snaplet DebugLoggerContext
#ifdef SSO
  , _ldapContext :: Snaplet LDAPContext
#endif
  }

makeLenses ''BeadContext

-- | Bead Context related handlers
type BeadHandler a = Handler BeadContext BeadContext a

-- | Bead Context with different view context
type BeadHandler' view = Handler BeadContext view

-- * Handlers

-- * Config

getConfiguration :: BeadHandler' b Config
getConfiguration = withTop configContext $ snapContextCata id

#ifdef SSO
-- * LDAP

-- Queries the given user and returns an LDAPResult for further processing
ldapQuery :: Username -> BeadHandler' b LDAPResult
ldapQuery username = withTop ldapContext . snapContextHandlerCata $ \l -> do
  resultEnvelope <- liftIO $ ldap (\daemon -> query daemon (usernameCata id username)) l
  liftIO resultEnvelope
#endif

-- * Timezone

getTimeZoneConverter :: BeadHandler' b TimeZoneConverter
getTimeZoneConverter = withTop timeZoneContext $ snapContextCata id

configuredDefaultTimeZone :: BeadHandler' b TimeZoneName
configuredDefaultTimeZone =
  (TimeZoneName . Config.defaultRegistrationTimezone) <$> getConfiguration

getServiceContext :: BeadHandler' b ServiceContext
getServiceContext = withTop serviceContext $ snapContextCata id

-- Calculates the default language which comes from the configuration
configuredDefaultDictionaryLanguage :: BeadHandler' b Language
configuredDefaultDictionaryLanguage = withTop dictionaryContext $ snapContextCata snd

-- | getDictionary returns a (Just dictionary) for the given language
--   if the dictionary is registered for the given language,
--   otherwise returns Nothing
getDictionary :: Language -> BeadHandler' b (Maybe Dictionary)
getDictionary l = withTop dictionaryContext $ snapContextCata (fmap fst . Map.lookup l . fst)

-- Computes a list with the defined languages and dictionary info
getDictionaryInfos :: BeadHandler' b DictionaryInfos
getDictionaryInfos = withTop dictionaryContext $ snapContextCata (Map.toList . Map.map snd . fst)

#ifdef EmailEnabled
-- Send email with a subject to the given address, using the right
-- template to the given values
-- E.g: Registration or ForgottenPassword
sendEmail :: (Template t)
  => Email -> Subject -> Message -> t -> BeadHandler' b ()
sendEmail address sub body value = withTop sendEmailContext . snapContextHandlerCata $ \send -> do
  msg <- liftIO . runEmailTemplate (emailTemplate body) $ value
  liftIO $ send address sub msg
#endif

-- Returns the bead temp directory
getTempDirectory :: BeadHandler' b FilePath
getTempDirectory = withTop tempDirContext $ snapContextCata id

#ifndef SSO
-- Returns True, if the username pass the check otherwise False
checkUsername :: String -> BeadHandler' b Bool
checkUsername usr = withTop checkUsernameContext . snapContextHandlerCata $ \f -> liftIO (f usr)
#endif

-- Generates a new password string
getRandomPassword :: BeadHandler' b String
getRandomPassword = withTop randomPasswordContext $ snapContextHandlerCata liftIO

-- | Log the message to the debug stream
debugMessage :: String -> BeadHandler' a ()
debugMessage msg = withTop debugLoggerContext . snapContextHandlerCata $ \logger ->
  liftIO (log (SnapLogger.snapLogger logger) DEBUG msg)

-- * Cookie-related functions

encryptCookie :: Auth.Cookie -> BeadHandler' a (Either T.Text B.ByteString)
encryptCookie c = withTop authContext $ do
  authMgr <- snapContextCata id
  liftIO $ Auth.encryptCookie authMgr c

setCookie :: Auth.Cookie -> BeadHandler' a (Either T.Text ())
setCookie c = do
  eEncrypted <- encryptCookie c
  case eEncrypted of
    Left encryptionError ->
      return $ Left encryptionError
    Right encrypted -> do
      now <- liftIO Time.getCurrentTime
      let sixMonths = now { Time.utctDay = Cal.addGregorianMonthsRollOver 6 (Time.utctDay now) }
      Right <$> modifyResponse (addResponseCookie (cookie encrypted sixMonths))
    where
      cookie :: B.ByteString -> UTCTime -> Snap.Cookie
      cookie contents expiration = Snap.Cookie {
          Snap.cookieName = BC.pack "token"
        , Snap.cookieValue = contents
        , Snap.cookieExpires = Just expiration
        , Snap.cookieDomain = Nothing
        , Snap.cookiePath = Nothing
        , Snap.cookieSecure = False
        , Snap.cookieHttpOnly = True
        }

decryptCookie :: B.ByteString -> BeadHandler' a (Either T.Text Auth.Cookie)
decryptCookie contents = withTop authContext $ do
  authMgr <- snapContextCata id
  liftIO $ Auth.decryptCookie authMgr contents

getCookie :: BeadHandler' v (Either T.Text Auth.Cookie)
getCookie = do
  mCookie <- Snap.getCookie (BC.pack "token")
  maybe (Right <$> defaultCookie) (decryptCookie . Snap.cookieValue) mCookie

defaultCookie :: BeadHandler' v Auth.Cookie
defaultCookie =
  Auth.NotLoggedInCookie <$> configuredDefaultDictionaryLanguage

getOrDefaultLanguage :: BeadHandler' v Language
getOrDefaultLanguage = do
  eCookie <- getCookie
  either
    (const configuredDefaultDictionaryLanguage)
    (return . Auth.cookieLanguage)
    eCookie

defaultUserState :: BeadHandler' v UserState
defaultUserState = do
  defaultLanguage <- configuredDefaultDictionaryLanguage
  return $ SC.userNotLoggedIn defaultLanguage
