{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ExistentialQuantification #-}
module Bead.View.ContentHandler (
    Action(..)
  , PageContents
  , setPageContents
  , RedirectionTarget
  , redirection
  , redirectTo
  , HtmlPage(..)
  , htmlPage
  , File
  , downloadLazy
  , downloadStrict
  , downloadText
  , beadHandler
  , logMessage
  , logMessageText
  , withUserState
  , changeUserState
  , userStory
  , registrationStory
  , getParameter
  , getParameterWithDefault
  , getParameterValues -- Calculates a list of values for the given parameter
  , getOptionalParameter -- Calculates the value of the given parameter if it is defined
  , getOptionalOrNonEmptyParameter
  , getJSONParam
  , getJSONParameters -- Calculates the json encoded parameter values into a list
  , getDictionaryInfos -- Calculates a list of language and dictionaryInfo
  , i18nE
  , i18nH
  , Mime(..)
  , mimeCata
  , pageSettings
  , modifyPageSettings
  , userState
  , userLanguage
  , setUserLanguage
  , setHomePage
  , userTimeZone
  , userTimeZoneToLocalTimeConverter
  , userTimeZoneToUTCTimeConverter
  , foundTimeZones
  , logout
  , ContentHandler
  , ContentHandler'
  , ContentError
  , contentError
  , UserTimeConverter
  , contentHandlerError
  , contentHandlerErrorMap
  , contentHandlerErrorMsg
  , module Bead.Controller.Logging
  , module Control.Monad.Except
  ) where

import           Control.Applicative
import           Control.Monad (void)
import           Control.Monad.Except
import           Control.Monad.State (StateT, gets, modify, put)
import           Data.Bifunctor (first, second)
import qualified Text.Blaze as B
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8  as BU
import qualified Data.ByteString.Lazy  as LB
import qualified Data.Map as Map (lookup)
import           Data.Maybe (isNothing, fromJust, fromMaybe)
import           Data.String (IsString(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime, LocalTime)
import qualified Data.Time as Time

import           Snap hiding (get, getCookie)
import           Snap.Blaze (blaze)
import           Snap.Util.FileUploads
import           Text.Blaze.Html5 (Html)

import           Bead.Config
import           Bead.Controller.Logging as L
import           Bead.Controller.Logging
import qualified Bead.Controller.Pages as P
import           Bead.Controller.ServiceContext (UserState)
import qualified Bead.Controller.ServiceContext as SC
import           Bead.Controller.UserStories (UserStory)
import qualified Bead.Controller.UserStories as S
import           Bead.Domain.Entities (TimeZoneName, PageSettings, Group, Course, shortGroupName, shortCourseName)
import           Bead.Domain.Relationships (GroupKey, CourseKey, HomePageContents)
import           Bead.Domain.TimeZone
import qualified Bead.View.AuthToken as Auth
import           Bead.View.BeadContext hiding (getDictionaryInfos)
import qualified Bead.View.BeadContext as BeadContext
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.DataBridge
import           Bead.View.Dictionary hiding (defaultLanguage)
import           Bead.View.Header (getCookie)
import           Bead.View.I18N (IHtml, getI18N)
import           Bead.View.RouteOf (ReqParam(..), routeOf)
import           Bead.View.Translation

import           Bead.View.Fay.JSON.ServerSide

newtype ContentError = ContentError (Maybe Text)

contentError
  nothing
  msg
  c = case c of
    (ContentError Nothing)    -> nothing
    (ContentError (Just msg)) -> msg

contentHandlerError :: String -> ContentError
contentHandlerError = contentHandlerErrorText . T.pack

contentHandlerErrorText :: Text -> ContentError
contentHandlerErrorText = ContentError . Just

contentHandlerErrorMap :: (Maybe Text -> a) -> ContentError -> a
contentHandlerErrorMap f (ContentError x) = f x

contentHandlerErrorMsg :: ContentError -> Text
contentHandlerErrorMsg = contentHandlerErrorMap (maybe "Unknown message" id)

{-

TODO: consider

-- | ContentHandler is a handler for rendering Bead pages or
--   information to the client. Mostly used for serving pages for
--   authenticated users.
type ContentHandler a = forall v. StateT (SC.UserState, PageSettings) (BeadHandler' v) a

-- | RequestHandler is a ContentHandler equipped with error handling,
--   since a request can be malformed or permission issues may
--   arise. It is used for handling GET and POST requests.
type RequestHandler a = ExceptT RequestError ContentHandler a

-}

type ContentHandler' b c = ExceptT ContentError (StateT (UserState, PageSettings) (BeadHandler' b)) c

-- ContentHandler is a handler for render Bead pages or information to the client
-- also equiped with error handling, mainly used for render inner pages.
type ContentHandler c = ContentHandler' BeadContext c

-- Data types for handling GET requests

type RedirectionTarget = P.PageDesc

data HtmlPage = HtmlPage {
    pageTitle  :: IHtml
  , pageBody   :: IHtml
  }

htmlPage :: Translation -> IHtml -> HtmlPage
htmlPage title body = HtmlPage {
    pageTitle = do
      msg <- getI18N
      return $ Bootstrap.pageHeader (msg title) Nothing
  , pageBody = body
  }

-- | Contents of a page is either contents of some other page
--   or some html.
type PageContents = Either RedirectionTarget HtmlPage

redirection :: RedirectionTarget -> PageContents
redirection = Left

setPageContents :: HtmlPage -> ContentHandler PageContents
setPageContents = return . Right

redirectTo :: RedirectionTarget -> ContentHandler PageContents
redirectTo = return . redirection

-- | A file to be downloaded consists of a filename, a mime info and
--   the means to write its contents into a response.
type File = (Text, Mime, BeadHandler ())

downloadStrict :: Text -> Mime -> B.ByteString -> ContentHandler File
downloadStrict filename mime contents = return (filename, mime, writeBS contents)

downloadLazy :: Text -> Mime -> LB.ByteString -> ContentHandler File
downloadLazy filename mime contents = return (filename, mime, writeLBS contents)

downloadText :: Text -> Text -> ContentHandler File
downloadText filename contents = return (filename, MimePlainText, writeText contents)

-- Data types for handling form submissions and modification requests

newtype Action = Action {
    performAction :: UserStory PageContents
  }

-- | The 'logMessage' logs a message at a given level using the service context logger
logMessage :: LogLevel -> String -> BeadHandler' b ()
logMessage lvl msg = logMessageText lvl (T.pack msg)

logMessageText :: LogLevel -> Text -> BeadHandler' b ()
logMessageText lvl msg = do
  context <- getServiceContext
  liftIO $ L.log (SC.logger context) lvl msg

beadHandler :: BeadHandler a -> ContentHandler a
beadHandler = lift . lift

userState :: ContentHandler UserState
userState = gets fst

withUserState :: (UserState -> ContentHandler c) -> ContentHandler c
withUserState = (userState >>=)

changeUserState :: (UserState -> UserState) -> ContentHandler ()
changeUserState = modify . first

-- | Returns the user's language from the session.
--   For registration and test agent, it returns the default language.
userLanguage :: ContentHandler Language
userLanguage = do
  s <- userState
  maybe defaultLanguage return (SC.getLanguage s)

setUserLanguage :: Language -> ContentHandler ()
setUserLanguage lang = changeUserState (SC.setLanguage lang)

defaultLanguage :: ContentHandler Language
defaultLanguage = beadHandler . withDictionary $ configuredDefaultDictionaryLanguage

setUserTimezone :: TimeZoneName -> ContentHandler ()
setUserTimezone timezone = changeUserState (SC.setTimeZone timezone)

setHomePage :: HomePageContents -> ContentHandler ()
setHomePage = changeUserState . SC.setHomePage

pageSettings :: ContentHandler PageSettings
pageSettings = gets snd

modifyPageSettings :: (PageSettings -> PageSettings) -> ContentHandler ()
modifyPageSettings f = modify (second f)

-- Produces a handler that returns the user's actual time zone
userTimeZone :: ContentHandler TimeZoneName
userTimeZone = do
  tz <- SC.getTimeZone <$> userState
  maybe defaultTimeZone return tz
  where
    defaultTimeZone :: ContentHandler TimeZoneName
    defaultTimeZone = beadHandler configuredDefaultTimeZone

-- Represents a functions that converts a given UTC time into
-- the user's timezone
type UserTimeConverter = UTCTime -> LocalTime

withUserTimeZoneContext :: (TimeZoneConverter -> TimeZoneName -> a) -> ContentHandler a
withUserTimeZoneContext f = do
  zi  <- userTimeZone
  tzc <- beadHandler getTimeZoneConverter
  return (f tzc zi)

-- Produces the a UserTimeZoneConverter function for the user's time zone
userTimeZoneToLocalTimeConverter :: ContentHandler UserTimeConverter
userTimeZoneToLocalTimeConverter = withUserTimeZoneContext zoneInfoToLocalTimeSafe

-- Produces a function that convert a given local time into a UTC time using
-- the user's actual time zone
userTimeZoneToUTCTimeConverter :: ContentHandler (LocalTime -> UTCTime)
userTimeZoneToUTCTimeConverter = withUserTimeZoneContext zoneInfoToUTCTimeSafe

-- Produces a list of the found time zones
foundTimeZones :: BeadHandler' b [TimeZoneName]
foundTimeZones = zoneInfos <$> getTimeZoneConverter

i18nE :: ContentHandler (Translation -> Text)
i18nE = do
  lang <- userLanguage
  -- If the dictionary is not found for the language stored in session
  -- the identical dictionary is returned. The fromString is necessary
  -- for the Attribute names and values used in html templating engines
  d <- beadHandler . withDictionary . getDictionary $ lang
  return (unDictionary $ maybe idDictionary id d) -- TODO: I18N

i18nH :: BeadHandler' v (Translation -> Text)
i18nH = do
  cookie <- fst <$> getCookie
  let lang = Auth.cookieLanguage cookie
  dict <- withDictionary $ getDictionary lang
  return $ maybe trans unDictionary dict

-- Tries to decode the given value with the parameter description, if
-- fails throws an error, otherwise returns the value
decodeParamValue :: Parameter a -> BU.ByteString -> ContentHandler' b a
decodeParamValue param value = do
  let v = T.unpack $ TE.decodeUtf8 value
      decoded = decode param v
  maybe
    (throwError . contentHandlerError . decodeError param $ v)
    return
    decoded

getParameter :: Parameter a -> ContentHandler' b a
getParameter param = do
  reqParam <- lift . getParam . TE.encodeUtf8 . name $ param
  maybe
    (throwError . contentHandlerError $ notFound param) -- TODO: I18N
    (decodeParamValue param)
    reqParam

getParameterWithDefault :: a -> Parameter a -> ContentHandler' b a
getParameterWithDefault defValue param = do
  reqParam <- lift . getParam . TE.encodeUtf8 . name $ param
  maybe
    (throwError . contentHandlerError $ notFound param) -- TODO: I18N
    (\bs -> if (B.null bs)
              then return defValue
              else (decodeParamValue param bs))
    reqParam

-- Calculates a list of values named and decoded by the given parameter
-- If the parameter is not found throws an error, if one of the parameter
-- values are not decodable throws an error otherwise
-- returns a list of the decoded values
getParameterValues :: Parameter a -> ContentHandler' b [a]
getParameterValues param = do
  params <- lift getParams
  let paramName = name param
  maybe
    (throwError . contentHandlerError $ notFound param) -- TODO: I18N
    (mapM (decodeParamValue param))
    (Map.lookup (TE.encodeUtf8 paramName) params)

-- Calculates a Just value named and decoded by the given paramater,
-- supposing that the parameter are optional, if it not presented
-- calculates Nothing, if decoding fails, throws an Error
getOptionalParameter :: Parameter a -> ContentHandler' b (Maybe a)
getOptionalParameter param = do
  params <- lift getParams
  let paramName = name param
  case Map.lookup (TE.encodeUtf8 paramName) params of
    Nothing  -> return Nothing
    Just []  -> throwError . contentHandlerErrorText $ T.concat [paramName, " contains zero values."] -- TODO: I18N
    Just [x] -> Just <$> decodeParamValue param x
    Just (_:_) -> throwError . contentHandlerErrorText $ T.concat [paramName, " has more than one value."] -- TODO: I18N

-- Calculates a Just value named and decoded by the given paramater,
-- supposing that the parameter are optional, if it not presented
-- calculates Nothing, if decoding fails, throws an Error
getOptionalOrNonEmptyParameter :: Parameter a -> ContentHandler' b (Maybe a)
getOptionalOrNonEmptyParameter param = do
  params <- lift getParams
  let paramName = name param
  case Map.lookup (TE.encodeUtf8 paramName) params of
    Nothing  -> return Nothing
    Just []  -> throwError . contentHandlerErrorText $ T.concat [paramName, " contains zero values."] -- TODO: I18N
    Just [x] -> case B.null x of
                  True  -> return Nothing
                  False -> Just <$> decodeParamValue param x
    Just (_:_) -> throwError . contentHandlerErrorText $ T.concat [paramName, " has more than one value."] -- TODO: I18N


getJSONParam :: (Data a) => Text -> Text -> ContentHandler a
getJSONParam param msg = do
  x <- lift . getParam . TE.encodeUtf8 $ param
  case x of
    Nothing -> throwError . contentHandlerErrorText $ msg
    Just y  -> case decodeFromFay . B.unpack $ y of
      Nothing -> throwError . contentHandlerError $ "Decoding error"
      Just z  -> return z

-- Decode multiple values for the given parameter names.
-- This approach can be used for checkbox contained values.
-- If no parameter is found in the request, an empty list is returned.
getJSONParameters :: (Data a, Show a) => Text -> Text -> ContentHandler [a]
getJSONParameters param msg = do
  params <- lift getParams
  case Map.lookup (TE.encodeUtf8 param) params of
    Nothing -> return []
    Just [] -> return []
    Just vs -> mapM decodePrm vs
  where
    decodePrm :: Data a => B.ByteString -> ContentHandler a
    decodePrm v =
      let v' = TE.decodeUtf8 v
      in case decodeFromFay (B.unpack v) of
           Nothing -> throwError . contentHandlerErrorText $ T.concat ["Decoding error:", v', " ", msg]
           Just  x -> return x

-- Computes a list that contains language and dictionary info pairs
getDictionaryInfos :: ContentHandler DictionaryInfos
getDictionaryInfos = beadHandler . BeadContext.withDictionary $ BeadContext.getDictionaryInfos

data Mime
  = MimeZip
  | MimePlainText

mimeCata :: a -> a -> Mime -> a
mimeCata zip plainText mime =
  case mime of
    MimeZip -> zip
    MimePlainText -> plainText

-- Runs a user story within a service context where the user is logged in
-- and throws a handler error if the story has failed
-- otherwise returns the computed value
userStory :: S.UserStory a -> ContentHandler a
userStory story = do
  context <- beadHandler $ getServiceContext
  i18n <- i18nE
  uState <- userState
  x <- liftIO $ S.runUserStory context i18n uState story
  case x of
    Left e  ->
      throwError . contentHandlerErrorText . S.translateUserError i18n $ e
    Right (y, uState') -> do
      changeUserState (const uState')
      return y

logout :: ContentHandler ()
logout = (userStory . S.logout) =<< defaultLanguage

-- Runs a UserStory in the registration context
registrationStory :: S.UserStory a -> ContentHandler a
registrationStory s = localState SC.Registration (userStory s)
  where
    localState :: UserState -> ContentHandler a -> ContentHandler a
    localState s handler = do
      uState <- userState
      changeUserState (const s)
      x <- handler
      changeUserState (const uState)
      return x

