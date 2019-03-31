{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE CPP #-}
module Bead.View.Header (
    acceptLanguage
  , acceptLanguageOrDefault
  , getCookie
  , loggedOutCookie
  , setCookie
#ifdef TEST
  , acceptLanguageTests
#endif
  ) where

import           Control.Monad.IO.Class (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Char (isSpace, isLetter)
import           Data.List (intersect)
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Time (UTCTime)
import qualified Data.Time as Time
import qualified Data.Time.Calendar as Cal
import           Data.Text (Text)
import           Snap (Handler, getHeader, getRequest, modifyResponse, addResponseCookie)
import qualified Snap
import           Text.Read (readMaybe)

import           Bead.Domain.Entities (Language(Language))
import qualified Bead.View.AuthToken as Auth
import           Bead.View.BeadContext (BeadHandler', DictionaryContext, getDictionaryInfos, configuredDefaultDictionaryLanguage, encryptCookie, decryptCookie, withDictionary)
import qualified Bead.View.BeadContext as Bead

#ifdef TEST
import           Test.Tasty.TestSet
#endif

getCookie :: BeadHandler' v (Either Text Auth.Cookie)
getCookie = do
  mCookie <- Snap.getCookie (BC.pack "token")
  maybe (Right <$> loggedOutCookie) (decryptCookie . Snap.cookieValue) mCookie

loggedOutCookie :: BeadHandler' v Auth.Cookie
loggedOutCookie = Auth.NotLoggedInCookie <$> withDictionary acceptLanguageOrDefault

setCookie :: Auth.Cookie -> BeadHandler' a (Either Text ())
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
      cookie :: ByteString -> UTCTime -> Snap.Cookie
      cookie contents expiration = Snap.Cookie {
          Snap.cookieName = BC.pack "token"
        , Snap.cookieValue = contents
        , Snap.cookieExpires = Just expiration
        , Snap.cookieDomain = Nothing
        , Snap.cookiePath = Nothing
        , Snap.cookieSecure = False
        , Snap.cookieHttpOnly = True
        }

acceptLanguageOrDefault :: Handler b DictionaryContext Language
acceptLanguageOrDefault = do
  lang <- acceptLanguage
  maybe configuredDefaultDictionaryLanguage return lang

acceptLanguage :: Handler b DictionaryContext (Maybe Language)
acceptLanguage = do
  acceptLanguages <- getHeader "Accept-Language" <$> getRequest
  case acceptLanguages of
    Nothing -> return Nothing
    Just userLangs -> do
      available <- map fst <$> getDictionaryInfos
      let languages = map acceptLanguageToLanguage
                      $ parseAcceptLanguageLine userLangs
          selectedLang = languages `intersect` available
      return $ listToMaybe selectedLang

-- Possible accept langauge value in Accept-Language header field.
data AcceptLanguage
  = AL_Simple  String
  | AL_Quality String Double
  deriving (Eq, Show)

-- Eg:
-- AL_Simple "en-US" represents "en-US"
-- AL_quality "en" 0.5 represents "en;q=0.5"

acceptLanguageCata
  simple
  quality
  al = case al of
    AL_Simple  s   -> simple s
    AL_Quality s q -> quality s q

parseAcceptLangValue :: ByteString -> Maybe AcceptLanguage
parseAcceptLangValue s = case map BC.unpack $ BC.split ';' s of
  [lang]            -> Just $ AL_Simple $ takeLetters lang
  [lang, 'q':'=':q] -> AL_Quality (takeLetters lang) <$> readMaybe q
  _                 -> Nothing
  where
    takeLetters :: String -> String
    takeLetters = takeWhile (\c -> isLetter c || c == '-') . dropWhile isSpace

parseAcceptLanguageLine :: ByteString -> [AcceptLanguage]
parseAcceptLanguageLine = catMaybes . map (parseAcceptLangValue . BC.dropWhile isSpace) . BC.split ','

-- Convert an accept language to language dropping the localization
-- and the quality informations
acceptLanguageToLanguage :: AcceptLanguage -> Language
acceptLanguageToLanguage = acceptLanguageCata
  (Language . dropLocalization)
  (\lang _q -> Language $ dropLocalization lang)
  where
    dropLocalization = takeWhile (/='-')


#ifdef TEST
acceptLanguageTests = group "acceptLanguage" $ do
  group "parse" $ do
    eqPartitions parseAcceptLangValue
      [ Partition "accept language parse empty string" "" Nothing "Empty string is parsed"
      , Partition "simple accept language" "en-US" (Just $ AL_Simple "en-US") "Simple language parameter is not parsed correctly"
      , Partition "simple accept language" "en;q=0.5" (Just $ AL_Quality "en" 0.5) "Simple language parameter is not parsed correctly"
      , Partition "noise for accept language" "qns;sdjfkj" Nothing "Noise is parsed"
      ]
  group "accept language line" $ eqPartitions parseAcceptLanguageLine
    [ Partition "simple" "en-US , en;q=0.5" [AL_Simple "en-US", AL_Quality "en" 0.5] "Parsing simple line failed"
    , Partition "browser" "hu-HU,hu;q=0.8,en-US;q=0.5,en;q=0.3" [AL_Simple "hu-HU", AL_Quality "hu" 0.8, AL_Quality "en-US" 0.5, AL_Quality "en" 0.3] "Parsing accept-language of a browser failed"
    ]
  group "accept language to language" $ eqPartitions acceptLanguageToLanguage
    [ Partition "Simple with localization" (AL_Simple "en-US") (Language "en") "Drop localization has failed"
    , Partition "Quality with localization" (AL_Quality "en-US" 0.5) (Language "en") "Drop localization has failed"
    ]
#endif
