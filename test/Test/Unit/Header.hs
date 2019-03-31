{-# LANGUAGE OverloadedStrings #-}

module Test.Unit.Header (
    tests
  ) where

import Bead.View.BeadContext (dictionarySnaplet)
import Bead.View.Dictionary (dictionaries, Language(Language))
import Bead.View.Header (acceptLanguageOrDefault)

import qualified Data.Map as M
import qualified Data.Text as T
import           Snap.Test (addHeader, get)
import           Snap.Snaplet.Test (evalHandler)

import Test.Tasty.TestSet (TestSet, test)
import Test.Tasty.HUnit (testCase, assertEqual)

tests :: TestSet ()
tests = do
  acceptLanguageOrDefaultTest

acceptLanguageOrDefaultTest :: TestSet ()
acceptLanguageOrDefaultTest = test $ testCase "acceptLanguageOrDefault tests" $ do
  let defLang = Language "de"
  res <- evalHandler Nothing (get "/home" M.empty) acceptLanguageOrDefault (dictionarySnaplet dictionaries defLang)
  either
    (\t -> fail $ "Error while handling get without accept-language: " ++ T.unpack t)
    (\lang -> assertEqual "Get without accept-language does not return the default language." defLang lang)
    res
  res <- evalHandler Nothing (get "/login" M.empty >> addHeader "Accept-Language" "hu-HU,hu;q=0.8,en-US;q=0.5,en;q=0.3") acceptLanguageOrDefault (dictionarySnaplet dictionaries defLang)
  either
    (\t -> fail $ "Error while handling get with Hungarian language: " ++ T.unpack t)
    (\lang -> assertEqual "Get with Hungarian language does not return Hungarian." (Language "hu") lang)
    res

  res <- evalHandler Nothing (get "/login" M.empty >> addHeader "Accept-Language" "en-US,en;q=0.9") acceptLanguageOrDefault  (dictionarySnaplet dictionaries defLang)
  either
    (\t -> fail $ "Error while handling get with US English language: " ++ T.unpack t)
    (\lang -> assertEqual "Get with US English language does not return English." (Language "en") lang)
    res

  res <- evalHandler Nothing (get "/login" M.empty >> addHeader "Accept-Language" "de-LU,de;q=0.8,en-US;q=0.5,en;q=0.3") acceptLanguageOrDefault  (dictionarySnaplet dictionaries defLang)
  either
    (\t -> fail $ "Error while handling get with US English and other languages: " ++ T.unpack t)
    (\lang -> assertEqual "Get with US English and other languages does not return English." (Language "en") lang)
    res

  res <- evalHandler Nothing (get "/submission" M.empty >> addHeader "Accept-Language" "fr-LU,fr;q=0.8,es-EC;q=0.6,es;q=0.2") acceptLanguageOrDefault  (dictionarySnaplet dictionaries defLang)
  either
    (\t -> fail $ "Error while handling get with languages not available: " ++ T.unpack t)
    (\lang -> assertEqual "Get with unavailable languages does not return the default." defLang lang)
    res
