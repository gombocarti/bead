{-# LANGUAGE CPP #-}
module Bead.Config (
    Config(..)
#ifdef SSO
  , SSOLoginConfig(..)
  , sSOLoginConfig
#else
  , StandaloneLoginConfig(..)
  , standaloneLoginConfig
#endif
  , defaultConfiguration
  , configCata
  , readConfiguration
  , module Bead.Config.Configuration
  ) where

import Control.Monad (join)

import System.Directory (doesFileExist)

import Bead.Config.Configuration
import Bead.Config.Parser

#ifdef TEST
import Test.Tasty.TestSet
#endif

-- Represents the hostname (and/or port) of the bead server
type Hostname = String
type Second   = Int

readConfiguration :: FilePath -> IO Config
readConfiguration path = do
  exist <- doesFileExist path
  case exist of
    False -> do
      putStrLn "Configuration file does not exist"
      putStrLn "!!! DEFAULT CONFIGURATION IS USED !!!"
      return defaultConfiguration
    True  -> do
      content <- readFile path
      case parseYamlConfig content of
        Left err -> do
          putStrLn "Configuration is not parseable"
          putStrLn "!!! DEFAULT CONFIGURATION IS USED !!!"
          putStrLn $ "Reason: " ++ err
          return defaultConfiguration
        Right c -> return c
