{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Logger where

import Data.Time
import Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

import System.FilePath (dropFileName)
import System.Directory (createDirectoryIfMissing)

import qualified System.FastLogger       as S
import qualified Bead.Controller.Logging as L

-- Represents a logger and an action to stop them
data SnapLogger = SnapLogger {
    snapLogger :: L.Logger
  , stopLogger :: IO ()
  }

-- | Creates a SnapLogger that logs every entry on every level
-- to the given file, assuming that the file exist and only
-- used by this logger
createSnapLogger :: FilePath -> IO SnapLogger
createSnapLogger logFile = do
  createDirectoryIfMissing True $ dropFileName logFile
  l <- S.newLogger logFile

  let logger lvl msg = logMessage lvl msg >>= S.logMsg l

  return $ SnapLogger {
      snapLogger = L.Logger logger
    , stopLogger = S.stopLogger l
    }

  where
    logMessage :: L.LogLevel -> Text -> IO ByteString
    logMessage lvl msg = do
      now <- getCurrentTime
      return $ BS.concat ["[", pack (formatTime defaultTimeLocale "%c" now), "] ", pack (show lvl), " - ", encodeUtf8 msg]
