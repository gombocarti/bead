module Bead.Controller.Logging where

import Prelude hiding (log)

import           Data.Text (Text)
import qualified Data.Text.IO as Text

data LogLevel
  = DEBUG
  | INFO
  | ERROR
  deriving (Show)

newtype Logger = Logger {
    log :: LogLevel -> Text -> IO ()
  }

mockLogger = Logger {
    log = \_ msg -> Text.putStrLn msg
  }

-- | Logger that does not log anything
nullLogger = Logger {
    log = \_ _ -> return ()
  }
