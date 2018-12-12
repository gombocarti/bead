-- Kills leftover submission-related processes.
-- Requires: cap_kill=ep.

import Control.Exception (try, SomeException)
import Control.Monad (forM_)
import System.Exit
import System.Posix

main :: IO ()
main = do
  pids <- map read . lines <$> getContents
  forM_ pids $ \pid -> do
    result <- try $ signalProcess sigKILL pid :: IO (Either SomeException ())
    case result of
      Left _ -> putStrLn $ unwords ["Kill: invalid pid:", show pid]
      Right _ -> return ()

