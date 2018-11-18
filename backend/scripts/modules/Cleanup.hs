-- Cleans up jail.
-- Requires cap_chown=ep.

import Control.Monad (forM_)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath ((</>))
import System.Posix

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do
      print "Cleanup: I need a path."
      exitWith (ExitFailure 1)
    targets -> do
      forM_ targets clean
      exitWith ExitSuccess

  where
    clean :: FilePath -> IO ()
    clean dir = do
      uid <- getRealUserID
      gid <- getRealGroupID
      contents <- listDirectory dir
      forM_ contents $ \entry -> do
        let p = dir </> entry
        traverseDirectoryRecursive p $ \p' -> do
          setOwnerAndGroup p' uid gid
          setFileMode p' accessModes
        status <- getFileStatus p
        if isDirectory status
          then removeDirectoryRecursive p
          else removeFile p

    traverseDirectoryRecursive :: FilePath -> (FilePath -> IO ()) -> IO ()
    traverseDirectoryRecursive dir action = do
      status <- getFileStatus dir
      if isDirectory status
        then do
          action dir
          entries <- listDirectory dir
          forM_ entries $ \entry ->
            traverseDirectoryRecursive (dir </> entry) action
        else return ()
