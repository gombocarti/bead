-- Executes the test script as a given user.
-- Requires cap_setuid,cap_chown=ep.

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import System.Environment
import System.Exit
import           System.Directory (listDirectory)
import           System.FilePath ((</>))
import           System.Posix (UserID, GroupID)
import qualified System.Posix as P

main :: IO ()
main = do
  args <- getArgs
  case args of
    (user : script : workdir : passthrough) -> do
      (uid, gid) <- (P.userID &&& P.userGroupID) <$> P.getUserEntryForName user
      P.setUserID uid
      uid' <- P.getRealUserID
      if uid /= uid'
        then do
          putStrLn $ concat ["I am not ", user, ". Aborting."]
          exitWith (ExitFailure 1)
        else do
          setOwnerAndGroupRecursive workdir uid gid
          P.executeFile ("/usr/local/bead/templates" </> script) False (workdir : passthrough) Nothing
    _ -> do
      putStrLn "Test: I need at least three arguments."
      exitWith (ExitFailure 1)

  where
    setOwnerAndGroupRecursive :: FilePath -> UserID -> GroupID -> IO ()
    setOwnerAndGroupRecursive dir uid gid = do
      P.setOwnerAndGroup dir uid gid
      P.setFileMode dir P.accessModes
      status <- P.getFileStatus dir
      if P.isDirectory status
        then do
          entries <- listDirectory dir
          forM_ entries $ \entry ->
            setOwnerAndGroupRecursive (dir </> entry) uid gid
        else
          return ()
