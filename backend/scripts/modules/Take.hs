import Control.Exception (try, SomeException)
import Data.Either (either)
import Data.List (isSuffixOf)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath ((<.>), (</>), FilePath, joinPath)
import System.Directory (copyFile, renameDirectory, listDirectory, createDirectoryIfMissing)
import qualified System.Posix as P

main :: IO ()
main = do
  jobs <- filter isUndone <$> listDirectory incoming
  case jobs of
    job : _ -> do
      copy (incoming </> job) (stage </> job)
      P.setFileMode (stage </> job) (P.unionFileModes P.ownerModes P.groupModes) -- drwxrwx---
      renameDirectory (incoming </> job) (incoming </> job <.> "pending")
      putStrLn job
--      exitWith $ either (const $ ExitFailure 1) (const ExitSuccess) (result :: Either SomeException ())
    _ ->
      exitWith $ ExitFailure 2
  where
    isUndone :: FilePath -> Bool
    isUndone p = not (any (`isSuffixOf` p) [".pending", ".locked"])

    incoming :: FilePath
    incoming = joinPath ["/", "home", "bead", "jobs", "main", "incoming"]

    stage :: FilePath
    stage = joinPath ["/", "home", "tester", "jails", "main", "job"]

    copy :: FilePath -> FilePath -> IO ()
    copy src dst = do
      files <- listDirectory src
      createDirectoryIfMissing True dst
      mapM_ (\file -> copyFile (src </> file) (dst </> file)) files
