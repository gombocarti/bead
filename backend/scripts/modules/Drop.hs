import Control.Monad (forM_)
import Control.Exception (try, SomeException)
import Data.Either (either)
import System.FilePath ((</>), FilePath, joinPath)
import System.Directory (listDirectory, createDirectoryIfMissing, copyFile)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.Posix (setFileMode, accessModes, getFileStatus, isRegularFile)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [job] -> do
      copy (stage </> job </> "result") (outgoing </> job)
      setFileMode (outgoing </> job) accessModes
--      exitWith $ either (const $ ExitFailure 1) (const ExitSuccess) (result :: Either SomeException ())
    _ ->
      exitWith $ ExitFailure 2
  where
    outgoing :: FilePath
    outgoing = joinPath ["/", "home", "bead", "jobs", "main", "outgoing"]

    stage :: FilePath
    stage = joinPath ["/", "home", "tester", "jails", "main", "job"]

    copy :: FilePath -> FilePath -> IO ()
    copy src dst = do
      files <- listDirectory src
      createDirectoryIfMissing True dst
      forM_ files $ \file -> do
        let p = src </> file
        status <- getFileStatus p
        if isRegularFile status
          then copyFile p (dst </> file)
          else return ()
