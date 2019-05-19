import Control.Exception (try, SomeException)
import Data.Either (either)
import Data.Function (on)
import Data.List (isSuffixOf, minimumBy)
import System.Exit (exitWith, ExitCode(..))
import System.FilePath ((<.>), (</>), FilePath, joinPath)
import System.Directory (copyFile, renameDirectory, listDirectory, createDirectoryIfMissing, removeDirectoryRecursive)
import qualified System.Posix as P

main :: IO ()
main = do
  mJob <- nextJob
  case mJob of
    Just job -> do
      let path = incoming </> job
          pending = path <.> "pending"
      renameDirectory path pending
      P.setFileMode pending P.accessModes  -- drwxrwxrwx
      copy pending (stage </> job)
      P.setFileMode (stage </> job) P.accessModes -- drwxrwxrwx
      removeDirectoryRecursive pending
      putStrLn job
--      exitWith $ either (const $ ExitFailure 1) (const ExitSuccess) (result :: Either SomeException ())
    _ ->
      exitWith $ ExitFailure 2
  where
    nextJob :: IO (Maybe FilePath)
    nextJob = minimumByCreationTime =<< filter isQueued <$> listDirectory incoming

      where
        minimumByCreationTime :: [FilePath] -> IO (Maybe FilePath)
        minimumByCreationTime [] = return Nothing
        minimumByCreationTime entries =
          Just . fst . minimumBy (compare `on` snd) <$> mapM (\e -> (,) e <$> P.accessTime <$> P.getFileStatus (incoming </> e)) entries

    isQueued :: FilePath -> Bool
    isQueued p = not (any (`isSuffixOf` p) [".pending", ".locked"])

    incoming :: FilePath
    incoming = joinPath ["/", "home", "bead", "jobs", "main", "incoming"]

    stage :: FilePath
    stage = joinPath ["/", "home", "tester", "jails", "main", "job"]

    copy :: FilePath -> FilePath -> IO ()
    copy src dst = do
      files <- listDirectory src
      createDirectoryIfMissing True dst
      mapM_ (\file -> copyFile (src </> file) (dst </> file)) files
