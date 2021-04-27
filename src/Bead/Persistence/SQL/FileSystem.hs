{-# LANGUAGE OverloadedStrings #-}
module Bead.Persistence.SQL.FileSystem where

import           Control.Applicative ((<$>))
import           Control.Exception (catch, catchJust, handleJust, SomeException, throwIO)
import           Control.DeepSeq (deepseq)
import           Control.Monad
import           Control.Monad.IO.Class
import           System.IO.Error (userError, isDoesNotExistError)

import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString as B
import           Data.List (sortOn, isSuffixOf)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.UUID.V4 (nextRandom)
import           System.Directory
import qualified System.Directory as Dir
import           System.Exit (ExitCode)
import           System.FilePath
import           System.IO
import           System.Posix.Types (COff(..))
import           System.Posix.Files (FileStatus, getFileStatus, fileSize, modificationTimeHiRes)
import           System.Process (readCreateProcessWithExitCode, cwd, proc)
import           Text.Read (readEither)

import           Bead.Domain.Entities
import           Bead.Domain.Relationships
import           Bead.Domain.Types

data_ :: FilePath
data_ = "data"

publicDir :: Username -> FilePath
publicDir = usernameCata (\u -> user </> u </> "public-files")

privateDir :: Username -> FilePath
privateDir = usernameCata (\u -> user </> u </> "private-files")

testOutgoing, testIncoming :: FilePath
testOutgoing = joinPath [data_, "test-outgoing"]
testIncoming = joinPath [data_, "test-incoming"]
user = joinPath [data_, "user"]
plagiarism = joinPath [data_, "plagiarism"]

fsDirs :: [FilePath]
fsDirs = [
    data_
  , testOutgoing
  , testIncoming
  , user
  , plagiarism
  ]

fileLoad :: (MonadIO io) => FilePath -> io String
fileLoad fname = liftIO $ do
  h <- openFile fname ReadMode
  hSetEncoding h utf8
  s <- hGetContents h
  s `deepseq` hClose h
  return s

fileLoadBS :: (MonadIO io) => FilePath -> io ByteString
fileLoadBS fname = liftIO $ do
  h <- openFile fname ReadMode
  hSetEncoding h utf8
  s <- BC.hGetContents h
  s `deepseq` hClose h
  return s

fileSave :: (MonadIO io) => FilePath -> String -> io ()
fileSave fname s = liftIO $ do
  handler <- openFile fname WriteMode
  hSetEncoding handler utf8
  hPutStr handler s
  hClose handler

fileSaveBS :: (MonadIO io) => FilePath -> ByteString -> io ()
fileSaveBS fname s = liftIO $ do
  handler <- openFile fname WriteMode
  hSetEncoding handler utf8
  BC.hPutStr handler s
  hClose handler

fileSaveText :: (MonadIO io) => FilePath -> Text -> io ()
fileSaveText fname s = liftIO $ TIO.writeFile fname s

filterDirContents :: (MonadIO io) => (FilePath -> IO Bool) -> FilePath -> io [FilePath]
filterDirContents f p = liftIO $ do
  contents <- listDirectory p
  filterM f $ map jp contents
    where
      jp x = joinPath [p, x]

getSubDirectories :: (MonadIO io) => FilePath -> io [FilePath]
getSubDirectories = filterDirContents doesDirectoryExist

getFilesInFolder :: (MonadIO io) => FilePath -> io [FilePath]
getFilesInFolder = filterDirContents doesFileExist

-- *

initFS :: (MonadIO io) => io ()
initFS = liftIO $ mapM_ (createDirectoryIfMissing True) fsDirs

removeFS :: (MonadIO io) => io ()
removeFS = liftIO $ catchJust (\e -> if isDoesNotExistError e then Just () else Nothing)
                              (removeDirectoryRecursive data_)
                              (const $ return ())

isSetUpFS :: (MonadIO io) => io Bool
isSetUpFS = liftIO . fmap and $ mapM doesDirectoryExist fsDirs

createDirectoryLocked :: FilePath -> (FilePath -> IO ()) -> IO ()
createDirectoryLocked d m = do
  let d' = d <.> "locked"
  createDirectory d'
  m d'
  renameDirectory d' d
                  
createUserFileDir :: (MonadIO io) => Username -> io ()
createUserFileDir u = liftIO $
  forM_ [privateDir u, publicDir u] $ createDirectoryIfMissing True

withUsersDir :: (MonadIO io) => Username -> (FilePath -> FilePath -> io a) -> io a
withUsersDir username action = do
  createUserFileDir username
  action (publicDir username) (privateDir username)

copyUsersFile :: (MonadIO io) => Username -> FilePath -> UsersFile FilePath -> io ()
copyUsersFile username tmpPath userfile = 
  withUsersDir username $ \public private -> 
    liftIO $ Dir.copyFile tmpPath $ usersFile (public </>) (private </>) userfile

saveUsersFile :: (MonadIO io) => Username -> FilePath -> UsersFile ByteString -> io ()
saveUsersFile username filename userfile = 
  withUsersDir username $ \public private ->
    liftIO $ usersFile (B.writeFile (public </> filename)) (B.writeFile (private </> filename)) userfile

-- Calculates the file modification time in UTC time from the File status
fileModificationInUTCTime :: FileStatus -> UTCTime
fileModificationInUTCTime = posixSecondsToUTCTime . modificationTimeHiRes

listFiles :: (MonadIO io) => Username -> io [(UsersFile FilePath, FileInfo)]
listFiles username = liftIO $ do
  createUserFileDir username
  privateFiles <- f (privateDir username) UsersPrivateFile
  publicFiles  <- f (publicDir username) UsersPublicFile
  return $ privateFiles ++ publicFiles
  where
    f dir typ = do
      paths <- getFilesInFolder dir
      forM paths $ \path -> do
        status <- getFileStatus path
        let info = FileInfo
                   (fileOffsetToInt $ fileSize status)
                   (fileModificationInUTCTime status)
        return (typ $ takeFileName path, info)

    fileOffsetToInt (COff x) = fromIntegral x

getFile :: (MonadIO io) => Username -> UsersFile FilePath -> io FilePath
getFile username userfile = 
  withUsersDir username $ \public private ->
    liftIO $ usersFile (f public) (f private) userfile
  where
    f dir fn = do
      let fname = dir </> fn
      exists <- doesFileExist fname
      unless exists $ error $ concat [
          "File (", fn, ") does not exist in users folder ("
        , show username, ")"
        ]
      return fname

-- Saves the test script, test case and the submission in a new
-- directory in test-outgoing.
saveTestJob :: (MonadIO io) => SubmissionKey -> Submission -> TestScript -> TestCase -> io FilePath
saveTestJob sk submission testScript testCase = liftIO $ do
  d <- (testOutgoing </>) . show <$> nextRandom
  createDirectoryLocked d $ \d' -> do
    withSubmissionKey sk (fileSave (d' </> "id"))
    fileSaveText (d' </> "script") (tsScript testScript)
    -- Save Simple or Zipped Submission
    withSubmissionValue (solution submission) (flip fileSaveText) (flip fileSaveBS) (d' </> "submission")
    -- Save Simple or Zipped Test Case
    withTestCaseValue (tcValue testCase) (flip fileSaveText) (flip fileSaveBS) (d' </> "tests")
  return d

-- Insert the feedback info for the file system part of the database. This method is
-- used by the tests only, and serves as a model for interfacing with the outside world.
insertTestFeedback :: (MonadIO io) => SubmissionKey -> [FeedbackInfo] -> io ()
insertTestFeedback sk feedbacks = liftIO $ do
  let sDir = submissionKeyMap (testIncoming </>) sk <.> "locked"
  createDirectoryIfMissing True sDir
  fileSave (sDir </> "id") (submissionKeyMap id sk)
  let student comment = fileSaveText (sDir </> "public") comment
      admin   comment = fileSaveText (sDir </> "private") comment
      result  bool    = fileSave (sDir </> "result") (show bool)
  mapM_ (feedbackInfo queued result student admin evaluated) feedbacks
  where
    evaluated _ _ = error "insertTestComment: Evaluation should not be inserted by test."
    queued = error "insertTestComment: QueuedForTest should not be inserted by tests."

finalizeTestFeedback :: (MonadIO io) => SubmissionKey -> io ()
finalizeTestFeedback sk = liftIO $ do
  let sDir = submissionKeyMap (testIncoming </>) sk
  renameDirectory (sDir <.> "locked") sDir

-- Test Feedbacks are stored in the persistence layer, in the
-- test-incomming directory each one in a file in a test job
-- subdirectory.
testFeedbacks :: (MonadIO io) => io [(SubmissionKey, [Feedback])]
testFeedbacks = liftIO (createFeedbacks =<< sortOnModificationTime =<< processables)
  where
    processables :: IO [FilePath]
    processables = filter (\p -> not $ or [ ".locked" `isSuffixOf` p
                                          , ".invalid" `isSuffixOf` p
                                          ]) <$>
      getSubDirectories testIncoming
  
    sortOnModificationTime :: [FilePath] -> IO [FilePath]
    sortOnModificationTime dirs =
      map fst . sortOn snd <$> mapM (\d -> (,) d . fileModificationInUTCTime <$> getFileStatus d) dirs

    createFeedbacks :: [FilePath] -> IO [(SubmissionKey, [Feedback])]
    createFeedbacks dirs = catMaybes <$> mapM
      (\p -> (Just <$> createFeedback p <* removeDirectoryRecursive p) `catch` setAside p `catch` eliminateException) dirs
      where
        setAside :: FilePath -> SomeException -> IO (Maybe (SubmissionKey, [Feedback]))
        setAside p exc = do
          let invalid = p ++ ".invalid"
          renameDirectory p invalid
          fileSave (invalid </> "reason") (show exc)
          return Nothing

        eliminateException :: SomeException -> IO (Maybe (SubmissionKey, [Feedback]))
        eliminateException _ = return Nothing

    createFeedback :: FilePath -> IO (SubmissionKey, [Feedback])
    createFeedback path = do
      sk <- SubmissionKey <$> fileLoad (path </> "id")
      msgForAdmin <- optional $ getFeedback (Right . MessageForAdmin . T.pack) "private"
      msgForStudent <- optional $ getFeedback (Right . MessageForStudent . T.pack) "public"
      testResult <- getFeedback (fmap TestResult . readEither) "result"
      return (sk, testResult : catMaybes [msgForAdmin, msgForStudent])
        where
          optional :: IO a -> IO (Maybe a)
          optional m =
            handleJust
              (\exc -> if isDoesNotExistError exc then Just () else Nothing)
              (\_ -> return Nothing)
              (Just <$> m)

          getFeedback :: (String -> Either String FeedbackInfo) -> FilePath -> IO Feedback
          getFeedback f file = do
            contents <- fileLoad (path </> file)
            case f contents of
              Right info -> do
                timestamp <- fileModificationInUTCTime <$> getFileStatus (path </> file)
                return $ Feedback info timestamp
              Left err ->
                throwIO $ userError $ concat ["Non-parseable data in file ", file, ": ", err]

uploadForMoss :: FilePath -> ProgrammingLanguage -> [(User, Submission)] -> IO (ExitCode, String)
uploadForMoss mossScriptPath prLang subms = do
  d <- (plagiarism </>) . show <$> nextRandom
  createDirectoryIfMissing True d
  files <- foldM (saveSubmissions d) [] subms
  let moss = (proc mossScriptPath ("-l" : prMossParameter prLang : files)) { cwd = Just d }
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode moss ""
  return (exitCode, stderr ++ "\n" ++ stdout)

  where
    saveSubmissions :: FilePath -> [FilePath] -> (User, Submission) -> IO [FilePath]
    saveSubmissions dir ps (u, subm) = do
    let saveSimple sol = do
          let (fname, ext) = submissionFilename u subm
              nameInDir = fname <.> ext
              submPath = dir </> nameInDir
          fileSaveText submPath sol
          return nameInDir
    submissionValue
      (\s -> do path <- saveSimple s
                return $ path : ps)
      (\_ -> return ps)
      (solution subm)
