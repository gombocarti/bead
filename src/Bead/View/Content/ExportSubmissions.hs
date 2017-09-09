{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Bead.View.Content.ExportSubmissions (
    exportSubmissions
  , exportSubmissionsOfGroups
  , exportSubmissionsOfOneGroup
  ) where

import qualified Codec.Archive.Zip as Zip
import           Control.Applicative ((<$>))
import           Data.Char (toUpper)
import           Control.Monad (forM, mapM)
import           Control.Monad.Trans (lift)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (intersperse)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.String (fromString)
import qualified Data.Time.Clock as UTC
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.LocalTime as LocalTime
import qualified Data.ByteString.Lazy.UTF8 as LBsUTF8
import qualified Data.ByteString.Lazy as LBs
import           Prelude hiding (zip)
import           System.FilePath ((</>), (<.>))

import qualified Bead.Controller.UserStories as Story
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.View.Content.GetSubmission (submissionFilename)
import qualified Bead.View.Content.Comments as C
import           Bead.View.Content
import           Bead.View.ContentHandler (Mime(MimeZip), downloadFile)
import           Bead.View.RequestParams (groupKeyParamName)

exportSubmissions :: DataHandler
exportSubmissions = DataHandler $ do
  ak <- getParameter assignmentKeyPrm
  exportAllSubmissions ak

exportAllSubmissions :: AssignmentKey -> ContentHandler ()
exportAllSubmissions ak = do
  sks <- userStory (Story.lastSubmissions ak)
  zipAssignmentAndSubmissions ak sks

exportSubmissionsOfGroups :: DataHandler
exportSubmissionsOfGroups = DataHandler $ do
  ak <- getParameter assignmentKeyPrm
  cgk <- userStory (Story.courseOrGroupOfAssignment ak)
  case cgk of
    Right gk -> exportAllSubmissions ak
    Left ck  -> do
      sks <- userStory $ do
        groups <- Story.administratedGroupsOfCourse ck
        usernames <- concat <$> mapM Story.subscribedToGroup groups
        catMaybes <$> mapM (Story.lastSubmission ak) usernames
      zipAssignmentAndSubmissions ak sks

exportSubmissionsOfOneGroup :: DataHandler
exportSubmissionsOfOneGroup = DataHandler $ do
  ak <- getParameter assignmentKeyPrm
  gk <- getParameter (customGroupKeyPrm groupKeyParamName)
  sks <- userStory $ do
    usernames <- Story.subscribedToGroup gk
    catMaybes <$> mapM (Story.lastSubmission ak) usernames
  zipAssignmentAndSubmissions ak sks

zipAssignmentText :: Assignment -> LocalTime.LocalTime -> Zip.Entry
zipAssignmentText assignment now =
  Zip.toEntry (title ++ ".txt") (localTimeInSeconds now) (LBsUTF8.fromString exercise)
    where
      title, exercise :: String
      (title, exercise) = Assignment.assignmentCata
                            (\name desc _type _start _end _evtype -> (removeAccents name, desc))
                            assignment

zip :: [(Zip.Entry, Zip.Entry)] -> Zip.Archive
zip = foldr step Zip.emptyArchive
    where
      step (subm, feedback) archive = add feedback (add subm archive)

add :: Zip.Entry -> Zip.Archive -> Zip.Archive
add = Zip.addEntryToArchive

zipAssignmentAndSubmissions :: AssignmentKey -> [SubmissionKey] -> ContentHandler ()
zipAssignmentAndSubmissions ak sks = do
  convertToLocalTime <- userTimeZoneToLocalTimeConverter
  now <- liftIO $ convertToLocalTime <$> UTC.getCurrentTime
  assignment <- userStory (Story.loadAssignment ak)
  let exerciseFile = zipAssignmentText assignment now
      atitle = Assignment.name assignment
      exportedFName = atitle ++ ".zip"
      submissionFolder = removeAccents atitle
  submissions <- zipSubmissions sks submissionFolder now
  downloadZip exportedFName (add exerciseFile submissions)

downloadZip :: String -> Zip.Archive -> ContentHandler ()
downloadZip filename zip = downloadFile filename (Zip.fromArchive zip) MimeZip

zipSubmissions :: [SubmissionKey] -> String -> LocalTime.LocalTime -> ContentHandler Zip.Archive
zipSubmissions sks folder now = do
  convertToLocalTime <- userTimeZoneToLocalTimeConverter
  msg <- lift i18nH
  files <- forM sks $ \sk -> do
    (submission, desc) <- userStory (Story.getSubmission sk)
    let subm     = zipSubmission submission desc folder convertToLocalTime
        feedback = zipFeedback desc folder now convertToLocalTime msg
    return (subm, feedback)
  return $ zip files

zipSubmission :: Submission -> SubmissionDesc -> String -> UserTimeConverter -> Zip.Entry
zipSubmission submission desc folder convertToLocalTime = submissionFile
  where
    fname, ext, path :: String
    (fname, ext) = submissionFilename desc
    path = (folder </> removeAccents fname <.> ext)

    submissionFile :: Zip.Entry
    submissionFile = 
      submissionCata
        (\contents postTime -> Zip.toEntry path (localTimeInSeconds . convertToLocalTime $ postTime) (toByteString contents))
        submission

    toByteString :: SubmissionValue -> LBs.ByteString
    toByteString solution = submissionValue LBsUTF8.fromString LBs.fromStrict solution

zipFeedback :: SubmissionDesc -> String -> LocalTime.LocalTime -> UserTimeConverter -> I18N -> Zip.Entry
zipFeedback desc folder now convertToLocalTime msg = feedbacksFile path
    where
      fname, path :: String
      (fname, _) = submissionFilename desc
      path = folder </> removeAccents (concat [fname, "_", msg $ msg_ExportSubmissions_Comments "comments" ]) <.> "txt"

      feedbacksFile :: String -> Zip.Entry
      feedbacksFile path = Zip.toEntry path (localTimeInSeconds now) (LBsUTF8.fromString $ unlines $ intersperse (replicate 10 '#') feedbacks)

      feedbacks :: [String]
      feedbacks = map (\cf -> unlines [ "## " ++ authorLine cf
                                      , ""
                                      , C.commentOrFeedbackText msg cf
                                      ])
                  $ C.sortDecreasingTime
                  $ C.submissionDescToCFs desc

      authorLine :: C.CommentOrFeedback -> String
      authorLine cf = unwords [show . convertToLocalTime . C.commentOrFeedbackTime $ cf, C.commentOrFeedbackAuthor msg cf]

{-
   This is a workaround. 
   File modification times should be in local time.
   But the zip api expects modification time in seconds from unix epoch.
   localTimeInSeconds shifts an utc time so that it represents the local time of type UTCTime and
   returns the seconds from unix epoch to the shifted utc time.
   That is, utc + diff in seconds from unix epoch.
-}
localTimeInSeconds :: LocalTime.LocalTime -> Integer
localTimeInSeconds = utcTimeInSeconds . LocalTime.localTimeToUTC LocalTime.utc
  where
    utcTimeInSeconds :: UTC.UTCTime -> Integer
    utcTimeInSeconds = round . Time.utcTimeToPOSIXSeconds

-- | Converts hungarian accute accented letters into ones without accents,
-- in order to be compatible with zip managers and to avoid errors and
-- unreadable files.
removeAccents :: String -> String
removeAccents = map removeAccent
  where
      removeAccent :: Char -> Char
      removeAccent c = case Map.lookup c conversion of
                         Just latinLetter -> latinLetter
                         Nothing -> c

      conversion :: Map.Map Char Char
      conversion = Map.fromList (matching ++ upperCaseMatching)
        where
          matching :: [(Char, Char)]
          matching = [ ('á', 'a')
                     , ('é', 'e')
                     , ('í', 'i')
                     , ('ó', 'o')
                     , ('ö', 'o')
                     , ('ő', 'o')
                     , ('ú', 'u')
                     , ('ü', 'u')
                     , ('ű', 'u')
                     ]

          upperCaseMatching :: [(Char, Char)]
          upperCaseMatching = map (\(c1, c2) -> (toUpper c1, toUpper c2)) matching
