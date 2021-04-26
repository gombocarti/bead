{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeApplications #-}
module Bead.View.Content.ExportSubmissions (
    exportSubmissions
  , exportSubmissionsOfGroups
  , exportSubmissionsOfOneGroup
  , getSubmissionsOfAssignmentInGroup
  , getSubmissionsOfUserInGroup
  , localTimeInSeconds
  ) where

import qualified Codec.Archive.Zip as Zip
import           Control.Applicative ((<$>))
import           Data.Char (toLower)
import           Data.Foldable (foldrM)
import           Control.Monad (foldM, forM, mapM)
import           Control.Monad.Trans (lift)
import           Control.Monad.IO.Class (liftIO)
import           Data.List (intersperse)
import           Data.Maybe (catMaybes)
import           Data.String (fromString)
import qualified Data.Time.Clock as UTC
import qualified Data.Time.Clock.POSIX as Time
import qualified Data.Time.LocalTime as LocalTime
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBs
import           Prelude hiding (zip)
import           System.FilePath ((</>), (<.>))

import           Bead.Controller.UserStories (UserStory)
import qualified Bead.Controller.UserStories as Story
import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.Domain.String (removeAccents, replaceSlash, porcelain)
import           Bead.View.Content.GetSubmission (submissionFilename)
import qualified Bead.View.Content.Comments as C
import           Bead.View.Content
import           Bead.View.ContentHandler (Mime(MimeZip))
import           Bead.View.RequestParams (groupKeyParamName)

exportSubmissions :: DataHandler
exportSubmissions = DataHandler $ do
  ak <- getParameter assignmentKeyPrm
  exportAllSubmissions ak

exportAllSubmissions :: AssignmentKey -> ContentHandler File
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

getSubmissionsOfUserInGroup :: DataHandler
getSubmissionsOfUserInGroup = DataHandler $ do
  gk <- getParameter (customGroupKeyPrm groupKeyParamName)
  uid <- getParameter uidPrm'
  subms <- userStory $ do
    asgs <- Story.allAssignmentsOfGroup gk
    username <- Story.uidToUsername uid
    foldrM (loadLastSubmission username) [] asgs
  convertToLocalTime <- userTimeZoneToLocalTimeConverter
  let zipFileName = E.uid T.pack uid
  downloadZip zipFileName (zipSubmissions convertToLocalTime subms)
    where
      loadLastSubmission :: Username -> (AssignmentKey, Assignment) -> [(Submission, Assignment)] -> UserStory [(Submission, Assignment)]
      loadLastSubmission u (ak, a) acc = do
        lastSk <- Story.lastSubmission ak u
        case lastSk of
          Just sk -> do
            subm <- Story.loadSubmission sk
            return $ (subm, a) : acc
          Nothing ->
            return acc

      zipSubmissions :: UserTimeConverter -> [(Submission, Assignment)] -> Zip.Archive
      zipSubmissions convertToLocalTime = foldr (zipSubmission convertToLocalTime) Zip.emptyArchive

      zipSubmission :: UserTimeConverter -> (Submission, Assignment) -> Zip.Archive -> Zip.Archive
      zipSubmission convertToLocalTime (s, a) archive = addSubmissionToArchive fileName convertToLocalTime s archive
        where
          fileName :: String
          fileName = porcelain (T.unpack $ Assignment.name a) <.> submissionExtension s

-- This one is intended for scripts. Therefore file name
-- consists only of uid and user full name is not included.
getSubmissionsOfAssignmentInGroup :: DataHandler
getSubmissionsOfAssignmentInGroup = DataHandler $ do
  ak <- getParameter assignmentKeyPrm
  gk <- getParameter (customGroupKeyPrm groupKeyParamName)
  (userSubmissions, asg) <- userStory $ do
    Story.isAdminOfGroupOrCourse gk
    asg <- Story.loadAssignment ak
    usernames <- Story.subscribedToGroup gk
    userSubmissions <- foldM
          (\acc uname -> do
              mSubmKey <- Story.lastSubmission ak uname
              case mSubmKey of
                Just sk -> do
                  u <- Story.loadUser uname
                  subm <- Story.loadSubmission sk
                  return ((u, subm) : acc)
                Nothing ->
                  return acc
          )
          []
          usernames
    return (userSubmissions, asg)
  convertToLocalTime <- userTimeZoneToLocalTimeConverter
  downloadZip (Assignment.name asg) (zipSubmissions convertToLocalTime userSubmissions)

  where
    zipSubmissions :: UserTimeConverter -> [(User, Submission)] -> Zip.Archive
    zipSubmissions convertToLocalTime = foldr (zipSubmission convertToLocalTime) Zip.emptyArchive

    zipSubmission :: UserTimeConverter -> (User, Submission) -> Zip.Archive -> Zip.Archive
    zipSubmission convertToLocalTime (u, s) archive = addSubmissionToArchive fileName convertToLocalTime s archive
      where
        fileName :: String
        fileName = E.uid porcelain (E.u_uid u) <.> submissionExtension s

submissionExtension :: Submission -> String
submissionExtension = E.submissionValue (const "txt") (const "zip") . solution

zipAssignmentText :: Assignment -> LocalTime.LocalTime -> Zip.Entry
zipAssignmentText assignment now =
  Zip.toEntry (removeAccents (T.unpack title) <.> "txt") (localTimeInSeconds now) (LBs.fromStrict $ TE.encodeUtf8 exercise)
    where
      title, exercise :: Text
      (title, exercise) = Assignment.assignmentCata
                            (\name desc _type _start _end _evtype -> (name, desc))
                            assignment

zipAssignmentAndSubmissions :: AssignmentKey -> [SubmissionKey] -> ContentHandler File
zipAssignmentAndSubmissions ak sks = do
  convertToLocalTime <- userTimeZoneToLocalTimeConverter
  now <- liftIO $ convertToLocalTime <$> UTC.getCurrentTime
  assignment <- userStory (Story.loadAssignment ak)
  let exerciseFile = zipAssignmentText assignment now
      atitle = Assignment.name assignment
      submissionFolder = removeAccents (T.unpack atitle)
  submissions <- zipSubmissions sks submissionFolder now
  downloadZip atitle (Zip.addEntryToArchive exerciseFile submissions)

downloadZip :: Text -> Zip.Archive -> ContentHandler File
downloadZip filename archive = downloadLazy (filename <> ".zip") MimeZip (Zip.fromArchive archive)

zipSubmissions :: [SubmissionKey] -> String -> LocalTime.LocalTime -> ContentHandler Zip.Archive
zipSubmissions sks folder now = do
  convertToLocalTime <- userTimeZoneToLocalTimeConverter
  msg <- i18nE
  foldM
    (\archive sk -> do
        (submission, desc) <- userStory (Story.getSubmission sk)
        let addSubmission = zipSubmission submission desc folder convertToLocalTime
            addFeedback = zipFeedback desc folder now convertToLocalTime msg
        return (addSubmission (addFeedback archive)))
    Zip.emptyArchive
    sks

zipSubmission :: Submission -> SubmissionDesc -> String -> UserTimeConverter -> Zip.Archive -> Zip.Archive
zipSubmission submission desc folder convertToLocalTime archive =
  addSubmissionToArchive path convertToLocalTime submission archive
    where
      fname, ext :: String
      (fname, ext) = submissionFilename desc

      path :: String
      path = (replaceSlash folder </> replaceSlash (removeAccents fname) <.> ext)

addSubmissionToArchive :: String -> UserTimeConverter -> Submission -> Zip.Archive -> Zip.Archive
addSubmissionToArchive path convertToLocalTime submission archive =
  Zip.addEntryToArchive
    (submissionCata
      (\solution postTime ->
          let timeStamp = localTimeInSeconds . convertToLocalTime $ postTime
              contents = E.submissionValueToByteString solution
          in Zip.toEntry path timeStamp contents)
      submission)
    archive

zipFeedback :: SubmissionDesc -> String -> LocalTime.LocalTime -> UserTimeConverter -> I18N -> Zip.Archive -> Zip.Archive
zipFeedback desc folder now convertToLocalTime msg archive =
  Zip.addEntryToArchive (feedbacksFile path) archive
    where
      fname :: String
      (fname, _) = submissionFilename desc

      path :: String
      path = replaceSlash folder </> replaceSlash (removeAccents (concat [fname, "_", T.unpack . msg $ msg_ExportSubmissions_Comments "comments" ])) <.> "txt"

      feedbacksFile :: String -> Zip.Entry
      feedbacksFile path = Zip.toEntry path (localTimeInSeconds now) (LBs.fromStrict $ TE.encodeUtf8 $ T.unlines $ intersperse (T.replicate 10 "#") feedbacks)

      feedbacks :: [Text]
      feedbacks = map (\cf -> T.unlines
                                [ "## " <> authorLine cf
                                , ""
                                , C.commentOrFeedbackText msg cf
                                ])
                  $ C.sortDecreasingTime
                  $ C.submissionDescToCFs desc

      authorLine :: C.CommentOrFeedback -> Text
      authorLine cf = T.unwords [T.pack . showDate . convertToLocalTime . C.commentOrFeedbackTime $ cf, C.commentOrFeedbackAuthor msg cf]

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
