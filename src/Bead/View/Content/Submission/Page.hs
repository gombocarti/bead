{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Submission.Page (
    submission
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import           Data.List (intersperse, partition)
import           Data.Maybe (listToMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time

import           Snap.Util.FileUploads
import           System.Directory (doesFileExist)
import           System.FilePath.Posix (takeExtension)
import qualified Text.Blaze as B
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

import           Bead.Config (maxUploadSizeInKb)
import qualified Bead.Controller.Pages as Pages
import           Bead.Controller.UserStories (UserStory)
import qualified Bead.Controller.UserStories as Story
import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Entity.Assignment as Assignment
import qualified Bead.Domain.Shared.Evaluation as Eval
import           Bead.View.Content hiding (submissionForm)
import qualified Bead.View.Content as C
import           Bead.View.ContentHandler (modifyPageSettings)
import           Bead.View.Content.Bootstrap ((.|.))
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.View.Content.StateVisualization as SV
import           Bead.View.Content.Submission.Common
import           Bead.View.Markdown (markdownToHtml, minHeaderLevel)

submission = ViewModifyHandler submissionPage submissionPostHandler

data PageData = PageData {
    asKey   :: AssignmentKey
  , asValue :: Assignment
  , asHasTestCase :: HasTestCase
  , asTimeConv :: UserTimeConverter
  , asNow :: UTCTime
  , asMaxFileSize :: Int
  , asLimit :: SubmissionLimit
  , asSubmissions :: [SubmissionInfo]
  , asCourse  :: Course
  , asGroup :: Maybe Group
  }

data UploadResult
  = PolicyFailure
  | File FilePath !ByteString
  | InvalidFile
  | UnnamedFile

submissionPage :: GETContentHandler
submissionPage = do
  ak <- getParameter assignmentKeyPrm
  ut <- userTimeZoneToLocalTimeConverter
  now <- liftIO $ getCurrentTime
  size <- fmap maxUploadSizeInKb $ beadHandler getConfiguration
  (asg, (course, grp), limit, submissions, hasTest) <- userStory $ do
    Story.doesBlockAssignmentView ak
    Story.isUsersAssignment ak
    asg <- Story.loadAssignment ak
    courseAndGroup <- Story.courseAndGroupOfAssignment ak
    lmt <- Story.assignmentSubmissionLimit ak
    submissions <- Story.userSubmissionInfos ak
    hasTest <- Story.hasAssignmentTestCase ak
    return $! (asg, courseAndGroup, lmt, submissions, hasTest)

  if (now < Assignment.start asg)
    then setPageContents $ htmlPage (msg_LinkText_Submission "Submission") $
           assignmentNotAvailableYetContent
    else do
      modifyPageSettings E.enableFullMarkdownRendering
      setPageContents $ htmlPage (msg_LinkText_Submission "Submission") $
        submissionContent $
          PageData {
              asKey = ak
            , asValue = asg
            , asHasTestCase = hasTest
            , asTimeConv = ut
            , asNow = now
            , asMaxFileSize = size
            , asLimit = limit
            , asSubmissions = submissions
            , asCourse = course
            , asGroup = grp
            }

submissionPostHandler :: POSTContentHandler
submissionPostHandler = do
  ak <- getParameter assignmentKeyPrm
  userStory $ do
    Story.doesBlockAssignmentView ak
    Story.isUsersAssignment ak
  uploadResult <- beadHandler $ do
    tmpDir <- getTempDirectory
    size <- maxUploadSizeInKb <$> getConfiguration
    let maxSize = fromIntegral (size * 1024)
    let uploadPolicy = setMaximumFormInputSize maxSize defaultUploadPolicy
    let perPartUploadPolicy = const $ allowWithMaximumSize maxSize
    handleFileUploads tmpDir uploadPolicy perPartUploadPolicy handlePart
  asg <- userStory $ Story.loadAssignment ak
  -- Assignment is for the user
  let aspects = Assignment.aspects asg
      self = Pages.submission ak ()
      action story = Action $ do
        story
        return $ redirection self
  if Assignment.isPasswordProtected aspects
    -- Password-protected assignment
    then do pwd <- getParameter (stringParameter (fieldName submissionPwdField) "Submission password")
            if Assignment.getPassword aspects == pwd
              -- Passwords do match
              then action <$> newSubmission ak aspects uploadResult
              -- Passwords do not match
              else return $ action $ Story.putErrorMessage $ msg_Submission_InvalidPassword "Invalid password, the solution could not be submitted!"
    -- Non-password protected assignment
    else action <$> newSubmission ak aspects uploadResult
  where
    newSubmission :: AssignmentKey -> Aspects -> [UploadResult] -> ContentHandler (UserStory ())
    newSubmission ak as up =
      if (not $ Assignment.isZippedSubmissions as)
        then do
           subm <- getParameter (textParameter (fieldName submissionTextField) "Submission text")
           return $ void $ submit $ SimpleSubmission subm
        else
          case uploadedFile of
            Just (File name contents) -> do
              let zipSignature = B.pack "PK"
              if (zipSignature `B.isPrefixOf` contents)
                then return $ void $ submit $ ZippedSubmission contents
                else return $ Story.putErrorMessage $ msg_Submission_File_InvalidFile
                       "The file to be uploaded does not appear to be a zip file. Invalid file extension and signature."
            Just PolicyFailure      -> return $
              Story.putErrorMessage $ msg_Submission_File_PolicyFailure
                "The upload policy has been violated, probably the file was too large."
            Nothing                 -> return $
              Story.putErrorMessage $ msg_Submission_File_NoFileReceived
                "No file has been received."
            _                       -> return $
              Story.putErrorMessage $ msg_Submission_File_InternalError
                "Some error happened during upload."
       where
         submit s = (E.Submission s <$> liftIO getCurrentTime) >>= Story.submitSolution ak
         uploadedFile = listToMaybe $ uncurry (++) $ partition isFile up

         isFile (File _ _) = True
         isFile _          = False

    handlePart _partInfo (Left _exception) = return PolicyFailure
    handlePart partInfo (Right filePath) =
      case (partFileName partInfo) of
        Just fp | not (B.null fp) -> do
          contents <- liftIO $ do
            exists <- doesFileExist filePath
            if exists
              then do
                body <- B.readFile filePath
                return $ Just body
              else return $ Nothing
          return $ case contents of
            Just body -> File (B.unpack fp) body
            _         -> InvalidFile
        _                         -> return UnnamedFile

assignmentNotAvailableYetContent :: IHtml
assignmentNotAvailableYetContent = do
  msg <- getI18N
  return $ Bootstrap.rowColMd12 $ Bootstrap.alert Bootstrap.Danger $
    B.toMarkup $ msg $ msg_Submission_AssignmentNotAvailableYet "The assignment is not available yet. Check back later."

submissionContent :: PageData -> IHtml
submissionContent p = do
  msg <- getI18N
  return $ do
    -- Informational table on the page
    Bootstrap.rowColMd12 $ Bootstrap.table $
      H.tbody $ do
        let course = asCourse p
            grp = asGroup p
          in (msg $ msg_Submission_Course "Course:") .|. maybe (courseName course) (fullGroupName course) grp
        (msg $ msg_Submission_Assignment "Assignment:") .|. (Assignment.name $ asValue p)
        when (Assignment.isActive (asValue p) (asNow p)) $
          (msg $ msg_Submission_Test "Test:") .|. (msg $ infoOnTestCase)
        (msg $ msg_Submission_Deadline "Deadline:")     .|.
          (showDate . (asTimeConv p) . Assignment.end $ asValue p)
        (msg $ msg_Submission_TimeLeft "Time left:")    .|. (startEndCountdownDiv
                "ctd"
                (msg $ msg_Submission_Days "day(s)")
                (msg $ msg_Submission_DeadlineReached "Deadline is reached")
                (asNow p)
                (Assignment.end $ asValue p))
        maybe (return ()) (uncurry (.|.)) (remainingTries msg (asLimit p))
    Bootstrap.rowColMd12 $ do
      submissionLimit
        (const $ submissionForm msg)
        (const . const $ submissionForm msg)
        (const $ limitReached msg)
        (asLimit p)

    Bootstrap.rowColMd12 $ do
      let submissions = asSubmissions p
      userSubmissionInfo msg submissions

    Bootstrap.rowColMd12 H.hr

    Bootstrap.rowColMd12 $
      minHeaderLevel 2 . markdownToHtml msg $ Assignment.desc $ asValue p

  where
    submission = Pages.submission (asKey p) ()
    aspects = Assignment.aspects $ asValue p

    limitReached :: I18N -> H.Html
    limitReached msg = Bootstrap.alert Bootstrap.Danger $ H.p $ B.toMarkup $ msg $
      msg_Submission_LimitReached "Submission limit is reached."

    infoOnTestCase :: Translation
    infoOnTestCase = case asHasTestCase p of
                       HasTestCase -> msg_SubmissionWillBeTested "The submission will be automatically tested."
                       DoesNotHaveTestCase -> msg_AssignmentDoesntHaveTestCase "The assignment does not have test case."

    submissionForm :: I18N -> H.Html
    submissionForm msg =
      if (Assignment.isActive (asValue p) (asNow p))
        then do
          h2 $ B.toMarkup $ msg $ msg_Submission_Solution "Solution"
          postForm (routeOf submission) `withId` (rFormId C.submissionForm) ! A.enctype "multipart/form-data" $ do
            assignmentPassword msg
            if (Assignment.isZippedSubmissions aspects)
              then
                Bootstrap.formGroup $ do
                  Bootstrap.helpBlock $
                    (msg $ msg_Submission_Info_File
                      "Please select a file with .zip extension to submit.  Note that the maximum file size in kilobytes: ") <>
                    (T.pack $ show $ asMaxFileSize p)
                  fileInput (fieldName submissionFileField)
              else
                Bootstrap.textArea (fieldName submissionTextField :: Text) ("" :: Text) Bootstrap.Medium ""
            -- alert has 20px spacing at the bottom
            Bootstrap.submitButton (fieldName submitSolutionBtn :: Text) (msg $ msg_Submission_Submit "Submit") ! A.style "margin-bottom: 20px;"
        else
          Bootstrap.alert Bootstrap.Danger $ H.p $ B.toMarkup . msg $
            msg_Submission_SubmissionFormDeadlineReached "Deadline is reached."

    assignmentPassword :: I18N -> H.Html
    assignmentPassword msg =
      when (Assignment.isPasswordProtected aspects) $ do
        H.p $ B.toMarkup . msg $ msg_Submission_Info_Password
          "This assignment can only accept submissions by providing the password."
        Bootstrap.passwordInput (fieldName submissionPwdField :: Text) (msg $ msg_Submission_Password "Password for the assignment:")

    userSubmissionInfo :: I18N -> [SubmissionInfo] -> H.Html
    userSubmissionInfo msg submissions =
      userSubmission msg (submissionLine msg) submissions

    userSubmission :: I18N -> (SubmissionInfo -> H.Html) -> [SubmissionInfo] -> H.Html
    userSubmission msg line submissions
      | not $ null submissions =
          Bootstrap.rowColMd12 $ Bootstrap.listGroupHeightLimit 4 $ mapM_ line submissions
      | otherwise =
          Bootstrap.rowColMd12 $ H.p $ B.toMarkup $ msg $ msg_Submission_NoSubmittedSolutions "There are no submissions."

    submissionLine :: I18N -> SubmissionInfo -> H.Html
    submissionLine msg (sk, state, time) = do
      Bootstrap.listGroupLinkItem
        (routeOf $ submissionDetails (asKey p) sk)
        (do B.toMarkup . showDate $ (asTimeConv p) time
            SV.formatSubmissionState SV.toColoredBadge msg state
        )
        where
          submissionDetails :: AssignmentKey -> SubmissionKey -> Pages.Page a b () c d f
          submissionDetails ak sk = Pages.submissionDetails ak sk ()
