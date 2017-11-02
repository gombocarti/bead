{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Submission.Page (
    submission
  ) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.ByteString.Char8 as B
import           Data.List (intersperse, partition)
import           Data.Maybe (listToMaybe)
import           Data.String (fromString)
import           Data.Time

import           Snap.Util.FileUploads
import           System.Directory (doesFileExist)
import           System.FilePath.Posix (takeExtension)
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

import           Bead.Config (maxUploadSizeInKb)
import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Entity.Assignment as Assignment
import qualified Bead.Domain.Shared.Evaluation as Eval
import           Bead.View.Content hiding (submissionForm)
import qualified Bead.View.Content as C
import           Bead.View.Content.Bootstrap ((.|.))
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.Submission.Common
import           Bead.View.Markdown (markdownToHtml)

submission = ViewModifyHandler submissionPage submissionPostHandler

data PageData = PageData {
    asKey   :: AssignmentKey
  , asValue :: Assignment
  , asDesc  :: AssignmentDesc
  , asTimeConv :: UserTimeConverter
  , asNow :: UTCTime
  , asMaxFileSize :: Int
  , asLimit :: SubmissionLimit
  , asSubmissions :: UserSubmissionInfo
  }

data UploadResult
  = PolicyFailure
  | File FilePath !ByteString
  | InvalidFile
  | UnnamedFile
  deriving (Eq,Show)

submissionPage :: GETContentHandler
submissionPage = do
  ak <- getParameter assignmentKeyPrm
  ut <- userTimeZoneToLocalTimeConverter
  now <- liftIO $ getCurrentTime
  size <- fmap maxUploadSizeInKb $ lift getConfiguration
  (limit, aDesc, asg, submissions) <- userStory $ do
    (aDesc, asg) <- Story.userAssignmentForSubmission ak
    lmt <- Story.assignmentSubmissionLimit ak
    submissions <- Story.userSubmissionInfos ak
    return $! (lmt, aDesc, asg, submissions)

  if (now < Assignment.start asg)
    then return assignmentNotAvailableYetContent
    else return $ submissionContent $
           PageData {
               asKey = ak
             , asValue = asg
             , asDesc = aDesc
             , asTimeConv = ut
             , asNow = now
             , asMaxFileSize = size
             , asLimit = limit
             , asSubmissions = submissions
             }

submissionPostHandler :: POSTContentHandler
submissionPostHandler = do
  uploadResult <- lift $ do
    tmpDir <- getTempDirectory
    size <- maxUploadSizeInKb <$> getConfiguration
    let maxSize = fromIntegral (size * 1024)
    let uploadPolicy = setMaximumFormInputSize maxSize defaultUploadPolicy
    let perPartUploadPolicy = const $ allowWithMaximumSize maxSize
    handleFileUploads tmpDir uploadPolicy perPartUploadPolicy handlePart
  ak <- getParameter assignmentKeyPrm
  (_desc,asg) <- userStory $ Story.userAssignmentForSubmission ak
  -- Assignment is for the user
  let aspects = Assignment.aspects asg
  if Assignment.isPasswordProtected aspects
    -- Password-protected assignment
    then do pwd <- getParameter (stringParameter (fieldName submissionPwdField) "Submission password")
            if Assignment.getPassword aspects == pwd
              -- Passwords do match
              then newSubmission ak aspects uploadResult
              -- Passwords do not match
              else return . ErrorMessage $ msg_Submission_InvalidPassword "Invalid password, the solution could not be submitted!"
    -- Non password protected assignment
    else newSubmission ak aspects uploadResult
  where
    newSubmission ak as up =
      if (not $ Assignment.isZippedSubmissions as)
        then submit $ SimpleSubmission <$> getParameter (stringParameter (fieldName submissionTextField) "Submission text")
        else
          case uploadedFile of
            Just (File name contents) ->
              if (takeExtension name == ".zip")
                then submit $ return $ ZippedSubmission contents
                else return $
                  ErrorMessage $ msg_Submission_File_InvalidFile
                    "The extension of the file to be uploaded is incorrect."
            Just PolicyFailure      -> return $
              ErrorMessage $ msg_Submission_File_PolicyFailure
                "The upload policy has been violated, probably the file was too large."
            Nothing                 -> return $
              ErrorMessage $ msg_Submission_File_NoFileReceived
                "No file has been received."
            _                       -> return $
              ErrorMessage $ msg_Submission_File_InternalError
                "Some error happened during upload."
       where
         submit s = NewSubmission ak <$> (E.Submission <$> s <*> liftIO getCurrentTime)
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
            Just body -> File (unpack fp) body
            _         -> InvalidFile
        _                         -> return UnnamedFile

assignmentNotAvailableYetContent :: IHtml
assignmentNotAvailableYetContent = do
  msg <- getI18N
  return $ Bootstrap.rowColMd12 $ Bootstrap.alert Bootstrap.Danger $
    H.p $ fromString $ msg $ msg_Submission_AssignmentNotAvailableYet "The assignment is not available yet. Check back later."

submissionContent :: PageData -> IHtml
submissionContent p = do
  msg <- getI18N
  return $ do
    -- Informational table on the page
    Bootstrap.rowColMd12 $ Bootstrap.table $
      H.tbody $ do
        (msg $ msg_Submission_Course "Course: ")         .|. (fromString . aGroup $ asDesc p)
        (msg $ msg_Submission_Admin "Teacher: ")         .|. (fromString . concat . intersperse ", " . sortHun . aTeachers $ asDesc p)
        (msg $ msg_Submission_Assignment "Assignment: ") .|. (fromString . Assignment.name $ asValue p)
        (msg $ msg_Submission_Deadline "Deadline: ")     .|.
          (fromString . showDate . (asTimeConv p) . Assignment.end $ asValue p)
        (msg $ msg_Submission_TimeLeft "Time left: ")    .|. (startEndCountdownDiv
                "ctd"
                (msg $ msg_Submission_Days "day(s)")
                (msg $ msg_Submission_DeadlineReached "Deadline is reached")
                (asNow p)
                (Assignment.end $ asValue p))
        maybe (return ()) (uncurry (.|.)) (remainingTries msg (asLimit p))
    Bootstrap.rowColMd12 $ do
      let submissions = asSubmissions p
      userSubmissionInfo msg submissions
       
    Bootstrap.rowColMd12 $ do
      submissionLimit
        (const $ submissionForm msg)
        (const . const $ submissionForm msg)
        (const $ limitReached msg)
        (asLimit p)
 
    Bootstrap.rowColMd12 $ do
      H.h2 $ fromString $ msg $ msg_Submission_Description "Description"
      H.div # assignmentTextDiv $ markdownToHtml $ Assignment.desc $ asValue p

  where
    submission = Pages.submission (asKey p) ()
    aspects = Assignment.aspects $ asValue p

    limitReached :: I18N -> H.Html
    limitReached msg = Bootstrap.alert Bootstrap.Danger $ H.p $ fromString $ msg $
      msg_Submission_LimitReached "Submission limit is reached."

    submissionForm :: I18N -> H.Html
    submissionForm msg =
      if (Assignment.isActive (asValue p) (asNow p))
        then do
          h2 $ fromString $ msg $ msg_Submission_Solution "Solution"
          postForm (routeOf submission) `withId` (rFormId C.submissionForm) ! A.enctype "multipart/form-data" $ do
            assignmentPassword msg
            if (Assignment.isZippedSubmissions aspects)
              then
                Bootstrap.formGroup $ do
                  Bootstrap.helpBlock $
                    (msg $ msg_Submission_Info_File
                      "Please select a file with .zip extension to submit.  Note that the maximum file size in kilobytes: ") ++
                    (fromString $ show $ asMaxFileSize p)
                  fileInput (fieldName submissionFileField)
              else
                Bootstrap.textArea (fieldName submissionTextField) "" ""
            Bootstrap.submitButton (fieldName submitSolutionBtn) (fromString $ msg $ msg_Submission_Submit "Submit")
        else
          Bootstrap.alert Bootstrap.Danger $ H.p $ fromString . msg $
            msg_Submission_SubmissionFormDeadlineReached "Deadline is reached."

    assignmentPassword :: I18N -> H.Html
    assignmentPassword msg =
      when (Assignment.isPasswordProtected aspects) $ do
        H.p $ fromString . msg $ msg_Submission_Info_Password
          "This assignment can only accept submissions by providing the password."
        Bootstrap.passwordInput (fieldName submissionPwdField) (msg $ msg_Submission_Password "Password for the assignment:")

    userSubmissionInfo :: I18N -> UserSubmissionInfo -> H.Html
    userSubmissionInfo msg submissions =
      userSubmission msg (submissionLine msg) submissions

    userSubmission :: I18N -> ((SubmissionKey, UTCTime, SubmissionInfo, EvaluatedBy) -> H.Html) -> UserSubmissionInfo -> H.Html
    userSubmission msg line submissions
      | not $ null submissions =
          Bootstrap.rowColMd12 $ Bootstrap.listGroupHeightLimit (4 * 42) $ mapM_ line submissions
      | otherwise =
          Bootstrap.rowColMd12 $ H.p $ fromString $ msg $ msg_Submission_NoSubmittedSolutions "There are no submissions."

    submissionLine :: I18N -> (SubmissionKey, UTCTime, SubmissionInfo, EvaluatedBy) -> H.Html
    submissionLine msg (sk, time, status, _t) = do
      Bootstrap.listGroupLinkItem
        (routeOf $ submissionDetails (asKey p) sk)
        (do Bootstrap.badge (resolveStatus msg status); fromString . showDate $ (asTimeConv p) time)
        where
          submissionDetails :: AssignmentKey -> SubmissionKey -> Pages.Page a b () c d
          submissionDetails ak sk = Pages.submissionDetails ak sk ()

    resolveStatus :: I18N -> SubmissionInfo -> String
    resolveStatus msg = submissionInfoCata
      (msg $ msg_Submission_NotFound "Not found")
      (msg $ msg_Submission_NotEvaluatedYet "Not evaluated yet")
      (bool (msg $ msg_Submission_TestsPassed "Tests are passed")
            (msg $ msg_Submission_TestsFailed "Tests are failed"))
      (const (evaluationResultMsg . Eval.evResult))
      where
        evaluationResultMsg :: Eval.EvaluationData Eval.Binary Eval.Percentage Eval.FreeForm -> String
        evaluationResultMsg = evaluationResultCata
          (Eval.binaryCata (Eval.resultCata
            (msg $ msg_Submission_Passed "Passed")
            (msg $ msg_Submission_Failed "Failed")))
          (Eval.percentageCata (fromString . scores))
          (Eval.freeForm fromString)

        scores :: Eval.Scores Double -> String
        scores (Eval.Scores [])  = "0%"
        scores (Eval.Scores [p]) = concat [show . round $ 100 * p, "%"]
        scores _                 = "???%"

