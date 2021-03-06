{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.SubmissionDetails.Page (
    submissionDetails
  ) where

import           Prelude hiding (div)

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List (intercalate)
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (getCurrentTime, UTCTime)

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.View.Content
import           Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.Comments
import           Bead.View.Content.Submission.Common
import           Bead.View.Markdown

import qualified Text.Blaze as B
import           Text.Blaze.Html5 as H
import           Text.Blaze.Html5.Attributes as A

submissionDetails = ViewModifyHandler submissionDetailsPage submissionDetailsPostHandler

data PageData = PageData {
    smKey :: SubmissionKey
  , aKey  :: AssignmentKey
  , smDetails :: SubmissionDetailsDesc
  , uTime :: UserTimeConverter
  , now   :: UTCTime
  , smLimit :: SubmissionLimit
  }

submissionDetailsPage :: GETContentHandler
submissionDetailsPage = do
  ak <- getParameter assignmentKeyPrm
  sk <- getParameter submissionKeyPrm

  (limit,sd) <- userStory $ do
    Story.doesBlockSubmissionView sk
    Story.isAccessibleSubmission sk
    sd  <- Story.submissionDetailsDesc sk
    lmt <- Story.assignmentSubmissionLimit ak
    return (lmt,sd)

  tc <- userTimeZoneToLocalTimeConverter
  currentTime <- liftIO getCurrentTime
  setPageContents $ htmlPage (msg_LinkText_SubmissionDetails "Submission Details") $
    submissionDetailsContent $ PageData {
        smKey = sk
      , aKey  = ak
      , smDetails = sd
      , uTime = tc
      , now = currentTime
      , smLimit = limit
      }

submissionDetailsPostHandler :: POSTContentHandler
submissionDetailsPostHandler = do
  ak <- getParameter assignmentKeyPrm
  sk <- getParameter submissionKeyPrm
  c  <- getParameter (textParameter (fieldName commentValueField) "Comment")
  now <- liftIO $ getCurrentTime
  mname <- getName <$> userState
  let uname = case mname of
                Just un -> un
                Nothing -> "???"

  _ <- userStory $ do
    Story.doesBlockSubmissionView sk
    Story.isAccessibleSubmission sk

  return $ Action $ do
    Story.createComment sk Comment {
        comment = c
      , commentAuthor = T.pack uname
      , commentDate = now
      , commentType = CT_Student
      }
    return $ redirection $ Pages.submissionDetails ak sk ()

  where
    getName :: UserState -> Maybe String
    getName = userStateCata
      (const Nothing)
      Nothing
      Nothing
      (\_username _uid name _lang _role _uuid _timezone _status _homePage -> Just name)

submissionDetailsContent :: PageData -> IHtml
submissionDetailsContent p = do
  msg <- getI18N
  return $ do
    let info = smDetails p
    let tc   = uTime p
    Bootstrap.rowColMd12 $ Bootstrap.table $ tbody $ do
      (msg $ msg_SubmissionDetails_Course "Course:")  .|. maybe (courseName $ sdCourse info) (fullGroupName (sdCourse info)) (sdGroup info)
      (msg $ msg_SubmissionDetails_Assignment "Assignment:") .|. (Assignment.name $ sdAssignment info)
      (msg $ msg_SubmissionDetails_Deadline "Deadline:")     .|. (showDate . tc . Assignment.end $ sdAssignment info)
      maybe (return ()) (uncurry (.|.)) (remainingTries msg (smLimit p))
    let asg = sdAssignment info
    let aspects = Assignment.aspects asg
    let isProtected = Assignment.isBallotBox aspects && (now p < Assignment.end asg)
    Bootstrap.rowColMd12 $ do
      if isProtected
        then do
          H.p $ B.toMarkup . msg $ msg_SubmissionDetails_BallotBox_Info $
            "The ballot box mode is active so no solutions can be accessed until the deadline."
        else do
          let downloadSubmissionButton =
                Bootstrap.buttonLink
                  (routeOf $ Pages.getSubmission (smKey p) ())
                  (msg $ msg_SubmissionDetails_Solution_Zip_Link "Download")
          if (Assignment.isZippedSubmissions aspects)
            then do
              Bootstrap.helpBlock $ B.toMarkup . msg $ msg_SubmissionDetails_Solution_Zip_Info $ mconcat
                [ "The submission was uploaded as a compressed file so it could not be displayed verbatim.  "
                , "But it may be downloaded as a file by clicking on the link."
                ]
              downloadSubmissionButton
            else do
              H.p $ B.toMarkup . msg $ msg_SubmissionDetails_Solution_Text_Info $
                "The submission may be downloaded as a plain text file by clicking on the link."
              Bootstrap.buttonGroup $ copyToClipboardButton msg submissionIdent <> downloadSubmissionButton
              H.br
              H.pre ! A.id (B.toValue submissionIdent) $ B.toMarkup $ sdSubmission info
    Bootstrap.rowColMd12 $ do
      H.a ! A.name (anchor SubmissionDetailsEvaluationDiv) $ mempty
      h2 $ B.toMarkup $ msg $ msg_SubmissionDetails_Evaluation "Evaluation"
      resolveStatus msg $ sdStatus info
    Bootstrap.rowColMd12 $ h2 $ B.toMarkup $ msg $ msg_Comments_Title "Comments"
    when isProtected $ do
      H.p $ B.toMarkup . msg $ msg_SubmissionDetails_BallotBox_Comment_Info $
        "When ballot box mode is active, no student comments are shown until the deadline."
    postForm (routeOf $ submissionDetails (aKey p) (smKey p)) $ do
      Bootstrap.textArea (fieldName commentValueField :: Text)
                         (B.toMarkup $ msg $ msg_SubmissionDetails_NewComment "New comment")
                         Bootstrap.Small
                         mempty
      Bootstrap.submitButton ("" :: Text) (B.toMarkup $ msg $ msg_SubmissionDetails_SubmitComment "Submit")
    let studentComments = forStudentCFs isProtected $ submissionDetailsDescToCFs info
    when (not $ null studentComments) $ do
      Bootstrap.rowColMd12 hr
      i18n msg $ commentsDiv "submission-details-comments-" tc studentComments

  where
    submissionDetails ak sk = Pages.submissionDetails ak sk ()
    submissionIdent = "code"

    resolveStatus :: I18N -> Maybe Text -> H.Html
    resolveStatus msg Nothing     = B.toMarkup . msg $ msg_Submission_NotEvaluatedYet "Not evaluated yet"
    resolveStatus _msg (Just str) = B.toMarkup str


invalidSubmission :: IHtml
invalidSubmission = do
  msg <- getI18N
  return $ do
    Bootstrap.rowColMd12 $ p $
      B.toMarkup $ msg $ msg_SubmissionDetails_InvalidSubmission "This submission cannot be accessed by this user."
