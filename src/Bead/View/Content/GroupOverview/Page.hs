module Bead.View.Content.GroupOverview.Page
  (
    groupOverview
  , groupOverviewAsStudent
  ) where

import qualified Bead.Controller.UserStories as S
import qualified Bead.Controller.Pages as Pages
import           Bead.Domain.Entities (Assessment, UserDesc, Username)
import qualified Bead.Domain.Entities as E
import           Bead.Domain.Relationships (AssessmentKey, ScoreBoard, ScoreInfo)
import qualified Bead.Domain.Relationships as R
import           Bead.View.Content (ViewHandler(ViewHandler), GETContentHandler)
import qualified Bead.View.Content as C
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Content.StateVisualization (formatEvResultMaybe, toLargeIcon)
import           Bead.View.Content.StudentView.Page (availableAssignmentsAssessments)
import           Bead.View.Content.SubmissionTable (submissionTable, submissionTableContext, groupButtonStyle)
import           Bead.View.ContentHandler (setHomePage)
import           Bead.View.I18N (IHtml, i18n, getI18N)
import           Bead.View.RequestParams (groupKeyParamName)
import           Bead.View.RouteOf (routeOf)
import           Bead.View.Translation (I18N)
import qualified Bead.View.Translation as T

import           Control.Monad (forM_)
import           Control.Monad.IO.Class (liftIO)
import           Data.Function (on)
import qualified Data.Map as Map
import           Data.List (sortBy)
import           Data.String (fromString)
import           Data.Time.Clock (getCurrentTime)
import           Text.Blaze.Html5 (Html)
import qualified Text.Blaze.Html5 as H

groupOverview :: ViewHandler
groupOverview = ViewHandler $ do
  gk <- C.getParameter (C.customGroupKeyPrm groupKeyParamName)
  (_, course, _, group_) <- C.userStory $ S.loadGroupAndCourse gk
  table <- C.userStory $ S.groupSubmissionTable gk
  context <- C.userStory submissionTableContext
  board <- C.userStory $ S.scoreBoardOfGroup gk
  now <- liftIO $ getCurrentTime
  setHomePage $ R.GroupOverview gk
  C.setPageContents $ C.HtmlPage {
      C.pageTitle =
        return $ Bootstrap.pageHeader (E.fullGroupName course group_) Nothing
    , C.pageBody = do
        sTbl <- submissionTable (R.groupKeyMap id gk) now context table
        aTbl <- assessmentTable board
        return $ sTbl <> aTbl
    }

groupOverviewAsStudent :: ViewHandler
groupOverviewAsStudent = ViewHandler $ do
  gk <- C.getParameter (C.customGroupKeyPrm groupKeyParamName)
  assignmentsAssessments @ (course, group_, _, _) <- C.userStory $ do
    (_ck, course, _gk, group_) <- S.loadGroupAndCourse gk
    (asgs, assessments) <- S.userAssignmentsAssessments gk
    return (course, group_, asgs, assessments)
  converter <- C.userTimeZoneToLocalTimeConverter
  now <- liftIO $ getCurrentTime
  msg <- C.i18nE
  setHomePage $ R.GroupOverview gk
  C.setPageContents $ C.HtmlPage {
      C.pageTitle = do
        msg <- getI18N
        return $ Bootstrap.pageHeader (E.fullGroupName course group_) (Just $ msg $ T.msg_LinkText_GroupOverviewAsStudent "Group View As Student")
    , C.pageBody =
        return $ availableAssignmentsAssessments msg now converter assignmentsAssessments
    }

-- assessment table for teachers
assessmentTable :: ScoreBoard -> IHtml
assessmentTable board
  | null assessments = return mempty
  | otherwise = do
      msg <- getI18N
      return $ do
        Bootstrap.rowColMd12 . H.p . fromString . msg $ T.msg_Home_AssessmentTable_Assessments "Assessments"
        Bootstrap.rowColMd12 . Bootstrap.table $ do
          H.tr $ do
            H.th . fromString . msg $ T.msg_Home_AssessmentTable_StudentName "Name"
            H.th . fromString . msg $ T.msg_Home_AssessmentTable_Username "Username"
            forM_ (zip assessments [1..]) (assessmentViewButton msg)
          forM_ (sortBy (E.compareHun `on` (E.ud_fullname . fst)) (R.sbUserLines board)) (userLine msg)
      where
        assessmentViewButton :: I18N -> ((AssessmentKey,Assessment),Int) -> Html
        assessmentViewButton msg ((ak,as),n) = H.td $ Bootstrap.customButtonLink style modifyLink assessmentName (show n)
            where
              style = [fst groupButtonStyle]
              modifyLink = routeOf $ Pages.modifyAssessment ak ()
              assessmentName = E.assessment (\title _desc _creation _cfg _visible -> title) as

        userLine :: I18N -> (UserDesc, [Maybe ScoreInfo]) -> Html
        userLine msg (userDesc, scores) = H.tr $ do
          H.td . H.string . E.ud_fullname $ userDesc
          H.td . H.string . E.uid id . E.ud_uid $ userDesc
          forM_ (zip scores (map fst assessments)) (H.td . scoreIcon msg (E.ud_username userDesc))

        scoreIcon :: I18N -> Username -> (Maybe ScoreInfo, AssessmentKey) -> Html
        scoreIcon msg username (i, ak) =
          Bootstrap.link target icon
          where
            target = case i of
                       Nothing -> newScoreLink ak username
                       Just info -> modifyScoreLink (R.scoreKeyOfInfo info)

            icon = formatEvResultMaybe toLargeIcon msg (R.evaluationOfInfo <$> i)

        newScoreLink ak u = routeOf $ Pages.newUserScore ak u ()
        modifyScoreLink sk = routeOf $ Pages.modifyUserScore sk ()

        assessments :: [(AssessmentKey,Assessment)]
        assessments = R.sbAssessments board
