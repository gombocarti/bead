{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.StudentView.Page
  ( studentView
  , availableAssignmentsAssessments
  ) where

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as S
import qualified Bead.Domain.Entities as E
import qualified Bead.Domain.Entity.Assignment as Assignment
import qualified Bead.Domain.Relationships as R
import           Bead.View.Content ( ViewHandler(ViewHandler), GETContentHandler, UserTimeConverter
                                   , Group, Course, Assessment, AssessmentKey, ScoreKey, ScoreInfo
                                   , AssignmentKey, Assignment, SubmissionKey, SubmissionState, SubmissionLimit
                                   , I18N
                                   )
import qualified Bead.View.Content as C
import qualified Bead.View.Content.Bootstrap as Bootstrap
import qualified Bead.View.Content.StateVisualization as SV
import           Bead.View.I18N (IHtml)
import           Bead.View.Markdown (markdownToHtml)
import           Bead.View.RequestParams (groupKeyParamName)
import           Bead.View.RouteOf (routeOf)
import qualified Bead.View.Translation as Tr

import           Control.Monad (when, forM)
import           Control.Monad.IO.Class (liftIO)
import           Data.String (fromString)
import qualified Data.Text as T
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Tuple.Utils (snd3)
import           Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

studentView :: ViewHandler
studentView = ViewHandler $ do
  gk <- C.getParameter (C.customGroupKeyPrm groupKeyParamName)
  ua@(course, group_, _, _) <- C.userStory $ do
    (_, course, _, group_) <- S.loadGroupAndCourse gk
    (asgs, assessments) <- S.userAssignmentsAssessments gk
    return (course, group_, asgs, assessments)
  now <- liftIO getCurrentTime
  converter <- C.userTimeZoneToLocalTimeConverter
  C.setPageContents $ C.HtmlPage {
      C.pageTitle = do
        return $ Bootstrap.pageHeader (E.fullGroupName course group_) Nothing
    , C.pageBody = viewAssignmentsAssessments now converter ua
    }

type ActiveAssignment = (AssignmentKey, Assignment, Maybe (SubmissionKey, SubmissionState), SubmissionLimit)
type ActiveAssessment = (AssessmentKey, Assessment, Maybe ScoreInfo)

activeAsg :: ActiveAssignment -> Assignment
activeAsg (_, a, _, _) = a

viewAssignmentsAssessments :: UTCTime -> UserTimeConverter -> (Course, Group, [ActiveAssignment], [ActiveAssessment]) -> IHtml
viewAssignmentsAssessments now timeconverter group_ = do
  msg <- C.getI18N
  return $ do
    when (hasAssignments group_) $ do
      H.p
        $ fromString . msg $ Tr.msg_Home_Assignments_Info $ concat
          [ "Submissions and their evaluations may be accessed by clicking on each assignment's link. "
          , "The table shows only the last evaluation per assignment."
          ]

      Bootstrap.alert Bootstrap.Info $
        markdownToHtml msg . msg $ Tr.msg_Home_EvaluationLink_Hint $
        "**Hint**: You can go straight to your submission by clicking on a link in the Evaluation column."

      availableAssignmentsAssessments msg now timeconverter group_

availableAssignmentsAssessments :: I18N -> UTCTime -> UserTimeConverter -> (Course, Group, [ActiveAssignment], [ActiveAssessment]) -> Html
availableAssignmentsAssessments msg now timeconverter g@(_, _, assignments, assessments) = do
  if (not (hasAssignments g || hasAssessments g))
    then H.p
         $ fromString
         $ msg $ Tr.msg_Home_HasNoAssignments "There are no available assignments yet."
    else do
      when (hasAssignments g) $ do
          let areIsolateds = areOpenAndIsolatedAssignments assignments
              visibleAsgs = if areIsolateds then (isolatedAssignments assignments) else assignments
              isLimited = isLimitedAssignments visibleAsgs
          when areIsolateds $
            Bootstrap.alert Bootstrap.Warning $
              markdownToHtml msg . msg $ Tr.msg_Home_ThereIsIsolatedAssignment $ concat
                [ "**Isolated mode**: There is at least one assignment which hides the normal assignments for "
                , "this course."
                ]
          Bootstrap.table $ do
            H.thead $ headerLine msg isLimited
            H.tbody $ mapM_ (assignmentLine msg isLimited) visibleAsgs
      -- Assessment table
      availableAssessments msg g
  where
    isLimitedAssignments = any limited

    limited = R.submissionLimit (const False) (\_ _ -> True) (const True) . (\(_ak,_a,_si,limit) -> limit)

    isOpenAndIsolated a = Assignment.isIsolated (Assignment.aspects a) && Assignment.isActive a now

    areOpenAndIsolatedAssignments = not . null . isolatedAssignments

    isolatedAssignments = filter (isOpenAndIsolated . activeAsg)

    headerLine :: I18N -> Bool -> H.Html
    headerLine msg isLimited = H.tr $ do
      H.th mempty
      H.th (fromString $ msg $ Tr.msg_Home_Assignment "Assignment")
      when isLimited $ H.th (fromString $ msg $ Tr.msg_Home_Limit "Limit")
      H.th (fromString $ msg $ Tr.msg_Home_Deadline "Deadline")
      H.th (fromString $ msg $ Tr.msg_Home_Evaluation "Evaluation")

    assignmentLine :: I18N -> Bool -> ActiveAssignment -> H.Html
    assignmentLine msg isLimited (ak, asg, subm, limit) = H.tr $ do
      case and [Assignment.isActive asg now, noLimitIsReached limit] of
        True ->
          H.td $ H.span
                 ! A.class_ "glyphicon glyphicon-lock"
                 ! A.style "visibility: hidden"
                 $ mempty
        False ->
          H.td $ H.span
                 ! A.class_ "glyphicon glyphicon-lock"
                 $ mempty
      H.td $ Bootstrap.link (routeOf (Pages.submission ak ())) (Assignment.name asg)
      when isLimited $ H.td (fromString . showLimit $ limit)
      H.td (fromString . C.showDate . timeconverter $ Assignment.end asg)
      H.td submissionStateLabel
      where
        noLimitIsReached = R.submissionLimit (const True) (\n _ -> n > 0) (const False)
        showLimit = fromString . R.submissionLimit
          (const "") (\n _ -> unwords [msg $ Tr.msg_Home_Remains "Remains:", show n]) (const $ msg $ Tr.msg_Home_Reached "Reached")

        submissionDetails :: SubmissionKey -> Pages.PageDesc
        submissionDetails key = Pages.submissionDetails ak key ()

        submissionStateLabel :: Html
        submissionStateLabel =
          maybe
          (Bootstrap.grayLabel $ T.pack $ msg $ Tr.msg_Home_SubmissionCell_NoSubmission "No submission")
          (\(key, state) ->
             Bootstrap.link (routeOf (submissionDetails key)) (SV.formatSubmissionState SV.toLabel msg state))
          subm

-- assessment table for students
availableAssessments :: I18N -> (Course, Group, [ActiveAssignment], [ActiveAssessment]) -> Html
availableAssessments msg (_, _, _, assessments) | null assessments = mempty
                                                | otherwise = do
  H.p . fromString . msg $ Tr.msg_Home_AssessmentTable_Assessments "Assessments"
  Bootstrap.table $ do
    H.tr header
    H.tr $ mapM_ (H.td . evaluationViewButton) [si | (_,_,si) <- assessments]
  where
      header = mapM_ (H.td . assessmentLabel) (zip [assessment | (_,assessment,_) <- assessments] [1..])
        where
          assessmentLabel :: (Assessment, Int) -> Html
          assessmentLabel (as, n) = Bootstrap.grayLabel (T.pack $ show n) ! tooltip
            where aTitle = E.assessment (\title _desc _creation _cfg _visible -> title) as
                  tooltip = A.title . fromString $ aTitle

      evaluationViewButton :: Maybe ScoreInfo -> Html
      evaluationViewButton i = case i of
                                 Nothing -> icon
                                 Just info -> Bootstrap.link (viewScoreLink info) icon

        where viewScoreLink info = routeOf $ Pages.viewUserScore (C.scoreKeyOfInfo info) ()
              icon = SV.formatEvResultMaybe SV.toLargeIcon msg (C.evaluationOfInfo <$> i)

hasAssessments :: (Course, Group, [ActiveAssignment], [ActiveAssessment]) -> Bool
hasAssessments (_, _, _, assmnts) = not . null $ assmnts

hasAssignments :: (Course, Group, [ActiveAssignment], [ActiveAssessment]) -> Bool
hasAssignments (_, _, asgs, _) = not . null $ asgs
