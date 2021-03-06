{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
module Bead.View.Content.All (
    pageContent
#ifdef TEST
  , pageContentTest
#endif
  ) where

import qualified Bead.Controller.Pages as Pages hiding (invariants)
import Bead.View.Content
import qualified Bead.View.ContentHandler as ContentHandler (logout)
import Bead.View.Content.CheckSimilarity.Moss (checkSimilarityMoss, viewMossReport)
import Bead.View.Content.Profile.Page
import Bead.View.Content.StudentView.Page
import Bead.View.Content.GroupOverview.Page
import Bead.View.Content.CourseManagement.Page
import Bead.View.Content.CourseManagement.GroupManagement
import Bead.View.Content.CourseManagement.TestScript (createTestScript, modifyTestScript)
import Bead.View.Content.Administration.Page
import Bead.View.Content.EvaluationTable.Page
import Bead.View.Content.Evaluation.Page
import Bead.View.Content.Assignment.Page
import Bead.View.Content.QueueSubmissionForTest (queueSubmissionForTest, queueAllSubmissionsForTest)
import Bead.View.Content.Submission.Page
import Bead.View.Content.SubmissionDetails.Page
import Bead.View.Content.SubmissionTable (deleteUsersFromGroup, deleteUsersFromCourse)
import Bead.View.Content.GroupRegistration.Page
import Bead.View.Content.UserDetails.Page
#ifndef SSO
import Bead.View.Content.SetUserPassword.Page
#endif
import Bead.View.Content.Notifications.Page
import Bead.View.Content.UploadFile.Page
import Bead.View.Content.ExportEvaluationsScores
import Bead.View.Content.ExportSubmissions
import Bead.View.Content.GetSubmission
import Bead.View.Content.GetCsv
import Bead.View.Content.Assessment.Page
import Bead.View.Content.Score.Page
import Bead.View.Content.Rest.Group (usersInGroup)
import Bead.View.Content.Rest.SubmissionTable.Page (submissionTable)
import Bead.View.Content.Welcome.Page (welcome)

import Data.Monoid (mempty)

#ifdef TEST
import Test.Tasty.TestSet
#endif

pageContent :: Pages.Page a b c d e f -> PageHandler
pageContent = Pages.constantsP
  nullViewHandler -- index
  nullViewHandler -- login
  logout
  welcome
  profile
  administration
  studentView
  groupOverview
  groupOverviewAsStudent
  courseManagement
  evaluationTable
  evaluation
  modifyEvaluation
  newGroupAssignment
  newCourseAssignment
  modifyAssignment
  viewAssignment
  newGroupAssignmentPreview
  newCourseAssignmentPreview
  modifyAssignmentPreview
  submission
  submissionDetails
  viewUserScore
  newUserScore
  modifyUserScore
  groupRegistration
  userDetails
  uploadFile
  createCourse
  createGroup
  assignCourseAdmin
  assignGroupAdmin
  createTestScript
  modifyTestScript
  changePassword
#ifndef SSO
  setUserPassword
#endif
  deleteUsersFromCourse
  deleteUsersFromGroup
  queueSubmissionForTest
  queueAllSubmissionsForTest
  unsubscribeFromCourse
  exportEvaluationsScoresAdminedGroups
  exportEvaluationsScoresAllGroups
  exportSubmissions
  exportSubmissionsOfGroups
  exportSubmissionsOfOneGroup
  getSubmission
  getSubmissionsOfUserInGroup
  getSubmissionsOfAssignmentInGroup
  getCourseCsv
  getGroupCsv
  newGroupAssessment
  newCourseAssessment
  fillNewGroupAssessmentPreview
  fillNewCourseAssessmentPreview
  modifyAssessment
  modifyAssessmentPreview
  viewAssessment
  notifications
  viewMossReport
  checkSimilarityMoss
  submissionTable
  usersInGroup
  where
    logout :: ViewHandler
    logout = ViewHandler (ContentHandler.logout >> redirectTo (Pages.index ()))

    nullViewHandler :: ViewHandler
    nullViewHandler = ViewHandler (setPageContents $ HtmlPage (return mempty) (return mempty))


#ifdef TEST

pageContentTest =
  assertProperty
    "Content handler is a total function"
    (Pages.pageKindCata view userView viewModify modify data_ restView . pageContent)
    Pages.pageGen
    "Content handler must be defined"
  where
      view !_x = True
      userView !_x = True
      viewModify !_x = True
      modify !_x = True
      data_ !_x = True
      restView !_x = True

#endif
