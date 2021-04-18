{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Controller.Pages where

import           Control.Monad (join)

import qualified Bead.View.Translation as Trans
import           Bead.Domain.Entities (Uid)
import qualified Bead.Domain.Entities      as E
import           Bead.Domain.Relationships ( GroupKey, CourseKey, AssignmentKey
                                           , AssessmentKey, ScoreKey, EvaluationKey, SubmissionKey
                                           , TestScriptKey, HomePageContents)
import qualified Bead.Domain.Relationships as R

#ifdef TEST
import           Control.Applicative
import           Test.Tasty.Arbitrary (alphaNum)
import           Test.Tasty.TestSet
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary (arbitrary)
#endif

data CourseManagementContents
  = GroupManagementContents
  | AssignmentsContents
  | TestScriptsContents
  | NewTestScriptContents
  | ModifyTestScriptContents TestScriptKey
  deriving (Eq, Read, Show)

courseManagementContentsCata :: a -> a -> a -> a -> (TestScriptKey -> a) -> CourseManagementContents -> a
courseManagementContentsCata
  groupManagementContents_
  assignmentsContents_
  testScriptsContents_
  newTestScriptContents_
  modifyTestScriptContents_
  contents =
    case contents of
      GroupManagementContents -> groupManagementContents_
      TestScriptsContents -> testScriptsContents_
      AssignmentsContents -> assignmentsContents_
      NewTestScriptContents -> newTestScriptContents_
      ModifyTestScriptContents testScriptKey -> modifyTestScriptContents_ testScriptKey

defaultCourseManagementContents :: CourseManagementContents
defaultCourseManagementContents = AssignmentsContents

-- View pages are rendered using data stored in the persistence
-- layer. They present information to the user.
data ViewPage a
  = Index a
  | Login a
  | Logout a
  | Welcome a
  | StudentView GroupKey a
  | GroupOverview GroupKey a
  | GroupOverviewAsStudent GroupKey a
  | CourseManagement CourseKey CourseManagementContents a
  | EvaluationTable a
  | ViewAssignment AssignmentKey a
  | Administration a
  | ViewAssessment AssessmentKey a
  | ViewUserScore ScoreKey a
  | Notifications a
  deriving (Eq, Show, Functor)

viewPageCata
  index
  login
  logout
  welcome
  studentView
  groupOverview
  groupOverviewAsStudent
  courseManagement
  evaluationTable
  viewAssignment
  administration
  viewAssessment
  viewUserScore
  notifications
  p = case p of
    Index a -> index a
    Login a -> login a
    Logout a -> logout a
    Welcome a -> welcome a
    StudentView gk a -> studentView gk a
    GroupOverview gk a -> groupOverview gk a
    GroupOverviewAsStudent gk a -> groupOverviewAsStudent gk a
    CourseManagement ck contents a -> courseManagement ck contents a
    EvaluationTable a -> evaluationTable a
    ViewAssignment ak a -> viewAssignment ak a
    Administration a -> administration a
    ViewAssessment ak a -> viewAssessment ak a
    ViewUserScore sk a -> viewUserScore sk a
    Notifications a -> notifications a

viewPageValue :: ViewPage a -> a
viewPageValue = viewPageCata
  id -- index
  id -- login
  id -- logout
  id -- welcome
  cid -- studentView
  cid -- groupOverview
  cid -- groupOverviewAsStudent
  c2id -- courseManagement
  id -- evaluationTable
  cid -- viewAssignment
  id -- administration
  cid -- viewAssessment
  cid -- viewUserScore
  id -- notifications
  where
    cid :: a -> b -> b
    cid = const id

    c2id :: a -> b -> c -> c
    c2id = const . const id

-- Page that extract information from the persistence
-- and only the data will be rendered in the response
-- (e.g. file download)
data DataPage a
  = ExportEvaluationsScoresAdminedGroups CourseKey a
  | ExportEvaluationsScoresAllGroups CourseKey a
  | ExportSubmissions AssignmentKey a
  | ExportSubmissionsOfGroups AssignmentKey E.Username a
  | ExportSubmissionsOfOneGroup AssignmentKey GroupKey a
  | GetSubmission SubmissionKey a
  | GetSubmissionsOfUserInGroup GroupKey Uid a
  | GetSubmissionsOfAssignmentInGroup GroupKey AssignmentKey a
  | GetCourseCsv CourseKey a
  | GetGroupCsv GroupKey a
  deriving (Eq, Show, Functor)

dataPageCata
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
  p = case p of
    ExportEvaluationsScoresAdminedGroups ck a -> exportEvaluationsScoresAdminedGroups ck a
    ExportEvaluationsScoresAllGroups ck a -> exportEvaluationsScoresAllGroups ck a
    ExportSubmissions ak a -> exportSubmissions ak a
    ExportSubmissionsOfGroups ak u a -> exportSubmissionsOfGroups ak u a
    ExportSubmissionsOfOneGroup ak gk a -> exportSubmissionsOfOneGroup ak gk a
    GetSubmission sk a -> getSubmission sk a
    GetSubmissionsOfUserInGroup gk uid a -> getSubmissionsOfUserInGroup gk uid a
    GetSubmissionsOfAssignmentInGroup gk ak a -> getSubmissionsOfAssignmentInGroup gk ak a
    GetCourseCsv ck a -> getCourseCsv ck a
    GetGroupCsv gk a -> getGroupCsv gk a

dataPageValue :: DataPage a -> a
dataPageValue = dataPageCata
  cid  -- exportEvaluationsScoresAdminedGroups
  cid  -- exportEvaluationsScoresAllGroups
  cid  -- exportSubmissions
  c2id -- exportSubmissionsOfGroups
  c2id -- exportSubmissionsOfOneGroup
  cid  -- getSubmission
  c2id -- getSubmissionsOfUserInGroup
  c2id -- getSubmissionsOfAssignmentInGroup
  cid  -- getCourseCsv
  cid  -- getGroupCsv
  where
    cid :: b -> a -> a
    cid = const id

    c2id :: c -> b -> a -> a
    c2id = const . cid

-- User View pages are rendered using the data stored in the
-- persistence and temporary data given by the user (e.g. form data).
-- This is mainly for information propagation to the user in a stateful way.
data UserViewPage a
  = NewGroupAssignmentPreview GroupKey a
  | NewCourseAssignmentPreview CourseKey a
  | ModifyAssignmentPreview AssignmentKey a
  | FillNewGroupAssessmentPreview GroupKey a
  | FillNewCourseAssessmentPreview CourseKey a
  | ModifyAssessmentPreview AssessmentKey a
  deriving (Eq, Show, Functor)

userViewPageCata
  newGroupAssignmentPreview
  newCourseAssignmentPreview
  modifyAssignmentPreview
  fillNewGroupAssessmentPreview
  fillNewCourseAssessmentPreview
  modifyAssessmentPreview
  p = case p of
    NewGroupAssignmentPreview gk a -> newGroupAssignmentPreview gk a
    NewCourseAssignmentPreview ck a -> newCourseAssignmentPreview ck a
    ModifyAssignmentPreview ak a -> modifyAssignmentPreview ak a
    FillNewGroupAssessmentPreview gk a -> fillNewGroupAssessmentPreview gk a
    FillNewCourseAssessmentPreview ck a -> fillNewCourseAssessmentPreview ck a
    ModifyAssessmentPreview ak a -> modifyAssessmentPreview ak a

userViewPageValue :: UserViewPage a -> a
userViewPageValue = userViewPageCata
  cid -- newGroupAssignmentPreview
  cid -- newCourseAssignmentPreview
  cid -- modifyAssignmentPreview
  cid -- fillNewGroupAssessmentPreview
  cid -- fillNewCourseAssessmentPreview
  cid -- modifyAssessmentPreview
  where
    cid = const id

-- View and Modify pages which rendered using data stored in the
-- persistence and after some user input it modifies the information
-- stored in the persistence.
data ViewModifyPage a
  = Profile a
  | Evaluation SubmissionKey a
  | ModifyEvaluation SubmissionKey EvaluationKey a
  | NewGroupAssignment GroupKey a
  | NewCourseAssignment CourseKey a
  | ModifyAssignment AssignmentKey a
  | ModifyAssessment AssessmentKey a
  | GroupRegistration a
  | UserDetails a
  | UploadFile a
#ifndef SSO
  | SetUserPassword a
#endif
  | SubmissionDetails AssignmentKey SubmissionKey a
  | Submission AssignmentKey a
  | NewUserScore AssessmentKey E.Username a
  | ModifyUserScore ScoreKey a
  | NewGroupAssessment GroupKey a
  | NewCourseAssessment CourseKey a
  deriving (Eq, Show, Functor)

viewModifyPageCata
  profile
  evaluation
  modifyEvaluation
  newGroupAssignment
  newCourseAssignment
  modifyAssignment
  modifyAssessment
  groupRegistration
  userDetails
  uploadFile
#ifndef SSO
  setUserPassword
#endif
  submissionDetails
  submission
  newUserScore
  modifyUserScore
  newGroupAssessment
  newCourseAssessment
  p = case p of
    Profile a -> profile a
    Evaluation sk a -> evaluation sk a
    ModifyEvaluation sk ek a -> modifyEvaluation sk ek a
    NewGroupAssignment gk a -> newGroupAssignment gk a
    NewCourseAssignment ck a -> newCourseAssignment ck a
    ModifyAssignment ak a -> modifyAssignment ak a
    ModifyAssessment ak a -> modifyAssessment ak a
    GroupRegistration a -> groupRegistration a
    UserDetails a -> userDetails a
    UploadFile a -> uploadFile a
#ifndef SSO
    SetUserPassword a -> setUserPassword a
#endif
    SubmissionDetails ak sk a -> submissionDetails ak sk a
    Submission ak a -> submission ak a
    NewUserScore assk u a -> newUserScore assk u a
    ModifyUserScore sk a -> modifyUserScore sk a
    NewGroupAssessment gk a -> newGroupAssessment gk a
    NewCourseAssessment ck a -> newCourseAssessment ck a

viewModifyPageValue :: ViewModifyPage a -> a
viewModifyPageValue = viewModifyPageCata
  id -- profile
  cid -- evaluation
  c2id -- modifyEvaluation
  cid -- newGroupAssignment
  cid -- newCourseAssignment
  cid -- modifyAssignment
  cid -- modifyAssessment
  id -- groupRegistration
  id -- userDetails
  id -- uploadFile
#ifndef SSO
  id -- setUserPassword
#endif
  c2id -- submissionDetails
  cid -- submission
  c2id -- newUserScore
  cid -- modifyUserScore
  cid -- newGroupAssessment
  cid -- newCourseAssessment
  where
    cid = const id
    c2id = const . cid

-- Modify page is a following and lously coupled page of a
-- view page, uses the information gathered from the user input
-- and changes information in the persistence layer
data ModifyPage a
  = CreateCourse a
  | CreateGroup CourseKey a
  | AssignCourseAdmin a
  | AssignGroupAdmin CourseKey a
  | CreateTestScript CourseKey a
  | ModifyTestScript CourseKey TestScriptKey a  -- CourseKey is needed for parentPage.
  | ChangePassword a
  | DeleteUsersFromCourse R.CourseKey a
  | DeleteUsersFromGroup R.GroupKey a
  | QueueSubmissionForTest SubmissionKey a
  | QueueAllSubmissionsForTest AssignmentKey a
  | UnsubscribeFromCourse R.GroupKey a
  deriving (Eq, Show, Functor)

modifyPageCata :: (a -> b)
               -> (CourseKey -> a -> b)
               -> (a -> b)
               -> (CourseKey -> a -> b)
               -> (CourseKey -> a -> b)
               -> (CourseKey -> TestScriptKey -> a -> b)
               -> (a -> b)
               -> (CourseKey -> a -> b)
               -> (GroupKey -> a -> b)
               -> (SubmissionKey -> a -> b)
               -> (AssignmentKey -> a -> b)
               -> (GroupKey -> a -> b)
               -> ModifyPage a
               -> b
modifyPageCata
  createCourse
  createGroup
  assignCourseAdmin
  assignGroupAdmin
  createTestScript
  modifyTestScript
  changePassword
  deleteUsersFromCourse
  deleteUsersFromGroup
  queueSubmissionForTest
  queueAllSubmissionsForTest
  unsubscribeFromCourse
  p = case p of
    CreateCourse a -> createCourse a
    CreateGroup ck a -> createGroup ck a
    AssignCourseAdmin a -> assignCourseAdmin a
    AssignGroupAdmin ck a -> assignGroupAdmin ck a
    CreateTestScript ck a -> createTestScript ck a
    ModifyTestScript ck tsk a -> modifyTestScript ck tsk a
    ChangePassword a -> changePassword a
    DeleteUsersFromCourse ck a -> deleteUsersFromCourse ck a
    DeleteUsersFromGroup gk a -> deleteUsersFromGroup gk a
    QueueSubmissionForTest sk a ->  queueSubmissionForTest sk a
    QueueAllSubmissionsForTest ak a -> queueAllSubmissionsForTest ak a
    UnsubscribeFromCourse gk a -> unsubscribeFromCourse gk a

modifyPageValue :: ModifyPage a -> a
modifyPageValue = modifyPageCata
  id -- createCourse
  cid -- createGroup
  id -- assignCourseAdmin
  cid -- assignGroupAdmin
  cid -- createTestScript
  c2id -- modifyTestScript
  id -- changePassword
  cid -- deleteUsersFromCourse
  cid -- deleteUsersFromGroup
  cid -- queueSubmissionForTest
  cid -- queueAllSubmissionsForTest
  cid -- unsubscribeFromCourse
  where
    cid = const id
    c2id = const . cid

data RestViewPage a
  = SubmissionTable GroupKey a
  | UsersInGroup GroupKey a
  deriving (Eq, Show, Functor)

restViewPageCata :: (GroupKey -> a -> b)
                 -> (GroupKey -> a -> b)
                 -> RestViewPage a -> b
restViewPageCata
  submissionTable
  usersInGroup
  p = case p of
    SubmissionTable gk a -> submissionTable gk a
    UsersInGroup gk a -> usersInGroup gk a

restViewPageValue :: RestViewPage a -> a
restViewPageValue = restViewPageCata (c id) (c id)
  where
    c = const
    c2 = const . c

-- The kind of the possible page types
data Page a b c d e f
  = View       (ViewPage a)
  | UserView   (UserViewPage b)
  | ViewModify (ViewModifyPage c)
  | Modify     (ModifyPage d)
  | Data       (DataPage e)
  | RestView   (RestViewPage f)
  deriving (Eq, Show)



liftPK
  view
  userView
  viewModify
  modify
  data_
  restView
  v = case v of
    View v       -> View       $ view v
    UserView u   -> UserView   $ userView u
    ViewModify v -> ViewModify $ viewModify v
    Modify m     -> Modify     $ modify m
    Data d       -> Data       $ data_ d
    RestView r   -> RestView   $ restView r

pfmap f0 f1 f2 f3 f4 f5 = liftPK (fmap f0) (fmap f1) (fmap f2) (fmap f3) (fmap f4) (fmap f5)

pageKindCata
  view
  userView
  viewModify
  modify
  data_
  restView
  q = case q of
    View p -> view p
    UserView p -> userView p
    ViewModify p -> viewModify p
    Modify p -> modify p
    Data p -> data_ p
    RestView r -> restView r

pageCata' = pageKindCata

pageValue :: Page a a a a a a -> a
pageValue = pageCata' viewPageValue userViewPageValue viewModifyPageValue modifyPageValue dataPageValue restViewPageValue

setPageValue :: a -> Page b c d e f g -> Page a a a a a a
setPageValue a = pfmap (const a) (const a) (const a) (const a) (const a) (const a)

type Page' a = Page a a a a a a

type PageDesc = Page' ()

pageToPageDesc :: Page a b c d e f -> PageDesc
pageToPageDesc = pfmap unit unit unit unit unit unit where
  unit :: a -> ()
  unit = const ()

index                     = View . Index
login                     = View . Login
logout                    = View . Logout
logoutWithText            = logout $ Trans.msg_LinkText_Logout "Logout"
welcome                      = View . Welcome
welcomeWithText              = welcome $ Trans.msg_LinkText_Welcome "Welcome"
studentView gk            = View . StudentView gk
groupOverview gk          = View . GroupOverview gk
groupOverviewAsStudent gk = View . GroupOverviewAsStudent gk
groupOverviewAsStudentWithText gk = groupOverviewAsStudent gk $ Trans.msg_LinkText_GroupOverviewAsStudent "Group View As Student"
courseManagement ck contents = View . CourseManagement ck contents
newTestScriptWithText ck  = courseManagement ck NewTestScriptContents $ Trans.msg_LinkText_NewTestScript "New Test Script"
evaluationTable           = View . EvaluationTable
viewAssignment ak         = View . ViewAssignment ak
administration            = View . Administration
administrationWithText    = administration $ Trans.msg_LinkText_Administration "Administration"
viewAssessment ak         = View . ViewAssessment ak
viewUserScore sk          = View . ViewUserScore sk
notifications             = View . Notifications
notificationsWithText     = notifications $ Trans.msg_LinkText_Notifications "Notifications"

exportEvaluationsScoresAdminedGroups ck          = Data . ExportEvaluationsScoresAdminedGroups ck
exportEvaluationsScoresAdminedGroupsWithText ck = exportEvaluationsScoresAdminedGroups ck $ Trans.msg_LinkText_ExportEvaluations "Export Evaluations of Admined Groups of this Course"
exportEvaluationsScoresAllGroups ck = Data . ExportEvaluationsScoresAllGroups ck
exportEvaluationsScoresAllGroupsWithText ck = exportEvaluationsScoresAllGroups ck $ Trans.msg_LinkText_ExportEvaluationsAllGroups "Export Evaluations of All Groups of this Course"
exportSubmissions ak                = Data . ExportSubmissions ak
exportSubmissionsWithText ak        = exportSubmissions ak $ Trans.msg_LinkText_ExportSubmissions "Export All Submissions"
exportSubmissionsOfGroups ak u      = Data . ExportSubmissionsOfGroups ak u
exportSubmissionsOfGroupsWithText ak u = exportSubmissionsOfGroups ak u $ Trans.msg_LinkText_ExportSubmissionsOfGroups "Export Admined Groups"
exportSubmissionsOfOneGroup ak gk   = Data . ExportSubmissionsOfOneGroup ak gk
exportSubmissionsOfOneGroupWithText ak gk   = exportSubmissionsOfOneGroup ak gk $ Trans.msg_LinkText_ExportSubmissionsOfOneGroup "Export This Group"
getSubmission sk                    = Data . GetSubmission sk
getSubmissionsOfUserInGroup gk uid  = Data . GetSubmissionsOfUserInGroup gk uid
getSubmissionsOfAssignmentInGroup gk ak = Data . GetSubmissionsOfAssignmentInGroup gk ak
getCourseCsv ck                     = Data . GetCourseCsv ck
getGroupCsv gk                      = Data . GetGroupCsv gk

newGroupAssignmentPreview gk  = UserView . NewGroupAssignmentPreview gk
newCourseAssignmentPreview ck = UserView . NewCourseAssignmentPreview ck
modifyAssignmentPreview ak    = UserView . ModifyAssignmentPreview ak

fillNewCourseAssessmentPreview ck = UserView . FillNewCourseAssessmentPreview ck
fillNewGroupAssessmentPreview gk  = UserView . FillNewGroupAssessmentPreview gk
modifyAssessmentPreview ak = UserView . ModifyAssessmentPreview ak

profile                = ViewModify . Profile
profileWithText        = profile $ Trans.msg_LinkText_Profile "Profile"
evaluation sk          = ViewModify . Evaluation sk
modifyEvaluation sk ek = ViewModify . ModifyEvaluation sk ek
newGroupAssignment gk  = ViewModify . NewGroupAssignment gk
newGroupAssignmentWithText gk = newGroupAssignment gk $ Trans.msg_LinkText_NewGroupAssignment "New Group Assignment"
newCourseAssignment ck = ViewModify . NewCourseAssignment ck
newCourseAssignmentWithText ck = newCourseAssignment ck $ Trans.msg_LinkText_NewCourseAssignmentPreview "New Course Assignment"
modifyAssignment ak    = ViewModify . ModifyAssignment ak
modifyAssessment ak    = ViewModify . ModifyAssessment ak
groupRegistration      = ViewModify . GroupRegistration
groupRegistrationWithText = groupRegistration $ Trans.msg_LinkText_GroupRegistration "Group Registration"
userDetails            = ViewModify . UserDetails
uploadFile             = ViewModify . UploadFile
uploadFileWithText     = uploadFile $ Trans.msg_LinkText_UploadFile "Upload file"
#ifndef SSO
setUserPassword        = ViewModify . SetUserPassword
#endif
submissionDetails ak sk = ViewModify . SubmissionDetails ak sk
submission ak           = ViewModify . Submission ak
newUserScore assk u     = ViewModify . NewUserScore assk u
newUserScoreWithText aask u = newUserScore aask u $ Trans.msg_LinkText_NewUserScore "New Score"
modifyUserScore sk      = ViewModify . ModifyUserScore sk
newCourseAssessment ck  = ViewModify . NewCourseAssessment ck
newGroupAssessment gk   = ViewModify . NewGroupAssessment gk
newGroupAssessmentWithText gk = newGroupAssessment gk $ Trans.msg_LinkText_NewGroupAssessment "New Group Assessment"

createCourse         = Modify . CreateCourse
createGroup ck       = Modify . CreateGroup ck
assignCourseAdmin    = Modify . AssignCourseAdmin
assignGroupAdmin ck  = Modify . AssignGroupAdmin ck
createTestScript ck  = Modify . CreateTestScript ck
modifyTestScript ck tsk           = Modify . ModifyTestScript ck tsk
changePassword                    = Modify . ChangePassword
deleteUsersFromCourse ck          = Modify . DeleteUsersFromCourse ck
deleteUsersFromGroup gk           = Modify . DeleteUsersFromGroup gk
queueSubmissionForTest sk         = Modify . QueueSubmissionForTest sk
queueSubmissionForTestWithText sk = queueSubmissionForTest sk $ Trans.msg_LinkText_QueueSubmissionForTest "Run Test"
queueAllSubmissionsForTest ak     = Modify . QueueAllSubmissionsForTest ak
queueAllSubmissionsForTestWithText ak = queueAllSubmissionsForTest ak $ Trans.msg_LinkText_QueueAllSubmissionsForTest "Run Test on All Submissions"
unsubscribeFromCourse gk          = Modify . UnsubscribeFromCourse gk

submissionTable gk = RestView . SubmissionTable gk
usersInGroup gk = RestView . UsersInGroup gk

-- Template method for the page data structure
pageCata
  index
  login
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
  submissionTable
  usersInGroup
  p = case p of
    (View (Index a)) -> index a
    (View (Login a)) -> login a
    (View (Logout a)) -> logout a
    (View (Welcome a)) -> welcome a
    (ViewModify (Profile a)) -> profile a
    (View (Administration a)) -> administration a
    (View (StudentView gk a)) -> studentView gk a
    (View (GroupOverview gk a)) -> groupOverview gk a
    (View (GroupOverviewAsStudent gk a)) -> groupOverviewAsStudent gk a
    (View (CourseManagement ck contents a)) -> courseManagement ck contents a
    (View (EvaluationTable a)) -> evaluationTable a
    (ViewModify (Evaluation sk a)) -> evaluation sk a
    (ViewModify (ModifyEvaluation sk ek a)) -> modifyEvaluation sk ek a
    (ViewModify (NewGroupAssignment gk a)) -> newGroupAssignment gk a
    (ViewModify (NewCourseAssignment ck a)) -> newCourseAssignment ck a
    (ViewModify (ModifyAssignment ak a)) -> modifyAssignment ak a
    (View (ViewAssignment ak a)) -> viewAssignment ak a
    (UserView (NewGroupAssignmentPreview gk a)) -> newGroupAssignmentPreview gk a
    (UserView (NewCourseAssignmentPreview ck a)) -> newCourseAssignmentPreview ck a
    (UserView (ModifyAssignmentPreview ak a)) -> modifyAssignmentPreview ak a
    (ViewModify (Submission ak a)) -> submission ak a
    (ViewModify (SubmissionDetails ak sk a)) -> submissionDetails ak sk a
    (View (ViewUserScore sk a)) -> viewUserScore sk a
    (ViewModify (NewUserScore assk u a)) -> newUserScore assk u a
    (ViewModify (ModifyUserScore sk a)) -> modifyUserScore sk a
    (ViewModify (GroupRegistration a)) -> groupRegistration a
    (ViewModify (UserDetails a)) -> userDetails a
    (ViewModify (UploadFile a)) -> uploadFile a
    (Modify (CreateCourse a)) -> createCourse a
    (Modify (CreateGroup ck a)) -> createGroup ck a
    (Modify (AssignCourseAdmin a)) -> assignCourseAdmin a
    (Modify (AssignGroupAdmin ck a)) -> assignGroupAdmin ck a
    (Modify (CreateTestScript ck a)) -> createTestScript ck a
    (Modify (ModifyTestScript ck tsk a)) -> modifyTestScript ck tsk a
    (Modify (ChangePassword a)) -> changePassword a
#ifndef SSO
    (ViewModify (SetUserPassword a)) -> setUserPassword a
#endif
    (Modify (DeleteUsersFromCourse ck a)) -> deleteUsersFromCourse ck a
    (Modify (DeleteUsersFromGroup gk a)) -> deleteUsersFromGroup gk a
    (Modify (QueueSubmissionForTest sk a)) -> queueSubmissionForTest sk a
    (Modify (QueueAllSubmissionsForTest ak a)) -> queueAllSubmissionsForTest ak a
    (Modify (UnsubscribeFromCourse gk a)) -> unsubscribeFromCourse gk a
    (Data (ExportEvaluationsScoresAdminedGroups ck a)) -> exportEvaluationsScoresAdminedGroups ck a
    (Data (ExportEvaluationsScoresAllGroups ck a)) -> exportEvaluationsScoresAllGroups ck a
    (Data (ExportSubmissions ak a)) -> exportSubmissions ak a
    (Data (ExportSubmissionsOfGroups ak u a)) -> exportSubmissionsOfGroups ak u a
    (Data (ExportSubmissionsOfOneGroup ak gk a)) -> exportSubmissionsOfOneGroup ak gk a
    (Data (GetSubmission sk a)) -> getSubmission sk a
    (Data (GetSubmissionsOfUserInGroup gk uid a)) -> getSubmissionsOfUserInGroup gk uid a
    (Data (GetSubmissionsOfAssignmentInGroup gk ak a)) -> getSubmissionsOfAssignmentInGroup gk ak a
    (Data (GetCourseCsv ck a)) -> getCourseCsv ck a
    (Data (GetGroupCsv gk a)) -> getGroupCsv gk a
    (ViewModify (NewGroupAssessment gk a)) -> newGroupAssessment gk a
    (ViewModify (NewCourseAssessment ck a)) -> newCourseAssessment ck a
    (UserView (FillNewGroupAssessmentPreview gk a)) -> fillNewGroupAssessmentPreview gk a
    (UserView (FillNewCourseAssessmentPreview ck a)) -> fillNewCourseAssessmentPreview ck a
    (ViewModify (ModifyAssessment ak a)) -> modifyAssessment ak a
    (UserView (ModifyAssessmentPreview ak a)) -> modifyAssessmentPreview ak a
    (View (ViewAssessment ak a)) -> viewAssessment ak a
    (View (Notifications a)) -> notifications a
    (RestView (SubmissionTable gk a)) -> submissionTable gk a
    (RestView (UsersInGroup gk a)) -> usersInGroup gk a

-- Constants that attached each of the page constructor
constantsP
  index_
  login_
  logout_
  welcome_
  profile_
  administration_
  studentView_
  groupOverview_
  groupOverviewAsStudent_
  courseManagement_
  evaluationTable_
  evaluation_
  modifyEvaluation_
  newGroupAssignment_
  newCourseAssignment_
  modifyAssignment_
  viewAssignment_
  newGroupAssignmentPreview_
  newCourseAssignmentPreview_
  modifyAssignmentPreview_
  submission_
  submissionDetails_
  viewUserScore_
  newUserScore_
  modifyUserScore_
  groupRegistration_
  userDetails_
  uploadFile_
  createCourse_
  createGroup_
  assignCourseAdmin_
  assignGroupAdmin_
  createTestScript_
  modifyTestScript_
  changePassword_
#ifndef SSO
  setUserPassword_
#endif
  deleteUsersFromCourse_
  deleteUsersFromGroup_
  queueSubmissionForTest_
  queueAllSubmissionsForTest_
  unsubscribeFromCourse_
  exportEvaluationsScoresAdminedGroups_
  exportEvaluationsScoresAllGroups_
  exportSubmissions_
  exportSubmissionsOfGroups_
  exportSubmissionsOfOneGroup_
  getSubmission_
  getSubmissionsOfUserInGroup_
  getSubmissionsOfAssignmentInGroup_
  getCourseCsv_
  getGroupCsv_
  newGroupAssessment_
  newCourseAssessment_
  fillNewGroupAssessmentPreview_
  fillNewCourseAssessmentPreview_
  modifyAssessment_
  modifyAssessmentPreview_
  viewAssessment_
  notifications_
  submissionTable_
  usersInGroup_
  = pageCata
      (c $ index index_)
      (c $ login login_)
      (c $ logout logout_)
      (c $ welcome welcome_)
      (c $ profile profile_)
      (c $ administration administration_)
      (\gk _ -> studentView gk studentView_)
      (\gk _ -> groupOverview gk groupOverview_)
      (\gk _ -> groupOverviewAsStudent gk groupOverviewAsStudent_)
      (\ck contents _ -> courseManagement ck contents courseManagement_)
      (c $ evaluationTable evaluationTable_)
      (\ek _ -> evaluation ek evaluation_)
      (\sk ek _ -> modifyEvaluation sk ek modifyEvaluation_)
      (\gk _ -> newGroupAssignment gk newGroupAssignment_)
      (\ck _ -> newCourseAssignment ck newCourseAssignment_)
      (\ak _ -> modifyAssignment ak modifyAssignment_)
      (\ak _ -> viewAssignment ak viewAssignment_)
      (\gk _ -> newGroupAssignmentPreview gk newGroupAssignmentPreview_)
      (\ck _ -> newCourseAssignmentPreview ck newCourseAssignmentPreview_)
      (\ak _ -> modifyAssignmentPreview ak modifyAssignmentPreview_)
      (\ak _ -> submission ak submission_)
      (\ak sk _ -> submissionDetails ak sk submissionDetails_)
      (\sk _ -> viewUserScore sk viewUserScore_)
      (\assk u _ -> newUserScore assk u newUserScore_)
      (\sk _ -> modifyUserScore sk modifyUserScore_)
      (c $ groupRegistration groupRegistration_)
      (c $ userDetails userDetails_)
      (c $ uploadFile uploadFile_)
      (c $ createCourse createCourse_)
      (\ck _ -> createGroup ck createGroup_)
      (c $ assignCourseAdmin assignCourseAdmin_)
      (\ck _ -> assignGroupAdmin ck assignGroupAdmin_)
      (\ck _ -> createTestScript ck createTestScript_)
      (\ck tsk _ -> modifyTestScript ck tsk modifyTestScript_)
      (c $ changePassword changePassword_)
#ifndef SSO
      (c $ setUserPassword setUserPassword_)
#endif
      (\ck _ -> deleteUsersFromCourse ck deleteUsersFromCourse_)
      (\gk _ -> deleteUsersFromGroup gk deleteUsersFromGroup_)
      (\sk _ -> queueSubmissionForTest sk queueSubmissionForTest_)
      (\ak _ -> queueAllSubmissionsForTest ak queueAllSubmissionsForTest_)
      (\gk _ -> unsubscribeFromCourse gk unsubscribeFromCourse_)
      (\ck _ -> exportEvaluationsScoresAdminedGroups ck exportEvaluationsScoresAdminedGroups_)
      (\ck _ -> exportEvaluationsScoresAllGroups ck exportEvaluationsScoresAllGroups_)
      (\ak _ -> exportSubmissions ak exportSubmissions_)
      (\ak u _ -> exportSubmissionsOfGroups ak u exportSubmissionsOfGroups_)
      (\ak gk _ -> exportSubmissionsOfOneGroup ak gk exportSubmissionsOfOneGroup_)
      (\sk _ -> getSubmission sk getSubmission_)
      (\gk uid _ -> getSubmissionsOfUserInGroup gk uid getSubmissionsOfUserInGroup_)
      (\gk ak _ -> getSubmissionsOfAssignmentInGroup gk ak getSubmissionsOfAssignmentInGroup_)
      (\ck _ -> getCourseCsv ck getCourseCsv_)
      (\gk _ -> getGroupCsv gk getGroupCsv_)
      (\gk _ -> newGroupAssessment gk newGroupAssessment_)
      (\ck _ -> newCourseAssessment ck newCourseAssessment_)
      (\gk _ -> fillNewGroupAssessmentPreview gk fillNewGroupAssessmentPreview_)
      (\ck _ -> fillNewCourseAssessmentPreview ck fillNewCourseAssessmentPreview_)
      (\ak _ -> modifyAssessment ak modifyAssessment_)
      (\ak _ -> modifyAssessmentPreview ak modifyAssessmentPreview_)
      (\ak _ -> viewAssessment ak viewAssessment_)
      (c $ notifications notifications_)
      (\gk _ -> submissionTable gk submissionTable_)
      (\gk _ -> usersInGroup gk usersInGroup_)
  where
    c = const


liftsP
  index_
  login_
  logout_
  welcome_
  profile_
  administration_
  studentView_
  groupOverview_
  groupOverviewAsStudent_
  courseManagement_
  evaluationTable_
  evaluation_
  modifyEvaluation_
  newGroupAssignment_
  newCourseAssignment_
  modifyAssignment_
  viewAssignment_
  newGroupAssignmentPreview_
  newCourseAssignmentPreview_
  modifyAssignmentPreview_
  submission_
  submissionDetails_
  viewUserScore_
  newUserScore_
  modifyUserScore_
  groupRegistration_
  userDetails_
  uploadFile_
  createCourse_
  createGroup_
  assignCourseAdmin_
  assignGroupAdmin_
  createTestScript_
  modifyTestScript_
  changePassword_
#ifndef SSO
  setUserPassword_
#endif
  deleteUsersFromCourse_
  deleteUsersFromGroup_
  queueSubmissionForTest_
  queueAllSubmissionsForTest_
  unsubscribeFromCourse_
  exportEvaluationsScores_
  exportEvaluationsScoresAllGroups_
  exportSubmissions_
  exportSubmissionsOfGroups_
  exportSubmissionsOfOneGroup_
  getSubmission_
  getSubmissionsOfUserInGroup_
  getSubmissionsOfAssignmentInGroup_
  getCourseCsv_
  getGroupCsv_
  newGroupAssessment_
  newCourseAssessment_
  fillNewGroupAssessmentPreview_
  fillNewCourseAssessmentPreview_
  modifyAssessment_
  modifyAssessmentPreview_
  viewAssessment_
  notifications_
  submissionTable_
  usersInGroup_
  = pageCata
      (index . index_)
      (login . login_)
      (logout . logout_)
      (welcome . welcome_)
      (profile . profile_)
      (administration . administration_)
      (\gk a -> studentView gk (studentView_ gk a))
      (\gk a -> groupOverview gk (groupOverview_ gk a))
      (\gk a -> groupOverviewAsStudent gk (groupOverviewAsStudent_ gk a))
      (\ck contents a -> courseManagement ck contents (courseManagement_ ck contents a))
      (evaluationTable . evaluationTable_)
      (\ek a -> evaluation ek (evaluation_ ek a))
      (\sk ek a -> modifyEvaluation sk ek (modifyEvaluation_ sk ek a))
      (\gk a -> newGroupAssignment gk (newGroupAssignment_ gk a))
      (\ck a -> newCourseAssignment ck (newCourseAssignment_ ck a))
      (\ak a -> modifyAssignment ak (modifyAssignment_ ak a))
      (\ak a -> viewAssignment ak (viewAssignment_ ak a))
      (\gk a -> newGroupAssignmentPreview gk (newGroupAssignmentPreview_ gk a))
      (\ck a -> newCourseAssignmentPreview ck (newCourseAssignmentPreview_ ck a))
      (\ak a -> modifyAssignmentPreview ak (modifyAssignmentPreview_ ak a))
      (\ak a -> submission ak (submission_ ak a))
      (\ak sk a -> submissionDetails ak sk (submissionDetails_ ak sk a))
      (\sk a -> viewUserScore sk (viewUserScore_ sk a))
      (\assk u a -> newUserScore assk u (newUserScore_ assk u a))
      (\sk a -> modifyUserScore sk (modifyUserScore_ sk a))
      (groupRegistration . groupRegistration_)
      (userDetails . userDetails_)
      (uploadFile . uploadFile_)
      (createCourse . createCourse_)
      (\ck a -> createGroup ck (createGroup_ ck a))
      (assignCourseAdmin . assignCourseAdmin_)
      (\ck a -> assignGroupAdmin ck (assignGroupAdmin_ ck a))
      (\ck a -> createTestScript ck (createTestScript_ ck a))
      (\ck tsk a -> modifyTestScript ck tsk (modifyTestScript_ ck tsk a))
      (changePassword . changePassword_)
#ifndef SSO
      (setUserPassword . setUserPassword_)
#endif
      (\ck a -> deleteUsersFromCourse ck (deleteUsersFromCourse_ ck a))
      (\gk a -> deleteUsersFromGroup gk (deleteUsersFromGroup_ gk a))
      (\sk a -> queueSubmissionForTest sk (queueSubmissionForTest_ sk a))
      (\ak a -> queueAllSubmissionsForTest ak (queueAllSubmissionsForTest_ ak a))
      (\gk a -> unsubscribeFromCourse gk (unsubscribeFromCourse_ gk a))
      (\ck a -> exportEvaluationsScoresAdminedGroups ck (exportEvaluationsScores_ ck a))
      (\ck a -> exportEvaluationsScoresAllGroups ck (exportEvaluationsScoresAllGroups_ ck a))
      (\ak a -> exportSubmissions ak (exportSubmissions_ ak a))
      (\ak u a -> exportSubmissionsOfGroups ak u (exportSubmissionsOfGroups_ ak u a))
      (\ak gk a -> exportSubmissionsOfOneGroup ak gk (exportSubmissionsOfOneGroup_ ak gk a))
      (\sk a -> getSubmission sk (getSubmission_ sk a))
      (\gk uid a -> getSubmissionsOfUserInGroup gk uid (getSubmissionsOfUserInGroup_ gk uid a))
      (\gk ak a -> getSubmissionsOfAssignmentInGroup gk ak (getSubmissionsOfAssignmentInGroup_ gk ak a))
      (\ck a -> getCourseCsv ck (getCourseCsv_ ck a))
      (\gk a -> getGroupCsv gk (getGroupCsv_ gk a))
      (\gk a -> newGroupAssessment gk (newGroupAssessment_ gk a))
      (\ck a -> newCourseAssessment ck (newCourseAssessment_ ck a))
      (\gk a -> fillNewGroupAssessmentPreview gk (fillNewGroupAssessmentPreview_ gk a))
      (\ck a -> fillNewCourseAssessmentPreview ck (fillNewCourseAssessmentPreview_ ck a))
      (\ak a -> modifyAssessment ak (modifyAssessment_ ak a))
      (\ak a -> modifyAssessmentPreview ak (modifyAssessmentPreview_ ak a))
      (\ak a -> viewAssessment ak (viewAssessment_ ak a))
      (notifications . notifications_)
      (\gk a -> submissionTable gk (submissionTable_ gk a))
      (\gk a -> usersInGroup gk (usersInGroup_ gk a))

isIndex (View (Index _)) = True
isIndex _ = False

isLogin (View (Login _)) = True
isLogin _ = False

isLogout (View (Logout _)) = True
isLogout _ = False

isWelcome (View (Welcome _)) = True
isWelcome _ = False

isProfile (ViewModify (Profile _)) = True
isProfile _ = False

isAdministration (View (Administration _)) = True
isAdministration _ = False

isStudentView (View (StudentView _ _)) = True
isStudentView _ = False

isGroupOverview (View (GroupOverview _ _)) = True
isGroupOverview _ = False

isGroupOverviewAsStudent (View (GroupOverviewAsStudent _ _)) = True
isGroupOverviewAsStudent _ = False

isCourseManagement (View (CourseManagement _ _ _)) = True
isCourseManagement _ = False

isCourseManagement_Assignments (View (CourseManagement _ AssignmentsContents _)) = True
isCourseManagement_Assignments _ = False

isEvaluationTable (View (EvaluationTable _)) = True
isEvaluationTable _ = False

isEvaluation (ViewModify (Evaluation _ _)) = True
isEvaluation _ = False

isModifyEvaluation (ViewModify (ModifyEvaluation _ _ _)) = True
isModifyEvaluation _ = False

isNewGroupAssignment (ViewModify (NewGroupAssignment _ _)) = True
isNewGroupAssignment _ = False

isNewCourseAssignment (ViewModify (NewCourseAssignment _ _)) = True
isNewCourseAssignment _ = False

isModifyAssignment (ViewModify (ModifyAssignment _ _)) = True
isModifyAssignment _ = False

isViewAssignment (View (ViewAssignment _ _)) = True
isViewAssignment _ = False

isNewGroupAssignmentPreview (UserView (NewGroupAssignmentPreview _ _)) = True
isNewGroupAssignmentPreview _ = False

isNewCourseAssignmentPreview (UserView (NewCourseAssignmentPreview _ _)) = True
isNewCourseAssignmentPreview _ = False

isModifyAssignmentPreview (UserView (ModifyAssignmentPreview _ _)) = True
isModifyAssignmentPreview _ = False

isSubmission (ViewModify (Submission _ _)) = True
isSubmission _ = False

isSubmissionDetails (ViewModify (SubmissionDetails _ _ _)) = True
isSubmissionDetails _ = False

isViewUserScore (View (ViewUserScore _ _)) = True
isViewUserScore _ = False

isNewUserScore (ViewModify (NewUserScore _ _ _)) = True
isNewUserScore _ = False

isModifyUserScore (ViewModify (ModifyUserScore _ _)) = True
isModifyUserScore _ = False

isGroupRegistration (ViewModify (GroupRegistration _)) = True
isGroupRegistration _ = False

isUserDetails (ViewModify (UserDetails _)) = True
isUserDetails _ = False

isUploadFile (ViewModify (UploadFile _)) = True
isUploadFile _ = False

isCreateCourse (Modify (CreateCourse _)) = True
isCreateCourse _ = False

isCreateGroup (Modify (CreateGroup _ _)) = True
isCreateGroup _ = False

isAssignCourseAdmin (Modify (AssignCourseAdmin _)) = True
isAssignCourseAdmin _ = False

isAssignGroupAdmin (Modify (AssignGroupAdmin _ _)) = True
isAssignGroupAdmin _ = False

isCreateTestScript (Modify (CreateTestScript _ _)) = True
isCreateTestScript _ = False

isModifyTestScript (Modify (ModifyTestScript _ _ _)) = True
isModifyTestScript _ = False

isChangePassword (Modify (ChangePassword _)) = True
isChangePassword _ = False

#ifndef SSO
isSetUserPassword (ViewModify (SetUserPassword _)) = True
isSetUserPassword _ = False
#endif

isDeleteUsersFromCourse (Modify (DeleteUsersFromCourse _ _)) = True
isDeleteUsersFromCourse _ = False

isDeleteUsersFromGroup (Modify (DeleteUsersFromGroup _ _)) = True
isDeleteUsersFromGroup _ = False

isQueueSubmissionForTest (Modify (QueueSubmissionForTest _ _)) = True
isQueueSubmissionForTest _ = False

isQueueAllSubmissionsForTest (Modify (QueueAllSubmissionsForTest _ _)) = True
isQueueAllSubmissionsForTest _ = False

isUnsubscribeFromCourse (Modify (UnsubscribeFromCourse _ _)) = True
isUnsubscribeFromCourse _ = False

isExportEvaluationsScoresAdminedGroups (Data (ExportEvaluationsScoresAdminedGroups _ _)) = True
isExportEvaluationsScoresAdminedGroups _ = False

isExportEvaluationsScoresAllGroups (Data (ExportEvaluationsScoresAllGroups _ _)) = True
isExportEvaluationsScoresAllGroups _ = False

isExportSubmissions (Data (ExportSubmissions _ _)) = True
isExportSubmissions _ = False

isExportSubmissionsOfGroups (Data (ExportSubmissionsOfGroups _ _ _)) = True
isExportSubmissionsOfGroups _ = False

isExportSubmissionsOfOneGroup (Data (ExportSubmissionsOfOneGroup _ _ _)) = True
isExportSubmissionsOfOneGroup _ = False

isGetSubmission (Data (GetSubmission _ _)) = True
isGetSubmission _ = False

isGetSubmissionsOfUserInGroup (Data (GetSubmissionsOfUserInGroup _ _ _)) = True
isGetSubmissionsOfUserInGroup _ = False

isGetSubmissionsOfAssignmentInGroup (Data (GetSubmissionsOfAssignmentInGroup _ _ _)) = True
isGetSubmissionsOfAssignmentInGroup _ = False

isGetCourseCsv (Data (GetCourseCsv _ _)) = True
isGetCourseCsv _ = False

isGetGroupCsv (Data (GetGroupCsv _ _)) = True
isGetGroupCsv _ = False

isNewGroupAssessment (ViewModify (NewGroupAssessment _ _)) = True
isNewGroupAssessment _ = False

isNewCourseAssessment (ViewModify (NewCourseAssessment _ _)) = True
isNewCourseAssessment _ = False

isFillNewGroupAssessmentPreview (UserView (FillNewGroupAssessmentPreview _ _)) = True
isFillNewGroupAssessmentPreview _ = False

isFillNewCourseAssessmentPreview (UserView (FillNewCourseAssessmentPreview _ _)) = True
isFillNewCourseAssessmentPreview _ = False

isModifyAssessment (ViewModify (ModifyAssessment _ _)) = True
isModifyAssessment _ = False

isModifyAssessmentPreview (UserView (ModifyAssessmentPreview _ _)) = True
isModifyAssessmentPreview _ = False

isViewAssessment (View (ViewAssessment _ _)) = True
isViewAssessment _ = False

isNotifications (View (Notifications _)) = True
isNotifications _ = False

isSubmissionTable (RestView (SubmissionTable _ _)) = True
isSubmissionTable _ = False

isUsersInGroup (RestView (UsersInGroup _ _)) = True
isUsersInGroup _ = False

-- Returns True if the given page satisfies one of the given predicates in the page predicate
-- list
isPage :: [Page a b c d e f -> Bool] -> Page a b c d e f -> Bool
isPage fs p = or $ map ($ p) fs

-- Shortcut binary or for the given predicates
(<||>) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f <||> g = \x -> case f x of
                   True -> True
                   False -> g x

regularPages = [
    isWelcome
  , isStudentView
  , isLogout
  , isProfile
  , isChangePassword
  , isSubmission
  , isSubmissionDetails
  , isGroupRegistration
  , isGetSubmission
  , isViewAssessment
  , isViewUserScore
  , isNotifications
  , isUnsubscribeFromCourse
  ]

groupAdminPages = [
    isGroupOverview
  , isGroupOverviewAsStudent
  , isCourseManagement_Assignments
  , isEvaluationTable
  , isEvaluation
  , isModifyEvaluation
  , isNewGroupAssignment
  , isModifyAssignment
  , isViewAssignment
  , isNewGroupAssignmentPreview
  , isModifyAssignmentPreview
  , isNewUserScore
  , isModifyUserScore
#ifndef SSO
  , isSetUserPassword
#endif
  , isUploadFile
  , isNewGroupAssessment
  , isFillNewGroupAssessmentPreview
  , isModifyAssessment
  , isModifyAssessmentPreview
  , isExportEvaluationsScoresAdminedGroups
  , isExportSubmissions
  , isExportSubmissionsOfGroups
  , isExportSubmissionsOfOneGroup
  , isGetGroupCsv
  , isEvaluation
  , isModifyEvaluation
  , isDeleteUsersFromCourse
  , isDeleteUsersFromGroup
  , isQueueSubmissionForTest
  , isQueueAllSubmissionsForTest
  , isSubmissionTable
  , isGetSubmissionsOfUserInGroup
  , isGetSubmissionsOfAssignmentInGroup
  , isUsersInGroup
  ]

courseAdminPages = [
    isGroupOverview
  , isGroupOverviewAsStudent
  , isCourseManagement
  , isCreateGroup
  , isAssignGroupAdmin
  , isCreateTestScript
  , isModifyTestScript
  , isEvaluationTable
  , isEvaluation
  , isModifyEvaluation
  , isNewCourseAssignment
  , isNewGroupAssignment
  , isModifyAssignment
  , isNewCourseAssignmentPreview
  , isNewGroupAssignmentPreview
  , isModifyAssignmentPreview
  , isViewAssignment
  , isNewUserScore
  , isModifyUserScore
#ifndef SSO
  , isSetUserPassword
#endif
  , isUploadFile
  , isNewCourseAssessment
  , isNewGroupAssessment
  , isFillNewCourseAssessmentPreview
  , isFillNewGroupAssessmentPreview
  , isModifyAssessment
  , isModifyAssessmentPreview
  , isExportEvaluationsScoresAdminedGroups
  , isExportEvaluationsScoresAllGroups
  , isExportSubmissions
  , isExportSubmissionsOfGroups
  , isExportSubmissionsOfOneGroup
  , isGetCourseCsv
  , isGetGroupCsv
  , isEvaluation
  , isModifyEvaluation
  , isDeleteUsersFromCourse
  , isDeleteUsersFromGroup
  , isQueueSubmissionForTest
  , isQueueAllSubmissionsForTest
  , isSubmissionTable
  , isGetSubmissionsOfUserInGroup
  , isGetSubmissionsOfAssignmentInGroup
  , isUsersInGroup
  ]

adminPages = [
    isAdministration
  , isCreateCourse
  , isUserDetails
  , isAssignCourseAdmin
  , isUploadFile
  ]

isUserViewPage :: Page a b c d e f -> Bool
isUserViewPage = pageCata'
  false -- view
  true  -- userView
  false -- viewModify
  false -- modify
  false -- data
  false -- restView
  where
    false = const False
    true  = const True

publicPages = [
    isIndex
  , isLogin
  ]

-- Returns a page predicate function depending on the role, which page transition is allowed,
-- from a given page
allowedPage :: E.Role -> Page a b c d e f -> Bool
allowedPage = E.roleCata student groupAdmin courseAdmin admin
  where
    student     = isPage regularPages
    groupAdmin  = isPage (groupAdminPages ++ regularPages)
    courseAdmin = isPage (courseAdminPages ++ regularPages)
    admin       = isPage (adminPages ++ regularPages)

homePageToPageDesc :: HomePageContents -> PageDesc
homePageToPageDesc = R.homePageContentsCata
                       (welcome ())                    -- Welcome
                       (\gk -> studentView gk ())   -- StudentView
                       (\gk -> groupOverview gk ()) -- GroupOverview
                       (\gk -> groupOverviewAsStudent gk ()) -- GroupOverViewAsStudent
                       (\ck -> courseManagement ck defaultCourseManagementContents ()) -- CourseManagement
                       (administration ()) -- Administration

#ifdef TEST

pageDescTest = assertProperty
  "Total page union: Regular, Admin and Public"
  (isPage ((join [ regularPages, groupAdminPages, courseAdminPages
                 , adminPages
                 , publicPages
                 ])))
  pageGen
  "Regular, Admin and NonMenu pages should cover all pages"
  where

pageGen :: Gen PageDesc
pageGen = oneof [
    nonParametricPages
  , parametricPages
  ] where
      showInt :: Int -> String
      showInt = show

      assignmentKey = R.AssignmentKey . showInt <$> choose (1,5000)
      assessmentKey = R.AssessmentKey . showInt <$> choose (1,5000)
      submissionKey = R.SubmissionKey . showInt <$> choose (1,5000)
      scoreKey      = R.ScoreKey . showInt      <$> choose (1,5000)
      evaluationKey = R.EvaluationKey . showInt <$> choose (1,5000)
      courseKey     = R.CourseKey . showInt     <$> choose (1,5000)
      groupKey      = R.GroupKey . showInt      <$> choose (1,5000)
      testScriptKey = R.TestScriptKey . showInt <$> choose (1,5000)
      username      = E.Username <$> vectorOf 6 alphaNum
      uid           = E.Uid <$> vectorOf 6 alphaNum
      courseManagementContents = oneof ((ModifyTestScriptContents <$> testScriptKey) : map pure [GroupManagementContents, TestScriptsContents, AssignmentsContents, NewTestScriptContents])

      nonParametricPages = elements [
          index ()
        , login ()
        , logout ()
        , welcome ()
        , profile ()
        , administration ()
        , evaluationTable ()
        , groupRegistration ()
        , userDetails ()
        , uploadFile ()
        , createCourse ()
        , assignCourseAdmin ()
        , changePassword ()
#ifndef SSO
        , setUserPassword ()
#endif
        , notifications ()
        ]

      parametricPages = oneof [
          evaluation <$> submissionKey <*> unit
        , studentView <$> groupKey <*> unit
        , groupOverview <$> groupKey <*> unit
        , groupOverviewAsStudent <$> groupKey <*> unit
        , courseManagement <$> courseKey <*> courseManagementContents <*> unit
        , createGroup <$> courseKey <*> unit
        , assignGroupAdmin <$> courseKey <*> unit
        , createTestScript <$> courseKey <*> unit
        , modifyTestScript <$> courseKey <*> testScriptKey <*> unit
        , modifyEvaluation <$> submissionKey <*> evaluationKey <*> unit
        , submission <$> assignmentKey <*> unit
        , submissionDetails <$> assignmentKey <*> submissionKey <*> unit
        , viewUserScore <$> scoreKey <*> unit
        , newUserScore <$> assessmentKey <*> username <*> unit
        , modifyUserScore <$> scoreKey <*> unit
        , deleteUsersFromCourse <$> courseKey <*> unit
        , deleteUsersFromGroup <$> groupKey <*> unit
        , queueSubmissionForTest <$> submissionKey <*> unit
        , queueAllSubmissionsForTest <$> assignmentKey <*> unit
        , unsubscribeFromCourse <$> groupKey <*> unit
        , newCourseAssignment <$> courseKey <*> unit
        , newGroupAssignment <$> groupKey <*> unit
        , modifyAssignment <$> assignmentKey <*> unit
        , viewAssignment <$> assignmentKey <*> unit
        , newCourseAssignmentPreview <$> courseKey <*> unit
        , newGroupAssignmentPreview <$> groupKey <*> unit
        , modifyAssignmentPreview <$> assignmentKey <*> unit
        , getSubmission <$> submissionKey <*> unit
        , exportEvaluationsScoresAdminedGroups <$> courseKey <*> unit
        , exportEvaluationsScoresAllGroups <$> courseKey <*> unit
        , exportSubmissions <$> assignmentKey <*> unit
        , exportSubmissionsOfGroups <$> assignmentKey <*> username <*> unit
        , exportSubmissionsOfOneGroup <$> assignmentKey <*> groupKey <*> unit
        , getCourseCsv <$> courseKey <*> unit
        , getGroupCsv <$> groupKey <*> unit
        , newGroupAssessment <$> groupKey <*> unit
        , newCourseAssessment <$> courseKey <*> unit
        , fillNewGroupAssessmentPreview <$> groupKey <*> unit
        , fillNewCourseAssessmentPreview <$> courseKey <*> unit
        , modifyAssessment <$> assessmentKey <*> unit
        , modifyAssessmentPreview <$> assessmentKey <*> unit
        , viewAssessment <$> assessmentKey <*> unit
        , submissionTable <$> groupKey <*> unit
        , getSubmissionsOfUserInGroup <$> groupKey <*> uid <*> unit
        , getSubmissionsOfAssignmentInGroup <$> groupKey <*> assignmentKey <*> unit
        , usersInGroup <$> groupKey <*> unit
        ]

      unit = return ()

#endif

