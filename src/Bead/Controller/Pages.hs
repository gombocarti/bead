{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
module Bead.Controller.Pages where

import           Control.Monad (join)

import qualified Bead.Domain.Entities      as E
import           Bead.Domain.Relationships as R

#ifdef TEST
import           Control.Applicative
import           Test.Tasty.Arbitrary (alphaNum)
import           Test.Tasty.TestSet
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Arbitrary (arbitrary)
#endif

-- View pages are rendered using the data stored in the
-- persistence layer. Mainly for information propagation
-- for the user.
data ViewPage a
  = Index a
  | Login a
  | Logout a
  | Home a
  | CourseOverview CourseKey a
  | EvaluationTable a
  | ViewAssignment AssignmentKey a
  | Administration a
  | CourseAdmin a
  | ViewAssessment AssessmentKey a
  | ViewUserScore ScoreKey a
  | Notifications a
  deriving (Eq, Ord, Show, Functor)

viewPageCata
  index
  login
  logout
  home
  courseOverview
  evaluationTable
  viewAssignment
  administration
  courseAdmin
  viewAssessment
  viewUserScore
  notifications
  p = case p of
    Index a -> index a
    Login a -> login a
    Logout a -> logout a
    Home a -> home a
    CourseOverview ck a -> courseOverview ck a
    EvaluationTable a -> evaluationTable a
    ViewAssignment ak a -> viewAssignment ak a
    Administration a -> administration a
    CourseAdmin a -> courseAdmin a
    ViewAssessment ak a -> viewAssessment ak a
    ViewUserScore sk a -> viewUserScore sk a
    Notifications a -> notifications a

viewPageValue :: ViewPage a -> a
viewPageValue = viewPageCata
  id -- index
  id -- login
  id -- logout
  id -- home
  cid -- courseOverview
  id -- evaluationTable
  cid -- viewAssignment
  id -- administration
  id -- courseAdmin
  cid -- viewAssessment
  cid -- viewUserScore
  id -- notifications
  where
    cid = const id

-- Pages that extract information from the persistence
-- and only the data will be rendered in the response
-- (e.g. file download)
data DataPage a
  = ExportEvaluationsScores CourseKey a
  | ExportEvaluationsScoresAllGroups CourseKey a
  | ExportSubmissions AssignmentKey a
  | ExportSubmissionsOfGroups AssignmentKey E.Username a
  | ExportSubmissionsOfOneGroup AssignmentKey GroupKey a
  | GetSubmission SubmissionKey a
  | GetCourseCsv CourseKey a
  | GetGroupCsv GroupKey a
  deriving (Eq, Ord, Show, Functor)

dataPageCata
  exportEvaluationsScores
  exportEvaluationsScoresAllGroups
  exportSubmissions
  exportSubmissionsOfGroups
  exportSubmissionsOfOneGroup
  getSubmission
  getCourseCsv
  getGroupCsv
  p = case p of
    ExportEvaluationsScores ck a -> exportEvaluationsScores ck a
    ExportEvaluationsScoresAllGroups ck a -> exportEvaluationsScoresAllGroups ck a
    ExportSubmissions ak a -> exportSubmissions ak a
    ExportSubmissionsOfGroups ak u a -> exportSubmissionsOfGroups ak u a
    ExportSubmissionsOfOneGroup ak gk a -> exportSubmissionsOfOneGroup ak gk a
    GetSubmission sk a -> getSubmission sk a
    GetCourseCsv ck a -> getCourseCsv ck a
    GetGroupCsv gk a -> getGroupCsv gk a

dataPageValue :: DataPage a -> a
dataPageValue = dataPageCata
  cid  -- exportEvaluationsScores
  cid  -- exportEvaluationsScoresAllGroups
  cid  -- exportSubmissions
  c2id -- exportSubmissionsOfGroups
  c2id -- exportSubmissionsOfOneGroup
  cid  -- getSubmission
  cid  -- getCourseCsv
  cid  -- getGropCsv
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
  deriving (Eq, Ord, Show, Functor)

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
  | NewTestScript a
  | ModifyTestScript TestScriptKey a
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
  deriving (Eq, Ord, Show, Functor)

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
  newTestScript
  modifyTestScript
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
    NewTestScript a -> newTestScript a
    ModifyTestScript tk a -> modifyTestScript tk a
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
  id -- newTestScript
  cid -- modifyTestScript
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
  | CreateGroup a
  | AssignCourseAdmin a
  | AssignGroupAdmin a
  | ChangePassword a
  | DeleteUsersFromCourse R.CourseKey a
  | DeleteUsersFromGroup R.GroupKey a
  | UnsubscribeFromCourse R.GroupKey a
  deriving (Eq, Ord, Show, Functor)

modifyPageCata
  createCourse
  createGroup
  assignCourseAdmin
  assignGroupAdmin
  changePassword
  deleteUsersFromCourse
  deleteUsersFromGroup
  unsubscribeFromCourse
  p = case p of
    CreateCourse a -> createCourse a
    CreateGroup a -> createGroup a
    AssignCourseAdmin a -> assignCourseAdmin a
    AssignGroupAdmin a -> assignGroupAdmin a
    ChangePassword a -> changePassword a
    DeleteUsersFromCourse ck a -> deleteUsersFromCourse ck a
    DeleteUsersFromGroup gk a -> deleteUsersFromGroup gk a
    UnsubscribeFromCourse gk a -> unsubscribeFromCourse gk a

modifyPageValue :: ModifyPage a -> a
modifyPageValue = modifyPageCata
  id -- createCourse
  id -- createGroup
  id -- assignCourseAdmin
  id -- assignGroupAdmin
  id -- changePassword
  cid -- deleteUsersFromCourse
  cid -- deleteUsersFromGroup
  cid -- unsubscribeFromCourse
  where
    cid = const id
    c2id = const . cid

data RestViewPage a
  = SubmissionTable GroupKey a
  deriving (Eq, Ord, Show, Functor)

restViewPageCata :: (GroupKey -> a -> b) -> RestViewPage a -> b
restViewPageCata submissionTable p = case p of
  SubmissionTable gk a -> submissionTable gk a

restViewPageValue :: RestViewPage a -> a
restViewPageValue = restViewPageCata (const id)

-- The kind of the possible page types
data Page a b c d e f
  = View       (ViewPage a)
  | UserView   (UserViewPage b)
  | ViewModify (ViewModifyPage c)
  | Modify     (ModifyPage d)
  | Data       (DataPage e)
  | RestView   (RestViewPage f)
  deriving (Eq, Ord, Show)

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

index                   = View . Index
login                   = View . Login
logout                  = View . Logout
home                    = View . Home
courseOverview ck       = View . CourseOverview ck
evaluationTable         = View . EvaluationTable
viewAssignment ak       = View . ViewAssignment ak
administration          = View . Administration
courseAdmin             = View . CourseAdmin
viewAssessment ak       = View . ViewAssessment ak
viewUserScore sk        = View . ViewUserScore sk
notifications           = View . Notifications

exportEvaluationsScores ck          = Data . ExportEvaluationsScores ck
exportEvaluationsScoresAllGroups ck = Data . ExportEvaluationsScoresAllGroups ck
exportSubmissions ak                = Data . ExportSubmissions ak
exportSubmissionsOfGroups ak u      = Data . ExportSubmissionsOfGroups ak u
exportSubmissionsOfOneGroup ak gk   = Data . ExportSubmissionsOfOneGroup ak gk
getSubmission sk                    = Data . GetSubmission sk
getCourseCsv ck                     = Data . GetCourseCsv ck
getGroupCsv gk                      = Data . GetGroupCsv gk

newGroupAssignmentPreview gk  = UserView . NewGroupAssignmentPreview gk
newCourseAssignmentPreview ck = UserView . NewCourseAssignmentPreview ck
modifyAssignmentPreview ak    = UserView . ModifyAssignmentPreview ak

fillNewCourseAssessmentPreview ck = UserView . FillNewCourseAssessmentPreview ck
fillNewGroupAssessmentPreview gk  = UserView . FillNewGroupAssessmentPreview gk
modifyAssessmentPreview ak = UserView . ModifyAssessmentPreview ak

profile                = ViewModify . Profile
evaluation sk          = ViewModify . Evaluation sk
modifyEvaluation sk ek = ViewModify . ModifyEvaluation sk ek
newGroupAssignment gk  = ViewModify . NewGroupAssignment gk
newCourseAssignment ck = ViewModify . NewCourseAssignment ck
modifyAssignment ak    = ViewModify . ModifyAssignment ak
modifyAssessment ak    = ViewModify . ModifyAssessment ak
groupRegistration      = ViewModify . GroupRegistration
userDetails            = ViewModify . UserDetails
newTestScript          = ViewModify . NewTestScript
modifyTestScript tk    = ViewModify . ModifyTestScript tk
uploadFile             = ViewModify . UploadFile
#ifndef SSO
setUserPassword        = ViewModify . SetUserPassword
#endif
submissionDetails ak sk = ViewModify . SubmissionDetails ak sk
submission ak           = ViewModify . Submission ak
newUserScore assk u     = ViewModify . NewUserScore assk u
modifyUserScore sk      = ViewModify . ModifyUserScore sk
newCourseAssessment ck  = ViewModify . NewCourseAssessment ck
newGroupAssessment gk   = ViewModify . NewGroupAssessment gk

createCourse      = Modify . CreateCourse
createGroup       = Modify . CreateGroup
assignCourseAdmin = Modify . AssignCourseAdmin
assignGroupAdmin  = Modify . AssignGroupAdmin
changePassword    = Modify . ChangePassword
deleteUsersFromCourse ck          = Modify . DeleteUsersFromCourse ck
deleteUsersFromGroup gk           = Modify . DeleteUsersFromGroup gk
unsubscribeFromCourse gk          = Modify . UnsubscribeFromCourse gk

submissionTable gk = RestView . SubmissionTable gk

-- Template method for the page data structure
pageCata
  index
  login
  logout
  home
  profile
  administration
  courseAdmin
  courseOverview
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
  newTestScript
  modifyTestScript
  uploadFile
  createCourse
  createGroup
  assignCourseAdmin
  assignGroupAdmin
  changePassword
#ifndef SSO
  setUserPassword
#endif
  deleteUsersFromCourse
  deleteUsersFromGroup
  unsubscribeFromCourse
  exportEvaluationsScores
  exportEvaluationsScoresAllGroups
  exportSubmissions
  exportSubmissionsOfGroups
  exportSubmissionsOfOneGroup
  getSubmission
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
  p = case p of
    (View (Index a)) -> index a
    (View (Login a)) -> login a
    (View (Logout a)) -> logout a
    (View (Home a)) -> home a
    (ViewModify (Profile a)) -> profile a
    (View (Administration a)) -> administration a
    (View (CourseAdmin a)) -> courseAdmin a
    (View (CourseOverview ck a)) -> courseOverview ck a
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
    (ViewModify (NewTestScript a)) -> newTestScript a
    (ViewModify (ModifyTestScript tsk a)) -> modifyTestScript tsk a
    (ViewModify (UploadFile a)) -> uploadFile a
    (Modify (CreateCourse a)) -> createCourse a
    (Modify (CreateGroup a)) -> createGroup a
    (Modify (AssignCourseAdmin a)) -> assignCourseAdmin a
    (Modify (AssignGroupAdmin a)) -> assignGroupAdmin a
    (Modify (ChangePassword a)) -> changePassword a
#ifndef SSO
    (ViewModify (SetUserPassword a)) -> setUserPassword a
#endif
    (Modify (DeleteUsersFromCourse ck a)) -> deleteUsersFromCourse ck a
    (Modify (DeleteUsersFromGroup gk a)) -> deleteUsersFromGroup gk a
    (Modify (UnsubscribeFromCourse gk a)) -> unsubscribeFromCourse gk a
    (Data (ExportEvaluationsScores ck a)) -> exportEvaluationsScores ck a
    (Data (ExportEvaluationsScoresAllGroups ck a)) -> exportEvaluationsScoresAllGroups ck a
    (Data (ExportSubmissions ak a)) -> exportSubmissions ak a
    (Data (ExportSubmissionsOfGroups ak u a)) -> exportSubmissionsOfGroups ak u a
    (Data (ExportSubmissionsOfOneGroup ak gk a)) -> exportSubmissionsOfOneGroup ak gk a
    (Data (GetSubmission sk a)) -> getSubmission sk a
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

-- Constants that attached each of the page constructor
constantsP
  index_
  login_
  logout_
  home_
  profile_
  administration_
  courseAdmin_
  courseOverview_
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
  newTestScript_
  modifyTestScript_
  uploadFile_
  createCourse_
  createGroup_
  assignCourseAdmin_
  assignGroupAdmin_
  changePassword_
#ifndef SSO
  setUserPassword_
#endif
  deleteUsersFromCourse_
  deleteUsersFromGroup_
  unsubscribeFromCourse_
  exportEvaluationsScores_
  exportEvaluationsScoresAllGroups_
  exportSubmissions_
  exportSubmissionsOfGroups_
  exportSubmissionsOfOneGroup_
  getSubmission_
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
  = pageCata
      (c $ index index_)
      (c $ login login_)
      (c $ logout logout_)
      (c $ home home_)
      (c $ profile profile_)
      (c $ administration administration_)
      (c $ courseAdmin courseAdmin_)
      (\ck _ -> courseOverview ck courseOverview_)
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
      (c $ newTestScript newTestScript_)
      (\tsk _ -> modifyTestScript tsk modifyTestScript_)
      (c $ uploadFile uploadFile_)
      (c $ createCourse createCourse_)
      (c $ createGroup createGroup_)
      (c $ assignCourseAdmin assignCourseAdmin_)
      (c $ assignGroupAdmin assignGroupAdmin_)
      (c $ changePassword changePassword_)
#ifndef SSO
      (c $ setUserPassword setUserPassword_)
#endif
      (\ck _ -> deleteUsersFromCourse ck deleteUsersFromCourse_)
      (\gk _ -> deleteUsersFromGroup gk deleteUsersFromGroup_)
      (\gk _ -> unsubscribeFromCourse gk unsubscribeFromCourse_)
      (\ck _ -> exportEvaluationsScores ck exportEvaluationsScores_)
      (\ck _ -> exportEvaluationsScoresAllGroups ck exportEvaluationsScoresAllGroups_)
      (\ak _ -> exportSubmissions ak exportSubmissions_)
      (\ak u _ -> exportSubmissionsOfGroups ak u exportSubmissionsOfGroups_)
      (\ak gk _ -> exportSubmissionsOfOneGroup ak gk exportSubmissionsOfOneGroup_)
      (\sk _ -> getSubmission sk getSubmission_)
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
  where
    c = const


liftsP
  index_
  login_
  logout_
  home_
  profile_
  administration_
  courseAdmin_
  courseOverview_
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
  newTestScript_
  modifyTestScript_
  uploadFile_
  createCourse_
  createGroup_
  assignCourseAdmin_
  assignGroupAdmin_
  changePassword_
#ifndef SSO
  setUserPassword_
#endif
  deleteUsersFromCourse_
  deleteUsersFromGroup_
  unsubscribeFromCourse_
  exportEvaluationsScores_
  exportEvaluationsScoresAllGroups_
  exportSubmissions_
  exportSubmissionsOfGroups_
  exportSubmissionsOfOneGroup_
  getSubmission_
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
  = pageCata
      (index . index_)
      (login . login_)
      (logout . logout_)
      (home . home_)
      (profile . profile_)
      (administration . administration_)
      (courseAdmin . courseAdmin_)
      (\ck a -> courseOverview ck (courseOverview_ ck a))
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
      (newTestScript . newTestScript_)
      (\tsk a -> modifyTestScript tsk (modifyTestScript_ tsk a))
      (uploadFile . uploadFile_)
      (createCourse . createCourse_)
      (createGroup . createGroup_)
      (assignCourseAdmin . assignCourseAdmin_)
      (assignGroupAdmin . assignGroupAdmin_)
      (changePassword . changePassword_)
#ifndef SSO
      (setUserPassword . setUserPassword_)
#endif
      (\ck a -> deleteUsersFromCourse ck (deleteUsersFromCourse_ ck a))
      (\gk a -> deleteUsersFromGroup gk (deleteUsersFromGroup_ gk a))
      (\gk a -> unsubscribeFromCourse gk (unsubscribeFromCourse_ gk a))
      (\ck a -> exportEvaluationsScores ck (exportEvaluationsScores_ ck a))
      (\ck a -> exportEvaluationsScoresAllGroups ck (exportEvaluationsScoresAllGroups_ ck a))
      (\ak a -> exportSubmissions ak (exportSubmissions_ ak a))
      (\ak u a -> exportSubmissionsOfGroups ak u (exportSubmissionsOfGroups_ ak u a))
      (\ak gk a -> exportSubmissionsOfOneGroup ak gk (exportSubmissionsOfOneGroup_ ak gk a))
      (\sk a -> getSubmission sk (getSubmission_ sk a))
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

isIndex (View (Index _)) = True
isIndex _ = False

isLogin (View (Login _)) = True
isLogin _ = False

isLogout (View (Logout _)) = True
isLogout _ = False

isHome (View (Home _)) = True
isHome _ = False

isProfile (ViewModify (Profile _)) = True
isProfile _ = False

isAdministration (View (Administration _)) = True
isAdministration _ = False

isCourseAdmin (View (CourseAdmin _)) = True
isCourseAdmin _ = False

isCourseOverview (View (CourseOverview _ _)) = True
isCourseOverview _ = False

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

isNewTestScript (ViewModify (NewTestScript _)) = True
isNewTestScript _ = False

isModifyTestScript (ViewModify (ModifyTestScript _ _)) = True
isModifyTestScript _ = False

isUploadFile (ViewModify (UploadFile _)) = True
isUploadFile _ = False

isCreateCourse (Modify (CreateCourse _)) = True
isCreateCourse _ = False

isCreateGroup (Modify (CreateGroup _)) = True
isCreateGroup _ = False

isAssignCourseAdmin (Modify (AssignCourseAdmin _)) = True
isAssignCourseAdmin _ = False

isAssignGroupAdmin (Modify (AssignGroupAdmin _)) = True
isAssignGroupAdmin _ = False

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

isUnsubscribeFromCourse (Modify (UnsubscribeFromCourse _ _)) = True
isUnsubscribeFromCourse _ = False

isExportEvaluationsScores (Data (ExportEvaluationsScores _ _)) = True
isExportEvaluationsScores _ = False

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
    isHome
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
    isEvaluationTable
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
  , isExportEvaluationsScores
  , isExportSubmissions
  , isExportSubmissionsOfGroups
  , isExportSubmissionsOfOneGroup
  , isGetGroupCsv
  , isEvaluation
  , isModifyEvaluation
  , isDeleteUsersFromCourse
  , isDeleteUsersFromGroup
  , isSubmissionTable
  ]

courseAdminPages = [
    isCourseAdmin
  , isCourseOverview
  , isCreateGroup
  , isAssignGroupAdmin
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
  , isNewTestScript
  , isModifyTestScript
  , isUploadFile
  , isNewCourseAssessment
  , isNewGroupAssessment
  , isFillNewCourseAssessmentPreview
  , isFillNewGroupAssessmentPreview
  , isModifyAssessment
  , isModifyAssessmentPreview
  , isExportEvaluationsScores
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
  , isSubmissionTable
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

menuPageList = map ($ ()) [
    home
  , profile
  , administration
  , courseAdmin
  , evaluationTable
  , groupRegistration
  , newTestScript
  , uploadFile
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

-- Produces a Page list that must be rendered in the page menu for the given role
menuPages :: E.Role -> PageDesc -> [PageDesc]
menuPages r p = filter allowedPage' menuPageList
  where
    allowedPage' p' = and [
        allowedPage r p'
      , p' /= p
      ]

-- Returns a Page descriptor for the given Modify or ViewModify
-- parent page, where the page needs to be redirected after the
-- modification of the data.
parentPage :: PageDesc -> Maybe PageDesc
parentPage = pageCata'
  (const Nothing)  -- view
  (const Nothing)  -- userView
  viewModifyParent -- viewModify
  modifyParent     -- modify
  (const Nothing)  -- data
  (const Nothing)  -- restView
  where
    c2 = const . const
    viewModifyParent = Just . viewModifyPageCata
      profile -- profile
      (const evaluationTable) -- evaluation
      (c2 evaluationTable) -- modifyEvaluation
      (const home) -- newGroupAssignment
      (const home) -- newCourseAssignment
      (const home) -- modifyAssignment
      (const home) -- modifyAssessment
      home -- groupRegistration
      administration -- userDetails
      home           -- newTestScript
      (const home)   -- modifyTestScript
      uploadFile     -- uploadFile
#ifndef SSO
      home           -- setUserPassword
#endif
      submissionDetails -- submissionDetails
      (const home)   -- submission
      (c2 home)      -- newUserScore
      (const home)   -- modifyUserScore
      (const home)   -- newGroupAssessment
      (const home)   -- newCourseAssessment

    modifyParent = Just . modifyPageCata
      administration -- createCourse
      courseAdmin    -- createGroup
      administration -- assignCourseAdmin
      courseAdmin    -- assignGroupAdmin
      profile        -- changePassword
      (const home)   -- deleteUsersFromCourse
      (const home)   -- deleteUsersFromGroup
      (const home)   -- unsubscribeFromCourse

#ifdef TEST

pageDescTest = assertProperty
  "Total page union: Regular, Admin and NonMenu"
  (isPage ((join [ regularPages, groupAdminPages, courseAdminPages
                 , adminPages, menuPagePred
                 , publicPages
                 ])))
  pageGen
  "Regular, Admin and NonMenu pages should cover all pages"
  where
    menuPagePred = [flip elem menuPageList]

pageGen :: Gen PageDesc
pageGen = oneof [
    nonParametricPages
  , parametricPages
  ] where
      showInt :: Int -> String
      showInt = show

      assignmentKey = AssignmentKey . showInt <$> choose (1,5000)
      assessmentKey = AssessmentKey . showInt <$> choose (1,5000)
      submissionKey = SubmissionKey . showInt <$> choose (1,5000)
      scoreKey      = ScoreKey . showInt      <$> choose (1,5000)
      evaluationKey = EvaluationKey . showInt <$> choose (1,5000)
      courseKey     = CourseKey . showInt     <$> choose (1,5000)
      groupKey      = GroupKey . showInt      <$> choose (1,5000)
      testScriptKey = TestScriptKey . showInt <$> choose (1,5000)
      username      = E.Username <$> vectorOf 6 alphaNum

      nonParametricPages = elements [
          index ()
        , login ()
        , logout ()
        , home ()
        , profile ()
        , administration ()
        , courseAdmin ()
        , evaluationTable ()
        , groupRegistration ()
        , userDetails ()
        , uploadFile ()
        , createCourse ()
        , createGroup ()
        , assignCourseAdmin ()
        , assignGroupAdmin ()
        , changePassword ()
#ifndef SSO
        , setUserPassword ()
#endif
        , newTestScript ()
        , notifications ()
        ]

      parametricPages = oneof [
          evaluation <$> submissionKey <*> unit
        , courseOverview <$> courseKey <*> unit
        , modifyEvaluation <$> submissionKey <*> evaluationKey <*> unit
        , submission <$> assignmentKey <*> unit
        , submissionDetails <$> assignmentKey <*> submissionKey <*> unit
        , viewUserScore <$> scoreKey <*> unit
        , newUserScore <$> assessmentKey <*> username <*> unit
        , modifyUserScore <$> scoreKey <*> unit
        , deleteUsersFromCourse <$> courseKey <*> unit
        , deleteUsersFromGroup <$> groupKey <*> unit
        , unsubscribeFromCourse <$> groupKey <*> unit
        , modifyTestScript <$> testScriptKey <*> unit
        , newCourseAssignment <$> courseKey <*> unit
        , newGroupAssignment <$> groupKey <*> unit
        , modifyAssignment <$> assignmentKey <*> unit
        , viewAssignment <$> assignmentKey <*> unit
        , newCourseAssignmentPreview <$> courseKey <*> unit
        , newGroupAssignmentPreview <$> groupKey <*> unit
        , modifyAssignmentPreview <$> assignmentKey <*> unit
        , getSubmission <$> submissionKey <*> unit
        , exportEvaluationsScores <$> courseKey <*> unit
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
        ]

      unit = return ()

#endif

