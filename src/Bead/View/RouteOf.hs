{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
module Bead.View.RouteOf (
    ReqParam(..)
  , RequestParam(..)
  , ReqParamValue(..)
  , routeOf
  , routeWithParams
  , routeWithOptionalParams
  , routeWithAnchor
  , queryString -- Creates a well-formed query string from base path and parameters
  , RoutePath
  , indexPath
  , loginPath
  , changeLanguagePath
  , logoutPath
  , welcomePath
  , errorPath
  , profilePath
  , studentViewPath
  , groupOverviewPath
  , groupOverviewAsStudentPath
  , courseManagementPath
  , modifyEvaluationPath
  , evaluationTablePath
  , evaluationPath
  , submissionPath
  , viewUserScorePath
  , newUserScorePath
  , modifyUserScorePath
  , uploadFilePath
  , markdownPath
  , submissionDetailsPath
  , submissionTablePath
  , getSubmissionsOfUserInGroupPath
  , getSubmissionsOfAssignmentInGroupPath
  , usersInGroupPath
  , administrationPath
  , groupRegistrationPath
  , createCoursePath
  , userDetailsPath
  , assignCourseAdminPath
  , createGroupPath
  , assignGroupAdminPath
  , newGroupAssignmentPath
  , newCourseAssignmentPath
  , modifyAssignmentPath
  , viewAssignmentPath
  , newGroupAssignmentPreviewPath
  , newCourseAssignmentPreviewPath
  , modifyAssignmentPreviewPath
  , changePasswordPath
  , createTestScriptPath
  , modifyTestScriptPath
#ifndef SSO
  , setUserPasswordPath
#endif
  , deleteUsersFromCoursePath
  , deleteUsersFromGroupPath
  , queueSubmissionForTestPath
  , queueAllSubmissionsForTestPath
  , unsubscribeFromCoursePath
  , pageRoutePath
  , exportEvaluationsScoresAdminedGroupsPath
  , exportEvaluationsScoresAllGroupsPath
  , exportSubmissionsPath
  , exportSubmissionsOfGroupsPath
  , exportSubmissionsOfOneGroupPath
  , getSubmissionPath
  , getCourseCsvPath
  , getGroupCsvPath
  , newGroupAssessmentPath
  , newCourseAssessmentPath
  , fillNewGroupAssessmentPreviewPath
  , fillNewCourseAssessmentPreviewPath
  , modifyAssessmentPath
  , modifyAssessmentPreviewPath
  , viewAssessmentPath
  , notificationsPath
  , viewMossScriptOutputPath
  , similarityCheckMossPath
  , staticPath
  , pageRequestParams
#ifdef TEST
  , routeOfTest
#endif
  ) where

import           Data.Text (Text)
import qualified Data.Text as T

import           Bead.Controller.Pages
import           Bead.View.Anchor
import           Bead.View.RequestParams

#ifdef TEST
import           Test.Tasty.TestSet
import           Test.QuickCheck.Arbitrary
#endif


-- Route Path represents the route in the HTTP request
type RoutePath = Text

indexPath :: RoutePath
indexPath = "/"

loginPath :: RoutePath
loginPath = "/login"

changeLanguagePath :: RoutePath
changeLanguagePath = "/change-language"

logoutPath :: RoutePath
logoutPath = "/logout"

welcomePath :: RoutePath
welcomePath = "/welcome"

errorPath :: RoutePath
errorPath = "/error"

profilePath :: RoutePath
profilePath = "/profile"

studentViewPath :: RoutePath
studentViewPath = "/student-view"

groupOverviewPath :: RoutePath
groupOverviewPath = "/group-overview"

groupOverviewAsStudentPath :: RoutePath
groupOverviewAsStudentPath = "/group-overview-as-student"

courseManagementPath :: RoutePath
courseManagementPath = "/course-management"

modifyEvaluationPath :: RoutePath
modifyEvaluationPath = "/modify-evaluation"

evaluationTablePath :: RoutePath
evaluationTablePath = "/evaluation-table"

evaluationPath :: RoutePath
evaluationPath = "/evaluation"

submissionPath :: RoutePath
submissionPath = "/submission"

viewUserScorePath :: RoutePath
viewUserScorePath = "/view-user-score"

newUserScorePath :: RoutePath
newUserScorePath = "/new-user-score"

modifyUserScorePath :: RoutePath
modifyUserScorePath = "/modify-user-score"

uploadFilePath :: RoutePath
uploadFilePath = "/upload-file"

markdownPath :: RoutePath
markdownPath = "/markdown"

submissionDetailsPath :: RoutePath
submissionDetailsPath = "/submission-details"

administrationPath :: RoutePath
administrationPath = "/administration"

groupRegistrationPath :: RoutePath
groupRegistrationPath = "/group-registration"

createCoursePath :: RoutePath
createCoursePath = "/create-course"

userDetailsPath :: RoutePath
userDetailsPath = "/user-details"

assignCourseAdminPath :: RoutePath
assignCourseAdminPath = "/assign-course-admin"

createGroupPath :: RoutePath
createGroupPath = "/create-group"

assignGroupAdminPath :: RoutePath
assignGroupAdminPath = "/assign-group-admin"

newGroupAssignmentPath :: RoutePath
newGroupAssignmentPath = "/new-group-assignment"

newCourseAssignmentPath :: RoutePath
newCourseAssignmentPath = "/new-course-assignment"

modifyAssignmentPath :: RoutePath
modifyAssignmentPath = "/modify-assignment"

viewAssignmentPath :: RoutePath
viewAssignmentPath = "/view-assignment"

newGroupAssignmentPreviewPath :: RoutePath
newGroupAssignmentPreviewPath = "/new-group-assignment-preview"

newCourseAssignmentPreviewPath :: RoutePath
newCourseAssignmentPreviewPath = "/new-course-assignment-preview"

modifyAssignmentPreviewPath :: RoutePath
modifyAssignmentPreviewPath = "/modify-assignment-preview"

changePasswordPath :: RoutePath
changePasswordPath = "/change-password"

createTestScriptPath :: RoutePath
createTestScriptPath = "/create-test-script"

modifyTestScriptPath :: RoutePath
modifyTestScriptPath = "/modify-test-script"

#ifndef SSO
setUserPasswordPath :: RoutePath
setUserPasswordPath = "/set-user-password"
#endif

deleteUsersFromCoursePath :: RoutePath
deleteUsersFromCoursePath = "/delete-users-from-course"

deleteUsersFromGroupPath :: RoutePath
deleteUsersFromGroupPath = "/delete-users-from-group"

queueSubmissionForTestPath :: RoutePath
queueSubmissionForTestPath = "/queue-submission-for-test"

queueAllSubmissionsForTestPath :: RoutePath
queueAllSubmissionsForTestPath = "/queue-all-submissions-for-test"

unsubscribeFromCoursePath :: RoutePath
unsubscribeFromCoursePath = "/unsubscribe-from-course"

exportEvaluationsScoresAdminedGroupsPath :: RoutePath
exportEvaluationsScoresAdminedGroupsPath = "/export-evaluations-scores-admined-groups"

exportEvaluationsScoresAllGroupsPath :: RoutePath
exportEvaluationsScoresAllGroupsPath = "/export-evaluations-scores-all-groups"

exportSubmissionsPath :: RoutePath
exportSubmissionsPath = "/export-submissions"

exportSubmissionsOfGroupsPath :: RoutePath
exportSubmissionsOfGroupsPath = "/export-submissions-of-groups"

exportSubmissionsOfOneGroupPath :: RoutePath
exportSubmissionsOfOneGroupPath = "/export-submissions-of-one-group"

getSubmissionPath :: RoutePath
getSubmissionPath = "/get-submission"

getSubmissionsOfUserInGroupPath :: RoutePath
getSubmissionsOfUserInGroupPath = "/get-submissions-of-user-in-group"

getSubmissionsOfAssignmentInGroupPath :: RoutePath
getSubmissionsOfAssignmentInGroupPath = "/get-submissions-of-assignment-in-group"

getCourseCsvPath :: RoutePath
getCourseCsvPath = "/get-course-csv"

getGroupCsvPath :: RoutePath
getGroupCsvPath = "/get-group-csv"

newGroupAssessmentPath :: RoutePath
newGroupAssessmentPath = "/new-group-assessment"

newCourseAssessmentPath :: RoutePath
newCourseAssessmentPath = "/new-course-assessment"

fillNewGroupAssessmentPreviewPath :: RoutePath
fillNewGroupAssessmentPreviewPath = "/fill-new-group-assessment-preview"

fillNewCourseAssessmentPreviewPath :: RoutePath
fillNewCourseAssessmentPreviewPath = "/fill-new-course-assessment-preview"

modifyAssessmentPath :: RoutePath
modifyAssessmentPath = "/modify-assessment"

modifyAssessmentPreviewPath :: RoutePath
modifyAssessmentPreviewPath = "/modify-assessment-preview"

viewAssessmentPath :: RoutePath
viewAssessmentPath = "/view-assessment"

notificationsPath :: RoutePath
notificationsPath = "/notifications"

viewMossScriptOutputPath :: RoutePath
viewMossScriptOutputPath = "/view-moss-script-output"

similarityCheckMossPath :: RoutePath
similarityCheckMossPath = "/similarity-check-moss"

submissionTablePath :: RoutePath
submissionTablePath = "/rest/submission-table"

usersInGroupPath :: RoutePath
usersInGroupPath = "/rest/users-in-group"

staticPath :: RoutePath
staticPath = ""

type PageRoutePath = Page RoutePath RoutePath RoutePath RoutePath RoutePath RoutePath

-- Returns a base path for the given page
pageRoutePath :: Page a b c d e f -> PageRoutePath
pageRoutePath = constantsP
    indexPath
    loginPath
    logoutPath
    welcomePath
    profilePath
    administrationPath
    studentViewPath
    groupOverviewPath
    groupOverviewAsStudentPath
    courseManagementPath
    evaluationTablePath
    evaluationPath
    modifyEvaluationPath
    newGroupAssignmentPath
    newCourseAssignmentPath
    modifyAssignmentPath
    viewAssignmentPath
    newGroupAssignmentPreviewPath
    newCourseAssignmentPreviewPath
    modifyAssignmentPreviewPath
    submissionPath
    submissionDetailsPath
    viewUserScorePath
    newUserScorePath
    modifyUserScorePath
    groupRegistrationPath
    userDetailsPath
    uploadFilePath
    createCoursePath
    createGroupPath
    assignCourseAdminPath
    assignGroupAdminPath
    createTestScriptPath
    modifyTestScriptPath
    changePasswordPath
#ifndef SSO
    setUserPasswordPath
#endif
    deleteUsersFromCoursePath
    deleteUsersFromGroupPath
    queueSubmissionForTestPath
    queueAllSubmissionsForTestPath
    unsubscribeFromCoursePath
    exportEvaluationsScoresAdminedGroupsPath
    exportEvaluationsScoresAllGroupsPath
    exportSubmissionsPath
    exportSubmissionsOfGroupsPath
    exportSubmissionsOfOneGroupPath
    getSubmissionPath
    getSubmissionsOfUserInGroupPath
    getSubmissionsOfAssignmentInGroupPath
    getCourseCsvPath
    getGroupCsvPath
    newGroupAssessmentPath
    newCourseAssessmentPath
    fillNewGroupAssessmentPreviewPath
    fillNewCourseAssessmentPreviewPath
    modifyAssessmentPath
    modifyAssessmentPreviewPath
    viewAssessmentPath
    notificationsPath
    viewMossScriptOutputPath
    similarityCheckMossPath
    submissionTablePath
    usersInGroupPath

type PageReqParams = Page [ReqParam] [ReqParam] [ReqParam] [ReqParam] [ReqParam] [ReqParam]

-- Calculates a request parameter list from the given page value
pageRequestParams :: Page a b c d e f -> PageReqParams
pageRequestParams = liftsP
  (c []) -- index
  (c []) -- login
  (c []) -- logout
  (c []) -- home
  (c []) -- profile
  (c []) -- administration
  (\gk _ -> [requestParam gk]) -- studentView
  (\gk _ -> [requestParam gk]) -- groupOverview
  (\gk _ -> [requestParam gk]) -- groupOverviewAsStudent
  (\ck contents _ -> [requestParam ck, requestParam contents]) -- courseManagement
  (c []) -- evaluationTable
  (\ek _ -> [requestParam ek]) -- evaluation
  (\sk ek _ -> [requestParam sk, requestParam ek]) -- modifyEvaluation
  (\gk _ -> [requestParam gk]) -- newGroupAssignment
  (\ck _ -> [requestParam ck]) -- newCourseAssignment
  (\ak _ -> [requestParam ak]) -- modifyAssignment
  (\ak _ -> [requestParam ak]) -- viewAssignment
  (\gk _ -> [requestParam gk]) -- newGroupAssignmentPreview
  (\ck _ -> [requestParam ck]) -- newCourseAssignmentPreview
  (\ak _ -> [requestParam ak]) -- modifyAssignmentPreview
  (\ak _ -> [requestParam ak]) -- submission
  (\ak sk _ -> [requestParam ak, requestParam sk]) -- submissionDetails
  (\sk _ -> [requestParam sk]) -- viewUserScore
  (\assk u _ -> [requestParam assk, requestParam u]) -- newUserScore
  (\sk _ -> [requestParam sk]) -- modifyUserScore
  (c []) -- groupRegistration
  (c []) -- userDetails
  (c []) -- uploadFile
  (c []) -- createCourse
  (\ck _ -> [requestParam ck]) -- createGroup
  (c []) -- assignCourseAdmin
  (\ck _ -> [requestParam ck]) -- assignGroupAdmin
  (\ck _ -> [requestParam ck]) -- createTestScript
  (\ck tsk _ -> [requestParam ck, requestParam tsk]) -- modifyTestScript
  (c []) -- changePassword
#ifndef SSO
  (c []) -- setUserPassword
#endif
  (\ck _ -> [requestParam ck]) -- deleteUsersFromCourse
  (\gk _ -> [requestParam gk]) -- deleteUsersFromGroup
  (\sk _ -> [requestParam sk]) -- queueSubmissionForTest
  (\ak _ -> [requestParam ak]) -- queueAllSubmissionsForTest
  (\gk _ -> [requestParam gk]) -- unsubscribeFromCourse
  (\ck _ -> [requestParam ck]) -- exportEvaluationsScoresAdminedGroups
  (\ck _ -> [requestParam ck]) -- exportEvaluationsScoresAllGroups
  (\ak _ -> [requestParam ak]) -- exportSubmissions
  (\ak u _ -> [requestParam ak, requestParam u])   -- exportSubmissionsOfGroups
  (\ak gk _ -> [requestParam ak, requestParam gk]) -- exportSubmissionsOfOneGroup
  (\sk _ -> [requestParam sk]) -- getSubmission
  (\gk uid _ -> [requestParam gk, requestParam uid]) -- getSubmissionsOfUserInGroup
  (\gk ak _ -> [requestParam gk, requestParam ak]) -- getSubmissionsOfAssignmentInGroup
  (\ck _ -> [requestParam ck]) -- getCourseCsv
  (\gk _ -> [requestParam gk]) -- getGroupCsv
  (\gk _ -> [requestParam gk]) -- newGroupAssessment
  (\ck _ -> [requestParam ck]) -- newCourseAssessment
  (\gk _ -> [requestParam gk]) -- fillNewGroupAssessmentPreview
  (\ck _ -> [requestParam ck]) -- fillNewCourseAssessmentPreview
  (\ak _ -> [requestParam ak]) -- modifyAssessment
  (\ak _ -> [requestParam ak]) -- modifyAssessmentPreview
  (\ak _ -> [requestParam ak]) -- viewAssessment
  (c []) -- notifications
  (\mk _ -> [requestParam mk]) -- viewMossScriptOutput
  (\ak _ -> [requestParam ak]) -- similarityCheckMoss
  (\gk _ -> [requestParam gk]) -- submissionTable
  (\gk _ -> [requestParam gk]) -- usersInGroup
    where
      c = const

-- Calculates the full path from a page value, including the base path and the
-- request parameters
routeOf :: Page a b c d e f -> Text
routeOf p = queryString (pageValue (pageRoutePath p)) (pageValue (pageRequestParams p))

-- Produces a query string for a GET request from the given base name, and the
-- given parameters
queryString :: Text -> [ReqParam] -> Text
queryString base []     = base
queryString base params = T.concat [base, "?",  T.intercalate "&" (map queryStringParam params)]

routeWithParams :: Page a b c d e f -> [ReqParam] -> Text
routeWithParams p rs = T.concat [routeOf p, "?", T.intercalate "&" (map queryStringParam rs)]

routeWithOptionalParams :: Page a b c d e f -> [ReqParam] -> Text
routeWithOptionalParams p rs = T.concat [routeOf p, "&",  T.intercalate "&" (map queryStringParam rs)]

routeWithAnchor :: (Anchor a) => Page a b c d e f -> a -> Text
routeWithAnchor p a = T.concat [routeOf p, "#", anchor a]

#ifdef TEST

routeOfTest =
  assertProperty
    "Non-empty RouteOr path values"
    (\p -> T.length (routeOf' p) > 0)
    pageGen
    "RouteOf strings must not be empty"
  where
    routeOf' :: PageDesc -> Text
    routeOf' = routeOf

#endif
