module Bead.Domain.Relationships where

-- Bead imports

import Bead.Domain.Types
import Bead.Domain.Entities
import Bead.Domain.Evaluation

-- Haskell imports

import Data.Either (either)
import Data.Function (on)
import Data.Time (UTCTime(..))
import Data.Map (Map)
import Data.List as List

-- * Relations

type RolePermissions = [(Role,[(Permission, PermissionObject)])]

data AssignmentDesc = AssignmentDesc {
    aActive   :: Bool
  , aTitle    :: String
  , aGroup    :: String
  , aTeachers :: [String]
  -- DeadLine for the assignment in UTC
  , aEndDate  :: UTCTime
  }

assignmentDescPermissions = ObjectPermissions [
    (P_Open, P_Assignment), (P_Open, P_Course)
  , (P_Open, P_Course)
  ]

data GroupDesc = GroupDesc {
    gName   :: String
  , gAdmins :: [String]
  } deriving (Show)

groupDescFold :: (String -> [String] -> a) -> GroupDesc -> a
groupDescFold f (GroupDesc n a) = f n a

groupDescPermissions = ObjectPermissions [
    (P_Open, P_Group)
  ]

data SubmissionDesc = SubmissionDesc {
    eGroup    :: String
  , eStudent  :: String
  , eSolution :: String
  , eConfig   :: EvaluationConfig
  , eAssignmentKey   :: AssignmentKey
  , eAssignmentTitle :: String
  , eAssignmentDesc  :: String
  , eComments :: [Comment]
  }

submissionDescPermissions = ObjectPermissions [
    (P_Open, P_Group), (P_Open, P_Course)
  , (P_Open, P_Submission), (P_Open, P_Assignment)
  , (P_Open, P_Comment)
  ]

type Status = Maybe String
type EvaluatedBy = String

-- List of the submissions made by a student for a given assignment
type UserSubmissionInfo = [(SubmissionKey, UTCTime, Status, EvaluatedBy)]

userSubmissionInfoCata
  :: ([a] -> b)
  -> ((SubmissionKey, UTCTime, Status, EvaluatedBy) -> a)
  -> UserSubmissionInfo
  -> b
userSubmissionInfoCata list info us = list $ map info us

-- List of the submission times made by a student for a given assignment
type UserSubmissionTimes = [UTCTime]

userSubmissionTimesCata
  :: ([a] -> b)
  -> (UTCTime -> a)
  -> UserSubmissionTimes
  -> b
userSubmissionTimesCata list time s = list $ map time s

data SubmissionListDesc = SubmissionListDesc {
    slGroup   :: String
  , slTeacher :: [String]
  , slSubmissions :: Either UserSubmissionTimes UserSubmissionInfo
  , slAssignment :: Assignment
  }

-- Sorts the given submission list description into descending order, by
-- the times of the given submissions
sortSbmListDescendingByTime :: SubmissionListDesc -> SubmissionListDesc
sortSbmListDescendingByTime s = s { slSubmissions = slSubmissions' }
  where
    userSubmissionTime (_submissionKey,time,_status,_evalatedBy) = time
    sortSubmissionTime = reverse . List.sort
    sortUserSubmissionInfo = reverse . List.sortBy (compare `on` userSubmissionTime)
    slSubmissions' = either (Left . sortSubmissionTime)
                            (Right . sortUserSubmissionInfo)
                            (slSubmissions s)

submissionListDescPermissions = ObjectPermissions [
    (P_Open, P_Group), (P_Open, P_Course)
  , (P_Open, P_Submission), (P_Open, P_Assignment)
  ]

data SubmissionDetailsDesc = SubmissionDetailsDesc {
    sdGroup :: String
  , sdTeacher :: [String]
  , sdAssignment :: Assignment
  , sdStatus :: Maybe String
  , sdSubmission :: String
  , sdComments :: [Comment]
  }

submissionDetailsDescPermissions = ObjectPermissions [
    (P_Open, P_Group), (P_Open, P_Course)
  , (P_Open, P_Assignment), (P_Open, P_Submission)
  , (P_Open, P_Comment)
  ]

data SubmissionInfo
  = Submission_Not_Found
  | Submission_Unevaluated
  | Submission_Result EvaluationKey EvaluationResult
  deriving (Show)

submissionInfoCata notFound unevaluated result s = case s of
  Submission_Not_Found   -> notFound
  Submission_Unevaluated -> unevaluated
  Submission_Result k r  -> result k r

siEvaluationKey :: SubmissionInfo -> Maybe EvaluationKey
siEvaluationKey Submission_Not_Found     = Nothing
siEvaluationKey Submission_Unevaluated   = Nothing
siEvaluationKey (Submission_Result ek _) = Just ek

-- Simple name for the assignment
type AssignmentName = String

-- Information to display on the UI
data TestScriptInfo = TestScriptInfo {
    tsiName :: String
  , tsiDescription :: String
  , tsiType :: TestScriptType
  }

-- InformationSoruce represents from where the information came from, from a course or from a group
data InfoSource
  = CourseInfSrc -- Represent that the generated information came from a course
  | GroupInfSrc  -- Represent that the generated information came from a group
  | GroupAdminCourseInfSrc -- Represents that the information came from a course, but requested by the course admin
  deriving (Show)

-- Template function for Information Source
infoSourceCata
  courseInfSrc
  groupInfSrc
  groupAdminCourseInfSrc
  i = case i of
    CourseInfSrc -> courseInfSrc
    GroupInfSrc  -> groupInfSrc
    GroupAdminCourseInfSrc -> groupAdminCourseInfSrc

data SubmissionTableInfo = SubmissionTableInfo {
    stCourse :: String
  , stOrigin :: InfoSource -- True indicates that the table is generated for a course, False for a group
  , stNumberOfAssignments :: Int
  , stEvalConfig  :: EvaluationConfig
  , stAssignments :: [AssignmentKey] -- Cronologically ordered list of assignments
  , stUsers       :: [Username]      -- Alphabetically ordered list of usernames
  , stUserLines   :: [(UserDesc, Maybe Result, [(AssignmentKey, SubmissionInfo)])]
  , stAssignmentNames :: Map AssignmentKey AssignmentName
  , stKey :: Either GroupKey CourseKey
  } deriving (Show)

submissionTableInfoCata
  course
  origin
  number
  config
  assignment
  assignments
  user
  users
  userline
  userlines
  assignmentNames
  key
  tableInfo
  t =
    tableInfo
      (course $ stCourse t)
      (origin $ stOrigin t)
      (number $ stNumberOfAssignments t)
      (config $ stEvalConfig t)
      (assignments . map assignment $ stAssignments t)
      (users . map user $ stUsers t)
      (userlines . map userline $ stUserLines t)
      (assignmentNames $ stAssignmentNames t)
      (key $ stKey t)

submissionTableInfoPermissions = ObjectPermissions [
    (P_Open, P_Course), (P_Open, P_Assignment)
  ]

-- TODO
checkSubmissionTableInfo :: SubmissionTableInfo -> Bool
checkSubmissionTableInfo _ = True

data UserSubmissionDesc = UserSubmissionDesc {
    usCourse         :: String
  , usAssignmentName :: String
  , usStudent        :: String
  , usSubmissions :: [(SubmissionKey, UTCTime, SubmissionInfo)]
  } deriving (Show)

userSubmissionDescPermissions = ObjectPermissions [
    (P_Open, P_Course), (P_Open, P_Assignment), (P_Open, P_Submission)
  ]

-- * Entity keys

newtype AssignmentKey = AssignmentKey String
  deriving (Eq, Ord, Show)

assignmentKeyMap :: (String -> a) -> AssignmentKey -> a
assignmentKeyMap f (AssignmentKey x) = f x

newtype UserKey = UserKey String
  deriving (Eq, Ord, Show)

newtype UserRegKey = UserRegKey String
  deriving (Eq, Ord, Show)

userRegKeyFold :: (String -> a) -> UserRegKey -> a
userRegKeyFold f (UserRegKey x) = f x

instance Str UserRegKey where
  str = userRegKeyFold id

newtype CommentKey = CommentKey String
  deriving (Eq, Ord, Show)

newtype SubmissionKey = SubmissionKey String
  deriving (Eq, Ord, Show)

submissionKeyMap :: (String -> a) -> SubmissionKey -> a
submissionKeyMap f (SubmissionKey s) = f s

-- Key for a given Test Script in the persistence layer
newtype TestScriptKey = TestScriptKey String
  deriving (Eq, Ord, Show)

-- Template function for the TestScriptKey value
testScriptKeyCata f (TestScriptKey x) = f x

-- Key for a given Test Case in the persistence layer
newtype TestCaseKey = TestCaseKey String
  deriving (Eq, Ord, Show)

-- Template function for the TestCaseKey value
testCaseKeyCata f (TestCaseKey x) = f x

newtype CourseKey = CourseKey String
  deriving (Eq, Ord, Show)

courseKeyMap :: (String -> a) -> CourseKey -> a
courseKeyMap f (CourseKey g) = f g

newtype GroupKey = GroupKey String
  deriving (Eq, Ord, Show)

groupKeyMap :: (String -> a) -> GroupKey -> a
groupKeyMap f (GroupKey g) = f g

newtype EvaluationKey = EvaluationKey String
  deriving (Eq, Ord, Show)

evaluationKeyMap :: (String -> a) -> EvaluationKey -> a
evaluationKeyMap f (EvaluationKey e) = f e

-- * Str instances

instance Str AssignmentKey where
  str (AssignmentKey s) = s

instance Str CourseKey where
  str (CourseKey c) = c

instance Str GroupKey where
  str (GroupKey g) = g
