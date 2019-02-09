{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Bead.Domain.Entities (
    AuthFailure(..)
  , Submission(..)
  , submissionCata
  , withSubmission
  , SubmissionValue(..)
  , submissionValue
  , withSubmissionValue
  , CourseName
  , UsersFullname
  , evaluationResultCata
  , allBinaryEval
  , allPercentEval
  , Evaluation(..)
  , resultString
  , evaluationToFeedback
  , CourseCode(..)
  , CGInfo(..)
  , cgInfoCata
  , Course(..)
  , courseCata
  , courseAppAna
  , Group(..)
  , groupCata
  , Role(..)
  , roleCata
  , roles
  , groupAdmin
  , OutsideRole(..)
  , parseRole
  , printRole
  , atLeastCourseAdmin
  , InRole(..)
  , Permission(..)
  , canOpen
  , canCreate
  , canModify
  , canDelete
  , PermissionObject(..)
  , PermissionObj(..)
  , ObjectPermissions(..)
  , Username(..)
  , usernameCata
  , withUsername
  , AsUsername(..)
  , Password
  , AsPassword(..)
  , passwordCata
  , Email(..)
  , emailFold
  , parseEmail
  , email'
  , emailCata
  , TimeZoneName(..)
  , timeZoneName
  , showDate
  , UserRegInfo(..)
  , userRegInfoCata
  , Language(..)
  , languageCata
  , Uid(..)
  , uid
  , User(..)
  , userCata
  , withUser
  , userAna
  , PersonalInfo(..)
  , personalInfoCata
  , withPersonalInfo
  , UserDesc(..)
  , mkUserDescription
  , UserRegistration(..)
  , userRegistration
  , UsersFile(..)
  , usersFile
  , FileInfo(..)
  , fileInfoCata
  , withFileInfo
  , fileInfoAppAna
  , Score(..)
  , PageSettings(..)
  , defaultPageSettings
  , StatusMessage(..)
  , statusMessage

  , module Bead.Domain.String
  , module Bead.Domain.Entity.Assessment
  , module Bead.Domain.Entity.Assignment
  , module Bead.Domain.Entity.Comment
  , module Bead.Domain.Entity.Feedback
  , module Bead.Domain.Entity.TestCase
  , module Bead.Domain.Entity.TestScript
  , module Bead.Domain.Evaluation

#ifdef TEST
  , entityTests
#endif
  ) where

import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Data
import           Data.Hashable (Hashable)
import           Data.List (findIndex, sortBy)
import           Data.Time (UTCTime(..), LocalTime, defaultTimeLocale)
import           Data.Time.Format (formatTime)
import           GHC.Generics (Generic)

import           Bead.Domain.String (CompareHu(..), sortHu)
import           Bead.Domain.Entity.Assessment
import           Bead.Domain.Entity.Assignment
import           Bead.Domain.Entity.Comment
import           Bead.Domain.Entity.Feedback
import           Bead.Domain.Entity.TestCase
import           Bead.Domain.Entity.TestScript
import           Bead.Domain.Evaluation
import           Bead.View.Translation

#ifdef TEST
import           Test.Tasty.Arbitrary
import           Test.Tasty.TestSet hiding (shrink)
#endif

data SubmissionValue
  = SimpleSubmission String
  | ZippedSubmission ByteString
  deriving (Eq, Show)

submissionValue
  simple
  zipped
  v = case v of
    SimpleSubmission s -> simple s
    ZippedSubmission z -> zipped z

withSubmissionValue v simple zipped = submissionValue simple zipped v

-- | Solution for one exercise
data Submission = Submission {
    solution         :: SubmissionValue
  , solutionPostDate :: UTCTime
  } deriving (Eq, Show)

-- | Template function for submission
submissionCata f (Submission sub subPostDate) = f sub subPostDate

-- | Template function for submission with flipped arguments
withSubmission s f = submissionCata f s

type CourseName = String

type UsersFullname = String

evaluationResultCata
  binary
  percentage
  freeForm
  e = case e of
    BinEval b -> binary b
    PctEval p -> percentage p
    FreeEval f -> freeForm f

allBinaryEval :: [EvaluationData b p f] -> Maybe [b]
allBinaryEval = sequence . map binaryEval

allPercentEval :: [EvaluationData b p f] -> Maybe [p]
allPercentEval = sequence . map percentEval

resultString :: EvResult -> TransMsg
resultString = evResultCata
  (binaryCata (resultCata
    (TransMsg $ msg_Domain_EvalPassed "Passed")
    (TransMsg $ msg_Domain_EvalFailed "Failed")))
  (\p -> case point p of
    Nothing -> TransMsg $ msg_Domain_EvalNoResultError "No evaluation result, some internal error happened!"
    Just q  -> TransPrmMsg (msg_Domain_EvalPercentage "%s%%") (show . round $ 100.0 * q))
  (freeForm (TransPrmMsg (msg_Domain_FreeForm "Evaluation: %s")))

evaluationToFeedback :: UTCTime -> User -> Evaluation -> Feedback
evaluationToFeedback t u e = Feedback info t where
  info = Evaluated (evaluationResult e) (writtenEvaluation e) (u_name u)

newtype CourseCode = CourseCode String
  deriving (Eq, Ord, Show)

-- Course or Group info. Some information is attached to
-- course or group
data CGInfo a
  = CourseInfo a
  | GroupInfo a
  deriving (Show)

-- Template function for the course or group info value
cgInfoCata
  course
  group
  cg = case cg of
    CourseInfo x -> course x
    GroupInfo  x -> group  x

-- | A course represent a course at the university
data Course = Course {
    courseName :: String
  , courseDesc :: String
  , courseTestScriptType :: TestScriptType
  } deriving (Eq, Show, Ord)

courseCata script course (Course name desc scriptType)
  = course name desc (script scriptType)

courseAppAna name desc test =
  Course <$> name <*> desc <*> test

-- | Groups are registered under the courses
data Group = Group {
    groupName  :: String
  , groupDesc  :: String
  } deriving (Eq, Show, Ord)

groupCata group (Group name desc)
  = group name desc

-- * Authorization and authentication

data AuthFailure
  = IncorrectUserOrPassword
  | UserNotFound

-- | Login roles
data Role
  = Student
  | GroupAdmin
  | CourseAdmin
  | Admin
  deriving (Data, Enum, Eq, Ord, Show, Typeable)

roleCata
  student
  groupAdmin
  courseAdmin
  admin
  r = case r of
    Student     -> student
    GroupAdmin  -> groupAdmin
    CourseAdmin -> courseAdmin
    Admin       -> admin

#ifdef TEST
instance Arbitrary Role where
  arbitrary = elements roles
  shrink = roleCata
    [GroupAdmin, CourseAdmin, Admin]
    [CourseAdmin, Admin]
    [Admin]
    []
#endif

roles = [Student, GroupAdmin, CourseAdmin, Admin]

-- Decides if the given role can admninstrate groups
-- Returns True if yes, otherwise False
groupAdmin = roleCata
  False
  True
  True
  False

data OutsideRole
  = EmptyRole
  | RegRole
  | TestAgentRole
  deriving (Eq, Ord)

parseRole :: String -> Maybe Role
parseRole "Student"      = Just Student
parseRole "Group Admin"  = Just GroupAdmin
parseRole "Course Admin" = Just CourseAdmin
parseRole "Admin"        = Just Admin
parseRole _              = Nothing

printRole = roleCata
  "Student"
  "Group Admin"
  "Course Admin"
  "Admin"

atLeastCourseAdmin Admin       = True
atLeastCourseAdmin CourseAdmin = True
atLeastCourseAdmin _           = False

class InRole r where
  isAdmin       :: r -> Bool
  isCourseAdmin :: r -> Bool
  isGroupAdmin  :: r -> Bool
  isStudent     :: r -> Bool

instance InRole Role where
  isAdmin       = (== Admin)
  isCourseAdmin = (>= CourseAdmin)
  isGroupAdmin  = (>= GroupAdmin)
  isStudent     = (== Student)

-- * Permissions

-- | Granted permission on a given operation
data Permission
  = P_Open
  | P_Create
  | P_Modify
  | P_Delete
  deriving (Show, Eq, Enum)

canOpen, canCreate, canModify, canDelete :: Permission -> Bool

canOpen   = flip elem [P_Open, P_Create, P_Modify, P_Delete]
canCreate = flip elem [P_Create, P_Modify, P_Delete]
canModify = flip elem [P_Modify, P_Delete]
canDelete = flip elem [P_Delete]

-- | Permissions are allowed on the following objects
data PermissionObject
  = P_Assignment
  | P_Assessment
  | P_UserReg
  | P_Submission
  | P_Evaluation
  | P_Comment
  | P_Feedback
  | P_Statistics
  | P_Password
  | P_GroupAdmin
  | P_User
  | P_Course
  | P_Group
  | P_CourseAdmin
  | P_AdminPage
  | P_PlainPage
  | P_TestScript
  | P_File
  | P_TestIncoming
  | P_TestCase
  | P_StudentPassword
  deriving (Eq, Ord, Show, Enum)

-- Permission Objects are dynamically associated with values
class PermissionObj p where
  permissionObject :: p -> PermissionObject

newtype ObjectPermissions = ObjectPermissions { permissions :: [(Permission, PermissionObject)] }

newtype Username = Username String
  deriving (Data, Eq, Ord, Read, Show, Typeable)

usernameCata :: (String -> a) -> Username -> a
usernameCata f (Username u) = f u

withUsername :: Username -> (String -> a) -> a
withUsername (Username u) f = f u

class AsUsername c where
  asUsername :: c -> Username

type Password = String

class AsPassword p where
  asPassword :: p -> Password

passwordCata :: (String -> a) -> Password -> a
passwordCata f p = f p

newtype Email = Email String
  deriving (Eq, Ord, Read)

emailFold :: (String -> a) -> Email -> a
emailFold f (Email e) = f e

parseEmail :: String -> Maybe Email
parseEmail = Just . Email

instance Show Email where
  show (Email e) = e

-- TODO: throw exception if email string is unacceptable
email' :: String -> Email
email' = Email

emailCata :: (String -> a) -> Email -> a
emailCata f (Email e) = f e

-- | Represents a name of a time zone based on the
-- location for the given time zone.
-- E.g: ZoneInfo "Europe/Budapest"
newtype TimeZoneName = TimeZoneName { unTzn :: String }
  deriving (Data, Eq, Ord, Read, Show, Typeable)

timeZoneName f (TimeZoneName z) = f z

#ifdef TEST
instance Arbitrary TimeZoneName where
  arbitrary = TimeZoneName <$> arbitrary
  shrink = fmap TimeZoneName . timeZoneName shrink
#endif

showDate :: LocalTime -> String
showDate = formatTime defaultTimeLocale "%F, %T"

-- UserRegInfo is a User Registration Info that consists of
-- a Username, a User ID, a Password, an Email Address, a Full Name, and a time zone
newtype UserRegInfo = UserRegInfo (String, String, String, String, String, TimeZoneName)

userRegInfoCata f (UserRegInfo (username, uid, password, email, fullName, timeZoneName))
  = f username uid password email fullName timeZoneName

-- The language what the dictionary represents.
newtype Language = Language String
  deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

languageCata f (Language l) = f l

instance Hashable Language

-- | Logged in user
data User = User {
    u_role     :: Role
  , u_username :: Username
  , u_email    :: Email
  , u_name     :: String
  , u_timezone :: TimeZoneName
  , u_language :: Language
  , u_uid      :: Uid
  } deriving (Eq, Ord, Show)

userCata f (User role username email name timeZone language uid) =
  f role username email name timeZone language uid

withUser = flip userCata

userAna role username email name timeZone language = User
  <$> role
  <*> username
  <*> email
  <*> name
  <*> timeZone
  <*> language

newtype PersonalInfo = PersonalInfo (Role, String, TimeZoneName, Language, Uid)

personalInfoCata :: (Role -> String -> TimeZoneName -> Language -> Uid -> a)
                 -> PersonalInfo
                 -> a
personalInfoCata f (PersonalInfo (role, name, timeZone, language, uid))
  = f role name timeZone language uid

withPersonalInfo :: PersonalInfo
                 -> (Role -> String -> TimeZoneName -> Language -> Uid -> a)
                 -> a
withPersonalInfo p f = personalInfoCata f p

data UserDesc = UserDesc {
    ud_username :: Username
  , ud_fullname :: String
  , ud_uid      :: Uid
  } deriving Eq

instance Ord UserDesc where
  compare = compareHu

mkUserDescription :: User -> UserDesc
mkUserDescription u = UserDesc {
    ud_username = u_username u
  , ud_fullname = u_name u
  , ud_uid      = u_uid u
  }

-- User ID is unique identifier for the user, which
-- can be different than the username
newtype Uid = Uid String
  deriving (Data, Eq, Ord, Read, Show, Typeable)

uid f (Uid x) = f x

-- | User awaiting for registration
data UserRegistration = UserRegistration {
    reg_username :: String
  , reg_email    :: String
  , reg_name     :: String -- User's full name
  , reg_token    :: String -- Token for identification
  , reg_timeout  :: UTCTime
  } deriving (Eq, Show, Read)

-- | Template function for the UserRegistration
userRegistration f (UserRegistration username email name token timeout) =
  f username email name token timeout

-- A file that a user can upload. Usually it is either a path or file contents.
data UsersFile a
  = UsersPublicFile  a
  | UsersPrivateFile a
  deriving (Data, Eq, Ord, Read, Show, Typeable)

-- Template function for User's file
usersFile
  public
  private
  f = case f of
    UsersPublicFile  x -> public x
    UsersPrivateFile x -> private x

-- File information that will be displayed on the UI
data FileInfo = FileInfo {
    fiSize :: Int     -- The size of the file in bytes
  , fiDate :: UTCTime -- The last modifcation date of the file
  }

-- Template function for the FileInfo value
fileInfoCata f (FileInfo size date) = f size date

-- Template function for the FileInfo value
withFileInfo (FileInfo size date) f = f size date

-- Applicative functor based FileInfo construction
fileInfoAppAna size date = FileInfo <$> size <*> date

data Score = Score ()
  deriving (Data, Eq, Ord, Read, Show, Typeable)

#ifdef TEST
instance Arbitrary Score where
  arbitrary = return (Score ())
  shrink _ = []
#endif

-- * PermObjs instance

instance PermissionObj Course where
  permissionObject _ = P_Course

instance PermissionObj Assignment where
  permissionObject _ = P_Assignment

instance PermissionObj UserRegistration where
  permissionObject _ = P_UserReg


-- * Ordering

instance CompareHu Username where
  compareHu (Username u) (Username u') = compareHu u u'

instance CompareHu UserDesc where
  compareHu (UserDesc username fullname _uid) (UserDesc username' fullname' _uid') =
    case compareHu fullname fullname' of
      EQ -> compareHu username username'
      other -> other

-- Status message is shown for the user on the UI
data StatusMessage a
  = SmNormal a -- Normal message
  | SmError a  -- Some several error happened, the user needs to be informed about.
#ifdef TEST
  deriving (Eq, Show)
#else
  deriving Eq
#endif

statusMessage
  normal
  err
  sm
  = case sm of
    SmNormal x -> normal x
    SmError x -> err x

-- | PageSettings controls how a HTML page is rendered.
newtype PageSettings = PageSettings { needsLatex :: Bool }

defaultPageSettings :: PageSettings
defaultPageSettings = PageSettings { needsLatex = False }

#ifdef TEST

entityTests = do
  roleTest

roleTest =
  assertProperty
    "parse and print role are inverse functions"
    (\r -> ((Just r) ==) . parseRole . printRole $ r)
    enumGen
    "printRole roles must generate string parseable by parseRole"

#endif
