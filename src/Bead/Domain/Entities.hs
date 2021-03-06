{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Domain.Entities (
    AuthFailure(..)
  , Submission(..)
  , submissionCata
  , withSubmission
  , isTextSubmission
  , SubmissionValue
  , submissionValue
  , withSubmissionValue
  , submissionValueToByteString
  , MossIncompatibilityReason(..)
  , mossIncompatibilityReason
  , MossScriptInvocation(..)
  , mossScriptInvocationCata
  , outputToMossScriptInvocation
  , evaluationResultCata
  , Evaluation(..)
  , evaluationCata
  , withEvaluation
  , resultString
  , evaluationToFeedback
  , CGInfo(..)
  , cgInfoCata
  , Course(..)
  , courseCata
  , courseAppAna
  , Group(..)
  , groupCata
  , shortCourseName
  , fullGroupName
  , shortGroupName
  , Role(..)
  , roleCata
  , roles
  , groupAdmin
  , OutsideRole(..)
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
  , Email(..)
  , emailCata
  , TimeZoneName(..)
  , timeZoneName
  , showDate
  , UserRegInfo(..)
  , userRegInfoCata
  , Language(..)
  , languageCata
  , ProgrammingLanguage(..)
  , programmingLanguages
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
  , TestScriptType(..)
  , testScriptTypeCata
  , TestScript(..)
  , testScriptCata
  , withTestScript
  , testScriptAppAna
  , TestCase(..)
  , UsersFile(..)
  , usersFile
  , FileInfo(..)
  , fileInfoCata
  , withFileInfo
  , fileInfoAppAna
  , Score(..)
  , score
  , PageSettings(..)
  , defaultPageSettings
  , enableFullMarkdownRendering
  , setHttpEquiv
  , httpEquiv
  , HttpEquiv(..)
  , CompareHun(..)
  , sortHun
  , StatusMessage(..)
  , statusMessage

  , module Bead.Domain.Entity.Assessment
  , module Bead.Domain.Entity.Assignment
  , module Bead.Domain.Entity.Comment
  , module Bead.Domain.Entity.Feedback
  , module Bead.Domain.Entity.TestCase

#ifdef TEST
  , entityTests
#endif
  ) where

import           Data.Char (isSpace)
import           Data.Bifunctor (first)
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Control.Applicative
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Data
import           Data.Hashable (Hashable)
import           Data.List (findIndex, sortBy)
import           Data.Maybe (fromMaybe)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import           Data.Time (UTCTime(..), LocalTime, defaultTimeLocale)
import           Data.Time.Format (formatTime)
import           GHC.Generics (Generic)
import           System.Exit (ExitCode(ExitSuccess, ExitFailure))

import           Bead.Domain.Entity.Assessment
import           Bead.Domain.Entity.Assignment
import           Bead.Domain.Entity.Comment
import           Bead.Domain.Entity.Feedback
import           Bead.Domain.Entity.TestCase
import           Bead.Domain.Evaluation
import           Bead.View.Translation

#ifdef TEST
import           Test.Tasty.Arbitrary
import           Test.Tasty.TestSet hiding (shrink)
#endif

isTextSubmission :: SubmissionFormat a b -> Bool
isTextSubmission = submissionType True False

type SubmissionValue = SubmissionFormat Text ByteString

submissionValue :: (Text -> a) -> (ByteString -> a) -> SubmissionValue -> a
submissionValue
  text
  zipped
  v = case v of
    TextSubmission s -> text s
    ZippedSubmission z -> zipped z

withSubmissionValue v text zipped = submissionValue text zipped v

-- | Solution for one exercise
data Submission = Submission {
    solution         :: SubmissionValue
  , solutionPostDate :: UTCTime
  } deriving (Eq, Show)

-- | Template function for submission
submissionCata f (Submission sub subPostDate) = f sub subPostDate

-- | Template function for submission with flipped arguments
withSubmission s f = submissionCata f s

submissionValueToByteString :: SubmissionValue -> BL.ByteString
submissionValueToByteString = submissionValue (BL.fromStrict . TE.encodeUtf8) BL.fromStrict

-- | Indicates whether why submissions of an assignment cannot be sent to Moss.
-- At least one submission must be textual.
data MossIncompatibilityReason
  = MossNoTextSubmission -- | No, there is no textual submission.
  | MossNoSubmissions -- | No, there are no submissions for the assignment.
  | MossOnlyOneTextualSubmission -- | No, there is only one textual submission for the assignment.

mossIncompatibilityReason :: a -> a -> a -> MossIncompatibilityReason -> a
mossIncompatibilityReason noTextSubmission noSubmissions onlyOneTextualSubmission reason =
  case reason of
    MossNoTextSubmission -> noTextSubmission
    MossNoSubmissions -> noSubmissions
    MossOnlyOneTextualSubmission -> onlyOneTextualSubmission

data MossScriptInvocation
  = MossScriptInvocationSuccess {
        mossOutput :: Text
      , mossReportUrl :: Text
      }
  | MossScriptInvocationFailure {
        mossOutput :: Text
      , mossExitCode :: ExitCode
      }
  | MossScriptInvocationNotInterpretableOutput {
        mossOutput :: Text
      }
    deriving (Eq, Show)

mossScriptInvocationCata :: (Text -> Text -> a) -> (Text -> ExitCode -> a) -> (Text -> a) -> MossScriptInvocation -> a
mossScriptInvocationCata success failure notInterpretable invocation =
  case invocation of
    MossScriptInvocationSuccess output reportUrl -> success output reportUrl
    MossScriptInvocationFailure output exitCode -> failure output exitCode
    MossScriptInvocationNotInterpretableOutput output -> notInterpretable output

outputToMossScriptInvocation :: ExitCode -> String -> MossScriptInvocation
outputToMossScriptInvocation ExitSuccess scriptOutput =
  let scriptOutput' = T.pack scriptOutput
  in case findUrl scriptOutput' of
       (output, url)
         | not (T.null url) -> MossScriptInvocationSuccess output url
         | otherwise -> MossScriptInvocationNotInterpretableOutput scriptOutput'

  where
    findUrl :: Text -> (Text, Text)
    findUrl t = case T.breakOnEnd urlSignature t of
                  (pre, post)
                    | T.null pre -> ("", "")
                    | T.isSuffixOf http pre -> (T.dropEnd (T.length http) pre, urlLine http post)
                    | T.isSuffixOf https pre -> (T.dropEnd (T.length https) pre, urlLine https post)
                    | otherwise -> findUrl (T.dropEnd (T.length urlSignature) pre)

      where
        urlLine :: Text -> Text -> Text
        urlLine prefix t = T.append prefix (T.takeWhile (not . isSpace) t)

        urlSignature :: Text
        urlSignature = "://"

        http :: Text
        http = "http://"

        https :: Text
        https = "https://"

outputToMossScriptInvocation exitCode scriptOutput = MossScriptInvocationFailure (T.pack scriptOutput) exitCode

evaluationResultCata
  binary
  percentage
  freeForm
  e = case e of
    BinEval b -> binary b
    PctEval p -> percentage p
    FreeEval f -> freeForm f

-- | Evaluation of a submission
data Evaluation = Evaluation {
    evaluationResult  :: EvResult
  , writtenEvaluation :: Text
  } deriving (Eq, Read, Show)

-- | Template function for the evaluation
evaluationCata f (Evaluation result written) = f result written

-- | Template function with flipped parameter for the evaluation
withEvaluation e f = evaluationCata f e

resultString :: EvResult -> TransMsg
resultString = evResultCata
  (binaryCata (resultCata
    (TransMsg $ msg_Domain_EvalPassed "Passed")
    (TransMsg $ msg_Domain_EvalFailed "Failed")))
  (\p -> case point p of
    Nothing -> TransMsg $ msg_Domain_EvalNoResultError "No evaluation result, some internal error happened!"
    Just q  -> TransPrmMsg (msg_Domain_EvalPercentage "%s%%") (T.pack . show . round $ 100.0 * q))
  (freeForm (TransPrmMsg (msg_Domain_FreeForm "Evaluation: %s")))

evaluationToFeedback :: UTCTime -> User -> Evaluation -> Feedback
evaluationToFeedback t u e = Feedback info t where
  info = Evaluated (evaluationResult e) (writtenEvaluation e) (T.pack $ u_name u)

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
    courseName :: Text
  , courseDesc :: Text
  , courseTestScriptType :: TestScriptType
  } deriving (Eq, Show, Ord)

courseCata script course (Course name desc scriptType)
  = course name desc (script scriptType)

courseAppAna name desc test =
  Course <$> name <*> desc <*> test

-- | Groups are registered under the courses
data Group = Group {
    groupName  :: Text
  , groupDesc  :: Text
  } deriving (Eq, Show, Ord)

groupCata group (Group name desc)
  = group name desc

shortCourseName :: Course -> Text
shortCourseName = courseName

fullGroupName :: Course -> Group -> Text
fullGroupName c g = T.unwords [courseName c, "-", groupName g]

shortGroupName :: Group -> Text
shortGroupName = groupName

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
  deriving (Data, Eq, Ord, Read, Show, Typeable, Generic)

instance Hashable Username

usernameCata :: (String -> a) -> Username -> a
usernameCata f (Username u) = f u

withUsername :: Username -> (String -> a) -> a
withUsername (Username u) f = f u

newtype Email = Email String
  deriving (Eq, Ord, Show)

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

-- Programming languages

data ProgrammingLanguage = ProgrammingLanguage {
    prHumanReadable :: Text
  , prMossParameter :: String
  } deriving (Show, Data)

c :: ProgrammingLanguage
c = ProgrammingLanguage { prHumanReadable = "C" , prMossParameter = "c" }

cpp :: ProgrammingLanguage
cpp = ProgrammingLanguage { prHumanReadable = "C++" , prMossParameter = "cpp" }

java :: ProgrammingLanguage
java = ProgrammingLanguage { prHumanReadable = "Java" , prMossParameter = "java" }

ml :: ProgrammingLanguage
ml = ProgrammingLanguage { prHumanReadable = "ML" , prMossParameter = "ml" }

pascal :: ProgrammingLanguage
pascal = ProgrammingLanguage { prHumanReadable = "Pascal" , prMossParameter = "pascal" }

ada :: ProgrammingLanguage
ada = ProgrammingLanguage { prHumanReadable = "Ada" , prMossParameter = "ada" }

lisp :: ProgrammingLanguage
lisp = ProgrammingLanguage { prHumanReadable = "Lisp" , prMossParameter = "lisp" }

scheme :: ProgrammingLanguage
scheme = ProgrammingLanguage { prHumanReadable = "Scheme" , prMossParameter = "scheme" }

haskell :: ProgrammingLanguage
haskell = ProgrammingLanguage { prHumanReadable = "Haskell" , prMossParameter = "haskell" }

fortran :: ProgrammingLanguage
fortran = ProgrammingLanguage { prHumanReadable = "Fortran" , prMossParameter = "fortran" }

vhdl :: ProgrammingLanguage
vhdl = ProgrammingLanguage { prHumanReadable = "VHDL" , prMossParameter = "vhdl" }

perl :: ProgrammingLanguage
perl = ProgrammingLanguage { prHumanReadable = "Perl" , prMossParameter = "perl" }

matlab :: ProgrammingLanguage
matlab = ProgrammingLanguage { prHumanReadable = "Matlab" , prMossParameter = "matlab" }

python :: ProgrammingLanguage
python = ProgrammingLanguage { prHumanReadable = "Python" , prMossParameter = "python" }

mips_assembly :: ProgrammingLanguage
mips_assembly = ProgrammingLanguage { prHumanReadable = "MIPS Assembly" , prMossParameter = "mips" }

prolog :: ProgrammingLanguage
prolog = ProgrammingLanguage { prHumanReadable = "Prolog" , prMossParameter = "prolog" }

spice :: ProgrammingLanguage
spice = ProgrammingLanguage { prHumanReadable = "Spice" , prMossParameter = "spice" }

visualBasic :: ProgrammingLanguage
visualBasic = ProgrammingLanguage { prHumanReadable = "Visual Basic" , prMossParameter = "vb" }

csharp :: ProgrammingLanguage
csharp = ProgrammingLanguage { prHumanReadable = "C#" , prMossParameter = "csharp" }

modula2 :: ProgrammingLanguage
modula2 = ProgrammingLanguage { prHumanReadable = "Modula2" , prMossParameter = "modula2" }

a8086_assembly :: ProgrammingLanguage
a8086_assembly = ProgrammingLanguage { prHumanReadable = "A8086 Assembly" , prMossParameter = "a8086" }

javascript :: ProgrammingLanguage
javascript = ProgrammingLanguage { prHumanReadable = "Javascript" , prMossParameter = "javascript" }

plsql :: ProgrammingLanguage
plsql = ProgrammingLanguage { prHumanReadable = "PL/SQL" , prMossParameter = "plsql" }

verilog :: ProgrammingLanguage
verilog = ProgrammingLanguage { prHumanReadable = "Verilog" , prMossParameter = "verilog" }

-- Languages supported by Moss
programmingLanguages :: [ProgrammingLanguage]
programmingLanguages =
  [ c, cpp, java, ml, pascal, ada, lisp, scheme
  , haskell, fortran, vhdl, perl, matlab, python, mips_assembly
  , prolog, spice, visualBasic, csharp, modula2, a8086_assembly
  , javascript, plsql, verilog
  ]

-- User ID is unique identifier for the user, which
-- can be different than the username
newtype Uid = Uid String
  deriving (Data, Eq, Ord, Read, Show, Typeable)

uid f (Uid x) = f x

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
  compare = compareHun

mkUserDescription :: User -> UserDesc
mkUserDescription u = UserDesc {
    ud_username = u_username u
  , ud_fullname = u_name u
  , ud_uid      = u_uid u
  }

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

-- Test Script Type represents a choice: The test cases for the
-- test script will be uploaded as plain text or a zip file
data TestScriptType
  = TestScriptSimple
  | TestScriptZipped
  deriving (Eq, Ord, Enum, Show, Read, Data, Typeable)

-- Template function for the TestScriptType
testScriptTypeCata
  simple
  zipped
  t = case t of
    TestScriptSimple -> simple
    TestScriptZipped -> zipped

#ifdef TEST
instance Arbitrary TestScriptType where
  arbitrary = elements [TestScriptSimple, TestScriptZipped]
  shrink = testScriptTypeCata
    [TestScriptZipped]
    []
#endif

-- Test Script defines a scripts that can be integrated with the
-- testing framework for the given course.
data TestScript = TestScript {
    tsName :: Text -- The name of the script
  , tsDescription :: Text -- The short description of the script
  , tsNotes :: Text -- The notes for the creator of the test cases, which are associated with the script
  , tsScript :: Text -- The script itself that will be subsctituated to the test frameworks shell script
  , tsType :: TestScriptType -- The type of the test script
  } deriving (Eq, Show, Read)

-- Template function for the TestScript
testScriptCata
  tc -- Transformation of the test script type
  f
  (TestScript
    name
    description
    notes
    script
    type_)
  = f name description notes script (tc type_)

-- Template function for the TestScript with flipped parameters
withTestScript t tc f = testScriptCata tc f t

-- Applicative functor based TestScript value creation
testScriptAppAna name desc notes script type_
  = TestScript <$> name <*> desc <*> notes <*> script <*> type_

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

-- Hungarian related charecter comparing, for accented characters.
-- Uses a given list, otherwise regular compare is applied.
class CompareHun c where
  compareHun :: c -> c -> Ordering

instance CompareHun Char where
  compareHun c c' = fromMaybe (compare c c') (compare <$> idx c <*> idx c')
    where
      idx        x = idxSmall x <|> idxCapital x
      idxSmall   x = findIndex (x==) hunSmall
      idxCapital x = findIndex (x==) hunCapital
      hunSmall     = "aábcdeéfghiíjklmnoóöőpqrstuúüűvwxyz"
      hunCapital   = "AÁBCDEÉFGHIÍJKLMNOÓÖŐPQRSTUÚÜŰVWXYZ"

instance CompareHun c => CompareHun [c] where
  compareHun [] []    = EQ
  compareHun [] (_:_) = LT
  compareHun (_:_) [] = GT
  compareHun (x:xs) (y:ys)
    = case compareHun x y of
        EQ -> compareHun xs ys
        other -> other

instance CompareHun Username where
  compareHun (Username u) (Username u') = compareHun u u'

instance CompareHun UserDesc where
  compareHun (UserDesc username fullname _uid) (UserDesc username' fullname' _uid') =
    case compareHun fullname fullname' of
      EQ -> compareHun username username'
      other -> other

instance CompareHun Text where
  compareHun t1 t2 =
    case (T.uncons t1, T.uncons t2) of
      (Nothing, Nothing) -> EQ
      (Nothing, _) -> LT
      (_, Nothing) -> GT
      (Just (c1, t1'), Just (c2, t2')) ->
        case compareHun c1 c2 of
          EQ -> compareHun t1' t2'
          result -> result

sortHun :: [String] -> [String]
sortHun = sortBy compareHun

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

data HttpEquiv
  = Reload Int -- ^ Reload current page after specified seconds

httpEquivCata :: (Int -> a)  -> HttpEquiv -> a
httpEquivCata reload h =
  case h of
    Reload n -> reload n

httpEquiv :: PageSettings -> H.Html
httpEquiv =
  maybe
    mempty
    (\equiv -> H.meta ! httpEquivCata
      (\seconds -> A.httpEquiv "refresh" <> A.content (H.toValue . T.pack . show $ seconds))
      equiv)
  . enableHttpEquiv

-- | PageSettings controls how a HTML page is rendered.
data PageSettings = PageSettings { needsLatex :: Bool
                                 , needsSyntaxHighlight :: Bool
                                 , enableHttpEquiv :: Maybe HttpEquiv
                                 }

defaultPageSettings :: PageSettings
defaultPageSettings = PageSettings { needsLatex = False
                                   , needsSyntaxHighlight = False
                                   , enableHttpEquiv = Nothing
                                   }

enableFullMarkdownRendering :: PageSettings -> PageSettings
enableFullMarkdownRendering s = s { needsLatex = True
                                  , needsSyntaxHighlight = True
                                  }

setHttpEquiv :: HttpEquiv -> PageSettings -> PageSettings
setHttpEquiv h s = s { enableHttpEquiv = Just h }

#ifdef TEST

entityTests = do
  compareHunTests

compareHunTests = group "compareHun" $ eqPartitions compareHun'
  [ Partition "Small normal letters a-a" ('a', 'a') EQ ""
  , Partition "Small normal letters d-z" ('d', 'z') LT ""
  , Partition "Small normal letters z-a" ('z', 'a') GT ""
  , Partition "Capital normal letters A-A" ('A', 'A') EQ ""
  , Partition "Capital normal letters D-Z" ('D', 'Z') LT ""
  , Partition "Capital normal letters Z-A" ('Z', 'A') GT ""
  , Partition "Small accented letters á-á" ('á', 'á') EQ ""
  , Partition "Small accented letters é-ú" ('é', 'ú') LT ""
  , Partition "Small accented letters ű-á" ('ű', 'á') GT ""
  , Partition "Capital accented letters Á-Á" ('á', 'á') EQ ""
  , Partition "Capital accented letters É-Ú" ('É', 'Ú') LT ""
  , Partition "Capital accented letters Ű-Á" ('Ű', 'Á') GT ""
  ] where compareHun' = uncurry compareHun

#endif
