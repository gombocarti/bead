{-# LANGUAGE Rank2Types #-}
module Test.Property.Persistence (
    admins
  , courses
  , courseAndGroupAssignments
  , courseAssignmentGen
  , createInterpreter
  , createTestData
  , groupAssignmentGen
  , groups
  , reinitPersistence
  , runPersistIOCmd
  , submissions
  , subscribeUsers
  , testCases
  , testScripts
  , tests
  , users
  ) where

import Control.Arrow ((&&&))
import Control.Applicative ((<$>), (<*>))
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent (forkIO)
import qualified Data.ByteString.Char8 as BS
import Data.Function (on)
import qualified Data.Set as Set
import qualified Data.HashSet as HashSet

import Data.List ((\\), intersperse, nub, find, sort, sortOn, maximumBy, delete, partition)
import qualified Data.Map as Map
import Data.Maybe
import Data.IORef
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time
import Data.Tuple.Utils (fst3, thd3)
import System.Directory hiding (copyFile)
import System.IO
import System.IO.Temp (createTempDirectory)
import System.FilePath ((</>))

import Bead.Persistence.Initialization
import Bead.Persistence.Persist hiding (groups, runPersistIOCmd)
import qualified Bead.Persistence.Persist as Persist
import Bead.Persistence.Relations
import Bead.Persistence.SQL.FileSystem (testOutgoing)

import qualified Test.Property.EntityGen as Gen
import Test.Property.Common (quick, check, success)

import Bead.Domain.Entities
import qualified Bead.Domain.Entity.Assignment as Assignment
import qualified Bead.Domain.Entity.Assessment as Assessment
import Bead.Domain.Entity.Comment
import Bead.Domain.Relationships
import Bead.Domain.Shared.Evaluation
import Test.QuickCheck as QuickCheck
import Test.QuickCheck.Monadic
import Test.Tasty
import Test.Tasty.HUnit (testCase)
import Test.Tasty.TestSet (add, group, test, ioTest, TestSet)
import Test.Tasty.QuickCheck (testProperty)

import System.IO.Unsafe

{- Mass test of the persistence layer -}

-- Properties

-- Load and save property. The save and a load of
-- the given data should be the same.
saveAndLoadIdenpotent :: (Eq v, Show v) => String -> (v -> Persist k) -> (k -> Persist v) -> Gen v -> PropertyM IO k
saveAndLoadIdenpotent name save load gen = do
  v <- pick gen
  key <- runPersistCmd $ save v
  v'  <- runPersistCmd $ load key
  assertEquals v v' (name ++ ": Save and load is not idenpotent")
  return key

-- Modification property. The saved, modificated and a load
-- of the given data should be the same
modification :: (Eq v, Show v) => String -> (v -> Persist k) -> (k -> v -> Persist ()) -> (k -> Persist v) -> Gen v -> PropertyM IO k
modification name save modify load gen = do
  k <- saveAndLoadIdenpotent name save load gen
  v <- pick gen
  runPersistCmd $ modify k v
  v' <- runPersistCmd $ load k
  assertEquals v v' (name ++ ": Modifed and load was different")
  return k

mossScriptInvocationSaveAndLoad :: AssignmentKey -> PropertyM IO MossScriptInvocationKey
mossScriptInvocationSaveAndLoad ak = do
  saveAndLoadIdenpotent
    "MossScriptInvocation"
    (\msi -> do
        key <- newMossScriptInvocationKey ak
        saveMossScriptInvocation key msi
        return key)
    (\key -> do
        result <- loadMossScriptInvocation key
        case result of
          (Just msi, _) -> return msi
          (Nothing, _) -> fail $ "Moss script invocation not found: " ++ show key)
    Gen.mossScriptInvocations

assignmentSaveAndLoad = saveAndLoadIdenpotent
  "Assignment"
  (saveAssignment)
  (loadAssignment)
  (Gen.assignments startDate endDate)

courseSaveAndLoad = saveAndLoadIdenpotent
  "Course"
  (saveCourse)
  (loadCourse)
  Gen.courses

groupSaveAndLoad = do
  ck <- saveAndLoadIdenpotent "Course" (saveCourse)  (loadCourse) Gen.courses
  gk <- saveAndLoadIdenpotent "Group"  (saveGroup ck) (loadGroup) Gen.groups
  gks <- runPersistCmd $ groupKeysOfCourse ck
  assertEquals [gk] gks "Group keys were different"
  ck' <- runPersistCmd $ courseOfGroup gk
  assertEquals ck ck' "Course keys were different"

courseAssignmentSaveAndLoad = do
  key <- saveAndLoadIdenpotent "Courses" (saveCourse) (loadCourse) Gen.courses
  saveAndLoadIdenpotent
    "Assignment"
    (saveCourseAssignment key)
    (loadAssignment)
    (Gen.assignments startDate endDate)

groupAssignmentSaveAndLoad = do
  key <- saveAndLoadIdenpotent "Courses" (saveCourse) (loadCourse) Gen.courses
  key1 <- saveAndLoadIdenpotent "Groups" (saveGroup key) (loadGroup) Gen.groups
  saveAndLoadIdenpotent
    "Assignment"
    (saveGroupAssignment key1)
    (loadAssignment)
    (Gen.assignments startDate endDate)

userSaveAndLoad u = do
  saveAndLoadIdenpotent "User"
    (\usr -> saveUser usr) (const (loadUser (u_username u))) (return u)

createOrLoadUser u = do
  exist <- runPersistCmd $ doesUserExist (u_username u)
  case exist of
    True  -> return ()
    False -> userSaveAndLoad u
  return u

multipleGroupsForCourse = do
  ck <- saveAndLoadIdenpotent "Course" (saveCourse) (loadCourse)  Gen.courses
  gk1 <- saveAndLoadIdenpotent "Group" (saveGroup ck) (loadGroup) Gen.groups
  gk2 <- saveAndLoadIdenpotent "Group" (saveGroup ck) (loadGroup) Gen.groups
  gks <- runPersistCmd $ groupKeysOfCourse ck
  assertEquals (Set.fromList gks) (Set.fromList [gk1,gk2]) "Groups key set were different"

-- Tries to save and load a submission for a given user and assignment
saveAndLoadSubmissionFor u ak =
  saveAndLoadIdenpotent "Submission"
    (saveSubmission ak u) (loadSubmission) (Gen.submissions startDate)

saveAndLoadSubmission = do
  ak <- groupAssignmentSaveAndLoad
  u <- pick Gen.users
  createOrLoadUser u
  sk <- saveAndLoadIdenpotent "Submission"
          (saveSubmission ak (u_username u)) (loadSubmission) (Gen.submissions startDate)
  return (ak,u,sk)



assignmentAndUserOfSubmission = do
  (ak, u, sk) <- saveAndLoadSubmission
  ak' <- runPersistCmd $ assignmentOfSubmission sk
  assertEquals ak ak' "Assertion keys were different"
  un <- runPersistCmd $ usernameOfSubmission sk
  assertEquals (u_username u) un "Username were different"

saveAndLoadComment = do
  (ak, u, sk) <- saveAndLoadSubmission
  ck <- saveAndLoadIdenpotent "Comment" (saveComment sk) (loadComment) (Gen.comments startDate)
  sk' <- runPersistCmd $ submissionOfComment ck
  assertEquals sk sk' "Submission keys were different"

evaluationConfigForSubmission sk = do
  ak <- runPersistCmd $ assignmentOfSubmission sk
  Assignment.evType <$> (runPersistCmd $ loadAssignment ak)

evaluationConfigForAssessement ak = runPersistCmd $ do
  a <- loadAssessment ak
  return $ evaluationCfg a

evaluationGroupSaveAndLoad = do
  (ak, u, sk) <- saveAndLoadSubmission
  cfg <- evaluationConfigForSubmission sk
  saveAndLoadIdenpotent
    "Evaluation" (saveSubmissionEvaluation sk) (loadEvaluation) (Gen.evaluations cfg)

massTest = testCase "Mass Test" massPersistenceTest

massTestParallel = testCase "Mass Test Parallel" $ do
  forkIO massPersistenceTest
  massPersistenceTest

-- ListRef is an IO reference to an arbitrary list
-- Interpretation: informational channel between different
-- part of test cases in the IO monad.
type ListRef a = IORef [a]

-- Creates a list reference with an empty list in it
createListRef :: IO (ListRef a)
createListRef = newIORef []

-- Inserts an element to the given list hold by the list reference
insertListRef :: ListRef a -> a -> IO ()
insertListRef r e = modifyIORef r (e:)

modifyListRef :: ListRef a -> ([a] -> [a]) -> IO ()
modifyListRef = modifyIORef

popListRef :: ListRef a -> IO a
popListRef r = do
  a:as <- readIORef r
  writeIORef r as
  return a

-- Returns the list hold in the list reference
listInRef :: ListRef a -> IO [a]
listInRef r = readIORef r

-- Creates the given number of files in the upload template directory
uploadTempFiles tmpDir n = do
  list <- createListRef
  quick n $ do
    (fp, handle) <- run $ openTempFile tmpDir "tmp.txt"
    words <- pick . listOf . listOf1 $ elements ['a' .. 'z']
    run $ do
      hPutStr handle $ concat $ intersperse " " words
      hClose handle
      insertListRef list fp
  listInRef list

-- Generate and store the given number of courses and returns the
-- course keys stored in the
courses :: Int -> IO [CourseKey]
courses n = do
  list <- createListRef
  quick n $ do
    ck <- courseSaveAndLoad
    run $ insertListRef list ck
  listInRef list

-- Generate and store the given number of groups and assigns them to random courses,
-- from the given list, returns all the created group keys
groups :: Int -> [CourseKey] -> IO [GroupKey]
groups n cs = do
  list <- createListRef
  quick n $ do
    ck <- pick $ elements cs
    gk <- saveAndLoadIdenpotent "Group" (saveGroup ck) (loadGroup) (Gen.groups)
    run $ insertListRef list gk
  listInRef list

mossScriptInvocations :: Int -> [AssignmentKey] -> IO [MossScriptInvocationKey]
mossScriptInvocations n aks = do
  list <- createListRef
  quick n $ do
    ak <- pick $ elements aks
    msik <- mossScriptInvocationSaveAndLoad ak
    run $ insertListRef list msik
  listInRef list

-- Generate and store the given number of assignment and assign them to random groups,
-- from the given list, returns all the created assignment keys
groupAssignmentGen :: Int -> [GroupKey] -> IO [AssignmentKey]
groupAssignmentGen n gs = do
  list <- createListRef
  quick n $ do
    gk <- pick $ elements gs
    ak <- saveAndLoadIdenpotent "Group assignment"
      (saveGroupAssignment gk) (loadAssignment) (Gen.assignments startDate endDate)
    run $ insertListRef list ak
  listInRef list

-- Generate and store the given numner of assignment and assign them to random courses,
-- from the given list, returns all the created assignment keys
courseAssignmentGen :: Int -> [CourseKey] -> IO [AssignmentKey]
courseAssignmentGen n cs = do
  list <- createListRef
  quick n $ do
    ck <- pick $ elements cs
    ak <- saveAndLoadIdenpotent "Course assignment"
      (saveCourseAssignment ck) (loadAssignment) (Gen.assignments startDate endDate)
    run $ insertListRef list ak
  listInRef list

groupAssessmentGen n gs = do
  list <- createListRef
  quick n $ do
    gk <- pick $ elements gs
    a <- pick Gen.assessments
    ak <- saveAndLoadIdenpotent "Group assessment"
      (saveGroupAssessment gk) (loadAssessment) (return a)
    run $ insertListRef list (ak, a)
  listInRef list

courseAssessmentGen n cs = do
  list <- createListRef
  quick n $ do
    ck <- pick $ elements cs
    a <- pick Gen.assessments
    ak <- saveAndLoadIdenpotent "Course assessment"
      (saveCourseAssessment ck) (loadAssessment) (return a)
    run $ insertListRef list (ak, a)
  listInRef list

-- Generate and store the given number of users, and returns the usernames found in the
-- persistence layer
users :: Int -> IO [Username]
users n = users' Gen.roleGen n

users' :: Gen Role -> Int -> IO [Username]
users' genRole n = do
  list <- createListRef
  quick n $ do
    u <- pick (genRole >>= Gen.users')
    u' <- createOrLoadUser u
    run $ insertListRef list (u_username u)
  listInRef list

-- Generate and store the given numner of admin users, and returns the usernames saved to
-- the persistence layer, if no admin was generated an error would be thrown
admins :: Int -> IO [Username]
admins n = do
  list <- createListRef
  quick n $ do
    u <- setAdmin <$> pick Gen.users
    let username = u_username u
    exist <- runPersistCmd $ doesUserExist username
    unless exist $ do
      createOrLoadUser u
      run $ insertListRef list username
  users <- listInRef list
  assertNonEmpty users "No admins were generated."
  return users
  where
    setAdmin u = u { u_role = Admin }

-- Select an admin and a course and set the admin as a course admin
setCourseAdmins :: [Username] -> [CourseKey] -> Int -> IO ()
setCourseAdmins as cs n =
  quick n $ do
    a <- pick $ elements as
    c <- pick $ elements cs
    runPersistCmd $ createCourseAdmin a c

-- Select an admin and a course and set the admin as a course admin
setGroupAdmins :: [Username] -> [GroupKey] -> Int -> IO ()
setGroupAdmins as gs n = do
  quick n $ do
    a <- pick $ elements as
    g <- pick $ elements gs
    runPersistCmd $ createGroupAdmin a g

-- SubmissionInfoList is a list from username, assignment-key and the submission-key for them.
-- Interpretation: The submission information about which user submitted which submission
-- for the given assignment.
type SubmissionInfoList = [((Username,AssignmentKey),(SubmissionKey, UTCTime))]

-- Throws away the assignment and the username from the submission information
infoListToSubmissionKeys :: SubmissionInfoList -> [SubmissionKey]
infoListToSubmissionKeys = map (fst . snd)

-- Generates and stores the given number of comments, for the randomly selected
-- submissions. Returns all the created comment key with the associated submissionKey.
comments :: Int -> [SubmissionKey] -> IO [(CommentKey, SubmissionKey)]
comments n ss = do
  list <- createListRef
  let now = utcTimeConstant
  quick n $ do
    sk <- pick $ elements ss
    ck <- saveAndLoadIdenpotent "Comment" (saveComment sk) (loadComment) (Gen.comments now)
    run $ insertListRef list (ck, sk)
  listInRef list

-- Generates and stores the given number of feedbacks, for the randomly selected
-- submissions. Returns all the created feedback key with the associated submissionKey.
feedbacks :: Int -> [SubmissionKey] -> IO [(FeedbackKey, SubmissionKey)]
feedbacks n ss = do
  list <- createListRef
  let now = utcTimeConstant
  quick n $ do
    sk <- pick $ elements ss
    fk <- saveAndLoadIdenpotent "Feedback" (fmap head . saveFeedbacks sk . (: [])) (loadFeedback) (Gen.feedbacks now)
    run $ insertListRef list (fk, sk)
  listInRef list

{- XXX
-- Generates and stores the given number of system notifications.
-- Returns all the creates notification key.
systemNotifications :: Int -> IO [NotificationKey]
systemNotifications n = do
  list <- createListRef
  quick n $ do
    nk <- saveAndLoadIdenpotent "Notification" saveSystemNotification loadNotification Gen.notifications
    run $ insertListRef list nk
  listInRef list

-- Generates and stores the given number of comment notifications for randomly selected comments
-- Returns the list of the associated notification and comment keys.
commentNotifications :: Int -> [CommentKey] -> IO [(NotificationKey, CommentKey)]
commentNotifications n cs = do
  list <- createListRef
  quick n $ do
    ck <- pick $ elements cs
    nk <- saveAndLoadIdenpotent "Notification" (saveCommentNotification ck) loadNotification Gen.notifications
    run $ insertListRef list (nk,ck)
  listInRef list

-- Generates and stores the given number of feedback notifications for randomly selected comments
-- Returns the list of the associated notification and feedback keys.
feedbackNotifications :: Int -> [FeedbackKey] -> IO [(NotificationKey, FeedbackKey)]
feedbackNotifications n fs = do
  list <- createListRef
  quick n $ do
    fk <- pick $ elements fs
    nk <- saveAndLoadIdenpotent "Notification" (saveFeedbackNotification fk) loadNotification Gen.notifications
    run $ insertListRef list (nk,fk)
  listInRef list
-}

-- Generate and store the given number of submissions, for the randomly selected
-- user and assignment. Returns all the created submission keys with the associated
-- username and assignment
submissions :: Int -> [Username] -> [AssignmentKey] -> IO SubmissionInfoList
submissions n us as = do
  list <- createListRef
  ds <- createListRef
  writeIORef ds dates
  quick n $ do
      d <- run $ popListRef ds
      u <- pick $ elements us
      ak <- pick $ elements as
      sk <- saveAndLoadIdenpotent "Submission"
        (saveSubmission ak u) (loadSubmission) (Gen.submissions d)
      run $ insertListRef list ((u,ak),(sk,d))
  listInRef list

-- Generate and store the given number of evaluations, for the randomly selected
-- submission, and returns all the created evaluation keys
evaluations n ss = do
  list <- createListRef
  quick n $ do
    sk <- pick $ elements ss
    cfg <- evaluationConfigForSubmission sk
    ek <- saveAndLoadIdenpotent "Evaluation"
      (saveSubmissionEvaluation sk) (loadEvaluation) (Gen.evaluations cfg)
    run $ insertListRef list ek
  listInRef list

-- Generates and stores the given number of test scripts for the randomly selected
-- courses, and returns all the created TestScriptKeys
testScripts :: Int -> [CourseKey] -> IO [TestScriptKey]
testScripts n cs = do
  list <- createListRef
  quick n $ do
    ck <- pick $ elements cs
    tsk <- saveAndLoadIdenpotent "TestScript"
      (saveTestScript ck) (loadTestScript) (Gen.testScripts)
    run $ insertListRef list tsk
  listInRef list

scores n us as = do
  list <- createListRef
  quick n $ do
    u <- pick $ elements us
    a <- pick $ elements as
    sk <- saveAndLoadIdenpotent "Score"
      (saveScore u a) (loadScore) (Gen.scores)
    run $ insertListRef list sk
  listInRef list

-- Generates and stores the given number of the test cases for the randomly selected
-- test scripts and assignments, and returns all the created TestCaseKeys
testCases :: Int -> [TestScriptKey] -> [AssignmentKey] -> IO [(AssignmentKey, TestScriptKey, TestCaseKey)]
testCases n tcs as = do
  list <- createListRef
  quick n $ do
    tsk <- pick $ elements tcs
    ak  <- pick $ elements as
    mtk <- runPersistCmd $ testCaseOfAssignment ak
    case mtk of
      Just tk -> return ()
      Nothing -> do tck <- saveAndLoadIdenpotent "TestCase"
                             (saveTestCase tsk ak) (loadTestCase) (Gen.testCases)
                    run $ insertListRef list (ak, tsk, tck)
  listInRef list

massPersistenceTest = do
  cs <- courses 100
  gs <- groups 250 cs
  gas <- groupAssignmentGen 400 gs
  cas <- courseAssignmentGen 400 cs
  let as = gas ++ cas
  us <- users 300
  ss <- infoListToSubmissionKeys <$> submissions 500 us as
  evaluations 400 ss
  return ()

runPropertyM :: Testable a => PropertyM IO a -> IO ()
runPropertyM = quick 1

quickWithCleanUp :: Testable b => IO a -> Int -> PropertyM IO b -> IO a
quickWithCleanUp cleanup n p = check cleanup $ quickCheckWithResult (success n) $ monadicIO p

-- The test are at very high level, we must see if basic load
-- properties are hold.

reinitPersistence = do
  init <- createPersistInit defaultConfig
  tearDown init
  initPersist init

initPersistence = do
  init <- createPersistInit defaultConfig
  initPersist init

groupsTests :: TestSet ()
groupsTests = test $ testCase "Groups tests" $ do
  reinitPersistence
  cs <- courses 20
  gs <- runPersistIOCmd $ Persist.groups
  assertTrue (null gs) "Group list is not empty initially."
  cgs <- fmap concat $ forM cs $ \ck -> do
    gks <- groups 20 [ck]
    gs <- runPersistIOCmd $ mapM Persist.loadGroup gks
    c <- runPersistIOCmd $ Persist.loadCourse ck
    return [(c, gk, g) | (gk, g) <- zip gks gs]
  cgs' <- runPersistIOCmd $ Persist.groups
  assertSetEquals cgs cgs' "A group is missing or there is a course-groupkey-group mismatch."
  assertEquals (length cgs) (length cgs') "A group is missing or a group is listed multiple times."

loadGroupAndCourseTests :: TestSet ()
loadGroupAndCourseTests = test $ testCase "Load group and course tests" $ do
  reinitPersistence
  cs <- courses 20
  cgs <- fmap concat $ forM cs $ \ck -> do
    gks <- groups 20 [ck]
    gs <- runPersistIOCmd $ mapM Persist.loadGroup gks
    c <- runPersistIOCmd $ Persist.loadCourse ck
    return [(ck, c, gk, g) | (gk, g) <- zip gks gs]
  quick 100 $ do
    (ck, c, gk, g) <- pick $ elements cgs
    (ck', c', gk', g') <- runPersistCmd $ loadGroupAndCourse gk
    assertEquals ck ck' "Course keys do not match."
    assertEquals c c' "Courses do not match."
    assertEquals gk gk' "Group keys do not match."
    assertEquals g g' "Groups do not match."

courseAdminsTests :: TestSet ()
courseAdminsTests = test $ testCase "Course admin [key] tests" $ do
  reinitPersistence
  cs <- courses 20
  u <- users 50
  testCourseAdmins cs u
  putStr "Second round, now with groups and group admins "
  reinitPersistence
  cs <- courses 20
  u <- users 50
  gs <- groups 200 cs
  setGroupAdmins u gs 150
  testCourseAdmins cs u

  where
    testCourseAdmins :: [CourseKey] -> [Username] -> IO ()
    testCourseAdmins cs u = do
      forM cs $ \c -> do
        ak <- runPersistIOCmd $ courseAdminKeys c
        a <- runPersistIOCmd $ courseAdmins c
        assertTrue (null ak && null a) "There are course admins for new courses."
      quick 100 $ do
        c <- pick $ elements cs
        u <- pick $ elements u
        user <- runPersistCmd $ loadUser u
        (ak, a, _, ak', a', _, ak'', a'') <- runPersistCmd $
                                               (,,,,,,,)
                                                 <$> courseAdminKeys c
                                                 <*> courseAdmins c
                                                 <*> createCourseAdmin u c
                                                 <*> courseAdminKeys c
                                                 <*> courseAdmins c
                                                 <*> createCourseAdmin u c
                                                 <*> courseAdminKeys c
                                                 <*> courseAdmins c
        assertSetEquals (u : ak) ak' "User wasn't set as course admin according to keys."
        assertSetEquals (user : a) a' "User wasn't set as course admin."
        assertTrue (length ak' - length ak <= 1) "A course admin is returned multiple times."
        assertTrue (length a' - length a <= 1) "A course admin is returned multiple times."
        assertSetEquals (u : ak) ak'' "User wasn't set as course admin according to keys second time"
        assertSetEquals (user : a) a'' "User wasn't set as course admin second time."
        assertTrue (length ak'' - length ak <= 1) "A course admin is listed multiple times second time."
        assertTrue (length a'' - length a <= 1) "A course admin is listed multiple times second time."

groupAdminsTests :: TestSet ()
groupAdminsTests = test $ testCase "Group admin [key] tests" $ do
  reinitPersistence
  cs <- courses 20
  gs <- groups 200 cs
  u <- users 50
  testGroupAdmins gs u
  putStr "Second round, now with course admins "
  reinitPersistence
  cs <- courses 20
  gs <- groups 200 cs
  u <- users 50
  setCourseAdmins u cs 100
  testGroupAdmins gs u

  where
    testGroupAdmins :: [GroupKey] -> [Username] -> IO ()
    testGroupAdmins gs u = do
      forM gs $ \g -> do
        ak <- runPersistIOCmd $ groupAdminKeys g
        a <- runPersistIOCmd $ groupAdmins g
        assertTrue (null ak && null a) "There are group admins for new groups."
      quick 100 $ do
        g <- pick $ elements gs
        u <- pick $ elements u
        user <- runPersistCmd $ loadUser u
        (ak, a, _, ak', a', _, ak'', a'') <- runPersistCmd $
                                               (,,,,,,,)
                                                 <$> groupAdminKeys g
                                                 <*> groupAdmins g
                                                 <*> createGroupAdmin u g
                                                 <*> groupAdminKeys g
                                                 <*> groupAdmins g
                                                 <*> createGroupAdmin u g
                                                 <*> groupAdminKeys g
                                                 <*> groupAdmins g
        assertSetEquals (u : ak) ak' "User wasn't set as group admin according to keys."
        assertSetEquals (user : a) a' "User wasn't set as group admin."
        assertTrue (length ak' - length ak <= 1) "A group admin is returned multiple times."
        assertTrue (length a' - length a <= 1) "A group admin is returned multiple times."
        assertSetEquals (u : ak) ak'' "User wasn't set as group admin according to keys second time"
        assertSetEquals (user : a) a'' "User wasn't set as group admin second time."
        assertTrue (length ak'' - length ak <= 1) "A group admin is listed multiple times second time."
        assertTrue (length a'' - length a <= 1) "A group admin is listed multiple times second time."

allAdminsTests :: TestSet ()
allAdminsTests = test $ testCase "All administrators tests" $ do
  reinitPersistence
  cs <- courses 20
  gs <- groups 200 cs
  us <- users' (elements [Student, Admin]) 200
  admins <- runPersistIOCmd allAdministrators
  assertEmpty admins "Admins' list is not empty initially."
  users <- runPersistIOCmd $ mapM loadUser us
  runPropertyM $ testAllAdmins 100 [] users
    where
      testAllAdmins :: Int -> [User] -> [User] -> PropertyM IO ()
      testAllAdmins 0 _ _ = return ()
      testAllAdmins n admins regulars = do
        let actions = ("regularToAdmin", regularToAdmin) : if null admins then [] else [("adminToAdmin", adminToAdmin), ("adminToRegular", adminToRegular)]        
        (i, a) <- pick $ elements $ zip [0..] (map fst actions)
        (admins', regulars') <- (snd $ actions !! i) admins regulars
        actualAdmins <- runPersistCmd allAdministrators
        assertEquals (sort $ map (u_uid &&& u_username) admins') (sort $ map (u_uid &&& u_username) actualAdmins) ("allAdministrators didn't reflect change among administrators made by " ++ a)
        testAllAdmins (n - 1) admins' regulars'

      regularToAdmin :: [User] -> [User] -> PropertyM IO ([User], [User])
      regularToAdmin admins regulars = do
        u <- pick $ elements regulars
        role <- pick Gen.teacherRoleGen
        let u' = u { u_role = role }
        runPersistCmd $ updateUser u'
        return ((u' : admins), (filter (\u'' -> u_uid u'' /= u_uid u) regulars))

      adminToAdmin :: [User] -> [User] -> PropertyM IO ([User], [User])
      adminToAdmin [] regulars = return ([], regulars)
      adminToAdmin admins regulars = do
        u <- pick $ elements admins
        role <- pick Gen.teacherRoleGen
        let u' = u { u_role = role }
        runPersistCmd $ updateUser u'
        return (admins, regulars)

      adminToRegular :: [User] -> [User] -> PropertyM IO ([User], [User])
      adminToRegular [] regulars = return ([], regulars)
      adminToRegular admins regulars = do
        u <- pick $ elements admins
        role <- pick Gen.nonTeacherRoleGen
        let u' = u { u_role = role }
        runPersistCmd $ updateUser u'
        return ((filter (\u'' -> u_uid u'' /= u_uid u) admins), (u' : regulars))


-- Tests that saving and loading do not change a MossScriptInvocaton
mossScriptInvocationSaveLoadTest :: TestSet ()
mossScriptInvocationSaveLoadTest = test $ testCase "MossScriptInvocation save and load test" $ do
  reinitPersistence
  cs <- courses 20
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 25 25 cs gs
  void $ mossScriptInvocations 250 as

courseAndGroupAssignments :: Int -> Int -> [CourseKey] -> [GroupKey] -> IO [AssignmentKey]
courseAndGroupAssignments cn gn cs gs = do
  cas <- courseAssignmentGen cn cs
  gas <- groupAssignmentGen gn gs
  return (cas ++ gas)

courseAndGroupAssessments :: Int -> Int -> [CourseKey] -> [GroupKey] -> IO [(AssessmentKey, Assessment)]
courseAndGroupAssessments cn gn cs gs = do
  cas <- courseAssessmentGen cn cs
  gas <- groupAssessmentGen gn gs
  return (cas ++ gas)

-- User can register course and groups and these groups and courses can have assignments.
-- The user can subscribe to groups and course, and it is necessary for him to
-- see the assignments of the groups and courses, and only the assignments of the
-- courses that the user registered, not others
userAssignmentKeyTest :: TestSet ()
userAssignmentKeyTest = test $ testCase "User assignment tests" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  us <- users 300
  quick 300 $ do
    u <- pick $ elements us
    gk <- pick $ elements gs
    ck <- runPersistCmd $ courseOfGroup gk
    runPersistCmd $ subscribe u gk
    gas <- runPersistCmd $ groupAssignments gk
    cas <- runPersistCmd $ courseAssignments ck
    let uas = gas ++ cas
    (asgs, _assessments) <- runPersistCmd $ userAssignmentsAssessments u gk
    assertTrue
      (HashSet.fromList uas == HashSet.fromList [ ak | (ak, _, _, _) <- asgs ])
      (unlines [
          "User assignment for a given course and group does not match the expected. User:", show u
        , " Group and course assignments: ", show uas, " User assignments:", show as
        , " Group: ", show gk
        , " Course: ", show ck
        , " Group assignments: ", show gas
        , " Course assignments: ", show cas
        ])

-- User can register course and groups and these groups and courses can have assessments.
-- The user can subscribe to groups and courses, and it is necessary for him to
-- see the assessments of the groups and courses, and only the assessments of the
-- courses that the user registered, not others
userAssessmentKeyTest :: TestSet ()
userAssessmentKeyTest = test $ testCase "User assessment tests" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssessments 150 150 cs gs
  us <- users 300
  quick 300 $ do
    u <- pick $ elements us
    gk <- pick $ elements gs
    ck <- runPersistCmd $ courseOfGroup gk
    runPersistCmd $ subscribe u gk
    gas <- runPersistCmd $ do
      assessments <- assessmentsOfGroup gk
      return [ ak | (ak, a) <- assessments, Assessment.visible a]
    let uas = gas
    (_asgs, assessments) <- runPersistCmd $ userAssignmentsAssessments u gk
    assertSetEquals uas [ ak | (ak, _, _) <- assessments ]
      (unlines [
          "User assessment for a given course and group does not match the expected. User:", show u
        , " Group and course assessments: ", show uas, " User assessments:", show as
        , " Group: ", show gk
        , " Course: ", show ck
        , " Group assessments visible: ", show gas
        ])

-- courseOrGroupOfAssignment returns the correct group or course
courseOrGroupAssignmentTest = test $ testCase "Course or group assignment tests" $ do
  reinitPersistence
  cs <- courses 100
  cs' <- generate $ vectorOf 70 $ elements cs
  cgs <- forM cs' $ \c -> do
    gs <- groups 30 [c]
    return (c, gs)
  groupAssignmentGen 150 (concatMap snd cgs)
  courseAssignmentGen 150 cs
  quick 500 $ do
    (c, gs) <- pick $ elements cgs
    g <- pick $ elements gs
    a <- pick (Gen.assignments startDate endDate)
    courseAssignment <- pick $ elements [True, False]
    if courseAssignment
      then do
        ak <- runPersistCmd $ saveCourseAssignment c a
        k <- runPersistCmd $ courseOrGroupOfAssignment ak
        either
          (\c' -> assertEquals c c' "Assignment course changed")
          (\_g -> fail "Course assignment has group")
          k
      else do
        ak <- runPersistCmd $ saveGroupAssignment g a
        k <- runPersistCmd $ courseOrGroupOfAssignment ak
        either
          (\_c -> fail "Group assignment has no group")
          (\g' -> assertEquals g g' "Assignment group changed")
          k

-- courseAndGroupOfAssignment returns the correct group and course
courseAndGroupAssignmentTest = test $ testCase "Course and group assignment tests" $ do
  reinitPersistence
  cs <- courses 100
  cs' <- generate $ vectorOf 70 $ elements cs
  cgs <- forM cs' $ \c -> do
    gs <- groups 30 [c]
    return (c, gs)
  groupAssignmentGen 150 (concatMap snd cgs)
  courseAssignmentGen 150 cs
  quick 500 $ do
    (c, gs) <- pick $ elements cgs
    g <- pick $ elements gs
    a <- pick (Gen.assignments startDate endDate)
    courseAssignment <- pick $ elements [True, False]
    if courseAssignment
      then do
        course <- runPersistCmd $ loadCourse c
        ak <- runPersistCmd $ saveCourseAssignment c a
        k <- runPersistCmd $ courseAndGroupOfAssignment ak
        case k of
          (course', Nothing) -> assertEquals course course' "Assignment course changed"
          _ -> fail "Course assignment has group"
      else do
        course <- runPersistCmd $ loadCourse c
        grp <- runPersistCmd $ loadGroup g
        ak <- runPersistCmd $ saveGroupAssignment g a
        k <- runPersistCmd $ courseAndGroupOfAssignment ak
        case k of
          (course', Just grp') -> do
            assertEquals course course' "Assignment course changed"
            assertEquals grp grp' "Assignment group changed"
          _ -> fail "Group assignment has no group"

-- Every submission has some kind of description
submissionDescTest = test $ testCase "Every submission has some kind of description" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  us <- users 300
  as <- courseAndGroupAssignments 150 150 cs gs
  ss <- infoListToSubmissionKeys <$> submissions 500 us as
  quick 500 $ do
    sk <- pick $ elements ss
    desc <- runPersistCmd $ submissionDesc sk
    assertFalse (T.null . courseName $ eCourse desc) "Course name was empty"
    maybe (return ()) (flip assertFalse "Group name was empty" . T.null . shortGroupName) $ eGroup desc
    assertNonEmpty (u_name $ eStudent desc) "Student name was empty"
    assertFalse (T.null . Assignment.name . eAssignment $ desc) "Assignment title was empty"
    assertEmpty (Map.toList $ eComments desc) "The comment list was not empty"

-- Allways the last evaluation is valid for the submission.
lastEvaluationTest = test $ testCase "Always the last evaluation is valid for the submission" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  us <- users 400
  assignmentSubmission1 <- Map.fromList <$> submissions 400 us as
  assignmentSubmission2 <- Map.fromList <$> submissions 400 us as
  -- If there is a user and an assignment in the new submission map
  -- and in the old one also, the submission keys must be different ones
  forM_ (Map.keys assignmentSubmission2) $ \key -> do
      case (Map.lookup key assignmentSubmission2, Map.lookup key assignmentSubmission1) of
        (Nothing, Nothing) -> fail "Impossible situation: key must exist"
        (Just v,  Just v')
          | v == v' -> fail "Submission is the old one after submitting the new one"
        _ -> return ()

createCourseAdmins n us cs = quick n $ do
  u <- pick $ elements us
  usr <- runPersistCmd $ loadUser u
  pre (u_role usr `elem` [CourseAdmin, Admin])
  c <- pick $ elements cs
  runPersistCmd $ createCourseAdmin u c

createGroupAdmins n us gs = quick n $ do
  u <- pick $ elements us
  g <- pick $ elements gs
  runPersistCmd $ createGroupAdmin u g

-- Every submission has a description, this description must be loaded
submissionDetailsDescTest = test $ testCase "Every submission has a description" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 150 150 cs gs
  us <- users 400
  createCourseAdmins 200 us cs
  createGroupAdmins 200 us gs
  ss <- infoListToSubmissionKeys <$> submissions 400 us as
  quick 1000 $ do
    sk <- pick $ elements ss
    desc <- runPersistCmd $ submissionDetailsDesc sk
    ak <- runPersistCmd $ assignmentOfSubmission sk
    ckGk <- runPersistCmd $ courseOrGroupOfAssignment ak
    case ckGk of
      Left ck -> do
        course <- runPersistCmd $ loadCourse ck
        assertEquals course (sdCourse desc) "Course was different"
        assertTrue (isNothing (sdGroup desc)) "Course assignment has a group"
      Right gk -> do
        grp <- runPersistCmd $ loadGroup gk
        course <- runPersistCmd $ courseOfGroup gk >>= loadCourse
        assertTrue (isJust (sdGroup desc)) "Group assignment has no group"
        assertEquals (Just grp) (sdGroup desc) "Group was different"
    assertFalse (T.null $ Assignment.desc $ sdAssignment desc) "Description was empty"
    when (isJust (sdStatus desc)) $ assertFalse (T.null $ fromJust $ sdStatus desc) "Status was empty"
    assertFalse (T.null $ sdSubmission desc) "Submission text was empty"
    forM_ (Map.toList $ sdComments desc) $ \(_,c) -> assertFalse (T.null $ comment c) "Comment was empty"

-- If the user administrates courses or groups, submission information about the
-- submission of the group or course attendees. The number of the tables are same as
-- the number of the groups and courses administrated by this user
submissionTablesTest = test $ testCase "Submission tables" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  us <- users 400
  as <- courseAndGroupAssignments 150 150 cs gs
  createCourseAdmins 200 us cs
  createGroupAdmins 200 us gs

  quick 500 $ do
    u <- pick $ elements us
    g <- pick $ elements gs
    c <- runPersistCmd $ courseOfGroup g
    runPersistCmd $ subscribe u g

  quick 1000 $ do
    u <- pick $ elements us
    acs <- runPersistCmd $ administratedCourses u
    ags <- runPersistCmd $ administratedGroups  u
    ts  <- runPersistCmd $ submissionTables     u
    forM_ ts $ \t -> do
--      assertTrue (length (stAssignments t) >= 0) "Invalid assignment list" TODO
      let asgNum = submissionTableInfoCata (\_ asgs _ _ _ -> length asgs) (\_ asgs _ _ _ -> length asgs) t
      forM_ (stiUserLines t) $ \(_, submissions) ->
        assertTrue (length submissions == asgNum) "Invalid user line number"
      forM_ (stiUsers t) $ usernameCata (\u -> assertNonEmpty u "Username was empty")

-- All the saved course must have a key and these
-- course keys must be listed
courseKeysTest = test $ testCase "All saved courses must have a key" $ do
  reinitPersistence
  savedKeys  <- Set.fromList <$> courses 100
  loadedKeys <- Set.fromList <$> (runPersistIOCmd $ courseKeys)
  assertEquals savedKeys loadedKeys "Saved and loaded courses were different"
  savedKeys2  <- Set.fromList <$> courses 50
  loadedKeys2 <- Set.fromList <$> (runPersistIOCmd $ courseKeys)
  assertTrue (Set.isSubsetOf loadedKeys loadedKeys2) "Not all old course keys were in the loaded set"
  assertTrue (Set.isSubsetOf savedKeys2 loadedKeys2) "New course keys were not in the loaded set"

userGroupsTest :: TestSet ()
userGroupsTest = test $ testCase "userGroups and userGroupKeys have to return all subscribed groups" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 100 cs
  us <- users 100
  forM us $ \u -> do
    ugs <- runPersistIOCmd $ userGroups u
    ugks <- runPersistIOCmd $ userGroupKeys u
    assertTrue (null ugs) "User subscribed groups list is not empty initially"
    assertTrue (null ugks) "User subscribed group keys list is not empty initially"
  quick 100 $ do
    u <- pick $ elements us
    ugs <- runPersistCmd $ userGroups u
    ugks <- runPersistCmd $ userGroupKeys u
    gk <- pick $ elements gs
    g <- runPersistCmd $ subscribe u gk >> loadGroup gk
    ugs' <- runPersistCmd $ userGroups u
    ugks' <- runPersistCmd $ userGroupKeys u
    ck <- runPersistCmd $ courseOfGroup gk
    c <- runPersistCmd $ loadCourse ck
    assertSetEquals ugs' ((ck, c, gk, g) : ugs) "A group is not listed by userGroups or there is a CourseKey-Course-GroupKey-Group mismatch."
    assertSetEquals ugks' (gk : ugks) "A group is not listed by userGroupsKeys."
    assertTrue (length ugs' - length ugs <= 1) "A subscribed group is listed multiple times."
    assertTrue (length ugks' - length ugks <= 1) "A subscribed group key is listed multiple times."

-- All the saved assignment must have a key and these keys must be listed
assignmentKeyTest = test $ testCase "All the saved assignments must have a key" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  saved  <- Set.fromList <$> (courseAndGroupAssignments 150 150 cs gs)
  loaded <- Set.fromList <$> (runPersistIOCmd $ assignmentKeys)
  assertEquals saved loaded "Saved and loaded assignment keys were different"
  saved2  <- Set.fromList <$> (courseAndGroupAssignments 50 50 cs gs)
  loaded2 <- Set.fromList <$> (runPersistIOCmd $ assignmentKeys)
  assertTrue (Set.isSubsetOf loaded loaded2) "Not all assignment keys were in the loaded set"
  assertTrue (Set.isSubsetOf saved2 loaded2) "New assignment keys were not in the loaded set"

-- All the saved submissions must have a key and these keys must be listed
filterSubmissionsTest = test $ testCase "All the saved submissions must have a key" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  us <- users 400
  saved  <- (Set.fromList . infoListToSubmissionKeys) <$> (submissions 500 us as)
  loaded <- Set.fromList <$> (runPersistIOCmd submissionKeys)
  assertEquals saved loaded "Saved and loaded submission keys were different"
  saved2  <- (Set.fromList . infoListToSubmissionKeys) <$> (submissions 100 us as)
  loaded2 <- Set.fromList <$> (runPersistIOCmd submissionKeys)
  assertTrue (Set.isSubsetOf loaded loaded2) "Not all submission keys were in the loaded set"
  assertTrue (Set.isSubsetOf saved2 loaded2) "New submission keys were not in the loaded set"

-- filterTextualSubmission should correctly return the list of textual submissions.
filterTextualSubmissionTest = test $ testCase "filterTextualSubmission test" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  us <- users 400
  sks <- infoListToSubmissionKeys <$> submissions 500 us as
  subms <- runPersistIOCmd $ mapM loadSubmission sks
  let (textual, zipped) = partition (isTextSubmission . solution . snd) (zip sks subms)
  quick 100 $ do
    textual' <- pick $ listOf $ elements $ map fst textual
    zipped' <- pick $ listOf $ elements $ map fst zipped
    sks' <- pick $ shuffle (textual' ++ zipped')
    output <- runPersistCmd $ filterTextSubmission sks'
    assertEquals (sort output) (Set.toAscList . Set.fromList $ textual') $ "filterTextSubmission returned a different list of submissions.\n" ++ show (sort output) ++ "\n" ++ show (sort textual')

submissionsTest = test $ testCase "Submission related tests" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  us <- users 400
  subms <- submissions 500 us as
  quick 100 $ do
    ((_, _), (sk, time)) <- pick $ elements subms
    postDate <- runPersistCmd $ postDateOfSubmission sk
    assertEquals postDate time "postDateOfSubmission reports different post date than expected."

-- Modified assignments must be untouched after loading them
modifyAssignmentsTest = test $ testCase "Modified assignments must be untouched after loading them" $ do
  cs <- courses 100
  gs <- groups 150 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  quick 1000 $ do
    ak <- pick $ elements as
    a0 <- runPersistCmd $ loadAssignment ak
    a  <- pick (Gen.assignments startDate endDate)
    runPersistCmd $ modifyAssignment ak a
    a1 <- runPersistCmd $ loadAssignment ak
    assertEquals a a1 "Modified and loaded assignments were different"

-- Modified evaluations must be untouched after loading them
modifyEvaluationTest = test $ testCase "Modified evaluations must be untouched after loading them" $ do
  cs <- courses 100
  gs <- groups 300 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  us <- users 200
  ss <- infoListToSubmissionKeys <$> (submissions 400 us as)
  es <- evaluations 600 ss
  quick 1000 $ do
    ek <- pick $ elements es
    msk <- runPersistCmd $ submissionOfEvaluation ek
    assertTrue
      (isJust msk)
      "There is no submission for the submission related evaluation"
    let sk = fromJust msk
    cfg <- evaluationConfigForSubmission sk
    e <- pick $ Gen.evaluations cfg
    runPersistCmd $ modifyEvaluation ek e
    e1 <- runPersistCmd $ loadEvaluation ek
    assertEquals e e1 $ concat
      [ "Modified and loaded evaluations were different "
      , "Evaluation key:", show ek
      , "Generated: ", show e
      , "Loaded: ", show e1
      ]

-- Subscribe users to groups
subscribeUsers :: Int -> [Username] -> [GroupKey] -> IO ()
subscribeUsers n us gs =
  quick n $ do
    u <- pick $ elements us
    g <- pick $ elements gs
    runPersistCmd $ subscribe u g

-- Test if the users make unsubscribe from the courses by the admin
deleteUsersFromCourseTest = test $ testCase "Delete user form course" $ do
  cs <- courses 50
  gs <- groups 250 cs
  us <- users 200
  subscribeUsers 500 us gs
  quick 1000 $ do
    u <- pick $ elements us
    ucs <- runPersistCmd $ userCourses u
    case ucs of
      [] -> testEmptyCourse cs u
      [c] -> testOneCourse u c
      cs' -> testMoreCourses u cs'
  where
    -- Test if selecting any of the course, trying to unsubscribe does not
    -- produce error and the number of the subscriptions does not change
    testEmptyCourse cs u = do
      c <- pick $ elements cs
      runPersistCmd $ deleteUserFromCourse c u
      ucs <- runPersistCmd $ userCourses u
      assertEquals [] ucs "Subscribed to some course."

    -- Test if subscribing from the course produces an empty course list
    testOneCourse u c = do
      runPersistCmd $ deleteUserFromCourse c u
      ucs <- runPersistCmd $ userCourses u
      assertEquals [] ucs "Subscribed to some course."

    -- Check if the deletion of one course removes only the deleted course
    testMoreCourses u cs' = do
      c <- pick $ elements cs'
      runPersistCmd $ deleteUserFromCourse c u
      ucs <- runPersistCmd $ userCourses u
      assertEquals ((length cs') - 1) (length ucs) "Not only one course was deleted"
      assertEquals (cs' \\ [c]) ucs "Nos the right course was deleted"

deleteUsersFromCourseNegativeTest = test $ testCase "Delete user from courses not belong to" $ do
  cs <- courses 50
  gs <- groups 250 cs
  us <- users 200
  subscribeUsers 500 us gs
  -- Tries to subscribe students from groups that are not attended in
  quick 1000 $ do
    u <- pick $ elements us
    ucs <- runPersistCmd $ userCourses u
    c' <- pick $ elements (cs \\ ucs)
    runPersistCmd $ deleteUserFromCourse c' u
    ucs' <- runPersistCmd $ userCourses u
    assertEquals ucs ucs' "User's course list has changed"

unsubscribeFromSubscribedGroupsTest = test $ testCase "User unsubscribes from a course" $ do
  cs <- courses 50
  gs <- groups 250 cs
  us <- users 200
  subscribeUsers 500 us gs
  quick 1000 $ do
    u <- pick $ elements us
    ugs <- runPersistCmd $ userGroupKeys u
    when (not $ null ugs) $ do
    g <- pick $ elements ugs
    join $ runPersistCmd $ do
      ucsb <- userCourses u
      ugsb <- userGroupKeys u
      c <- courseOfGroup  g
      unregscb <- unsubscribedFromCourse c
      unregsgb <- unsubscribedFromGroup g
      unsubscribe u g
      ucsa <- userCourses u
      ugsa <- userGroupKeys u
      unregsca <- unsubscribedFromCourse c
      unregsga <- unsubscribedFromGroup g
      return $ case g `elem` ugsb of
        True -> do
          assertEquals (length ugsa) (length ugsb - 1) $ concat
            [ "User is not unsubscribed from group "
            , " Before unsubscription: ", show ugsb
            , " After unsubscription: ", show ugsa
            ]
          assertSetEquals (ugsb) (g:ugsa) "User is not unsubscribed from group #2"
          assertFalse (u `elem` unregsgb) "User was in the group unsubscribed list"
          assertTrue  (u `elem` unregsga) "User is not in the group unsubscribed list"
          -- First unsubscription, before and after values must differs
        False -> do
          -- Second unsubscription, before and after values must be the same
          assertSetEquals (ugsb) (ugsa) "User is unsubscribed from course #2"
          assertSetEquals (unregsgb) (unregsga) "User is in the course unsubscribed list"

saveLoadAndModifyTestScriptsTest = test $ testCase "Save, load and modify test scripts" $ do
  reinitPersistence
  cs <- courses 200
  tss <- testScripts 1000 cs
  quick 1000 $ do
    ts <- pick $ elements tss
    nts <- pick $ Gen.testScripts
    join $ runPersistCmd $ do
      modifyTestScript ts nts
      nts' <- loadTestScript ts
      ck <- courseOfTestScript ts
      ctss <- testScriptsOfCourse ck
      return $ do
        assertEquals nts nts' "Modifing the test script failed"
        assertTrue (elem ts ctss) "Test Script is not in it's course"

saveLoadAndModifyTestCasesTest :: TestSet ()
saveLoadAndModifyTestCasesTest = test $ testCase "Save, load and modify test cases" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  tss <- testScripts 500 cs
  as <- courseAndGroupAssignments 100 100 cs gs
  tcs <- map thd3 <$> testCases 1000 tss as
  quick 1000 $ do
    tc  <- pick $ elements tcs
    ntc <- pick $ Gen.testCases
    join $ runPersistCmd $ do
      modifyTestCase tc ntc
      ntc' <- loadTestCase tc
      return $ do
        assertEquals ntc ntc' "Modification of the test case has failed"

-- Check testCaseOfAssignment. It should reflect effects of
-- saveTestCase and removeTestCaseAssignment and should be invariant
-- for modifyTestCase.
--
-- The test case manipulation functions (save, modify, remove) should not
-- affect other assignments.
testCaseOfAssignmentTest :: TestSet ()
testCaseOfAssignmentTest = test $ testCase "Check test case of assignment" $ do
  reinitPersistence
  cs <- courses 10
  gs <- groups 40 cs
  tss <- testScripts 10 cs
  as <- courseAndGroupAssignments 5 5 cs gs
  noneHasTestCase <- runPersistIOCmd $ allM (fmap isNothing . testCaseOfAssignment) as
  assertTrue noneHasTestCase "An assignment has test case initially."
  runPropertyM $ foldM_ (\(withTestCase, withoutTestCase) _ -> do
             a <- pick (elements as)
             case find ((== a) . fst) withTestCase of
               Just (a, tck) -> do
                 tc <- pick (Gen.testCases)
                 runPersistCmd $ do
                   modifyTestCase tck tc
                   noneChanged <- allM (\(a', tck') -> (== Just tck') <$> testCaseOfAssignment a') withTestCase
                   noneHasTestCase <- allM (fmap isNothing . testCaseOfAssignment) withoutTestCase
                   liftIO $ assertTrue noneChanged "Test case key of assignment changed."
                   liftIO $ assertTrue noneHasTestCase "Test case for an assignment is set after change."
                   removeTestCaseAssignment tck a
                   hasNoTestCase <- isNothing <$> testCaseOfAssignment a
                   let withTestCase' = delete (a, tck) withTestCase
                   noneChanged <- allM (\(a', tck') -> (== Just tck') <$> testCaseOfAssignment a') withTestCase'
                   noneHasTestCase <- allM (fmap isNothing . testCaseOfAssignment) withoutTestCase
                   liftIO $ assertTrue noneChanged "The test case key of an assignment changed after removal."
                   liftIO $ assertTrue noneHasTestCase "Test case key for an assignment is set after removal."
                   return (withTestCase', withoutTestCase)
               Nothing -> do
                 ts <- pick (elements tss)
                 tc <- pick (Gen.testCases)
                 runPersistCmd $ do
                   tck <- saveTestCase ts a tc
                   mtck' <- testCaseOfAssignment a
                   let withoutTestCase' = delete a withoutTestCase
                   liftIO $ assertEquals (Just tck) mtck' "Test case key of assignment is different or missing."
                   noneChanged <- allM (\(a', tck') -> (== Just tck') <$> testCaseOfAssignment a') withTestCase
                   noneHasTestCase <- allM (fmap isNothing . testCaseOfAssignment) withoutTestCase'
                   liftIO $ assertTrue noneChanged "The test case key of an assignment changed after save."
                   liftIO $ assertTrue noneHasTestCase "Test case key for an assignment is set after save."
                   return ((a, tck) : withTestCase, withoutTestCase')
         )
    ([], [])
    [1..2 * length as]

  where
    allM :: Monad m => (a -> m Bool) -> [a] -> m Bool
    allM p l = and <$> mapM p l

-- Creates a temporary directory for the bead in the system's temp dir
createBeadTempDir :: IO FilePath
createBeadTempDir = do
  tmp <- getTemporaryDirectory
  createTempDirectory tmp "bead."

userFileSaveTest = test $ testCase "Save user's file" $ do
  reinitPersistence
  tmpDir <- createBeadTempDir
  us <- users 100
  quickWithCleanUp (removeDirectoryRecursive tmpDir) 1000 $ do
    u <- pick $ elements us
    fn <- pick $ vectorOf 8 $ elements ['a'..'z']
    contents <- pick $ Gen.manyWords
    ufs <- map fst <$> (runPersistCmd $ listFiles u)
    saveFileTest u fn contents ufs UsersPublicFile
    ufs2 <- map fst <$> (runPersistCmd $ listFiles u)
    saveFileTest u fn contents ufs2 UsersPrivateFile

  where
    saveFileTest :: Username -> String -> String -> [UsersFile FilePath] -> (forall a. a -> UsersFile a) -> PropertyM IO ()
    saveFileTest u fn contents ufs tag = join $ runPersistCmd $ do
      saveFile u fn (tag (BS.pack contents))
      ufs' <- map fst <$> listFiles u
      path <- getFile u (tag fn)
      contents' <- liftIO $ readFile path
      return $ do
        assertSetEquals (tag fn:ufs) ufs'
          $ concat ["New file was not saved into the ", show u, " dir"]
        assertTrue (length path > 0) "Invalid path"
        assertEquals contents contents' "The file content changed during saving"


userFileCopyTest = test $ testCase "Copy, list, and get user's data file path" $ do
  reinitPersistence
  tmpDir <- createBeadTempDir
  us <- users 100
  fs <- uploadTempFiles tmpDir 1000
  let userFileTypes = [UsersPublicFile, UsersPrivateFile]
  quickWithCleanUp (removeDirectoryRecursive tmpDir) 1000 $ do
    u <- pick $ elements us
    f <- pick $ elements fs
    fn <- pick $ oneof
      [ t <$> vectorOf 8 (elements ['a'..'z']) | t <- userFileTypes ]
    ufs <- map fst <$> (runPersistCmd $ listFiles u)
    join $ case fn `elem` ufs of
      True  -> testOverwriteFile u f fn ufs
      False -> testCopyFile u f fn ufs

  where
    testCopyFile u f fn ufs = runPersistCmd $ do
      copyFile u f fn
      ufs' <- map fst <$> listFiles u
      path <- getFile u fn
      contents <- liftIO $ readFile f
      contents' <- liftIO $ readFile path
      return $ do
        assertSetEquals (fn:ufs) ufs'
          $ concat ["New file was not copied into the ", show u, " dir"]
        assertTrue (length path > 0) "Invalid path"
        assertEquals contents contents' "The file content changed during copying"

    testOverwriteFile u f fn ufs = runPersistCmd $ do
      path  <- getFile u fn
      copyFile u f fn
      content <- liftIO $ readFile f
      path' <- getFile u fn
      content' <- liftIO $ readFile path'
      return $ do
        assertEquals path path' "The overwritted file path's has changed"
        assertEquals content content' "The file content was not overwritten"

userOverwriteFileTest = test $ testCase "Overwrite user's data file" $ do
  reinitPersistence
  tmpDir <- createBeadTempDir
  us <- users 100
  fs <- uploadTempFiles tmpDir 1000
  let userFileTypes = [UsersPublicFile, UsersPrivateFile]
  forM_ us $ \u -> quick 5 $ do
    f <- pick $ elements fs
    fn <- pick $ oneof
      [ t <$> (vectorOf 8 $ elements ['a'..'z']) | t <- userFileTypes ]
    runPersistCmd $ copyFile u f fn
  quickWithCleanUp (removeDirectoryRecursive tmpDir) 1000 $ do
    u <- pick $ elements us
    ufs <- map fst <$> (runPersistCmd $ listFiles u)
    f <- pick $ elements fs
    fn <- pick $ elements ufs
    join $ runPersistCmd $ do
      path <- getFile u fn
      copyFile u f fn
      content <- liftIO $ readFile f
      path' <- getFile u fn
      content' <- liftIO $ readFile path'
      ufs' <- map fst <$> listFiles u
      return $ do
        assertSetEquals ufs ufs' "The user's file set was changed"
        assertEquals path path' "The user's file path was changed"
        assertEquals content content' "The user's file content is not copied correctly"

testJobCreationTest :: TestSet ()
testJobCreationTest = test $ testCase "Test job creation" $ do
  reinitPersistence
  us <- users 400
  cs <- courses 50
  gs <- groups 200 cs
  tss <- testScripts 100 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  tcs <- testCases 600 tss as
  ss <- submissions 1500 us as
  quick 1000 $ do
    ((_u,ak),(sk, _d)) <- pick $ elements ss
    entriesBeforeSave <- liftIO $ listDirectory testOutgoing
    runPersistCmd $ do
       queueSubmissionForTest sk
       mtck <- testCaseOfAssignment ak
       maybe (testIfHasNoTestJob entriesBeforeSave) (testIfHasTestJob entriesBeforeSave sk) mtck

  where
    testIfHasNoTestJob :: [FilePath] -> Persist ()
    testIfHasNoTestJob entriesBeforeSave =
      liftIO $ do
        entries <- listDirectory testOutgoing
        assertEquals (sort entriesBeforeSave) (sort entries) "Test Job directory exists"

    testIfHasTestJob :: [FilePath] -> SubmissionKey -> TestCaseKey -> Persist ()
    testIfHasTestJob entriesBeforeSave sk tck = do
      -- Domain knowledge is used
      entries <- liftIO $ listDirectory testOutgoing
      let [job] = map (testOutgoing </>) $ sort entries \\ sort entriesBeforeSave
      sk' <- liftIO $ SubmissionKey <$> readFile (job </> "id")
      script     <- liftIO $ TIO.readFile $ job </> "script"
      submission2 <- loadSubmission sk
      assertSubmissions <- withSubmissionValue (solution submission2)
        (\sol -> do testSolution <- liftIO $ TIO.readFile $ job </> "submission"
                    return $ assertEquals sol testSolution "Solutions are different")
        (\sol -> do testSolution <- liftIO $ BS.readFile $ job </> "submission"
                    return $ assertEquals sol testSolution "Solutions are different")
      tsk <- testScriptOfTestCase tck
      script2     <- loadTestScript tsk
      case2       <- loadTestCase   tck
      assertTests <- withTestCaseValue
        (tcValue case2)
        (\testValue -> do tests <- liftIO $ TIO.readFile $ job </> "tests"
                          return $ assertEquals tests testValue "Tests are different")
        (\testValue -> do tests <- liftIO $ BS.readFile $ job </> "tests"
                          return $ assertEquals tests testValue "Tests are different")
      liftIO $ do
        assertEquals sk' sk "SubmissionKeys are different"
        assertEquals script (tsScript script2) "Scripts are different"
        assertTests
        assertSubmissions

insertAndFinalizeTestFeedback :: SubmissionKey -> [FeedbackInfo] -> Persist ()
insertAndFinalizeTestFeedback sk testFeedback = do
  insertTestFeedback sk testFeedback
  finalizeTestFeedback sk

finalizeFeedbacksTest = test $ testCase "Locked feedback tests" $ do
  reinitPersistence
  us <- users 100
  cs <- courses 10
  gs <- groups 50 cs
  as <- courseAndGroupAssignments 200 200 cs gs
  ss <- submissions 1500 us as
  lockedFeedbackList <- createListRef
  finalizedFeedbackList <- createListRef
  quick 1000 $ do
    ((_u,_ak),(sk, _d)) <- pick $ elements ss
    locked <- run $ listInRef lockedFeedbackList
    if sk `elem` locked
      then checkIfCanBeFinalized sk lockedFeedbackList
      else checkIfCanBeAttached sk lockedFeedbackList

  where
    checkIfCanBeAttached sk locked = do
      run $ insertListRef locked sk
      testFeedback <- pick $ Gen.testFeedbackInfo
      runPersistCmd $ do
        insertTestFeedback sk testFeedback
        cks <- map fst <$> testFeedbacks
        liftIO $ assertFalse (sk `elem` cks)
                   "Test Locked Feedback occurs in the feedback list."

    checkIfCanBeFinalized sk locked = do
      run $ modifyListRef locked (delete sk)
      runPersistCmd $ do
        finalizeTestFeedback sk
        cks <- map fst <$> testFeedbacks
        liftIO $ assertTrue (sk `elem` cks)
                   "Test Finalized Feedback does not occur in the feedback list."

unevaluatedScoresTests = test $ testCase "Unevaluated scores" $ do
  reinitPersistence
  us <- users 50
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssessments 300 300 cs gs
  scs <- scores 500 us (map fst as)
  quick 1000 $ do
    s <- pick $ elements scs
    -- All the saved scores should have an assessment
    -- All the saved scores should have appear in the score list of the assessment
    a <- runPersistCmd $ assessmentOfScore s
    sa <- runPersistCmd $ scoresOfAssessment a
    assertTrue (elem s sa) ("The score was not in the score list of assessment: " ++ show s)

    -- All the saved scores should have a username
    -- All the saved scores should have appear in the score list of the user
    u <- runPersistCmd $ usernameOfScore s
    su <- runPersistCmd $ scoresOfUser u
    assertTrue (elem s su) ("The score was not in the score list of user: " ++ show s)

    -- All the saved scores should not have an evaluation
    e <- runPersistCmd $ evaluationOfScore s
    assertEquals Nothing e "Unevaluated score has an evaluation"

scoreEvaluationTests = test $ testCase "Evaluated scores" $ do
  reinitPersistence
  us <- users 50
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssessments 300 300 cs gs
  scs <- scores 500 us (map fst as)
  quick 1000 $ do
    s <- pick $ elements scs
    es <- runPersistCmd $ evaluationOfScore s
    -- All the score evaluation should not have a submission key
    case es of
      -- All the evaluated scores should have the evaluation key
      Nothing -> do
        -- TODO: Copy the evaluation of the score
        cfg <- runPersistCmd $ evalConfigOfScore s
        e   <- pick $ Gen.evaluations cfg
        ek  <- runPersistCmd $ saveScoreEvaluation s e
        ek' <- runPersistCmd $ evaluationOfScore s
        assertEquals (Just ek) (fst <$> ek') "The freshly evaluated score does not have the score key."
        assertEquals (Just e) (snd <$> ek') "The freshly evaluated score does not have the right evaluation."
        s'  <- runPersistCmd $ scoreOfEvaluation ek
        assertEquals (Just s) s' "The score evaluation does not have the score."
        sbm <- runPersistCmd $ submissionOfEvaluation ek
        assertEquals Nothing sbm "The score evaluation has a submission key."

      -- All the evaluation of the score should have the score key
      Just (ek, _) -> do
        s'  <- runPersistCmd $ scoreOfEvaluation ek
        assertEquals (Just s) s' "The score evaluation does not have the score."
        sbm <- runPersistCmd $ submissionOfEvaluation ek
        assertEquals Nothing sbm "The score evaluation has a submission key."
  where
    evalConfigOfScore s = do
      ak <- assessmentOfScore s
      a <- loadAssessment ak
      return $! evaluationCfg a

assessmentTests = test $ testCase "Assessment tests" $ do
  reinitPersistence
  cs <- courses 100
  gs <- groups 200 cs
  as <- courseAndGroupAssessments 300 300 cs gs
  runPropertyM $ foldM_ (\as' _ -> testAssessments as') as [1..1000]

  where
    groupOrCourseOf :: AssessmentKey -> PropertyM IO (Maybe CourseKey, Maybe GroupKey)
    groupOrCourseOf a = runPersistCmd $ do
      c <- courseOfAssessment a
      g <- groupOfAssessment a
      return (c,g)

    testAssessments :: [(AssessmentKey, Assessment)] -> PropertyM IO [(AssessmentKey, Assessment)]
    testAssessments as = do
      (ak, a) <- pick $ elements as

      -- All the assessemnt should have either a course or group
      (c,g) <- groupOrCourseOf ak

      case (c,g) of
        (Nothing, Nothing) -> fail "There was no course or group of the assessment."
        (Just _, Just _)   -> fail "There were course and group of assessment."

        -- The course of assessment should appear in its course assessment list
        (Just c, Nothing)  -> do
          cas <- runPersistCmd $ assessmentsOfCourse c
          case lookup ak cas of
            Nothing -> fail "The course assessment was not registered in its course."
            Just a' -> assertEquals a' a "The course assessment was different."
        -- The group of the assessment should appear in its group assessment list
        (Nothing, Just g)  -> do
          gas <- runPersistCmd $ assessmentsOfGroup g
          case lookup ak gas of
            Nothing -> fail "The group assessment was not registered in its group."
            Just a' -> assertEquals a' a "The group assessment was different."

      -- All the non scores assessment should have empty score list
      s <- runPersistCmd $ scoresOfAssessment ak
      assertEquals s [] "There were some scores for the assessement"

      -- The modification of an assessment should be stored properly
      asm <- pick $ Gen.assessments
      runPersistCmd $ modifyAssessment ak asm
      asm' <- runPersistCmd $ loadAssessment ak
      assertEquals asm asm' "The modification of the assessment has failed."

      -- The modification of an assessment should not change its group or course
      (c',g') <- groupOrCourseOf ak
      assertEquals (c,g) (c',g') "The course or group of the assessment has changed after modification."

      return $ (ak, asm) : filter (\(ak', _) -> ak' /= ak) as

openSubmissionsTest = test $ testCase "Open submissions list" $ do
  reinitPersistence
  us <- users 50
  as <- admins 10
  cs <- courses 10
  gs <- groups 40 cs
  asg <- courseAndGroupAssignments 30 30 cs gs
  setCourseAdmins as cs 100
  setGroupAdmins as gs 100
  subscribeUsers 400 us gs
  submissions 100 us asg
  quick 100 $ do
    a <- pick $ elements as
    os <- runPersistCmd $ openedSubmissionInfo a
    let adminedCourses = map fst $ osAdminedCourse os
        adminedGroups  = map fst $ osAdminedGroup os
        relatedCourses = map fst $ osRelatedCourse os
    pre (not (or [null adminedCourses, null adminedGroups, null relatedCourses]))
    checkAdminedCourse a adminedCourses
    checkAdminedGroup a adminedGroups
    checkRelatedCourse a relatedCourses
  return ()
  where
    -- Check if the user of the assignment attends a course that
    -- the admin administrates. Check if the course of the submission
    -- is administrated by the admin. Check if the course is related to the
    -- admin via groups that the user administrates
    checkAdminedCourse a sks = runPersistCmd $ do
      adminedCourses <- map fst <$> administratedCourses a
      relatedCourses <- map fst3 <$> administratedGroups a
      let courses = adminedCourses ++ relatedCourses
      forM_ sks $ \sk -> do
        u  <- usernameOfSubmission sk
        isInCourse <- or <$> mapM (isUserInCourse u) courses
        assertTrue isInCourse $ join
          [ "ADMINED COURSE: User is not registered in administrated course: "
          , show courses, " user: ", show u, " admin: ", show a
          ]
        ak <- assignmentOfSubmission sk
        ks <- courseOrGroupOfAssignment ak
        case ks of
          Right gk -> fail $ join
            [ "ADMINED COURSES: Group ", show gk, " Assignment ", show ak
            , " Submission ", show sk
            ]
          Left ck -> do
            assertTrue (elem ck courses) $ join
              [ "ADMINED COURSES: The course key was not administrated by the user or associated for the group "
              , " admin ", show a, " ", show ck
              ]
    -- Check if the user of the assignment attends a group that
    -- the admin administrates. Check if the submission is administrated by the admin
    -- checks if the submission is administrated by the admin.
    checkAdminedGroup  a sks = runPersistCmd $ do
      adminedGroups <- concatMap (map fst . thd3) <$> administratedGroups a
      forM_ sks $ \sk -> do
        u <- usernameOfSubmission sk
        isInGroup <- or <$> mapM (isUserInGroup u) adminedGroups
        assertTrue isInGroup $ join
          [ "ADMINED GROUP: User is not registered in administrated groups:"
          , show adminedGroups, " user: ", show u, " admin: ", show a
          ]
        ak <- assignmentOfSubmission sk
        ks <- courseOrGroupOfAssignment ak
        case ks of
          Right gk -> do
            assertTrue (elem gk adminedGroups) $ join
              [ "ADMINED GROUP: The group key was not administrated by the user "
              , show a, " ", show gk
              ]
          Left ck -> fail $ join
            [ "ADMINED GROUP: Course ", show ck, " Assignment ", show ak
            , " Submission ", show sk
            ]
    checkRelatedCourse a sks = runPersistCmd $ do
      groupsPerCourse <- administratedGroups a
      let groups = concatMap (map fst . thd3) groupsPerCourse
      courses <- (map fst3 groupsPerCourse ++) <$> (map fst <$> administratedCourses a)
      forM_ sks $ \sk -> do
        u <- usernameOfSubmission sk
        isNotInGroup <- (not . or) <$> mapM (isUserInGroup u) groups
        assertTrue isNotInGroup $ join
          [ "RELATED COURSE: User is registered in administrated group: ", show groups
          , " user: ", show u, " admin: ", show a
          ]
        ak <- assignmentOfSubmission sk
        ks <- courseOrGroupOfAssignment ak
        case ks of
          Right gk -> fail $ join
            [ "RELATED COURSE: Group ", show gk, " Assignment ", show ak
            , " user: ", show u, " admin: ", show a
            ]
          Left ck -> do
            assertTrue (elem ck courses) $ join
              [ "RELATED COURSE: The course key was not administrated by the user "
              , show a, " ", show ck, " student ", show u, " submission ", show sk
              ]

queuedForTestFeedbackTest :: TestSet ()
queuedForTestFeedbackTest = test $ testCase "Insert Queued for test feedbacks" $ do
  createInterpreter
  reinitPersistence
  us <- users 100
  cs <- courses 5
  gs <- groups 40 cs
  as <- courseAndGroupAssignments 10 10 cs gs
  ss <- submissions 100 us as
  ts <- testScripts 5 cs
  tck <- testCases 30 ts as
  let asWithTest = HashSet.fromList $ map fst3 tck
      allSubms = map (fst . snd) ss
  quick 500 $ do
    ((_u, ak), (sk, _d)) <- pick $ elements ss
    runPersistCmd $ do
      unAffected <- mapM (\sk -> (,) sk <$> getFeedbacks sk) (delete sk allSubms)
      fs <- getFeedbacks sk
      now <- liftIO getCurrentTime
      queueSubmissionForTest sk
      fs' <- getFeedbacks sk
      let fis = map info fs
          fis' = map info fs'
      if HashSet.member ak asWithTest
        then do
          assertEquals (QueuedForTest : fis) fis' "Queued for test feedback isn't inserted but assignment has test case."
          let latest = maximumBy (compare `on` postDate) fs'
          assertEquals QueuedForTest (info latest) "Latest feedback is not 'queued for test'."
          assertTrue (postDate latest >= now) "Latest feedback is earlier than time of request for testing."
        else assertEquals fis fis' "Queued for test feedback is inserted but assignment does not have test case."
      assertUnchanged unAffected
        where
          getFeedbacks :: SubmissionKey -> Persist [Feedback]
          getFeedbacks sk = sortOn postDate <$> (feedbacksOfSubmission sk >>= mapM loadFeedback)

          assertUnchanged :: [(SubmissionKey, [Feedback])] -> Persist ()
          assertUnchanged unaffected = forM_ unaffected $ \(sk, fs) -> do
            fs' <- getFeedbacks sk
            assertEquals fs fs' "A submission got a new feedback but it shouldn't."

{-
-- All the notifications for comments returns the given comment key, and no feedback key
saveCommentNotificationTest = test $ testCase "Comment notifications" $ do
  reinitPersistence
  us <- users 400
  cs <- courses 50
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 300 300 cs gs
  ss <- submissions 500 us as
  cks <- comments 1500 (map snd ss)
  let now = utcTimeConstant
  quick 1000 $ do
    (ck,sk) <- pick $ elements cks
    notif <- pick $ Gen.notifications
    nk <- runPersistCmd $ saveCommentNotification ck notif
    (mck,mfk,users) <- runPersistCmd $ do
      mck <- commentOfNotification nk
      mfk <- feedbackOfNotification nk
      users <- usersOfNotification nk
      return (mck,mfk,users)
    assertEquals (Just ck) mck   "Commented notification has no comment key."
    assertEquals Nothing   mfk   "Commented notification has a notification key."
    assertEquals []        users "Commented notification had a non-empty users list."

-- All the notifications for feedback returns the given feedback key, and no comment key
saveFeedbackNotificationTest = test $ testCase "Feedback notifications" $ do
  reinitPersistence
  us <- users 400
  cs <- courses 50
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 300 300 cs gs
  ss <- submissions 500 us as
  fs <- feedbacks 1500 (map snd ss)
  quick 1000 $ do
    (fk,sk) <- pick $ elements fs
    notif <- pick $ Gen.notifications
    nk <- runPersistCmd $ saveFeedbackNotification fk notif
    (mck,mfk,users) <- runPersistCmd $ do
      mck <- commentOfNotification nk
      mfk <- feedbackOfNotification nk
      users <- usersOfNotification nk
      return (mck,mfk,users)
    assertEquals Nothing   mck   "Feedback notification has a comment key."
    assertEquals (Just fk) mfk   "Feedback notification has no notification key."
    assertEquals []        users "Feedback notification had a non-empty users list."

-- All the system notification does not returns an feedback or comment key, and they are
-- associated to the users
attachedSystemNotificationTest = test $ testCase "System notifications with attached users" $ do
  reinitPersistence
  us <- users 400
  ns <- systemNotifications 1500
  quick 1000 $ do
    user <- pick $ elements us
    nk <- pick $ elements ns
    (mck,mfk,users,nks) <- runPersistCmd $ do
      attachNotificationToUser user nk
      mck <- commentOfNotification nk
      mfk <- feedbackOfNotification nk
      users <- usersOfNotification nk
      nks   <- notificationsOfUser user
      return (mck,mfk,users,nks)
    assertEquals Nothing   mck     "System notification has no comment key."
    assertEquals Nothing   mfk     "System notification has no notification key."
    assertTrue   (elem user users) "System notification is not associated with the selected user on the notification side."
    assertTrue   (elem nk   nks)   "System notification is not associated with the selected user on the user side."

attachedNotificationTest = test $ testCase "Notifications with attached users" $ do
  reinitPersistence
  us <- users 400
  cs <- courses 50
  gs <- groups 200 cs
  as <- courseAndGroupAssignments 300 300 cs gs
  ss <- submissions 500 us as
  cks <- comments 200 (map snd ss)
  fs <- feedbacks 200 (map snd ss)
  cns <- map fst <$> commentNotifications 600 (map fst cks)
  fns <- map fst <$> feedbackNotifications 600 (map fst fs)
  sns <- systemNotifications 600
  let ns = cns ++ fns ++ sns
  let sfns = sns ++ fns
  let scns = sns ++ cns
  quick 1000 $ do
    user <- pick $ elements us
    nk   <- pick $ elements ns
    (mck,mfk,users,nks) <- runPersistCmd $ do
      attachNotificationToUser user nk
      mck <- commentOfNotification nk
      mfk <- feedbackOfNotification nk
      users <- usersOfNotification nk
      nks   <- notificationsOfUser user
      return (mck,mfk,users,nks)
    assertTrue   (elem user users) "System notification is not associated with the selected user on the notification side."
    assertTrue   (elem nk   nks)   "System notification is not associated with the selected user on the user side."
    case mck of
      Nothing -> assertTrue (elem nk sfns) "A non commented notification had a comment."
      Just ck -> assertTrue (elem nk cns) "A commented notification lost its comment key."
    case mfk of
      Nothing -> assertTrue (elem nk scns) ("A non feedback notification had a feedback." ++ show nk)
      Just fk -> assertTrue (elem nk fns) "A feedback notification lost its feedback key."
-}

-- * Run persistent command

-- TODO: FIX this dirty hack to instatiate only once the persistent layer
persistRef :: IORef Interpreter
persistRef = unsafePerformIO $ newIORef undefined

createInterpreter :: IO ()
createInterpreter = do
  interp <- createPersistInterpreter defaultConfig
  writeIORef persistRef interp

getPersistInterpreter :: IO Interpreter
getPersistInterpreter = readIORef persistRef

runPersistCmd :: Persist a -> PropertyM IO a
runPersistCmd m = do
  interp <- run getPersistInterpreter
  x <- run $ runPersist interp m
  case x of
    Left msg -> fail msg
    Right x  -> return x

runPersistIOCmd :: Persist a -> IO a
runPersistIOCmd m = do
  interp <- getPersistInterpreter
  x <- runPersist interp m
  case x of
    Left msg -> fail msg
    Right x  -> return x

startDate :: UTCTime
startDate = read "2013-03-01 12:00:00"

endDate :: UTCTime
endDate = read "2013-03-30 12:00:00"

dates :: [UTCTime]
dates = map (\sec -> addUTCTime (fromIntegral sec) startDate) ([0..] :: [Int])

tests = do
  ioTest "Init persist interpreter" createInterpreter
  propertyTests
  massTests
  complexTests


propertyTests = group "Persistence Layer QuickCheck properties" $ do
  add initPersistenceLayer
  add $ testProperty "Assignment Save and Load" $ monadicIO $ void assignmentSaveAndLoad
  add $ testProperty "Course Save and Load" $ monadicIO $ void courseSaveAndLoad
  add $ testProperty "Group Save and Load" $ monadicIO groupSaveAndLoad
  add $ testProperty "Course Assignment Save and Load" $ monadicIO $ void courseAssignmentSaveAndLoad
  add $ testProperty "Group Assignment Save and Load" $ monadicIO $ void groupAssignmentSaveAndLoad
  add $ testProperty "User Save and Load" $ monadicIO (pick Gen.users >>= userSaveAndLoad)
  add $ testProperty "Multiple groups for course" $ monadicIO $ void multipleGroupsForCourse
  add $ testProperty "Submission Save and Load" $ monadicIO $ void saveAndLoadSubmission
  add $ testProperty "Assignment and user of submission" $ monadicIO assignmentAndUserOfSubmission
  add $ testProperty "Comment save and load" $ monadicIO saveAndLoadComment
  add $ testProperty "Evaluation save and load" $ monadicIO $ void evaluationGroupSaveAndLoad
  add cleanUpPersistence


massTests = group "Persistence Layer Mass tests" $ do
  test initPersistenceLayer
  test massTest
  test massTestParallel
  test cleanUpPersistence

complexTests :: TestSet ()
complexTests = group "Persistence Layer Complex tests" $ do
  test initPersistenceLayer
  userAssignmentKeyTest
  userAssessmentKeyTest
  courseOrGroupAssignmentTest
  courseAndGroupAssignmentTest
  mossScriptInvocationSaveLoadTest
  submissionDescTest
  lastEvaluationTest
  submissionDetailsDescTest
  submissionTablesTest
  courseKeysTest
  courseAdminsTests
  groupAdminsTests
  allAdminsTests
  groupsTests
  loadGroupAndCourseTests
  userGroupsTest
  assignmentKeyTest
  filterSubmissionsTest
  filterTextualSubmissionTest
  submissionsTest
  modifyAssignmentsTest
  modifyEvaluationTest
  deleteUsersFromCourseTest
  deleteUsersFromCourseNegativeTest
  unsubscribeFromSubscribedGroupsTest
  saveLoadAndModifyTestScriptsTest
  saveLoadAndModifyTestCasesTest
  testCaseOfAssignmentTest
  userFileSaveTest
  userFileCopyTest
  userOverwriteFileTest
  testJobCreationTest
  queuedForTestFeedbackTest
  finalizeFeedbacksTest
  openSubmissionsTest
  assessmentTests
  unevaluatedScoresTests
  scoreEvaluationTests
{- XXX
  saveCommentNotificationTest
  saveFeedbackNotificationTest
  attachedSystemNotificationTest
  attachedNotificationTest
-}
  test cleanUpPersistence

monadicProperty gen prop = monadicIO (forAllM gen prop)

initPersistenceLayer = testCase "Initialization" $ do
  init <- createPersistInit defaultConfig
  initPersist init

cleanUpPersistence = testCase "Clean up" $ do
  init <- createPersistInit defaultConfig
  tearDown init

-- Fails if the two given list does not represent the same set
assertSetEquals :: (Monad m, Show a, Eq a, Ord a) => [a] -> [a] -> String -> m ()
assertSetEquals xs ys msg = assertEquals
 (Set.fromList xs) (Set.fromList ys) (concat [msg, " ", show xs, " ", show ys])

-- The test will fail with the given message, if the given values are different
assertEquals :: (Monad m, Eq a) => a -> a -> String -> m ()
assertEquals x y msg
  | x == y    = return ()
  | otherwise = fail msg

-- The test will fail with the given message, if the boolean value is false
assertTrue :: (Monad m) => Bool -> String -> m ()
assertTrue True  _   = return ()
assertTrue False msg = fail msg

-- The test will fail with the given message, if the boolean value is true
assertFalse :: (Monad m) => Bool -> String -> m ()
assertFalse False _ = return ()
assertFalse True msg = fail msg

-- The test will fail with the given message, if the list is null
assertNonEmpty :: (Monad m) => [a] -> String -> m ()
assertNonEmpty [] msg = fail msg
assertNonEmpty _ _ = return ()

-- The test will fail with the given message, if the list is not empty
assertEmpty :: (Monad m) => [a] -> String -> m ()
assertEmpty [] _ = return ()
assertEmpty _ msg = fail msg

whenM :: (Monad m) => m Bool -> m () -> m ()
whenM m a = do
  x <- m
  when x a

runPersistTestSet t = do
  test initPersistenceLayer
  test t
  test cleanUpPersistence

-- * Helpers

utcTimeConstant :: UTCTime
utcTimeConstant = read "2015-08-27 17:08:58 UTC"

-- * Consistent data generation

createTestData n = do
  createInterpreter
  let noOfUsers = 1100
  let noOfCourses = 14 * n
  let noOfGroups = 6 * noOfCourses
  print "Init persistence ..."
  initPersistence
  print "Creating users ..."
  us <- users (1100 * n)
  print "Creating courses ..."
  cs <- courses (14 * n)
  print "Creating groups ..."
  gs <- groups noOfGroups cs

  print "Subscribing users to groups ..."
  quick (4 * noOfUsers) $ do
    gk <- pick $ elements gs
    u  <- pick $ elements us
    runPersistCmd $ subscribe u gk

  as <- courseAndGroupAssignments (8 * noOfCourses) (8 * noOfGroups) cs gs
  let noOfSubmissions = 15000 * n
  print "Creating submissions ..."
  quick noOfSubmissions $ do
    u <- pick $ elements us
    subscribed <- runPersistCmd $ userGroupKeys u
    gk <- pick $ elements subscribed
    aks <- runPersistCmd $ userAssignmentKeyList u gk
    when (not $ null aks) $ do
      ak <- pick $ elements aks
      s  <- pick $ Gen.submissions startDate
      sk <- runPersistCmd $ saveSubmission ak u s
      return ()

  print "Creating comments ..."
  quick (3 * noOfSubmissions) $ do
    u <- pick $ elements us
    subscribed <- runPersistCmd $ userGroupKeys u
    gk <- pick $ elements subscribed
    aks <- runPersistCmd $ userAssignmentKeyList u gk
    when (not $ null aks) $ do
      ak <- pick $ elements aks
      sks <- runPersistCmd $ userSubmissions u ak
      when (not $ null sks) $ do
        sk <- pick $ elements sks
        c  <- pick $ Gen.comments startDate
        ck <- runPersistCmd $ saveComment sk c
        return ()

  print "Creating evaluations ..."
  quick (4 * noOfSubmissions) $ do
    u <- pick $ elements us
    subscribed <- runPersistCmd $ userGroupKeys u
    gk <- pick $ elements subscribed
    aks <- runPersistCmd $ userAssignmentKeyList u gk
    when (not $ null aks) $ do
      ak <- pick $ elements aks
      sks <- runPersistCmd $ userSubmissions u ak
      when (not $ null sks) $ do
        sk <- pick $ elements sks
        cfg <- evaluationConfigForSubmission sk
        e <- pick $ Gen.evaluations cfg
        runPersistCmd $ saveSubmissionEvaluation sk e
        return ()

  where
    userAssignmentKeyList :: Username -> GroupKey -> Persist [AssignmentKey]
    userAssignmentKeyList u gk =
      (\(asgs, _) -> [ ak | (ak, _, _, _) <- asgs ]) <$>
        userAssignmentsAssessments u gk

