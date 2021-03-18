{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Bead.Controller.UserStories where

import           Bead.Domain.Entities hiding (name, uid)
import qualified Bead.Domain.Entities as Entity (uid)
import qualified Bead.Domain.Entity.Assignment as Assignment
import qualified Bead.Domain.Entity.Assessment as Assessment
import qualified Bead.Domain.Entity.Notification as Notification
import           Bead.Domain.Relationships as Rel
import           Bead.Domain.RolePermission (permission)
import           Bead.Controller.ServiceContext (ServiceContext, UserState)
import qualified Bead.Controller.ServiceContext as SC
import           Bead.Controller.Logging  as L
import           Bead.Controller.Pages    as P hiding (modifyEvaluation,modifyAssessment)
import           Bead.Persistence.Persist (Persist)
import qualified Bead.Persistence.Persist   as Persist
import qualified Bead.Persistence.Relations as Persist
import qualified Bead.Persistence.Guards    as Persist
import           Bead.View.Translation

import           Control.Lens (_1, view)
import           Control.Applicative
import           Control.Exception
import           Control.Monad hiding (guard)
import qualified Control.Monad.State  as CMS
import qualified Control.Monad.Except as CME
import qualified Control.Monad.Reader as CMR
import           Control.Monad.Trans
import           Prelude hiding (log, userError)
import           Data.ByteString (ByteString)
import           Data.Hashable
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Foldable (foldrM)
import           Data.Function (on)
import           Data.List (nub, (\\))
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes, fromMaybe, isJust)
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.String
import           Data.Time (UTCTime(..), getCurrentTime)
import           Data.UUID (UUID)
import qualified Data.UUID as UUID (toString) 
import qualified Data.UUID.V4 as UUID
import           Numeric (showHex)
import           Text.Printf (printf)

-- User error can be a message that need to be displayed, or
-- a parametrized message with a string parameter that needs
-- to be resolved in the place where the message is rendered
newtype UserError = UserError TransMsg
#ifdef TEST
  deriving Show
#endif

-- Template method for the UserError functions
userErrorCata f (UserError t) = f t

-- Creates a user error that contains a non-parametrized message
userError :: Translation String -> UserError
userError = UserError . TransMsg

-- Creates a user error that contains a parametrized message, with one parameter
userParamError :: Translation String -> String -> UserError
userParamError t p = UserError (TransPrmMsg t p)

-- Creates a user error that contains a parametrized message, with 2 parameters
userPrm2Error :: Translation String -> String -> String -> UserError
userPrm2Error t p1 p2 = UserError (TransPrm2Msg t p1 p2)

-- Creates a user error that contains a parametrized message, with 3 parameters
userPrm3Error :: Translation String -> String -> String -> String -> UserError
userPrm3Error t p1 p2 p3 = UserError (TransPrm3Msg t p1 p2 p3)

-- Translates the given user error with the given translation function,
-- applying the parameters if necessary to the parametrized messages
translateUserError :: (Translation String -> String) -> UserError -> String
translateUserError = userErrorCata . translateMessage

userErrorNoMsg :: UserError
userErrorNoMsg = userError (msg_UserStoryError_UnknownError "Unknown Error: No message.")

userErrorWithMsg :: String -> UserError
userErrorWithMsg msg = userParamError (msg_UserStoryError_Message "Some error happened: %s") msg

-- The User Story Context contains a service context and the localization transformation.
-- The service context is used for user manipulation.
-- The localization is used for translation of the messages that will be stored in
--  the persistence layer
type UserStoryContext = (ServiceContext, I18N)

newtype UserStory a = UserStory {
    unStory :: CMR.ReaderT UserStoryContext (CMS.StateT UserState (CME.ExceptT UserError IO)) a
  } deriving (Monad, CMS.MonadState UserState
                   , CME.MonadError UserError
                   , CMR.MonadReader UserStoryContext
                   , Functor
                   , Applicative
                   , MonadIO)

runUserStory
  :: ServiceContext
  -> I18N
  -> UserState
  -> UserStory a
  -> IO (Either UserError (a, UserState))
runUserStory context i18n userState
  = CME.runExceptT
  . flip CMS.runStateT userState
  . flip CMR.runReaderT (context, i18n)
  . unStory

-- * High level user stories

-- | The user logs in with a given username
login :: Username -> UserStory User
login username = 
  withUsername username $ \uname -> do
    logMessage INFO $ unwords [uname, "is trying to log in"]
    validUser <- persistence $ Persist.doesUserExist username
    if validUser
      then do
        sessionId <- liftIO $ UUID.nextRandom
        logMessage INFO $ concat [uname, " logs in with session ", UUID.toString sessionId]
        loadUserData username sessionId
      else
        errorPage . userError $ msg_UserStoryError_InvalidUsernameOrPassword "Invalid username or password."

logout :: Language -> UserStory ()
logout defaultLanguage = logAction INFO "logs out" $
  changeUserState (SC.userNotLoggedIn . fromMaybe defaultLanguage . SC.getLanguage)

doesUserExist :: Username -> UserStory Bool
doesUserExist u = logAction INFO ("searches after user " ++ show u) $ do
  authorize P_Open P_User
  persistence $ Persist.doesUserExist u

-- | The authorized user creates a new user
createUser :: User -> UserStory ()
createUser newUser = do
  authorize P_Create P_User
  persistence $ Persist.saveUser newUser
  logger      <- asksLogger
  liftIO $ log logger INFO $ "User is created: " ++ show (u_username newUser)

-- Updates the current user's full name, timezone and language in the persistence layer
changeUserDetails :: String -> TimeZoneName -> Language -> UserStory ()
changeUserDetails name timezone language = logAction INFO ("changes fullname, timezone and language") $ do
  user <- currentUser
  changeUserState $ SC.setTimeZone timezone . SC.setLanguage language
  persistence $ Persist.updateUser user { u_name = name , u_timezone = timezone , u_language = language }
  putStatusMessage $ msg_UserStory_ChangedUserDetails "The user details have been updated."

-- Updates the user information
updateUser :: User -> UserStory ()
updateUser u = logAction INFO ("updates user " ++ (usernameCata id $ u_username u)) $ do
  authorize P_Modify P_User
  persistence $ Persist.updateUser u

-- | Selecting users that satisfy the given criteria
selectUsers :: (User -> Bool) -> UserStory [User]
selectUsers f = logAction INFO "selects some users" $ do
  authorize P_Open P_User
  persistence $ Persist.filterUsers f

-- | Load another user's data if the current user is authorized to open
-- other users' profile
loadUser :: Username -> UserStory User
loadUser u = logAction INFO "Loading user information" $ do
  authorize P_Open P_User
  persistence $ Persist.loadUser u

loadUserDesc :: Username -> UserStory UserDesc
loadUserDesc u = mkUserDescription <$> loadUser u

uidToUsername :: Uid -> UserStory Username
uidToUsername uid = logAction INFO ("Loads username of uid " ++ show uid) $ do
  authorize P_Open P_User
  persistence $ Persist.uidToUsername uid

-- Returns the username who is active in the current userstory
username :: UserStory Username
username = CMS.gets SC.usernameInState

-- The UserStory calculation returns the current user's profile data
currentUser :: UserStory User
currentUser = logAction INFO "Load the current user's data" $ do
  u <- SC.usernameInState <$> userState
  persistence $ Persist.loadUser u

-- Saves (copies) a file to the user's directory from the given filepath.
-- If the user has no permission for the uploading an error is thrown
copyUsersFile :: FilePath -> UsersFile FilePath -> UserStory ()
copyUsersFile tempPath usersfile = logAction INFO logMessage $ do
  authorize P_Create P_File
  u <- username
  persistence $ Persist.copyFile u tempPath usersfile
  where
    msg file = " uploads a file " ++ show file
    logMessage = usersFile msg msg usersfile

-- Saves a file to the user's directory with given name and contents.
-- If the user has no permission for the uploading an error is thrown
saveUsersFile :: FilePath -> UsersFile ByteString -> UserStory ()
saveUsersFile filename contents = logAction INFO logMessage $ do
  authorize P_Create P_File
  u <- username
  persistence $ Persist.saveFile u filename contents
  where
    logMessage = " uploads a file " ++ filename

-- List all the user's file. If the user has no permission for the listing
-- of files an error is thrown
listUsersFiles :: UserStory [(UsersFile FilePath, FileInfo)]
listUsersFiles = logAction INFO " lists all his files" $ do
  authorize P_Open P_File
  u <- username
  persistence $ Persist.listFiles u

-- Returns the user's data file real path, for further processing, if
-- the user has authentication, otherwise throws an error page
getFilePath :: UsersFile FilePath -> UserStory FilePath
getFilePath usersfile = logAction INFO logMessage $ do
  authorize P_Open P_File
  u <- username
  persistence $ Persist.getFile u usersfile
  where
    msg u = " asks the file path: " ++ show u
    logMessage = usersFile msg msg usersfile

-- Produces true if the given user is the student of the courses or groups which
-- the actual user administrates.
isStudentOfMine :: Username -> UserStory Bool
isStudentOfMine student = logAction INFO (concat ["Student ", usernameCata id student, " of the actual user"]) $ do
  authorize P_Modify P_StudentPassword
  u <- username
  persistence $ Persist.isStudentOf student u

administratedCourses :: UserStory [(CourseKey, Course)]
administratedCourses = logAction INFO "selects adminstrated courses" $ do
  authorize P_Open P_Course
  u <- username
  persistence $ Persist.administratedCourses u

-- Produces a list of group keys, group and the full name of the group
administratedGroups :: UserStory [(CourseKey, Course, [(GroupKey, Group)])]
administratedGroups = logAction INFO "selects administrated groups" $ do
  authorize P_Open P_Group
  u <- username
  persistence $ Persist.administratedGroups u

-- | The 'create' function is an abstract function
--   for other creators like, createCourse and createExercise
create
  :: (PermissionObj o)
  => (o -> v -> String)      -- ^ Descriptor for the logger
  -> o                       -- ^ The object to save
  -> (o -> Persist v)        -- ^ Saver function of the persistence
  -> UserStory v
create descriptor object saver = do
  authorize P_Create (permissionObject object)
  value <- persistence (saver object)
  logMessage INFO $ descriptor object value
  return value

#ifndef SSO
createUserReg :: UserRegistration -> UserStory UserRegKey
createUserReg u = logAction INFO "Creates user registration" $ do
  create descriptor u Persist.saveUserReg
  where
    descriptor x _ = reg_username x

loadUserReg :: UserRegKey -> UserStory UserRegistration
loadUserReg k = logAction INFO "Loading user registration" $ do
  authorize P_Open P_UserReg
  persistence $ Persist.loadUserReg k
#endif

-- | Creates a new course
createCourse :: Course -> UserStory CourseKey
createCourse course = logAction INFO "creates course" $ do
  authorize P_Create P_Course
  key <- create descriptor course Persist.saveCourse
  putStatusMessage $ msg_UserStory_CreateCourse "The course has been created."
  return key
  where
    descriptor course _ =
      printf "Course is created: %s"
        (show (courseName course))

selectCourses :: (CourseKey -> Course -> Bool) -> UserStory [(CourseKey, Course)]
selectCourses f = logAction INFO "selects some courses" $ do
  authorize P_Open P_Course
  persistence $ Persist.filterCourses f

loadCourse :: CourseKey -> UserStory (Course,[GroupKey])
loadCourse k = logAction INFO ("loads course: " ++ show k) $ do
  authorize P_Open P_Course
  persistence $ do
    c  <- Persist.loadCourse k
    ks <- Persist.groupKeysOfCourse k
    return (c,ks)

createCourseAdmin :: Username -> CourseKey -> UserStory ()
createCourseAdmin user ck = logAction INFO "sets user to course admin" $ do
  authorize P_Create P_CourseAdmin
  authorize P_Open   P_User
  persistence $ do
    cas <- Persist.courseAdminKeys ck
    Persist.createCourseAdmin user ck
    c <- Persist.loadCourse ck
    now <- liftIO getCurrentTime
    let msg = Notification.NE_CourseAdminCreated (courseName c)
    let affected = [user]
    Persist.notifyUsers (Notification.Notification msg now Notification.System) affected
    userUser <- Persist.loadUser user
    let msg = Notification.NE_CourseAdminAssigned (courseName c) (u_name userUser)
    let affected = cas
    Persist.notifyUsers (Notification.Notification msg now Notification.System) affected
  putStatusMessage $ msg_UserStory_SetCourseAdmin "The user has become a course administrator."

-- Produces a list of courses and users who administrators for the courses
allCourseAdministrators :: UserStory [(Course, [User])]
allCourseAdministrators = logAction INFO "lists courses and their admins" $ do
  authorize P_Open P_Course
  authorize P_Open P_User
  persistence $ do
    Persist.filterCourses (\_ _ -> True) >>= (mapM $ \(ck,c) -> do
      admins <- Persist.courseAdmins ck
      return (c, admins))

allAdministrators :: UserStory [User]
allAdministrators = logAction INFO "lists all admins" $ do
  authorize P_Open P_Group
  authorize P_Open P_User
  persistence $ Persist.allAdministrators

groupAdmins :: GroupKey -> UserStory [User]
groupAdmins gk = logAction INFO ("lists group admins of group " ++ show gk) $ do
  authorize P_Open P_Group
  authorize P_Open P_User
  persistence $ Persist.groupAdmins gk

-- Deletes the given users from the given course if the current user is a course
-- admin for the given course, otherwise redirects to the error page
deleteUsersFromCourse :: CourseKey -> [Username] -> UserStory ()
deleteUsersFromCourse ck sts = logAction INFO ("deletes users from course: " ++ show ck) $ do
  authorize P_Modify P_Course
  u <- username
  join $ persistence $ do
    admined <- Persist.isAdministratedCourse u ck
    if admined
      then do mapM_ (Persist.deleteUserFromCourse ck) sts
              return . putStatusMessage $
                msg_UserStory_UsersAreDeletedFromCourse "The students have been removed from the course."
      else return $ do
             logMessage INFO . violation $ printf "The user tries to delete users from a course (%s) which not belongs to him" (courseKeyMap id ck)
             errorPage . userError $ msg_UserStoryError_NoCourseAdminOfCourse "The user is not course admin for the course."

-- Saves the given test script associated with the given course, if the
-- current user have authorization for the operation and if he administrates the
-- course given in the parameter. If authorization violation happens the page
-- redirects to the error page
saveTestScript :: CourseKey -> TestScript -> UserStory ()
saveTestScript ck ts = logAction INFO ("creates new test script for course: " ++ show ck) $ do
  authorize P_Create P_TestScript
  join $ withUserAndPersist $ \u -> do
    let user = u_username u
    cs <- map fst <$> Persist.administratedCourses user
    case ck `elem` cs of
      False -> return . errorPage . userError $ msg_UserStoryError_NoCourseAdminOfCourse "The user is not course admin for the course."
      True -> do
        Persist.saveTestScript ck ts
        now <- liftIO getCurrentTime
        c   <- Persist.loadCourse ck
        let msg = Notification.NE_TestScriptCreated (u_name u) (courseName c)
        cas <- Persist.courseAdminKeys ck
        gks <- Persist.groupKeysOfCourse ck
        gas <- concat <$> mapM Persist.groupAdminKeys gks
        let affected = nub (cas ++ gas) \\ [user]
        Persist.notifyUsers (Notification.Notification msg now Notification.System) affected
        return . putStatusMessage $
          msg_UserStory_NewTestScriptIsCreated "The test script has been created."

-- Overwrite the test script with the given one if the current user administrates
-- the course that are of the given test script otherwise redirects to the error page
modifyTestScript :: TestScriptKey -> TestScript -> UserStory ()
modifyTestScript tsk ts = logAction INFO ("modifies the existing test script: " ++ show tsk) $ do
  authorize P_Modify P_TestScript
  join $ withUserAndPersist $ \u -> do
    let user = u_username u
    cs <- map fst <$> Persist.administratedCourses user
    ck <- Persist.courseOfTestScript tsk
    case ck `elem` cs of
      False -> return . errorPage . userError $ msg_UserStoryError_NoAssociatedTestScript "You are trying to modify someone else's test script."
      True -> do
        Persist.modifyTestScript tsk ts
        now <- liftIO getCurrentTime
        c   <- Persist.loadCourse ck
        let msg = Notification.NE_TestScriptUpdated (u_name u) (tsName ts) (courseName c)
        cas <- Persist.courseAdminKeys ck
        gks <- Persist.groupKeysOfCourse ck
        gas <- concat <$> mapM Persist.groupAdminKeys gks
        let affected = nub (cas ++ gas) \\ [user]
        Persist.notifyUsers (Notification.Notification msg now Notification.System) affected
        return . putStatusMessage $
          msg_UserStory_ModifyTestScriptIsDone "The test script has been updated."

-- | Loads the test script if the user has authorization for the load, and
-- otherwise redirects to the error page
loadTestScript :: TestScriptKey -> UserStory (TestScript, CourseKey)
loadTestScript tsk = logAction INFO ("loads the test script: " ++ show tsk) $ do
  authorize P_Open P_TestScript
  persistence $ do
    ck <- Persist.courseOfTestScript tsk
    ts <- Persist.loadTestScript tsk
    return (ts, ck)

-- | Returns Just test case key and test case for the given assignment if there any, otherwise Nothing
testCaseOfAssignment :: AssignmentKey -> UserStory (Maybe (TestCaseKey, TestCase, TestScriptKey))
testCaseOfAssignment ak = logAction INFO ("loads the test case for assignment: " ++ show ak) $ do
  persistence $ do
    mtk <- Persist.testCaseOfAssignment ak
    maybe
      (return Nothing)
      (\tk -> do tc  <- Persist.loadTestCase tk
                 tsk <- Persist.testScriptOfTestCase tk
                 return (Just (tk, tc, tsk)))
      mtk

hasAssignmentTestCase :: AssignmentKey -> UserStory HasTestCase
hasAssignmentTestCase ak = logAction INFO (unwords ["checks whether assignment", show ak, "has test case"]) $
  persistence $ maybe DoesNotHaveTestCase (const HasTestCase) <$> (Persist.testCaseOfAssignment ak)

-- | Returns the test scripts of the given assignments, that are attached to the course of the assignment
testScriptInfosOfAssignment :: AssignmentKey -> UserStory [(TestScriptKey, TestScriptInfo)]
testScriptInfosOfAssignment ak = do
  authorize P_Open P_TestScript
  persistence $ do
    keys <- Persist.courseOrGroupOfAssignment ak
    ck   <- either (return) Persist.courseOfGroup keys
    tsks <- Persist.testScriptsOfCourse ck
    tss  <- mapM loadTestScriptWithKey tsks
    return tss
  where
    loadTestScriptWithKey tk = do
      ti <- Persist.testScriptInfo tk
      return (tk, ti)

-- | Returns the test scripts of the given group, that are arrached to the course of the group
testScriptInfosOfGroup :: GroupKey -> UserStory [(TestScriptKey, TestScriptInfo)]
testScriptInfosOfGroup gk = do
  authorize P_Open P_TestScript
  persistence $ do
    ck   <- Persist.courseOfGroup gk
    tsks <- Persist.testScriptsOfCourse ck
    tss  <- mapM loadTestScriptWithKey tsks
    return tss
  where
    loadTestScriptWithKey tk = do
      ti <- Persist.testScriptInfo tk
      return (tk, ti)

-- | Returns the test scripts of the given course
testScriptInfosOfCourse :: CourseKey -> UserStory [(TestScriptKey, TestScriptInfo)]
testScriptInfosOfCourse ck = do
  authorize P_Open P_TestScript
  persistence $ do
    tsks <- Persist.testScriptsOfCourse ck
    tss  <- mapM loadTestScriptWithKey tsks
    return tss
  where
    loadTestScriptWithKey tk = do
      ti <- Persist.testScriptInfo tk
      return (tk, ti)

-- Deletes the given users from the given group if the current user is a group
-- admin for the given group, otherwise redirects to the error page
deleteUsersFromGroup :: GroupKey -> [Username] -> UserStory ()
deleteUsersFromGroup gk sts = logAction INFO ("delets users form group: " ++ show gk) $ do
  authorize P_Modify P_Group
  join $ withUserAndPersist $ \u -> do
    let user = u_username u
    admined <- Persist.isAdminOfGroupOrCourse user gk
    if admined
      then do ck <- Persist.courseOfGroup gk
              mapM_ (\student -> Persist.unsubscribe student gk) sts
              now <- liftIO getCurrentTime
              g   <- Persist.loadGroup gk
              let msg = Notification.NE_RemovedFromGroup (groupName g) (u_name u)
              let affected = sts
              Persist.notifyUsers (Notification.Notification msg now Notification.System) affected
              return . putStatusMessage $
                msg_UserStory_UsersAreDeletedFromGroup "The students have been removed from the group."
      else return $ do
             logMessage INFO . violation $ printf "The user tries to delete users from group (%s) which is not administrated by him" (groupKeyMap id gk)
             errorPage . userError $ msg_UserStoryError_NoGroupAdminOfGroup "You are not a group admin for the group."

createGroupAdmin :: Username -> GroupKey -> UserStory ()
createGroupAdmin user gk = logAction INFO "sets user as a group admin of a group" $ do
  authorize P_Create P_GroupAdmin
  authorize P_Open   P_User
  let uname = usernameCata id user
  admin <- username
  join . persistence $ do
    info <- Persist.personalInfo user
    withPersonalInfo info $ \role _name _tz _lang _ui -> do
      admined <- Persist.isAdministratedCourseOfGroup admin gk
      if (and [groupAdmin role, admined])
        then do Persist.createGroupAdmin user gk
                now <- liftIO getCurrentTime
                g   <- Persist.loadGroup gk
                ck  <- Persist.courseOfGroup gk
                c   <- Persist.loadCourse ck
                adminUser <- Persist.loadUser admin
                userUser  <- Persist.loadUser user
                let userName  = u_name userUser
                let adminName = u_name adminUser
                let msg = Notification.NE_GroupAdminCreated
                            (courseName c) adminName (groupName g)
                let affected = [user] \\ [admin]
                Persist.notifyUsers (Notification.Notification msg now Notification.System) affected
                let username = usernameCata id user
                let msg = Notification.NE_GroupAssigned (groupName g) (courseName c) adminName userName
                cas <- Persist.courseAdminKeys ck
                let affected = cas \\ [admin]
                Persist.notifyUsers (Notification.Notification msg now Notification.System) affected
                return $ putStatusMessage $ msg_UserStory_SetGroupAdmin "The user has become a teacher."
        else return . CME.throwError $ userParamError (msg_UserStoryError_NoGroupAdmin "%s is not a group admin!") $ uname

-- Unsubscribes the student from the given group (and course) if the group is one of the student's group
-- and the sutdent did not submit any solutions for the assignments of the group. In that
-- case the error page is rendered
unsubscribeFromCourse :: GroupKey -> UserStory ()
unsubscribeFromCourse gk = logAction INFO ("unsubscribes from group: " ++ show gk) $ do
  u <- username
  join $ persistence $ do
    registered <- Persist.isUserInGroup u gk
    case registered of
      False -> return . errorPage . userError $ msg_UserStoryError_NoGroupAdminOfGroup "You are not group admin for the group."
      True -> do
        ck <- Persist.courseOfGroup gk
        s <- (&&) <$> Persist.isThereASubmissionForGroup u gk
                  <*> Persist.isThereASubmissionForCourse u ck
        if s then (return . errorPage . userError $ msg_UserStoryError_AlreadyHasSubmission "You have already submitted some solution for the assignments of the course.")
             else do
               Persist.unsubscribe u gk
               return . putStatusMessage $
                 msg_UserStory_SuccessfulCourseUnsubscription "Unregistration was successful."

-- | Adds a new group to the given course
createGroup :: CourseKey -> Group -> UserStory GroupKey
createGroup ck g = logAction INFO ("creats group " ++ show (groupName g)) $ do
  authorize P_Create P_Group
  join $ withUserAndPersist $ \u -> do
    let user = u_username u
    admined <- Persist.isAdministratedCourse user ck
    if admined
      then do key <- Persist.saveGroup ck g
              now <- liftIO getCurrentTime
              c   <- Persist.loadCourse ck
              let msg = Notification.NE_GroupCreated (courseName c) (u_name u) (groupName g)
              cas <- Persist.courseAdminKeys ck
              let affected = cas \\ [user]
              Persist.notifyUsers (Notification.Notification msg now Notification.System) affected
              return $ do
                putStatusMessage $ msg_UserStory_CreateGroup "The group has been created."
                return key
      else return . errorPage $ userError nonAdministratedCourse

loadGroup :: GroupKey -> UserStory Group
loadGroup gk = logAction INFO ("loads group " ++ show gk) $ do
  authorize P_Open P_Group
  persistence $ Persist.loadGroup gk

loadGroupAndCourse :: GroupKey -> UserStory (CourseKey, Course, GroupKey, Group)
loadGroupAndCourse gk = logAction INFO ("loads group and course for " ++ show gk) $ do
  authorize P_Open P_Group
  persistence $ Persist.loadGroupAndCourse gk

-- | Checks is the user is subscribed for the group
isUserInGroup :: GroupKey -> UserStory Bool
isUserInGroup gk = logAction INFO ("checks if user is in the group " ++ show gk) $ do
  authorize P_Open P_Group
  state <- userState
  persistence $ Persist.isUserInGroup (SC.usernameInState state) gk

courseOfGroup :: GroupKey -> UserStory CourseKey
courseOfGroup gk = logAction INFO ("gets course of the group " ++ show gk) $ do
  authorize P_Open P_Group
  persistence $ Persist.courseOfGroup gk

-- | Checks if the user is subscribed for the course
isUserInCourse :: CourseKey -> UserStory Bool
isUserInCourse ck = logAction INFO ("checks if user is in the course " ++ show ck) $ do
  authorize P_Open P_Course
  state <- userState
  persistence $ Persist.isUserInCourse (SC.usernameInState state) ck

-- | Lists all users subscribed for the given course
subscribedToCourse :: CourseKey -> UserStory [Username]
subscribedToCourse ck = logAction INFO ("lists all users in course " ++ show ck) $ do
  authorize P_Open P_Course
  isCourseOrGroupAdmin ck
  persistence $ Persist.subscribedToCourse ck

-- | Lists all users subscribed for the given group
subscribedToGroup :: GroupKey -> UserStory [Username]
subscribedToGroup gk = logAction INFO ("lists all users in group " ++ show gk) $ do
  authorize P_Open P_Group
  isAdminOfGroupOrCourse gk
  persistence $ Persist.subscribedToGroup gk


-- | Register the user in the group, if the user has not submitted
-- any solutions for other groups of the actual course, otherwise
-- puts a message on the UI indicating that the group change is
-- not allowed.
subscribeToGroup :: GroupKey -> UserStory Bool
subscribeToGroup gk = logAction INFO ("subscribes to the group " ++ (show gk)) $ do
  authorize P_Open P_Group
  state <- userState
  (message, success) <- persistence $ do
    let u = SC.usernameInState state
    ck  <- Persist.courseOfGroup gk
    gks <- Persist.groupsOfUsersCourse u ck
    hasSubmission <- isThereASubmission u gks
    case hasSubmission of
      True -> return $ (msg_UserStory_SubscribedToGroup_ChangeNotAllowed
        "It is not possible to move between groups as there are submissions for the current group.", False)
      False -> do
        mapM_ (Persist.unsubscribe u) gks
        Persist.subscribe u gk
        return $ (msg_UserStory_SubscribedToGroup "Successful registration.", True)
  putStatusMessage message
  return success
  where
    isThereASubmission :: Username -> HashSet GroupKey -> Persist Bool
    isThereASubmission u gks =
      foldM
        (\found gk ->
          if found
          then return found
          else do
            aks <- Persist.groupAssignments gk
            any isJust <$> mapM (flip Persist.lastSubmission u) aks
        )
        False
        gks

-- Returns for each subscribed group the group key, the group itself, admins and
-- a boolean value indicating that the user already submitted a
-- solution for the group or the course of the group
attendedGroups :: UserStory [(Course, GroupKey, Group, [User], Bool)]
attendedGroups = logAction INFO "lists subscribed groups with info" $ do
  authorize P_Open P_Group
  uname <- username
  persistence $ do
    gs <- Persist.userGroups uname
    forM gs $ \(ck, course, gk, g) -> do
      admins <- Persist.groupAdmins gk
      hasSubmitted <- isThereASubmission uname ck gk 
      return (course, gk, g, admins, hasSubmitted)
  where
    isThereASubmission u ck gk = do
      (||) <$> Persist.isThereASubmissionForGroup u gk
           <*> Persist.isThereASubmissionForCourse u ck

subscribedGroups :: UserStory [(CourseKey, Course, GroupKey, Group)]
subscribedGroups = logAction INFO "lists all subscribed groups" $ do
  withUserAndPersist $ \u -> Persist.userGroups (u_username u)

testCaseModificationForAssignment :: Username -> AssignmentKey -> TCModification -> Persist ()
testCaseModificationForAssignment u ak = tcModificationCata noModification fileOverwrite textOverwrite tcDelete where
  noModification = return ()

  fileOverwrite tsk uf = do
      let usersFileName = usersFile id id uf
          testCase = TestCase {
              tcName        = usersFileName
            , tcDescription = usersFileName
            , tcValue       = ZippedTestCase ""
            , tcInfo        = usersFileName
            }
      mtk <- Persist.testCaseOfAssignment ak
      -- TODO: Join the test case creation and test case file copy
      tk <- case mtk of
        Just tk -> Persist.modifyTestCase tk testCase >> return tk
        Nothing -> Persist.saveTestCase tsk ak testCase
      Persist.modifyTestScriptOfTestCase tk tsk
      Persist.copyTestCaseFile tk u uf
      return ()

  textOverwrite tsk t = do
      a <- Persist.loadAssignment ak
      let name = Assignment.name a
          testCase = TestCase {
              tcName        = name
            , tcDescription = name
            , tcValue       = SimpleTestCase t
            , tcInfo        = ""
            }
      mtk <- Persist.testCaseOfAssignment ak
      tk <- case mtk of
        Just tk -> Persist.modifyTestCase tk testCase >> return tk
        Nothing -> Persist.saveTestCase tsk ak testCase
      Persist.modifyTestScriptOfTestCase tk tsk

  tcDelete = do
      mtk <- Persist.testCaseOfAssignment ak
      case mtk of
        Nothing -> return ()
        Just tk -> Persist.removeTestCaseAssignment tk ak

-- Interprets the TCCreation value, copying a binary file or filling up the
-- normal test case file with the plain value, creating the test case for the
-- given assingment
testCaseCreationForAssignment :: Username -> AssignmentKey -> TCCreation -> Persist ()
testCaseCreationForAssignment u ak = tcCreationCata noCreation fileCreation textCreation where

  noCreation = return ()

  fileCreation tsk usersfile = do
      let usersFileName = usersFile id id usersfile
          testCase = TestCase {
              tcName        = usersFileName
            , tcDescription = usersFileName
            , tcValue       = ZippedTestCase ""
            , tcInfo        = usersFileName
            }
      tk <- Persist.saveTestCase tsk ak testCase
      Persist.copyTestCaseFile tk u usersfile
      return ()

  -- Set plain text as test case value
  textCreation tsk plain = do
      a <- Persist.loadAssignment ak
      let name = Assignment.name a
          testCase = TestCase {
              tcName        = name
            , tcDescription = name
            , tcValue       = SimpleTestCase plain
            , tcInfo        = ""
            }
      Persist.saveTestCase tsk ak testCase
      return ()

createGroupAssignment :: GroupKey -> Assignment -> TCCreation -> UserStory AssignmentKey
createGroupAssignment gk a tc = logAction INFO msg $ do
  authorize P_Open   P_Group
  authorize P_Create P_Assignment
  when (null $ Assignment.name a) $
    errorPage . userError $ msg_UserStoryError_EmptyAssignmentTitle
      "Assignment title is empty."
  when (null $ Assignment.desc a) $
    errorPage . userError $ msg_UserStoryError_EmptyAssignmentDescription
      "Assignment description is empty."

  join . withUserAndPersist $ \u -> do
    let user = u_username u
    admined <- Persist.isAdminOfGroupOrCourse user gk
    if admined
      then do ak <- Persist.saveGroupAssignment gk a
              testCaseCreationForAssignment user ak tc
              now <- liftIO getCurrentTime
              g  <- Persist.loadGroup gk
              ck <- Persist.courseOfGroup gk
              c  <- Persist.loadCourse ck
              let msg = Notification.NE_GroupAssignmentCreated
                          (u_name u) (groupName g) (courseName c) (Assignment.name a)
              gas <- Persist.groupAdminKeys gk
              let affected = HashSet.toList $ user `HashSet.delete` HashSet.fromList gas
              Persist.notifyUsers (Notification.Notification msg now $ Notification.Assignment ak) affected
              sbs <- Persist.subscribedToGroup gk
              let affected = HashSet.toList $ HashSet.fromList sbs `HashSet.difference` HashSet.fromList (user : gas)
              let time = Assignment.start a
              Persist.notifyUsers (Notification.Notification msg time $ Notification.Assignment ak) affected
              return $ do
                statusMsg a
                logMessage INFO $ descriptor ak
                return ak
      else return $ do
             logMessage INFO . violation $ printf "User tries to access to group: %s" (groupKeyMap id gk)
             errorPage $ userError nonAdministratedGroup
  where
    descriptor key = printf "Exercise is created with id: %s" (assignmentKeyMap id key)
    msg = "creates assignment for group " ++ show gk
    statusMsg = const .
      putStatusMessage $ msg_UserStory_NewGroupAssignment "The group assignment has been created."

createCourseAssignment :: CourseKey -> Assignment -> TCCreation -> UserStory AssignmentKey
createCourseAssignment ck a tc = logAction INFO msg $ do
  authorize P_Open P_Course
  authorize P_Create P_Assignment
  when (null $ Assignment.name a) $
    errorPage . userError $ msg_UserStoryError_EmptyAssignmentTitle
      "Assignment title is empty."
  when (null $ Assignment.desc a) $
    errorPage . userError $ msg_UserStoryError_EmptyAssignmentDescription
      "Assignment description is empty."

  join . withUserAndPersist $ \u -> do
    let user = u_username u
    admined <- Persist.isAdministratedCourse user ck
    if admined
      then do ak <- Persist.saveCourseAssignment ck a
              testCaseCreationForAssignment user ak tc
              now <- liftIO getCurrentTime
              c <- Persist.loadCourse ck
              let msg = Notification.NE_CourseAssignmentCreated
                          (u_name u) (courseName c) (Assignment.name a)
              cas <- Persist.courseAdminKeys ck
              gks <- Persist.groupKeysOfCourse ck
              gas <- concat <$> mapM Persist.groupAdminKeys gks
              sbs <- Persist.subscribedToCourse ck
              let affected = nub (gas ++ cas) \\ [user]
              Persist.notifyUsers (Notification.Notification msg now $ Notification.Assignment ak) affected
              let affected = nub (sbs \\ (gas ++ cas ++ [user]))
              let time = Assignment.start a
              Persist.notifyUsers (Notification.Notification msg time $ Notification.Assignment ak) affected
              return $ do
                statusMsg a
                logMessage INFO $ descriptor ak
                return ak
      else return $ do
             logMessage INFO . violation $ printf "User tries to access to course: %s" (courseKeyMap id ck)
             errorPage $ userError nonAdministratedCourse
  where
    descriptor key = printf "Exercise is created with id: %s" (assignmentKeyMap id key)
    msg = "creates assignment for course " ++ show ck
    statusMsg = const .
      putStatusMessage $ msg_UserStory_NewCourseAssignment "The course assignment has been created."

-- | The 'loadAssignment' loads an assignment from the persistence layer
loadAssignment :: AssignmentKey -> UserStory Assignment
loadAssignment k = logAction INFO ("loads assignment " ++ show k) $ do
  authorize P_Open P_Assignment
  persistence $ Persist.loadAssignment k

courseOrGroupOfAssignment :: AssignmentKey -> UserStory (Either CourseKey GroupKey)
courseOrGroupOfAssignment ak = logAction INFO ("gets course key or group key of assignment " ++ show ak) $
  persistence (Persist.courseOrGroupOfAssignment ak)

courseAndGroupOfAssignment :: AssignmentKey -> UserStory (Course, Maybe Group)
courseAndGroupOfAssignment ak = logAction INFO ("gets course and group of assignment " ++ show ak) $
  persistence (Persist.courseAndGroupOfAssignment ak)

createGroupAssessment :: GroupKey -> Assessment -> UserStory AssessmentKey
createGroupAssessment gk a = logAction INFO ("creates assessment for group " ++ show gk) $ do
  authorize P_Open P_Group
  authorize P_Create P_Assessment
  isAdminOfGroupOrCourse gk
  ak <- persistence (Persist.saveGroupAssessment gk a)
  withUserAndPersist $ \u -> do
    let user = u_username u
    now <- liftIO getCurrentTime
    ck <- Persist.courseOfGroup gk
    g  <- Persist.loadGroup gk
    c  <- Persist.loadCourse ck
    let msg = Notification.NE_GroupAssessmentCreated
                (u_name u) (groupName g) (courseName c) (Assessment.title a)
    gas <- Persist.groupAdminKeys gk
    sbs <- Persist.subscribedToGroup gk
    let affected = nub (gas ++ sbs) \\ [user]
    Persist.notifyUsers (Notification.Notification msg now $ Notification.Assessment ak) affected
  return ak

createCourseAssessment :: CourseKey -> Assessment -> UserStory AssessmentKey
createCourseAssessment ck a = logAction INFO ("creates assessment for course " ++ show ck) $ do
  authorize P_Open P_Course
  authorize P_Create P_Assessment
  isAdministratedCourse ck
  ak <- persistence (Persist.saveCourseAssessment ck a)
  withUserAndPersist $ \u -> do
    let user = u_username u
    now <- liftIO getCurrentTime
    c <- Persist.loadCourse ck
    let msg = Notification.NE_CourseAssessmentCreated
                (u_name u) (courseName c) (Assessment.title a)
    cas <- Persist.courseAdminKeys ck
    sbs <- Persist.subscribedToCourse ck
    let affected = nub (cas ++ sbs) \\ [user]
    Persist.notifyUsers (Notification.Notification msg now $ Notification.Assessment ak) affected
  return ak

modifyAssessment :: AssessmentKey -> Assessment -> UserStory ()
modifyAssessment ak a = logAction INFO ("modifies assessment " ++ show ak) $ do
  authorize P_Open P_Assessment
  authorize P_Modify P_Assessment
  isAdministratedAssessment ak
  hasScore <- isThereAScore ak
  withUserAndPersist $ \u -> do
    let user = u_username u
    new <- if hasScore
             then do
               -- Overwrite the assignment type with the old one
               -- if there is a score for the given assessment
               evConfig <- Assessment.evaluationCfg <$> Persist.loadAssessment ak
               return (a { Assessment.evaluationCfg = evConfig})
             else return a
    Persist.modifyAssessment ak new
    now <- liftIO getCurrentTime
    let msg = Notification.NE_AssessmentUpdated (u_name u) (Assessment.title a)
    mck <- Persist.courseOfAssessment ak
    mgk <- Persist.groupOfAssessment ak
    affected <- case (mck, mgk) of
                  (Just ck, _) -> do
                    cas <- Persist.courseAdminKeys ck
                    sbs <- Persist.subscribedToCourse ck
                    return $ nub (cas ++ sbs) \\ [user]
                  (_, Just gk) -> do
                    gas <- Persist.groupAdminKeys gk
                    sbs <- Persist.subscribedToGroup gk
                    return $ nub (gas ++ sbs) \\ [user]
                  _            -> return []
    Persist.notifyUsers (Notification.Notification msg now $ Notification.Assessment ak) affected
    when (hasScore && Assessment.evaluationCfg a /= Assessment.evaluationCfg new) $
      void . return . putStatusMessage . msg_UserStory_AssessmentEvalTypeWarning $ concat
        [ "The evaluation type of the assessment is not modified. "
        , "A score is already submitted."
        ]

modifyAssessmentAndScores :: AssessmentKey -> Assessment -> Map Username Evaluation -> UserStory ()
modifyAssessmentAndScores ak a scores = logAction INFO ("modifies assessment and scores of " ++ show ak) $ do
  modifyAssessment ak a
  cGKey <- courseOrGroupOfAssessment ak
  case cGKey of
    Left ck -> do
      usernames <- subscribedToCourse ck
      modifyScoresOfUsers usernames
    Right gk -> do
      usernames <- subscribedToGroup gk
      modifyScoresOfUsers usernames
    where
      modifyScoresOfUsers :: [Username] -> UserStory ()
      modifyScoresOfUsers usernames = do
          forM_ usernames $ \u -> do
            case Map.lookup u scores of
              Nothing -> return ()
              Just newEv -> do
                mScoreInfo <- scoreInfoOfUser u ak
                case mScoreInfo of
                  Just si -> persistence $
                     Persist.modifyEvaluation (Rel.evaluationKeyOfInfo si) newEv
                  _ -> void $ saveUserScore u ak newEv

loadAssessment :: AssessmentKey -> UserStory Assessment
loadAssessment ak = logAction INFO ("loads assessment " ++ show ak) $ do
  authorize P_Open P_Assessment
  persistence (Persist.loadAssessment ak)

courseOrGroupOfAssessment :: AssessmentKey -> UserStory (Either CourseKey GroupKey)
courseOrGroupOfAssessment ak = logAction INFO ("gets course key or group key of assessment " ++ show ak) $
  persistence (Persist.courseOrGroupOfAssessment ak)

assessmentDesc :: AssessmentKey -> UserStory AssessmentDesc
assessmentDesc ak = logAction INFO ("loads information of assessment " ++ show ak) $ do
  authorize P_Open P_Assessment
  persistence (Persist.assessmentDesc ak)

usernameOfScore :: ScoreKey -> UserStory Username
usernameOfScore sk = logAction INFO ("looks up the user of score " ++ show sk) $ do
  persistence (Persist.usernameOfScore sk)

assessmentOfScore :: ScoreKey -> UserStory AssessmentKey
assessmentOfScore sk = logAction INFO ("looks up the assessment of score " ++ show sk) $ do
  persistence (Persist.assessmentOfScore sk)

isThereAScore :: AssessmentKey -> UserStory Bool
isThereAScore ak = logAction INFO ("checks whether there is a score for the assessment " ++ show ak) $
  persistence (not . null <$> Persist.scoresOfAssessment ak)

scoreInfo :: ScoreKey -> UserStory (Maybe ScoreInfo)
scoreInfo sk = logAction INFO ("loads score information of score " ++ show sk) $ do
  persistence (Persist.scoreInfo sk)

scoreDesc :: ScoreKey -> UserStory ScoreDesc
scoreDesc sk = logAction INFO ("loads score description of score " ++ show sk) $ do
  authPerms scoreDescPermissions
  currentUser <- username
  scoreUser <- usernameOfScore sk
  if (currentUser == scoreUser)
    then persistence $ Persist.scoreDesc sk
    else do
      logMessage INFO . violation $ printf "The user tries to view a score (%s) that not belongs to him."
                                           (show sk)
      errorPage $ userError nonAccessibleScore

saveUserScore :: Username -> AssessmentKey -> Evaluation -> UserStory ScoreKey
saveUserScore u ak evaluation = logAction INFO ("saves user score of " ++ show u ++ " for assessment " ++ show ak) $ do
  authorize P_Open P_Assessment
  scoreInfo <- scoreInfoOfUser u ak
  case scoreInfo of
    Nothing ->
        persistence $ do
          sk <- Persist.saveScore u ak (Score ())
          void $ Persist.saveScoreEvaluation sk evaluation
          return sk
    Just si -> do
        logMessage INFO "Other admin just gave a score for this user's assessment"
        putStatusMessage $ msg_UserStoryError_ScoreAlreadyExists
          "This user already has a score for this assessment"
        return (Rel.scoreKeyOfInfo si)

modifyUserScore :: ScoreKey -> Evaluation -> UserStory ()
modifyUserScore sk newEvaluation = logAction INFO ("modifies user score " ++ show sk) $ do
  mEKey <- persistence (Persist.evaluationOfScore sk)
  maybe (return ()) (\(eKey, _) -> modifyEvaluation eKey newEvaluation) mEKey

saveScoresOfCourseAssessment :: CourseKey -> Assessment -> Map Username Evaluation -> UserStory ()
saveScoresOfCourseAssessment ck a evaluations = do
  ak <- createCourseAssessment ck a
  logAction INFO ("saves scores of assessment " ++ show ak ++ " of course " ++ show ck) $ do
    users <- subscribedToCourse ck
    persistence (mapM_ (saveEvaluation ak) users)
      where
        saveEvaluation ak user = case Map.lookup user evaluations of
                                   Just eval -> do
                                     sk <- Persist.saveScore user ak score
                                     void $ Persist.saveScoreEvaluation sk eval
                                   Nothing   -> return ()

        score = Score ()

saveScoresOfGroupAssessment :: GroupKey -> Assessment -> Map Username Evaluation -> UserStory ()
saveScoresOfGroupAssessment gk a evaluations = do
  ak <- createGroupAssessment gk a
  logAction INFO ("saves scores of assessment " ++ show ak ++ " of group " ++ show gk) $ do
    users <- subscribedToGroup gk
    persistence (mapM_ (saveEvaluation ak) users)
      where
        saveEvaluation ak user = case Map.lookup user evaluations of
                                   Just eval -> do
                                     sk <- Persist.saveScore user ak score
                                     void $ Persist.saveScoreEvaluation sk eval
                                   Nothing   -> return ()

        score = Score ()

-- Produces the score key, score info for the specific user and assessment.
-- Returns Nothing if there are multiple scoreinfo available.
scoreInfoOfUser :: Username -> AssessmentKey -> UserStory (Maybe ScoreInfo)
scoreInfoOfUser u ak = logAction INFO ("loads score info of user " ++ show u ++ " and assessment " ++ show ak) $
  persistence $ Persist.scoreInfoOfUser u ak

scoresOfGroup :: GroupKey -> AssessmentKey -> UserStory [(UserDesc, Maybe ScoreInfo)]
scoresOfGroup gk ak = logAction INFO ("lists scores of group " ++ show gk ++ " and assessment " ++ show ak) $ do
  authorize P_Open P_Group
  isAdminOfGroupOrCourse gk
  usernames <- subscribedToGroup gk
  forM usernames $ \u -> do
    userDesc <- loadUserDesc u
    mScoreInfo <- scoreInfoOfUser u ak
    return (userDesc, mScoreInfo)

scoresOfCourse :: CourseKey -> AssessmentKey -> UserStory [(UserDesc, Maybe ScoreInfo)]
scoresOfCourse ck ak = logAction INFO ("lists scores of course " ++ show ck ++ " and assessment " ++ show ak) $ do
  authorize P_Open P_Course
  isCourseOrGroupAdmin ck
  usernames <- subscribedToCourse ck
  forM usernames $ \u -> do
    userDesc <- loadUserDesc u
    mScoreInfo <- scoreInfoOfUser u ak
    return (userDesc, mScoreInfo)

scoreBoardOfGroup :: GroupKey -> UserStory ScoreBoard
scoreBoardOfGroup gk = logAction INFO ("gets scoreboard of group " ++ show gk) $ do
  authPerms scoreBoardPermissions
  isAdminOfGroupOrCourse gk
  persistence $ Persist.scoreBoardOfGroup gk

-- Puts the given status message to the actual user state
putStatusMessage :: Translation String -> UserStory ()
putStatusMessage = changeUserState . SC.setStatus . SmNormal

-- Puts the given message as the error status message to the actual user state
putErrorMessage :: Translation String -> UserStory ()
putErrorMessage = changeUserState . SC.setStatus . SmError

-- Logs the error message into the logfile and, also throw as an error
errorPage :: UserError -> UserStory a
errorPage e = do
  logMessage ERROR $ translateUserError trans e
  CME.throwError e

-- * Low level user story functionality

authPerms :: ObjectPermissions -> UserStory ()
authPerms = mapM_ (uncurry authorize) . permissions

-- | Authorize the user for the given operation.
--   It throws exception if the user is not authorized
--   for the given operation
authorize :: Permission -> PermissionObject -> UserStory ()
authorize p o = do
  er <- CMS.gets SC.userRole
  case er of

    Left EmptyRole ->
      errorPage $ userError (msg_UserStoryError_UserIsNotLoggedIn "The user is not logged in.")

    Left RegRole -> case elem (p,o) regPermObjects of
      True  -> return ()
      False -> errorPage $ userPrm2Error
        (msg_UserStoryError_RegistrationProcessError $ unlines [
           "During the registration process some internal error happened ",
           "and tries to reach other processes %s %s."])
        (show p) (show o)

    Left TestAgentRole -> case elem (p,o) testAgentPermObjects of
      True -> return ()
      False -> errorPage $ userPrm2Error
        (msg_UserStoryError_TestAgentError $ unlines [
           "During the automated testing process some internal error happened ",
           "and tries to reach other processes %s %s."])
        (show p) (show o)

    Right r -> case permission r p o of
      True  -> return ()
      False -> errorPage $ userPrm3Error
        (msg_UserStoryError_AuthenticationNeeded "Authentication needed %s %s %s")
          (show r) (show p) (show o)
  where
    regPermObjects = [
        (P_Create, P_User),    (P_Open, P_User)
      , (P_Create, P_UserReg), (P_Open, P_UserReg)
      , (P_Modify, P_User)
      ]

    testAgentPermObjects = [
        (P_Open, P_TestIncoming), (P_Open, P_Submission), (P_Create, P_Feedback)
      ]

-- | Log error message through the log subsystem
logErrorMessage :: String -> UserStory ()
logErrorMessage = logMessage ERROR

-- | Log a message through the log subsystem
logMessage :: LogLevel -> String -> UserStory ()
logMessage level msg = do
  CMS.get >>=
    SC.userStateCata
      userNotLoggedIn
      registration
      testAgent
      loggedIn
  where
    logMsg prefix =
      asksLogger >>= (\lgr -> (liftIO $ log lgr level $ unwords [prefix, msg]))

    userNotLoggedIn _  = logMsg "[USER NOT LOGGED IN]"
    registration       = logMsg "[REGISTRATION]"
    testAgent          = logMsg "[TEST AGENT]"
    loggedIn _ u _ _ _ uuid _ _ _ = logMsg (unwords [Entity.uid id u, UUID.toString uuid])


-- | Change user state
changeUserState :: (UserState -> UserState) -> UserStory ()
changeUserState = CMS.modify

loadUserData :: Username -> UUID -> UserStory User
loadUserData uname sessionId = do
  u <- persistence $ Persist.loadUser uname
  flip userCata u $ \r _ _ n tz lang ui -> do
    CMS.put $ SC.UserLoggedIn {
        SC.user = uname
      , SC.uid = ui
      , SC.name = n
      , SC._language = lang
      , SC.role = r
      , SC.uuid = sessionId
      , SC._timeZone = tz
      , SC._status = Nothing
      , SC._homePage = Rel.defaultHomePage r
      }
  return u

userState :: UserStory UserState
userState = CMS.get

submitSolution :: AssignmentKey -> Submission -> UserStory SubmissionKey
submitSolution ak s = logAction INFO ("submits solution for assignment " ++ show ak) $ do
  authorize P_Open   P_Assignment
  authorize P_Create P_Submission
  a <- loadAssignment ak
  checkActiveAssignment a
  join $ withUserAndPersist $ \u -> do
    let user = u_username u
    attended <- Persist.isUsersAssignment user ak
    if attended
      then do removeUserOpenedSubmissions user ak
              sk <- Persist.saveSubmission ak user s
              Persist.queueSubmissionForTest sk
              return (return sk)
      else return $ do
             logMessage INFO . violation $
               printf "The user tries to submit a solution for an assignment which not belongs to him: (%s)" (assignmentKeyMap id ak)
             errorPage $ userError nonRelatedAssignment
  where
    -- TODO: Change the ABI to remove the unevaluated automatically
    removeUserOpenedSubmissions u ak = do
      sks <- Persist.usersOpenedSubmissions ak u
      mapM_ (Persist.removeFromOpened ak u) sks

    checkActiveAssignment :: Assignment -> UserStory ()
    checkActiveAssignment asg = do
      now <- liftIO getCurrentTime
      let (start, end) = Assignment.assignmentCata (\_name _desc _asp start end _evtype -> (start, end)) asg
      when (now < start) . errorPage . userError $
        msg_UserStoryError_AssignmentNotAvailableYet "The assignment is not available yet."
      when (now > end) . errorPage . userError $
        msg_UserStoryError_SubmissionDeadlineIsReached "The submission deadline is reached."
      return ()

queueSubmissionForTest :: SubmissionKey -> UserStory ()
queueSubmissionForTest sk = logAction INFO (unwords ["queues submission", show sk, "for test"]) $ do
  isAdministratedSubmission sk
  persistence $ Persist.queueSubmissionForTest sk

queueAllSubmissionsForTest :: AssignmentKey -> UserStory ()
queueAllSubmissionsForTest ak = logAction INFO (unwords ["queues all submissions of", show ak, "for test"]) $ do
  isAdministratedAssignment ak
  sks <- lastSubmissions ak
  persistence $ mapM_ Persist.queueSubmissionForTest sks

-- Returns all groups with admins for which the user have not submitted
-- a solution already
availableGroups :: UserStory [(Course, GroupKey, Group, [User])]
availableGroups = logAction INFO "lists available groups" $ do
  authorize P_Open P_Group
  u <- username
  persistence $ do
    allGroups <- Persist.groups
    available <- filterM (thereIsNoSubmission u) allGroups
    forM available $ \(c, gk, g) -> do
      admins <- Persist.groupAdmins gk
      return (c, gk, g, admins)
  where
    thereIsNoSubmission :: Username -> (Course, GroupKey, Group) -> Persist Bool
    thereIsNoSubmission u (_, gk, _) = not <$> Persist.isThereASubmissionForGroup u gk

userSubmissionKeys :: AssignmentKey -> UserStory [SubmissionKey]
userSubmissionKeys ak = logAction INFO msg $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Submission
  withUserAndPersist $ \u -> Persist.userSubmissions (u_username u) ak
  where
    msg = "lists the submissions for assignment " ++ show ak

-- |Loads infos of submissions uploaded by a user to an assignment.
-- It is used by the Evaluation page.
-- The elements in the result list are in reverse chronological order: the first element is the most recent.
submissionInfos :: Username -> AssignmentKey -> UserStory [SubmissionInfo]
submissionInfos u ak = logAction INFO msg $ do
  authorize P_Open P_Submission
  isAdministratedAssignment ak
  persistence $ Persist.userSubmissionInfos u ak
  where
    msg = concat ["lists ", show u,"'s submissions for assignment ", show ak]

-- |Loads information on submissions uploaded by the current user to an assignment.
-- It is used by the Submission page.
-- The elements in the result list are in reverse chronological order: the first element is the most recent.
userSubmissionInfos :: AssignmentKey -> UserStory [SubmissionInfo]
userSubmissionInfos ak = logAction INFO ("loads info on submissions for assigment " ++ show ak) $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Submission
  isUsersAssignment ak
  withUserAndPersist $ \u -> Persist.userSubmissionInfos (u_username u) ak

lastSubmission :: AssignmentKey -> Username -> UserStory (Maybe SubmissionKey)
lastSubmission ak u = logAction INFO ("loads the key of the last submission for assignment " ++ show ak) $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Submission
  persistence $ Persist.lastSubmission ak u

lastSubmissions :: AssignmentKey -> UserStory [SubmissionKey]
lastSubmissions ak = logAction INFO ("loads last submissions of users for assignment " ++ show ak) $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Submission
  key <- persistence $ Persist.courseOrGroupOfAssignment ak
  usernames <- either subscribedToCourse subscribedToGroup key
  catMaybes <$> mapM (lastSubmission ak) usernames

submissionDetailsDesc :: SubmissionKey -> UserStory SubmissionDetailsDesc
submissionDetailsDesc sk = logAction INFO msg $ do
  authPerms submissionDetailsDescPermissions
  persistence $ Persist.submissionDetailsDesc sk
  where
    msg = "loads information about submission " ++ show sk

loadSubmission :: SubmissionKey -> UserStory Submission
loadSubmission sk = logAction INFO ("loads submission " ++ show sk) $ do
  authorize P_Open P_Submission
  persistence $ Persist.loadSubmission sk

-- Checks if the submission is accessible for the user and loads it,
-- otherwise throws an exception.
getSubmission :: SubmissionKey -> UserStory (Submission, SubmissionDesc)
getSubmission sk = logAction INFO ("downloads submission " ++ show sk) $ do
  authorize P_Open P_Submission
  isAccessibleBallotBoxSubmission sk
  persistence $ do
    s <- Persist.loadSubmission sk
    d <- Persist.submissionDesc sk
    return (s,d)

-- Creates a submission limit for the given assignment
assignmentSubmissionLimit :: AssignmentKey -> UserStory SubmissionLimit
assignmentSubmissionLimit key = logAction INFO msg $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Submission
  withUserAndPersist $ \u -> Persist.submissionLimitOfAssignment (u_username u) key
  where
    msg = "user assignments submission Limit"

userAssignmentsAssessments :: GroupKey -> UserStory ([(AssignmentKey, Assignment, Maybe (SubmissionKey, SubmissionState), SubmissionLimit)], [(AssessmentKey, Assessment, Maybe ScoreInfo)])
userAssignmentsAssessments gk = logAction INFO ("lists assignments and assessments of group " ++ show gk) $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Assessment
  authorize P_Open P_Course
  authorize P_Open P_Group
  withUserAndPersist (\u -> Persist.userAssignmentsAssessments (u_username u) gk)

allAssignmentsOfGroup :: GroupKey -> UserStory [(AssignmentKey, Assignment)]
allAssignmentsOfGroup gk = logAction INFO ("lists all assignments of group" ++ show gk) $ do
  isAdminOfGroupOrCourse gk
  persistence $ Persist.allAssignmentsOfGroup gk

submissionDescription :: SubmissionKey -> UserStory SubmissionDesc
submissionDescription sk = logAction INFO msg $ do
  authPerms submissionDescPermissions
  isAdministratedSubmission sk
  persistence $ Persist.submissionDesc sk
  where
    msg = "loads submission information for " ++ show sk

openSubmissions :: UserStory OpenedSubmissions
openSubmissions = logAction INFO ("lists unevaluated submissions") $ do
  authorize P_Open P_Submission
  u <- username
  persistence $ Persist.openedSubmissionInfo u

courseSubmissionTable :: CourseKey -> UserStory SubmissionTableInfo
courseSubmissionTable ck = logAction INFO ("gets submission table for course " ++ show ck) $ do
  authPerms submissionTableInfoPermissions
  isAdministratedCourse ck
  persistence $ Persist.courseSubmissionTableInfo ck

groupSubmissionTable :: GroupKey -> UserStory SubmissionTableInfo
groupSubmissionTable gk = logAction INFO ("gets submission table for group " ++ show gk) $ do
  authPerms submissionTableInfoPermissions
  isAdminOfGroupOrCourse gk
  persistence $ Persist.groupSubmissionTableInfo gk

submissionTables :: UserStory [SubmissionTableInfo]
submissionTables = logAction INFO "lists submission tables" $ do
  authPerms submissionTableInfoPermissions
  withUserAndPersist $ Persist.submissionTables . u_username

newEvaluation :: SubmissionKey -> Evaluation -> UserStory ()
newEvaluation sk e = logAction INFO ("saves new evaluation for " ++ show sk) $ do
  authorize P_Open   P_Submission
  authorize P_Create P_Evaluation
  isAdministratedSubmission sk
  now <- liftIO $ getCurrentTime
  userData <- currentUser
  join . withUserAndPersist $ \u -> do
    let user = u_username u
    mek <- Persist.evaluationOfSubmission sk
    case mek of
      Nothing -> do
        ek <- Persist.saveSubmissionEvaluation sk e
        Persist.removeOpenedSubmission sk
        Persist.saveFeedbacks sk [evaluationToFeedback now userData e]
        let msg = Notification.NE_EvaluationCreated (u_name u) (withSubmissionKey sk id)
        ak  <- Persist.assignmentOfSubmission sk
        mck <- Persist.courseOfAssignment ak
        mgk <- Persist.groupOfAssignment ak
        submitter <- Persist.usernameOfSubmission sk
        affected <- case (mck, mgk) of
          (Just ck, _) -> do
            cas <- Persist.courseAdminKeys ck
            return $ nub ([submitter] ++ cas) \\ [user]
          (_, Just gk) -> do
            gas <- Persist.groupAdminKeys gk
            return $ nub ([submitter] ++ gas) \\ [user]
          _            -> return []
        Persist.notifyUsers (Notification.Notification msg now $ Notification.Evaluation ek) affected
        return (return ())
      Just _ -> return $ do
        logMessage INFO "Other admin just evaluated this submission"
        putStatusMessage $ msg_UserStory_AlreadyEvaluated
          "Other admin just evaluated this submission"

modifyEvaluation :: EvaluationKey -> Evaluation -> UserStory ()
modifyEvaluation ek e = logAction INFO ("modifies evaluation " ++ show ek) $ do
  authorize P_Modify P_Evaluation
  now <- liftIO $ getCurrentTime
  userData <- currentUser
  join . withUserAndPersist $ \u -> do
    let user = u_username u
    sbk <- Persist.submissionOfEvaluation ek
    sck <- Persist.scoreOfEvaluation ek
    case (sbk, sck) of
      (Just _, Just _) -> return . errorPage $ userErrorWithMsg "Impossible, submission and score have the same evaluation"
      (Nothing, Just sk) -> do
        let admined = True
        if admined
          then do Persist.modifyEvaluation ek e
                  -- Persist.saveFeedback sk (evaluationToFeedback now userData e)
                  let msg = Notification.NE_AssessmentEvaluationUpdated (u_name u) (scoreKey id sk)
                  affected <- do
                    ak <- Persist.assessmentOfScore sk
                    mck <- Persist.courseOfAssessment ak
                    mgk <- Persist.groupOfAssessment ak
                    recipient <- Persist.usernameOfScore sk
                    case (mck, mgk) of
                      (Just ck, _) -> do
                        cas <- Persist.courseAdminKeys ck
                        sbs <- Persist.subscribedToCourse ck
                        return $ nub ([recipient] ++ cas) \\ [user]
                      (_, Just gk) -> do
                        gas <- Persist.groupAdminKeys gk
                        sbs <- Persist.subscribedToGroup gk
                        return $ nub ([recipient] ++ gas) \\ [user]
                      _            -> return []
                  Persist.notifyUsers (Notification.Notification msg now $ Notification.Evaluation ek) affected
                  return (return ())
          else return $ do
                  logMessage INFO . violation $
                    printf "The user tries to modify an evaluation (%s) that not belongs to him."
                           (show ek)
                  errorPage $ userError nonAdministratedSubmission
      (Just sk, Nothing) -> do
        admined <- Persist.isAdminedSubmission user sk
        if admined
          then do Persist.modifyEvaluation ek e
                  Persist.saveFeedbacks sk [evaluationToFeedback now userData e]
                  let msg = Notification.NE_AssignmentEvaluationUpdated (u_name u) (withSubmissionKey sk id)
                  affected <- do
                    ak  <- Persist.assignmentOfSubmission sk
                    mck <- Persist.courseOfAssignment ak
                    mgk <- Persist.groupOfAssignment ak
                    submitter <- Persist.usernameOfSubmission sk
                    case (mck, mgk) of
                      (Just ck, _) -> do
                        cas <- Persist.courseAdminKeys ck
                        return $ nub ([submitter] ++ cas) \\ [user]
                      (_, Just gk) -> do
                        gas <- Persist.groupAdminKeys gk
                        return $ nub ([submitter] ++ gas) \\ [user]
                      _            -> return []
                  Persist.notifyUsers (Notification.Notification msg now $ Notification.Evaluation ek) affected
                  return (return ())
          else return $ do
                  logMessage INFO . violation $
                    printf "The user tries to modify an evaluation (%s) that not belongs to him."
                           (show ek)
                  errorPage $ userError nonAdministratedSubmission
      (Nothing, Nothing) -> return (return ())

createComment :: SubmissionKey -> Comment -> UserStory ()
createComment sk c = logAction INFO ("comments on " ++ show sk) $ do
  authorize P_Open   P_Submission
  authorize P_Create P_Comment
  join $ withUserAndPersist $ \u -> do
    let user = u_username u
    canComment <- Persist.canUserCommentOn user sk
    admined  <- Persist.isAdministratedSubmission user sk
    attended <- Persist.isUserOfSubmission user sk
    if (canComment && (admined || attended))
      then do ck <- Persist.saveComment sk c
              let Comment { commentAuthor = author, commentDate = now, comment = body } = c
              let maxLength   = 100
              let maxLines    = 5
              let trimmedBody = (init $ unlines $ take maxLines $ lines $ take maxLength body) ++ "..."
              let msg = Notification.NE_CommentCreated author (withSubmissionKey sk id) trimmedBody
              ak <- Persist.assignmentOfSubmission sk
              mck <- Persist.courseOfAssignment ak
              mgk <- Persist.groupOfAssignment ak
              submitter <- Persist.usernameOfSubmission sk
              affected <- case (mck, mgk) of
                (Just ck, _) -> do
                  cas <- Persist.courseAdminKeys ck
                  gks <- Persist.groupsOfUsersCourse submitter ck
                  gas <- foldM (\admins gk -> (++ admins) <$> Persist.groupAdminKeys gk) [] gks
                  return $ nub ([submitter] ++ cas ++ gas) \\ [user]
                (_, Just gk) -> do
                  gas <- Persist.groupAdminKeys gk
                  return $ nub ([submitter] ++ gas) \\ [user]
                _            -> return []
              Persist.notifyUsers (Notification.Notification msg now $ Notification.Comment ck) affected
              return (return ())
      else return $ do
              logMessage INFO . violation $ printf "The user tries to comment on a submission (%s) that not belongs to him" (show sk)
              errorPage . userError $ msg_UserStoryError_NonCommentableSubmission "The submission is not commentable"

#ifdef TEST
-- Insert test feedback with the TestAgent only for testing purposes.
insertTestFeedback :: SubmissionKey -> [FeedbackInfo] -> UserStory ()
insertTestFeedback sk fbs = do
  authorize P_Create P_Feedback
  persistence $ do
    Persist.insertTestFeedback sk fbs
    Persist.finalizeTestFeedback sk
#endif

-- Test agent user story, that reads out all the feedbacks that the test daemon left
-- and saves them
testAgentFeedbacks :: UserStory ()
testAgentFeedbacks = do
  authorize P_Open P_TestIncoming
  authorize P_Open P_Submission
  authorize P_Create P_Feedback
  persistence $ do
    feedbacks <- Persist.testFeedbacks
    forM_ feedbacks (uncurry Persist.saveFeedbacks)

-- List all the related notifications for the active user and marks them
-- as seen if their state is new.
notifications :: UserStory [(Notification.Notification, Notification.NotificationState, Notification.NotificationReference)]
notifications = do
  now <- liftIO $ getCurrentTime
  orderedNotifs <- withUserAndPersist $ \u -> do
              let user = u_username u
              notifs <- Persist.notificationsOfUser user (Just notificationLimit)
              forM notifs (\(k,s,_p) -> do
                notif <- Persist.loadNotification k
                notifRef <- Persist.notificationReference (Notification.notifType notif)
                when (s == Notification.New) $ Persist.markSeen user k
                return (notif, s, notifRef))
  return orderedNotifs

noOfUnseenNotifications :: UserStory Int
noOfUnseenNotifications = do
  withUserAndPersist $ Persist.noOfUnseenNotifications . u_username

submissionsForAssignment :: AssignmentKey -> UserStory [SubmissionKey]
submissionsForAssignment ak = logAction INFO ("gets all the submission keys for assignment " ++ show ak) $ do
  authorize P_Open P_Assignment
  authorize P_Open P_Submission
  isAdministratedAssignment ak
  persistence $ Persist.submissionsForAssignment ak

-- | Checks if there is at least one submission for the given assignment
isThereASubmission :: AssignmentKey -> UserStory Bool
isThereASubmission ak = logAction INFO ("are there submissions for " ++ show ak) $ do
  authorize P_Open P_Assignment
  isAdministratedAssignment ak
  fmap (not . null) $ submissionsForAssignment ak

assignmentOfSubmission :: SubmissionKey -> UserStory AssignmentKey
assignmentOfSubmission sk = logAction INFO ("gets assignment of submission " ++ show sk) $ do
  authorize P_Open P_Assignment
  persistence $ Persist.assignmentOfSubmission sk

-- | Modify the given assignment but keeps the evaluation type if there is
-- a submission for the given assignment, also shows a warning message if the
-- modification of the assignment type is not viable.
modifyAssignment :: AssignmentKey -> Assignment -> TCModification -> UserStory ()
modifyAssignment ak a tc = logAction INFO ("modifies assignment " ++ show ak) $ do
  authorize P_Modify P_Assignment
  isAdministratedAssignment ak
  withUserAndPersist $ \u -> do
    let user = u_username u
    Persist.modifyAssignment ak a
    testCaseModificationForAssignment user ak tc
    now <- liftIO getCurrentTime
    let msg = Notification.NE_AssignmentUpdated (u_name u) (Assignment.name a)
    mck <- Persist.courseOfAssignment ak
    mgk <- Persist.groupOfAssignment ak
    affected <- case (mck, mgk) of
                  (Just ck, _) -> do
                    cas <- Persist.courseAdminKeys ck
                    gks <- Persist.groupKeysOfCourse ck
                    gas <- concat <$> mapM Persist.groupAdminKeys gks
                    return $ HashSet.toList $ user `HashSet.delete` HashSet.fromList cas `HashSet.union` HashSet.fromList gas
                  (_, Just gk) -> do
                    gas <- Persist.groupAdminKeys gk
                    return $ HashSet.toList $ user `HashSet.delete` HashSet.fromList gas
                  _            -> return []
    Persist.notifyUsers (Notification.Notification msg now $ Notification.Assignment ak) affected
    if (Assignment.start a <= now)
      then do
        affected <- case (mck, mgk) of
                      (Just ck, _) -> do
                        cas <- Persist.courseAdminKeys ck
                        gks <- Persist.groupKeysOfCourse ck
                        gas <- concat <$> mapM Persist.groupAdminKeys gks
                        sbs <- Persist.subscribedToCourse ck
                        return $ HashSet.toList $ HashSet.fromList sbs `HashSet.difference` HashSet.fromList (user : cas ++ gas)
                      (_, Just gk) -> do
                        gas <- Persist.groupAdminKeys gk
                        sbs <- Persist.subscribedToGroup gk
                        return $ HashSet.toList $ HashSet.fromList sbs `HashSet.difference` HashSet.fromList (user : gas)
                      _            -> return []
        Persist.notifyUsers (Notification.Notification msg now $ Notification.Assignment ak) affected
      else do
        nks <- Persist.notificationsOfAssignment ak
        forM_ nks $ \nk -> do
          mNot <- do
            nt <- Persist.loadNotification nk
            case (mck, mgk, Notification.notifEvent nt) of
              (Just ck, _, Notification.NE_CourseAssignmentCreated _ _ _)  -> return $ Just nt
              (_, Just gk, Notification.NE_GroupAssignmentCreated _ _ _ _) -> return $ Just nt
              _ -> return Nothing
          case mNot of
            Just n -> do
              let newDate = Assignment.start a
              let n' = n { Notification.notifDate = newDate }
              Persist.updateNotification nk n'
              Persist.updateUserNotification nk newDate
            _ -> return ()

-- * Guards

-- Checks with the given guard function if the user has passed the guard,
-- otherwise logs a violation printf message, and renders the error page with
-- the given user error
guard
  :: (Show a)
  => (Username -> a -> Persist Bool) -- Guard
  -> String                          -- Violation printf message
  -> UserError                       -- Error for the error page
  -> a                               -- The value for the guard
  -> UserStory ()
guard g m e x = do
  u <- username
  join . persistence $ do
    passed <- g u x
    if passed
      then (return (return ()))
      else return $ do
             logMessage INFO . violation $ printf m (show x)
             errorPage e

-- Checks if the given course is administrated by the actual user and
-- throws redirects to the error page if not, otherwise do nothing
isAdministratedCourse :: CourseKey -> UserStory ()
isAdministratedCourse = guard
  Persist.isAdministratedCourse
  "The user tries to access a course (%s) which is not administrated by him."
  (userError nonAdministratedCourse)

-- Checks if the given group is administrated by the actual user and
-- throws redirects to the error page if not, otherwise do nothing
isAdminOfGroupOrCourse :: GroupKey -> UserStory ()
isAdminOfGroupOrCourse = guard
  Persist.isAdminOfGroupOrCourse
  "The user tries to access a group (%s) which is not administrated by him."
  (userError nonAdministratedGroup)

isCourseOrGroupAdmin :: CourseKey -> UserStory ()
isCourseOrGroupAdmin = guard
  Persist.isCourseOrGroupAdmin
  "The user tries to access a course (%s) where she is not an admin."
  (userError notCourseOrGroupAdmin)

isAdministratedSubmission :: SubmissionKey -> UserStory ()
isAdministratedSubmission = guard
  Persist.isAdministratedSubmission
  "The user tries to access a submission (%s) which is not administrated by him."
  (userError nonAdministratedSubmission)

groupsOfCourse :: CourseKey -> UserStory [GroupKey]
groupsOfCourse ck = do
  authorize P_Open P_Course
  isCourseOrGroupAdmin ck
  persistence $ Persist.groupKeysOfCourse ck

-- Produces a list of groupkeys, each of them represents a group of
-- the given course and is administrated by the current user.
administratedGroupsOfCourse :: CourseKey -> UserStory [GroupKey]
administratedGroupsOfCourse ck = do
  authorize P_Open P_Course
  withUserAndPersist $ \u -> do
    gks <- Persist.groupKeysOfCourse ck
    filterM (Persist.isAdminOfGroup (u_username u)) gks

-- Checks if the given assignment is administrated by the actual user and
-- throws redirects to the error page if not, otherwise do nothing
isAdministratedAssignment :: AssignmentKey -> UserStory ()
isAdministratedAssignment = guard
  Persist.isAdministratedAssignment
  "The user tries to access an assignment (%s) which is not administrated by him."
  (userError nonAdministratedAssignment)

-- Checks if the given assessment is administrated by the actual user and
-- throws redirects to the error page if not, otherwise do nothing
isAdministratedAssessment :: AssessmentKey -> UserStory ()
isAdministratedAssessment = guard
  Persist.isAdministratedAssessment
  "User tries to modify the assessment (%s) which is not administrated by him."
  (userError nonAdministratedAssessment)

-- Checks if the given assignment is an assignment of a course or group that
-- the users attend otherwise, renders the error page
isUsersAssignment :: AssignmentKey -> UserStory ()
isUsersAssignment = guard
  (\u ak -> or <$> sequence [Persist.isUsersAssignment u ak, Persist.isAdministratedAssignment u ak])
  "The user tries to access an assignment (%s) which is not of him's."
  (userError nonRelatedAssignment)

isAdministratedTestScript :: TestScriptKey -> UserStory ()
isAdministratedTestScript = guard
  Persist.isAdministratedTestScript
  "The user tries to access a test script (%s) which is not administrated by him."
  (userError nonAdministratedTestScript)

-- Checks if the submission is submitted by the user, or is part of a course
-- or group that the user administrates
isAccessibleSubmission :: SubmissionKey -> UserStory ()
isAccessibleSubmission = guard
  Persist.isAccessibleSubmission
  "The user tries to download a submission (%s) which is not accessible for him."
  (userError nonAccessibleSubmission)

-- This action implements a check similar that of `isAccessibleSubmission` but
-- it also considers if the assignment of the submission is in ballot box mode.
isAccessibleBallotBoxSubmission :: SubmissionKey -> UserStory ()
isAccessibleBallotBoxSubmission = guard
  Persist.isAccessibleBallotBoxSubmission
  "The user tries to download a submission (%s) which is not accessible for him."
  (userError nonAccessibleSubmission)

doesBlockSubmissionView :: SubmissionKey -> UserStory ()
doesBlockSubmissionView = guard
  Persist.doesBlockSubmissionView
  "The user tries to access a blocked submission (%s)."
  (userError blockedSubmission)

doesBlockAssignmentView :: AssignmentKey -> UserStory ()
doesBlockAssignmentView = guard
  Persist.doesBlockAssignmentView
  "The user tries to access a blocked assignment (%s)."
  (userError blockedAssignment)

-- * User Story combinators

-- * Tools

-- Creates a message adding the "VIOLATION: " preffix to it
violation :: String -> String
violation = ("[GUARD VIOLATION]: " ++)

asksLogger :: UserStory Logger
asksLogger = CMR.asks (SC.logger . fst)

asksPersistInterpreter :: UserStory Persist.Interpreter
asksPersistInterpreter = CMR.asks (SC.persistInterpreter . fst)

asksI18N :: UserStory I18N
asksI18N = CMR.asks snd

-- | The 'logAction' first logs the message after runs the given operation
logAction :: LogLevel -> String -> UserStory a -> UserStory a
logAction level msg s = do
  logMessage level (concat [msg, " ..."])
  x <- s
  logMessage level (concat [msg, " ... DONE"])
  return x

withUserAndPersist :: (User -> Persist a) -> UserStory a
withUserAndPersist f = do
  u <- currentUser
  persistence (f u)

-- | Lifting a persistence action, if some error happens
-- during the action we create a unique hash ticket and we display
-- the ticket to the user, and log the original message with the
-- ticket itself
persistence :: Persist a -> UserStory a
persistence m = do
  interp <- asksPersistInterpreter
  x <- liftIO . try $ Persist.runPersist interp m
  case x of
    (Left e) -> do
      -- Exception happened somewhere
      up <- userPart
      let err = showSomeException e
      let xid = encodeMessage (concat [up, " ", err])
      logMessage ERROR $ concat ["Exception in persistence layer: ", err, " XID: ", xid]
      CME.throwError $ userParamError
        (msg_UserStoryError_XID "Some internal error happened, XID: %s")
        xid
    (Right (Left e)) -> do
      -- No exception but error processing the persistence command
      up <- userPart
      let xid = encodeMessage (concat [up, " ", e])
      logMessage ERROR $ concat ["Persistence error: ", e, "XID: ", xid]
      CME.throwError $ userParamError
        (msg_UserStoryError_XID "Some internal error happened, XID: %s")
        xid
    (Right (Right x)) -> return x -- Everything went fine
  where
    showSomeException :: SomeException -> String
    showSomeException = show

    encodeMessage :: String -> String
    encodeMessage = flip showHex "" . abs . hash

    userPart = (SC.userStateCata userNotLoggedIn registration testAgent loggedIn) <$> CMS.get
      where
        userNotLoggedIn _  = "Not logged in user!"
        registration       = "Registration"
        testAgent          = "Test Agent"
        loggedIn _ ui _ _ _ uuid _ _ _ = concat [Entity.uid id ui, " ", UUID.toString uuid]

-- * User Error Messages

nonAdministratedCourse = msg_UserStoryError_NonAdministratedCourse "The course is not administrated by you."
nonAdministratedGroup  = msg_UserStoryError_NonAdministratedGroup "This group is not administrated by you."
notCourseOrGroupAdmin = msg_UserStoryError_NotCourseOrGroupAdmin "You are not an admin in this course."
nonAdministratedAssignment = msg_UserStoryError_NonAdministratedAssignment "This assignment is not administrated by you."
nonAdministratedAssessment = msg_UserStoryError_NonAdministratedAssessment "This assessment is not administrated by you."
nonAdministratedSubmission = msg_UserStoryError_NonAdministratedSubmission "The submission is not administrated by you."
nonAdministratedTestScript = msg_UserStoryError_NonAdministratedTestScript "The test script is not administrated by you."
nonRelatedAssignment = msg_UserStoryError_NonRelatedAssignment "You can not access the assignment."
nonAccessibleSubmission = msg_UserStoryError_NonAccessibleSubmission "You can not access the submission."
blockedAssignment = msg_UserStoryError_BlockedAssignment "The assignment is blocked by an isolated assignment."
blockedSubmission = msg_UserStoryError_BlockedSubmission "The submission is blocked by an isolated assignment."
nonAccessibleScore = msg_UserStoryError_NonAccessibleScore "The score does not belong to you."

-- * constants

notificationLimit :: Int
notificationLimit = 100
