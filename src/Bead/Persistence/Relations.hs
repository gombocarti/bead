{-# LANGUAGE CPP #-}
module Bead.Persistence.Relations (
    assignmentDesc
  , submissionDesc
  , submissionDetailsDesc
  , isAdminedSubmission
  , canUserCommentOn
  , submissionTables
  , courseSubmissionTableInfo
  , groupSubmissionTableInfo
  , userAssignmentsAssessments
  , userSubmissionInfos
  , userLastSubmission
  , courseAndGroupOfAssignment
  , courseOrGroupOfAssignment
  , courseOrGroupOfAssessment
  , administratedGroupsWithCourseName
  , groupsOfUsersCourse
  , removeOpenedSubmission
  , deleteUserFromCourse -- Deletes a user from a course, searching the roup id for the unsubscription
  , isThereASubmissionForGroup -- Checks if the user submitted any solutions for the group
  , isThereASubmissionForCourse -- Checks if the user submitted any solutions for the course
  , testScriptInfo -- Calculates the test script information for the given test key
  , openedSubmissionInfo -- Calculates the opened submissions for the user from the administrated groups and courses
  , submissionLimitOfAssignment
  , scoreBoardOfGroup
  , scoreBoards
  , scoreInfo
  , scoreInfoOfUser
  , scoreDesc
  , assessmentDesc
  , notificationReference
#ifdef TEST
  , persistRelationsTests
#endif
  ) where

{-
This module contains higher level functionality for querying information
using the primitves defined in the Persist module. Mainly Relations module
related information is computed.
-}

import           Control.Applicative
import           Control.Arrow ((&&&), Kleisli(Kleisli), runKleisli)
import           Control.Monad (foldM, forM, when)
import           Control.Monad.IO.Class
import           Data.Bifunctor (second)
import           Data.Function (on)
import           Data.List ((\\), nub, sortBy, sortOn, intersect, find)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe (catMaybes)
import           Data.Ord (Down(Down))
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Tuple.Utils (fst3, snd3)
import           Data.Foldable (traverse_)

import           Bead.Domain.Entities hiding (fullGroupName)
import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Entity.Assignment as Assignment
import qualified Bead.Domain.Entity.Assessment as Assessment
import qualified Bead.Domain.Entity.Notification as Notification
import           Bead.Domain.Relationships
import           Bead.Domain.Shared.Evaluation
import           Bead.Persistence.Persist
import           Bead.View.Translation

#ifdef TEST
import           Bead.Persistence.Initialization

import           Test.Tasty.TestSet
#endif

-- * Combined Persistence Tasks

-- Retrieves keys of groups which the user attends in.
groupsOfUsersCourse :: Username -> CourseKey -> Persist (HashSet GroupKey)
groupsOfUsersCourse u ck = do
  ugs <- HashSet.fromList <$> userGroupKeys u
  cgs <- HashSet.fromList <$> groupKeysOfCourse ck
  return $ HashSet.intersection ugs cgs

#ifdef TEST
groupsOfUsersCourseTest :: TestSet ()
groupsOfUsersCourseTest = do
  let course  = Course "name" "desc" TestScriptSimple
      course' = Course "name'" "desc'" TestScriptSimple
      group  = Group "name" "desc"
      group0 = Group "name0" "desc"
      group1 = Group "name1" "desc"
      group' = Group "name'" "desc"
      user1name = Username "USER1"
      user1  = User Student user1name (Email "email") "name"
                    (TimeZoneName "Europe/Budapest") (Language "hu")
                    (Uid "USER1")

  ioTest "User assignment keys with group and course assignments" $ do
    init <- createPersistInit defaultConfig
    interp <- createPersistInterpreter defaultConfig
    initPersist init
    result <- runPersist interp $ do
      saveUser user1
      c <- saveCourse course
      c' <- saveCourse course'
      [g, g0, g1] <- mapM (saveGroup c) [group, group0, group1]
      g' <- saveGroup c' group'
      subscribed <- groupsOfUsersCourse user1name c
      equals HashSet.empty subscribed "The unsubscribed user has some groups."
      
      subscribe user1name g
      subscribe user1name g0
      subscribed0 <- groupsOfUsersCourse user1name c
      equals (HashSet.fromList [g, g0]) subscribed0
        "User doesn't seem to be subscribed to all groups."

      subscribed' <- groupsOfUsersCourse user1name c'
      equals HashSet.empty subscribed'
        "User seem to be subscribed to a different course."

      subscribe user1name g'
      subscribed'' <- groupsOfUsersCourse user1name c'
      equals (HashSet.fromList [g']) subscribed''
        "User doesn't seem to be subscribed to second course."

      return ()
    tearDown init
    return result
  return ()

#endif

-- This function is not really used outside of Persistence but the testing code
-- resides outside of Persistence so this function is exported.
-- Once the testing code is moved here, this function can be made private.
courseAndGroupOfAssignment :: AssignmentKey -> Persist (Course, Maybe Group)
courseAndGroupOfAssignment ak = do
  ckGk <- courseOrGroupOfAssignment ak
  case ckGk of
    Left ck -> do
      c <- loadCourse ck
      return (c, Nothing)
    Right gk -> do
      c <- courseOfGroup gk >>= loadCourse
      g <- loadGroup gk
      return (c, Just g)

courseOrGroupOfAssignment :: AssignmentKey -> Persist (Either CourseKey GroupKey)
courseOrGroupOfAssignment ak = do
  mGk <- groupOfAssignment ak
  case mGk of
    Just gk -> return . Right $ gk
    Nothing -> do
      mCk <- courseOfAssignment ak
      case mCk of
        Just ck -> return . Left $ ck
        Nothing -> error $ "Impossible: No course or groupkey was found for the assignment:" ++ show ak

assignmentDesc :: UTCTime -> Username -> AssignmentKey -> Persist AssignmentDesc
assignmentDesc now user key = do
  a <- loadAssignment key
  limit <- submissionLimitOfAssignment user key
  let aspects = Assignment.aspects a
  (c, g) <- courseAndGroupOfAssignment key
  return $! AssignmentDesc {
      aActive = Assignment.isActive a now
    , aIsolated = Assignment.isIsolated aspects
    , aLimit = limit
    , aTitle  = Assignment.name a
    , aCourse = c
    , aGroup  = g
    , aEndDate = Assignment.end a
    }

administratedGroupsWithCourseName :: Username -> Persist [(GroupKey, Group, String)]
administratedGroupsWithCourseName u = do
  gs <- administratedGroups u
  forM gs $ \(gk,g) -> do
    fn <- fullGroupName gk
    return (gk,g,fn)

-- Produces a full name for a group including the name of the course.
fullGroupName :: GroupKey -> Persist String
fullGroupName gk = do
  ck <- courseOfGroup gk
  course <- loadCourse ck
  group <- loadGroup gk
  return $ Domain.fullGroupName course group

submissionDesc :: SubmissionKey -> Persist SubmissionDesc
submissionDesc sk = do
  submission <- loadSubmission sk
  un <- usernameOfSubmission sk
  user <- loadUser un
  let u = u_name user
  let uid = u_uid user
  info <- submissionInfo sk
  ak <- assignmentOfSubmission sk
  asg <- loadAssignment ak
  created <- assignmentCreatedTime ak
  cgk <- courseOrGroupOfAssignment ak
  cs  <- commentsOfSubmission sk >>= \cks -> forM cks $ \ck ->
            (,) ck <$> loadComment ck
  fs  <- mapM loadFeedback =<< feedbacksOfSubmission sk
  case cgk of
    Left ck  -> do
      course <- loadCourse ck
      return SubmissionDesc {
          eCourse   = courseName course
        , eGroup    = Nothing
        , eStudent  = u
        , eUsername = un
        , eUid      = uid
        , eSolution = submissionValue id (const "zipped") (solution submission)
        , eSubmissionInfo = info
        , eAssignment     = asg
        , eAssignmentKey  = ak
        , eAssignmentDate = created
        , eComments = Map.fromList cs
        , eFeedbacks = fs
        }
    Right gk -> do
      group <- loadGroup gk
      let gname = groupName group
      ck   <- courseOfGroup gk
      cname <- courseName <$> loadCourse ck
      return SubmissionDesc {
          eCourse   = cname
        , eGroup    = Just $ groupName group
        , eStudent  = u
        , eUsername = un
        , eUid      = uid
        , eSolution = submissionValue id (const "zipped") (solution submission)
        , eSubmissionInfo = info
        , eAssignment     = asg
        , eAssignmentKey  = ak
        , eAssignmentDate = created
        , eComments = Map.fromList cs
        , eFeedbacks = fs
        }

-- Calculates the opened submissions for the user from the administrated groups and courses
openedSubmissionInfo :: Username -> Persist OpenedSubmissions
openedSubmissionInfo u = do
  acs <- map fst <$> administratedCourses u
  ags <- map fst <$> administratedGroups  u
  agcs <- (\\ acs) <$> mapM courseOfGroup ags
  courseAsgs <- concat <$> mapM courseAssignments acs
  groupAsgs <- concat <$> mapM groupAssignments  ags
  let isCourseAsg = flip elem courseAsgs
  let isGroupAsg  = flip elem groupAsgs
  relatedCourseAsgs <- concat <$> mapM courseAssignments agcs
  let isRelatedCourseAsg = flip elem relatedCourseAsgs
  courseUser  <- (Set.fromList . concat) <$> mapM subscribedToCourse acs
  subscribedToGroupAgs <- (Set.fromList . concat) <$> mapM subscribedToGroup ags
  let isGroupUser = flip Set.member subscribedToGroupAgs
  subscribedToCourseAgcs <- (flip Set.difference courseUser . Set.fromList . concat) <$> mapM subscribedToCourse agcs
  let isRelatedCourseUser = flip Set.member subscribedToCourseAgcs
  let isCourseUser = flip Set.member courseUser
   -- Non Evaluated Submissions
  nonEvalSubs <- openedSubmissionSubset
                   (Set.fromList $ concat [courseAsgs, groupAsgs, relatedCourseAsgs]) -- Assignments
                   (foldr1 Set.union [subscribedToGroupAgs, subscribedToCourseAgcs, courseUser]) -- Users
  assignmentAndUsers <- mapM assignmentAndUserOfSubmission nonEvalSubs
  let filterSubmissions os (sk, ak, student) =
        let sku = (sk, ()) in
        let separate ak student
              | (isRelatedCourseAsg ak && isGroupUser student)  = os { osAdminedCourse = sku:osAdminedCourse os }
              | (isCourseAsg ak && isGroupUser student)  = os { osAdminedCourse = sku:osAdminedCourse os }
              | (isGroupAsg ak && isGroupUser student)   = os { osAdminedGroup  = sku:osAdminedGroup os  }
              | (isRelatedCourseAsg ak && isRelatedCourseUser student) = os { osRelatedCourse = sku:osRelatedCourse os }
              | (isCourseAsg ak && isRelatedCourseUser student) = os { osRelatedCourse = sku:osRelatedCourse os }
              | (isCourseAsg ak && isCourseUser student) = os { osRelatedCourse = sku:osRelatedCourse os }
              | otherwise = os
        in separate ak student

  let OpenedSubmissions adminedCourse adminedGroup relatedCourse
        = foldl filterSubmissions empty assignmentAndUsers

  OpenedSubmissions
    <$> mapM (submissionKeyAndDesc . fst) adminedCourse
    <*> mapM (submissionKeyAndDesc . fst) adminedGroup
    <*> mapM (submissionKeyAndDesc . fst) relatedCourse
    where
      empty = OpenedSubmissions [] [] []

      submissionKeyAndDesc sk =
        (,) <$> pure sk <*> submissionDesc sk

      assignmentAndUserOfSubmission sk =
        (,,) <$> pure sk <*> assignmentOfSubmission sk <*> usernameOfSubmission sk

-- |Loads information on submissions uploaded by a user to an assignment.
-- The elements in the result list are in reverse chronological order: the first element is the most recent.
userSubmissionInfos :: Username -> AssignmentKey -> Persist [SubmissionInfo]
userSubmissionInfos u ak = do
  us <- userSubmissions u ak
  infos <- mapM submissionInfo  us
  return $ sortSbmDescendingByTime infos

submissionEvalStr :: SubmissionKey -> Persist (Maybe String)
submissionEvalStr sk = do
  mEk <- evaluationOfSubmission sk
  case mEk of
    Nothing -> return Nothing
    Just ek -> eString <$> loadEvaluation ek
  where
    eString = Just . translateMessage trans . resultString . evaluationResult

submissionDetailsDesc :: SubmissionKey -> Persist SubmissionDetailsDesc
submissionDetailsDesc sk = do
  ak <- assignmentOfSubmission sk
  (c, g) <- courseAndGroupOfAssignment ak
  asg <- loadAssignment ak
  sol <- solution <$> loadSubmission sk
  cs  <- commentsOfSubmission sk >>= \cks -> forM cks $ \ck ->
            (,) ck <$> loadComment ck
  fs  <- mapM loadFeedback =<< (feedbacksOfSubmission sk)
  s   <- submissionEvalStr sk
  return SubmissionDetailsDesc {
    sdCourse = c
  , sdGroup   = g
  , sdAssignment = asg
  , sdStatus     = s
  , sdSubmission = submissionValue id (const "zipped") sol
  , sdComments   = Map.fromList cs
  , sdFeedbacks  = fs
  }

-- | Checks if the assignment of the submission is adminstrated by the user
isAdminedSubmission :: Username -> SubmissionKey -> Persist Bool
isAdminedSubmission u sk = do
  -- Assignment of the submission
  ak <- assignmentOfSubmission sk

  -- Assignment Course Key
  ack <- either return (courseOfGroup) =<< (courseOrGroupOfAssignment ak)

  -- All administrated courses
  groupCourses <- mapM (courseOfGroup . fst) =<< (administratedGroups u)
  courses <- map fst <$> administratedCourses u
  let allCourses = nub (groupCourses ++ courses)

  return $ elem ack allCourses


-- TODO
canUserCommentOn :: Username -> SubmissionKey -> Persist Bool
canUserCommentOn _u _sk = return True

-- Produces assignments, information about the submissions and
-- assessments which are associated with subscribed groups of the
-- user.
--
-- Assignments are sorted by deadline in descending order (from future to
-- to the farthest back in time).
--
-- Assessments are sorted by creation time (from oldest to youngest).
userAssignmentsAssessments :: Username -> Persist [(Group, Course, [(AssignmentKey, AssignmentDesc, Maybe (SubmissionKey, SubmissionState))], [(AssessmentKey, Assessment, Maybe ScoreKey, ScoreInfo)])]
userAssignmentsAssessments u = do
  now <- liftIO getCurrentTime
  groups <- userGroups u
  forM groups $ \(ck, course, gk, grp) -> do
    gAsgs <- groupAssignments gk
    cAsgs <- courseAssignments ck
    let asgs = cAsgs ++ gAsgs
    asgDescs <- (sortAssignments . catMaybes) <$> mapM (createAssignmentDesc u now) asgs
    assmnts <- assessmentsOfGroup gk
    assmntDescs <- (sortAssessments . catMaybes) <$> mapM (createAssessmentDesc u) assmnts
    return (grp, course, asgDescs, assmntDescs)

  where
    -- Produces the assignment description if the assignment is active
    -- Returns Nothing if the assignment is not visible for the user
    createAssignmentDesc :: Username -> UTCTime -> AssignmentKey -> Persist (Maybe (AssignmentKey, AssignmentDesc, Maybe (SubmissionKey, SubmissionState)))
    createAssignmentDesc u now ak = do
      a <- loadAssignment ak
      case (now < Assignment.start a) of
        True -> return Nothing
        False -> do
          desc <- assignmentDesc now u ak
          info <- userLastSubmission u ak
          return $ (Just (ak, desc, submKeyAndState <$> info))

    -- Produces an assessment and information about the evaluations for the
    -- assessments.
    createAssessmentDesc :: Username -> AssessmentKey -> Persist (Maybe (AssessmentKey, Assessment, Maybe ScoreKey, ScoreInfo))
    createAssessmentDesc u ak = do
      assessment <- loadAssessment ak
      if Assessment.visible assessment
        then do
          mScoreInfo <- scoreInfoOfUser u ak
          case mScoreInfo of
            Nothing         -> return $ Just (ak, assessment, Nothing, Score_Not_Found)
            Just (sk,sInfo) -> return $ Just (ak, assessment, sk, sInfo)
        else return Nothing

    sortAssignments :: [(a, AssignmentDesc, b)] -> [(a, AssignmentDesc, b)]
    sortAssignments = sortOn (Down . aEndDate . snd3)

    sortAssessments :: [(a, Assessment, b, c)] -> [(a, Assessment, b, c)]
    sortAssessments = sortOn (\(_, as, _, _) -> Assessment.created as)

-- Returns all the submissions of the users for the groups that the
-- user administrates
submissionTables :: Username -> Persist [SubmissionTableInfo]
submissionTables u = do
  groupKeys <- map fst <$> administratedGroups u
  groupTables  <- mapM (groupSubmissionTableInfo) groupKeys
  return groupTables

groupSubmissionTableInfo :: GroupKey -> Persist SubmissionTableInfo
groupSubmissionTableInfo gk = do
  ck <- courseOfGroup gk
  gassignments <- groupAssignments gk
  cassignments <- courseAssignments ck
  usernames   <- subscribedToGroup gk
  name <- fullGroupName gk
  mkGroupSubmissionTableInfo name usernames cassignments gassignments ck gk

-- Returns the course submission table information for the given course key
courseSubmissionTableInfo :: CourseKey -> Persist SubmissionTableInfo
courseSubmissionTableInfo ck = do
  assignments <- courseAssignments ck
  usernames   <- subscribedToCourse ck
  name        <- courseName <$> loadCourse ck
  mkCourseSubmissionTableInfo name usernames assignments ck

-- Sort the given keys into an ordered list based on the time function
sortKeysByTime :: (key -> Persist UTCTime) -> [key] -> Persist [key]
sortKeysByTime time keys = map snd . sortOn fst <$> mapM getTime keys
  where
    getTime k = do
      t <- time k
      return (t,k)

loadAssignmentInfos :: [AssignmentKey] -> Persist [(AssignmentKey, Assignment, HasTestCase)]
loadAssignmentInfos as = mapM loadAssignmentInfo as
  where
    loadAssignmentInfo a = do
       asg <- loadAssignment a
       hasTestCase <- maybe DoesNotHaveTestCase (const HasTestCase) <$> testCaseOfAssignment a
       return (a, asg, hasTestCase)

lastSubmissionAsgKey :: Username -> AssignmentKey -> Persist (AssignmentKey, Maybe SubmissionInfo)
lastSubmissionAsgKey u ak = addKey <$> (userLastSubmission u ak)
  where
    addKey s = (ak,s)

mkCourseSubmissionTableInfo
  :: String -> [Username] -> [AssignmentKey] -> CourseKey
  -> Persist SubmissionTableInfo
mkCourseSubmissionTableInfo courseName us as key = do
  assignments <- sortKeysByTime assignmentCreatedTime as
  assignmentInfos <- loadAssignmentInfos as
  users <- subscribedToCourse key
  ulines <- forM users $ \u -> do
    ud <- userDescription u
    sInfos <- mapM (lastSubmissionAsgKey u) as
    return (ud, Map.fromList . map (second submKeyAndState) . removeNotFound $ sInfos)
  groups <- groupKeysOfCourse key
  groupOfUser <- fmap (Map.fromList . concat) $ forM groups $ \gkey -> do
    group_ <- loadGroup gkey
    admins <- groupAdmins gkey
    users <- subscribedToGroup gkey
    return $ [(u, (group_, admins)) | u <- users]

  return CourseSubmissionTableInfo {
      stiCourse = courseName
    , stiUsers = us
    , stiAssignments = assignmentInfos
    , stiUserLines = ulines
    , stiGroups = groupOfUser
    , stiCourseKey = key
    }

mkGroupSubmissionTableInfo
  :: String
  -> [Username] -> [AssignmentKey] -> [AssignmentKey]
  -> CourseKey -> GroupKey
  -> Persist SubmissionTableInfo
mkGroupSubmissionTableInfo courseName us cas gas ckey gkey = do
  cAssignmentInfos <- loadAssignmentInfos cas
  gAssignmentInfos <- loadAssignmentInfos gas
  cgAssignments <- sortKeysByTime createdTime (map CourseInfo cAssignmentInfos ++ map GroupInfo gAssignmentInfos)
  ulines <- forM us $ \u -> do
    ud <- userDescription u
    casInfos <- mapM (lastSubmissionAsgKey u) cas
    gasInfos <- mapM (lastSubmissionAsgKey u) gas
    return (ud, Map.fromList . map (second submKeyAndState) . removeNotFound $ casInfos ++ gasInfos)
  return GroupSubmissionTableInfo {
      stiCourse = courseName
    , stiUsers = us
    , stiCGAssignments = cgAssignments
    , stiUserLines = ulines
    , stiCourseKey = ckey
    , stiGroupKey  = gkey
    }
  where
    createdTime :: CGInfo (AssignmentKey, Assignment, a) -> Persist UTCTime
    createdTime = cgInfoCata
      (assignmentCreatedTime . fst3)
      (assignmentCreatedTime . fst3)

removeNotFound :: [(a, Maybe b)] -> [(a, b)]
removeNotFound abs = [(a, b) | (a, Just b) <- abs]

-- |Loads information on a 'Submission'.
-- It loads a 'Submission' exactly once from the database, to get the time of upload.
submissionInfo :: SubmissionKey -> Persist SubmissionInfo
submissionInfo sk = do
  state <- stateOfSubmission sk
  submission <- loadSubmission sk
  return $ (sk, state, (solutionPostDate submission))

-- Produces the score key, score info for the specific user and assessment.
-- Returns Nothing if there are multiple scoreinfos available.
scoreInfoOfUser :: Username -> AssessmentKey -> Persist (Maybe (Maybe ScoreKey, ScoreInfo))
scoreInfoOfUser u ak = do
  scoreKeys <- scoreOfAssessmentAndUser u ak
  case scoreKeys of
    []   -> return . Just $ (Nothing, Score_Not_Found)
    [sk] -> do info <- scoreInfo sk
               return . Just $ (Just sk,info)
    _    -> return Nothing

-- Produces information for the given score
scoreInfo :: ScoreKey -> Persist ScoreInfo
scoreInfo sk = do
  mEk <- evaluationOfScore sk
  case mEk of
    Nothing -> return Score_Not_Found
    Just ek -> Score_Result ek . evaluationResult <$> loadEvaluation ek

-- Produces the info on the last submission for the given user and assignment
userLastSubmission :: Username -> AssignmentKey -> Persist (Maybe SubmissionInfo)
userLastSubmission u ak =
  (maybe (return Nothing) ((Just <$>) . submissionInfo)) =<< lastSubmission ak u

-- Helper computation which removes the given submission from
-- the opened submission directory, which is optimized by
-- assignment and username keys, for the quickier lookup
removeOpenedSubmission :: SubmissionKey -> Persist ()
removeOpenedSubmission sk = do
  ak <- assignmentOfSubmission sk
  u  <- usernameOfSubmission sk
  removeFromOpened ak u sk

-- Make unsibscribe a user from a course if the user attends in the course
-- otherwise do nothing
deleteUserFromCourse :: CourseKey -> Username -> Persist ()
deleteUserFromCourse ck u = do
  groups <- groupsOfUsersCourse u ck
  traverse_ (unsubscribe u) groups

testScriptInfo :: TestScriptKey -> Persist TestScriptInfo
testScriptInfo tk = do
  script <- loadTestScript tk
  return TestScriptInfo {
      tsiName = tsName script
    , tsiDescription = tsDescription script
    , tsiType = tsType script
    }

-- Returns True if the given student submitted at least one solution for the
-- assignments for the given group, otherwise False
isThereASubmissionForGroup :: Username -> GroupKey -> Persist Bool
isThereASubmissionForGroup u gk = do
  aks <- groupAssignments gk
  (not . null . catMaybes) <$> mapM (flip (lastSubmission) u) aks

-- Returns True if the given student submitted at least one solution for the
-- assignments for the given group, otherwise False
isThereASubmissionForCourse :: Username -> CourseKey -> Persist Bool
isThereASubmissionForCourse u ck = do
  aks <- courseAssignments ck
  (not . null . catMaybes) <$> mapM (flip (lastSubmission) u) aks

-- Returns the number of the possible submission for the given assignment
-- by the given user.
submissionLimitOfAssignment :: Username -> AssignmentKey -> Persist SubmissionLimit
submissionLimitOfAssignment username key =
  calcSubLimit <$> (loadAssignment key) <*> (length <$> userSubmissions username key)

scoreBoards :: Username -> Persist (Map (Either CourseKey GroupKey) ScoreBoard)
scoreBoards u = do
  groupKeys <- map (Right . fst) <$> administratedGroups u
  courseKeys <- map (Left . fst) <$> administratedCourses u
  let keys = courseKeys ++ groupKeys
  Map.fromList . zip keys <$> mapM scoreBoard keys

scoreBoardOfGroup :: GroupKey -> Persist ScoreBoard
scoreBoardOfGroup = scoreBoard . Right

scoreBoard :: Either CourseKey GroupKey -> Persist ScoreBoard
scoreBoard key = do
  assessmentKeys <- assessmentsOf
  users <- subscriptions
  board <- foldM boardColumn (Map.empty,Map.empty) assessmentKeys
  assessments <- mapM loadAssessment assessmentKeys
  userDescriptions <- mapM userDescription users
  name <- loadName
  return $ mkScoreBoard board name (sortByCreationTime (zip assessmentKeys assessments)) userDescriptions
  where
        mkScoreBoard (scores,infos) n as us =
          either (\k -> CourseScoreBoard scores infos k n as us)
                 (\k -> GroupScoreBoard scores infos k n as us)
                 key
        assessmentsOf = either assessmentsOfCourse assessmentsOfGroup key
        subscriptions = either subscribedToCourse subscribedToGroup key
        loadName      = either (fmap courseName . loadCourse) (fmap groupName . loadGroup) key
        boardColumn :: (Map (AssessmentKey,Username) ScoreKey,Map ScoreKey ScoreInfo)
                    -> AssessmentKey
                    -> Persist (Map (AssessmentKey,Username) ScoreKey,Map ScoreKey ScoreInfo)
        boardColumn board assessment = do
                       scoresKeys <- scoresOfAssessment assessment
                       foldM (cell assessment) board scoresKeys

        cell assessment (scores,infos) scoreKey = do
                       user <- usernameOfScore scoreKey
                       info <- scoreInfo scoreKey
                       return (Map.insert (assessment,user) scoreKey scores,Map.insert scoreKey info infos)

        sortByCreationTime :: [(AssessmentKey, Assessment)] -> [(AssessmentKey, Assessment)]
        sortByCreationTime = sortOn (Assessment.created . snd)

scoreDesc :: ScoreKey -> Persist ScoreDesc
scoreDesc sk = do
  ak <- assessmentOfScore sk
  as <- loadAssessment ak
  info <- scoreInfo sk
  courseOrGroup <- courseOrGroupOfAssessment ak
  (course, group) <- case courseOrGroup of
    Left ck -> do
      course <- loadCourse ck
      return (course, Nothing)
    Right gk -> do
        group <- loadGroup gk
        ck <- courseOfGroup gk
        course <- loadCourse ck
        return (course, Just group)
  return $ ScoreDesc course group info as

assessmentDesc :: AssessmentKey -> Persist AssessmentDesc
assessmentDesc ak = do
  courseOrGroup <- courseOrGroupOfAssessment ak
  (course,group,teachers) <- case courseOrGroup of
    Left ck -> do
      course <- loadCourse ck
      teachers <- courseAdmins ck
      return (courseName course, Nothing, teachers)
    Right gk -> do
      group <- loadGroup gk
      ck <- courseOfGroup gk
      course <- loadCourse ck
      teachers <- groupAdmins gk
      return (courseName course, Just . groupName $ group, teachers)
  assessment <- loadAssessment ak
  return $ AssessmentDesc course group (map u_name teachers) ak assessment

courseOrGroupOfAssessment :: AssessmentKey -> Persist (Either CourseKey GroupKey)
courseOrGroupOfAssessment ak = do
  maybeGk <- groupOfAssessment ak
  case maybeGk of
    Just gk -> return . Right $ gk
    Nothing -> do
      maybeCk <- courseOfAssessment ak
      case maybeCk of
        Just ck -> return . Left $ ck
        Nothing -> error $ "Impossible: No course or groupkey was found for the assessment:" ++ show ak

notificationReference :: Notification.NotificationType -> Persist Notification.NotificationReference
notificationReference = Notification.notificationType comment evaluation assignment assessment system
  where
    comment ck = do
      sk <- submissionOfComment ck
      ak <- assignmentOfSubmission sk
      return $ Notification.NRefComment ak sk ck

    evaluation ek = do
      msubk <- submissionOfEvaluation ek
      mscrk <- scoreOfEvaluation ek
      case (msubk, mscrk) of
        (Nothing, Nothing) -> error "No submission or score are found for evaluation."
        (Just _, Just _)   -> error "Both submission and score are found for evaluation."
        (Just sk, Nothing) -> do
          ak <- assignmentOfSubmission sk
          return $ Notification.NRefSubmissionEvaluation ak sk ek
        (Nothing, Just sk) ->
          return $ Notification.NRefScoreEvaluation sk ek

    assignment ak = return $ Notification.NRefAssignment ak

    assessment ak = return $ Notification.NRefAssessment ak

    system = return Notification.NRefSystem

#ifdef TEST
persistRelationsTests :: TestSet ()
persistRelationsTests = do
  groupsOfUsersCourseTest
#endif
