{-# LANGUAGE CPP, TypeFamilies, FlexibleInstances #-}
module Bead.Persistence.SQL.Submission where

import           Control.Applicative
import           Data.Bifunctor (bimap)
import qualified Data.ByteString.Char8 as BC
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as Text

import           Database.Persist.Sql
import           Database.Esqueleto ( select, from, where_, on, like
                                    , limit, orderBy, desc, InnerJoin(InnerJoin)
                                    , val, (&&.), (^.), unValue
                                    )
import qualified Database.Esqueleto as Esq

import qualified Bead.Domain.Entities as Domain
import qualified Bead.Domain.Relationships as Domain
import qualified Bead.Domain.Shared.Evaluation as Domain
import           Bead.Persistence.SQL.Class
import           Bead.Persistence.SQL.Entities
import           Bead.Persistence.SQL.JSON (decodeEvaluationResult, decodeFeedbackInfo)

#ifdef TEST
import qualified Data.Set as Set
import           Data.String (fromString)

import           Bead.Persistence.SQL.Assignment
import           Bead.Persistence.SQL.Course
import           Bead.Persistence.SQL.User
import           Bead.Persistence.SQL.MySQLTestRunner
import           Bead.Persistence.SQL.TestData

import           Test.Tasty.TestSet (TestSet, ioTest, shrink, equals)
#endif

-- * Submission

-- Saves the submission for a given assignment, submitted by the given user
saveSubmission :: Domain.AssignmentKey -> Domain.Username -> Domain.Submission -> Persist Domain.SubmissionKey
saveSubmission assignmentKey username submission =
  withUser
    username
    (persistError "saveSubmission" $ "No user is found" ++ show username)
    (\userEnt -> do
       key <- insert (fromDomainValue submission)
       let assignmentKey' = toEntityKey assignmentKey
           userKey        = entityKey userEnt
       insertUnique (SubmissionsOfAssignment assignmentKey' key)
       insertUnique (UserOfSubmission key userKey)
       insert (UserSubmissionOfAssignment key assignmentKey' userKey)
       insert (OpenedSubmission key assignmentKey' userKey)
       return $! toDomainKey key)

-- Loads the given submission from the database
loadSubmission :: Domain.SubmissionKey -> Persist Domain.Submission
loadSubmission key = do
  mSub <- get (toEntityKey key)
  return $!
    maybe
      (persistError "loadSubmission" $ "submission is not found." ++ show key)
      toDomainValue
      mSub

-- Returns the assignment for the submission
assignmentOfSubmission :: Domain.SubmissionKey -> Persist Domain.AssignmentKey
assignmentOfSubmission key = do
  assignments <- selectList [SubmissionsOfAssignmentSubmission ==. toEntityKey key] []
  return $!
    maybe
      (persistError "assignmentOfSubmission" $ "no submission was found " ++ show key)
      (toDomainKey . submissionsOfAssignmentAssignment . entityVal)
      (listToMaybe assignments)

-- Returns the username for the submission
usernameOfSubmission :: Domain.SubmissionKey -> Persist Domain.Username
usernameOfSubmission key = do
  usersOfSub <- selectList [UserOfSubmissionSubmission ==. toEntityKey key] []
  maybe
    (persistError "usernameOfSubmission" $ "No submission was found " ++ show key)
    (\userOfSub -> do
        let userId = userOfSubmissionUser $ entityVal userOfSub
        mUser <- get userId
        maybe
          (persistError "usernameOfSubmission" $ "No user us found " ++ show userId)
          (\user -> return $! Domain.Username . Text.unpack $ userUsername user)
          mUser)
    (listToMaybe usersOfSub)

-- Lists all the submissions stored in the database
submissionKeys :: Persist [Domain.SubmissionKey]
submissionKeys = map toDomainKey <$> selectSubmissionKeys
  where
    selectSubmissionKeys :: Persist [Key Submission]
    selectSubmissionKeys = selectKeysList [] []

-- Returns the evaluation for the submission if the evalution exist, otherwise Nothing
evaluationOfSubmission :: Domain.SubmissionKey -> Persist (Maybe Domain.EvaluationKey)
evaluationOfSubmission key = do
  es <- selectList [ SubmissionOfEvaluationSubmission ==. toEntityKey key] []
  return $
    fmap (toDomainKey . submissionOfEvaluationEvaluation . entityVal)
         (listToMaybe es)

stateOfSubmission :: Domain.SubmissionKey -> Persist Domain.SubmissionState
stateOfSubmission sk = do
  mEvResult <- evaluation
  case mEvResult of
    Nothing -> do
      mF <- testFeedback
      return $ case mF of
        Just (Domain.TestResult r) -> Domain.Submission_Tested r
        _                          -> Domain.Submission_Unevaluated
    Just (ek, evResult) -> return $ Domain.Submission_Result ek evResult
  where
    sk' :: Key Submission
    sk' = toEntityKey sk

    evaluation :: Persist (Maybe (Domain.EvaluationKey, Domain.EvResult))
    evaluation = do
      es <- select $ from $ \(se `InnerJoin` e) -> do
        on (se ^. SubmissionOfEvaluationEvaluation Esq.==. e ^. EvaluationId)
        where_ (se ^. SubmissionOfEvaluationSubmission Esq.==. val sk')
        limit 1
        return (e ^. EvaluationId, e ^. EvaluationResult)
      return (bimap (fromEntityKey . unValue) (decodeEvaluationResult . unValue) <$> listToMaybe es)

    testFeedback :: Persist (Maybe Domain.FeedbackInfo)
    testFeedback = do
      fs <- select $ from $ \(fs `InnerJoin` f) -> do
        on (fs ^. FeedbacksOfSubmissionFeedback Esq.==. f ^. FeedbackId)
        where_ (fs ^. FeedbacksOfSubmissionSubmission Esq.==. val sk' &&. (f ^. FeedbackInfo `like` val "{\"TestResult\":%"))
        orderBy [desc (f ^. FeedbackDate)]
        limit 1
        return (f ^. FeedbackInfo)
      return (decodeFeedbackInfo . unValue <$> listToMaybe fs)

-- Returns all the comments for the given submission
commentsOfSubmission :: Domain.SubmissionKey -> Persist [Domain.CommentKey]
commentsOfSubmission key =
  map (toDomainKey . commentsOfSubmissionComment . entityVal) <$>
    selectList [CommentsOfSubmissionSubmission ==. toEntityKey key] []

-- Returns all the feedbacks for the given submission
feedbacksOfSubmission :: Domain.SubmissionKey -> Persist [Domain.FeedbackKey]
feedbacksOfSubmission key =
  map (toDomainKey . feedbacksOfSubmissionFeedback . entityVal) <$>
    selectList [FeedbacksOfSubmissionSubmission ==. toEntityKey key] []

-- Returns the last submission of an assignment submitted by the given user if the
-- user is submitted something otherwise Nothing
lastSubmission :: Domain.AssignmentKey -> Domain.Username -> Persist (Maybe Domain.SubmissionKey)
lastSubmission assignmentKey username = do
  submissionKeys <- select $ from $ \(u `InnerJoin` usa `InnerJoin` s) -> do
    on (u ^. UserId Esq.==. usa ^. UserSubmissionOfAssignmentUser &&.
        usa ^. UserSubmissionOfAssignmentSubmission Esq.==. s ^. SubmissionId)
    where_ (u ^. UserUsername Esq.==. val (Domain.usernameCata Text.pack username) &&.
            usa ^. UserSubmissionOfAssignmentAssignment Esq.==. val (toEntityKey assignmentKey))
    orderBy [desc (s ^. SubmissionPostDate)]
    limit 1
    return (s ^. SubmissionId)
  return (fromEntityKey . unValue <$> listToMaybe submissionKeys)

-- Remove the submission from the opened (which need to be evaluated) queue
removeFromOpened :: Domain.AssignmentKey -> Domain.Username -> Domain.SubmissionKey -> Persist ()
removeFromOpened ak username sk = withUser username (return ()) $ \userEnt -> void $ do
  deleteWhere
    [ OpenedSubmissionSubmission ==. toEntityKey sk
    , OpenedSubmissionAssignment ==. toEntityKey ak
    , OpenedSubmissionUser       ==. entityKey userEnt
    ]

-- Returns all the opened submissions
openedSubmissions :: Persist [Domain.SubmissionKey]
openedSubmissions = do
  openeds <- selectOpenedSubmissions
  return $! map (toDomainKey . openedSubmissionSubmission . entityVal) openeds
  where
    selectOpenedSubmissions :: Persist [Entity OpenedSubmission]
    selectOpenedSubmissions = selectList [] []

-- Returns the opened submissions that are associated with the given assignments or users
openedSubmissionSubset :: Set Domain.AssignmentKey -> Set Domain.Username -> Persist [Domain.SubmissionKey]
openedSubmissionSubset assignemts users = do
  userKeys <- catMaybes <$> mapM userKey (Set.toList users)
  openeds <- selectList
    ([ OpenedSubmissionAssignment <-. (map toEntityKey $ Set.toList assignemts) ]
      ||. [ OpenedSubmissionUser <-. userKeys ])
    []
  return $! map (toDomainKey . openedSubmissionSubmission . entityVal) openeds

-- Calculates all the opened submission for a given user and a given assignment
usersOpenedSubmissions :: Domain.AssignmentKey -> Domain.Username -> Persist [Domain.SubmissionKey]
usersOpenedSubmissions key username =
  withUser
    username
    (persistError "usersOpenedSubmissions" $ "No user is found" ++ show username)
    (\userEnt -> do
       map (toDomainKey . openedSubmissionSubmission . entityVal) <$> selectList
         [ OpenedSubmissionAssignment ==. toEntityKey key
         , OpenedSubmissionUser       ==. entityKey userEnt
         ] [])

#ifdef TEST
submissionTests :: TestSet ()
submissionTests = do
  shrink "Submission end-to-end story."
    (do ioTest "Submission end-to-end test case" $ runSql $ do
          c  <- saveCourse course
          ca <- saveCourseAssignment c asg
          saveUser user1
          ls0 <- lastSubmission ca user1name
          equals Nothing ls0 "Some submission is returned"
          s  <- saveSubmission ca user1name sbm
          os <- openedSubmissions
          equals [s] os "The opened submission returned wrong set"
          sbm' <- loadSubmission s
          equals sbm sbm' "Saved and loaded submission were different."
          ca' <- assignmentOfSubmission s
          equals ca ca' "Wrong assignment key was returned for the submission"
          uname' <- usernameOfSubmission s
          equals user1name uname' "Wrong username was returned for the submission"
          ls <- lastSubmission ca user1name
          equals (Just s) ls "Wrong last submission was returned"
          s2 <- saveSubmission ca user1name sbm2
          ls <- lastSubmission ca user1name
          equals (Just s2) ls "Wrong last submission after a resubmission"
          os <- openedSubmissions
          equals
            (Set.fromList [s,s2])
            (Set.fromList os)
            "The opened submission returned wrong set after the second submission"
          saveUser user2
          s3 <- saveSubmission ca user2name sbm
          os2 <- openedSubmissions
          equals
            (Set.fromList [s,s2,s3])
            (Set.fromList os2)
            "The opened submission returned wrong set after the second user's submission"
          os4 <- usersOpenedSubmissions ca user1name
          equals
            (Set.fromList [s,s2])
            (Set.fromList os4)
            "The users submission set was not calculated correctly #1"
          os5 <- usersOpenedSubmissions ca user2name
          equals
            (Set.fromList [s3])
            (Set.fromList os5)
            "The users submission set was not calculated correctly #2"
          us1 <- userSubmissions user1name ca
          equals
            (Set.fromList [s,s2])
            (Set.fromList us1)
            "Submissions for user1 and course assignment were wrong"
          us2 <- userSubmissions user2name ca
          equals
            (Set.fromList [s3])
            (Set.fromList us2)
            "Submissions for user2 and course assignment were wrong"
          ca2 <- saveCourseAssignment c asg
          us3 <- userSubmissions user1name ca2
          equals [] us3 "Submissions found for user assignment pair that does not have any"
          removeFromOpened ca user1name s
          os6 <- openedSubmissions
          equals
            (Set.fromList [s2,s3])
            (Set.fromList os6)
            "The opened submission returned wrong set after removing one opened submissions for user1"
          return ())
    (do ioTest "Save and load submission" $ runSql $ do
          c  <- saveCourse course
          ca <- saveCourseAssignment c asg
          saveUser user1
          s  <- saveSubmission ca user1name sbm
          sbm' <- loadSubmission s
          equals sbm sbm' "Saved and loaded submission were different.")
#endif
