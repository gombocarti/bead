{-# LANGUAGE OverloadedStrings, CPP #-}
module Bead.View.Content.Home.Data where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Time
import           Data.Tuple.Utils (snd3)

import           Bead.View.Content hiding (userState)
import           Bead.View.Content.SubmissionTable

type ActiveAssignment = (AssignmentKey, AssignmentDesc, Maybe (SubmissionKey, SubmissionState))

type ActiveAssessment = (AssessmentKey, Assessment, Maybe ScoreKey, ScoreInfo)

activeAsgDesc :: ActiveAssignment -> AssignmentDesc
activeAsgDesc = snd3

hasAssessments :: (Group, Course, [ActiveAssignment], [ActiveAssessment]) -> Bool
hasAssessments (_, _, _, assmnts) = not . null $ assmnts

hasAssignments :: (Group, Course, [ActiveAssignment], [ActiveAssessment]) -> Bool
hasAssignments (_, _, asgs, _) = not . null $ asgs

data HomePageData = HomePageData {
    userState   :: UserState
  , hasCourses  :: Bool -- True if the user has administrated courses
  , hasGroups   :: Bool -- True if the user has administrated groups
  , assignmentsAssessments :: [(Group, Course, [ActiveAssignment], [ActiveAssessment])]
  , sTables     :: [SubmissionTableInfo]
  , assessmentTables :: Map (Either CourseKey GroupKey) ScoreBoard
  , timeConverter :: UserTimeConverter
    -- ^ The converter function that converts a given utc time into the user's local timezone
  , submissionTableCtx :: SubmissionTableContext
  , now :: UTCTime
  }

administratedCourseMap = stcAdminCourses . submissionTableCtx
administratedGroupMap  = stcAdminGroups  . submissionTableCtx
courseTestScripts      = stcCourseTestScriptInfos . submissionTableCtx
