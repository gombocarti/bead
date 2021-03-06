{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Assignment.Data where

import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Bead.View.Content

-- * Content Handlers

data PageData
  = PD_Course {
      pdTimeZone    :: UserTimeConverter
    , pdTime        :: UTCTime
    , pdCourse      :: (CourseKey, Course)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile FilePath]
    }
  | PD_Group {
      pdTimeZone    :: UserTimeConverter
    , pdTime        :: UTCTime
    , pdGroup       :: (GroupKey, Group)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile FilePath]
    }
  | PD_Assignment {
      pdTimeZone      :: UserTimeConverter
    , pdAssignmentKey :: AssignmentKey
    , pdAssignment    :: Assignment
    , pdTestScripts   :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile     :: [UsersFile FilePath]
    , pdTestCase      :: Maybe (TestCaseKey, TestCase, TestScriptKey)
    }
  | PD_ViewAssignment {
      pdTimeZone      :: UserTimeConverter
    , pdAssignmentKey :: AssignmentKey
    , pdAssignment    :: Assignment
    , pdTestScript    :: Maybe TestScriptInfo
    , pdTestCaseInfo  :: Maybe (TestCaseKey, TestCase, TestScriptKey)
    }
  | PD_Course_Preview {
      pdTimeZone    :: UserTimeConverter
    , pdTime        :: UTCTime
    , pdCourse      :: (CourseKey, Course)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile FilePath]
    , pdAssignmentPreview :: Assignment
    , pdTCCreationPreview :: TCCreationParameters
    }
  | PD_Group_Preview {
      pdTimeZone    :: UserTimeConverter
    , pdTime        :: UTCTime
    , pdGroup       :: (GroupKey, Group)
    , pdTestScripts :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile   :: [UsersFile FilePath]
    , pdAssignmentPreview :: Assignment
    , pdTCCreationPreview :: TCCreationParameters
    }
  | PD_Assignment_Preview {
      pdTimeZone      :: UserTimeConverter
    , pdAssignmentKey :: AssignmentKey
    , pdAssignment    :: Assignment
    , pdTestScripts   :: Maybe [(TestScriptKey, TestScriptInfo)]
    , pdUsersFile     :: [UsersFile FilePath]
    , pdTestCase      :: Maybe (TestCaseKey, TestCase, TestScriptKey)
    , pdTCModificationPreview :: TCModificationParameters
    }

type TCModificationParameters = (Maybe (Maybe TestScriptKey), Maybe (Either () (UsersFile FilePath)), Maybe Text)

tcmpTextTestCase (_,_,t) = t
tcmpFileTestCase (_,t,_) = t
tcmpTestScriptKey (k,_,_) = k

type TCCreationParameters = (Maybe (Maybe TestScriptKey), Maybe (UsersFile FilePath), Maybe Text)

tccTestScriptKey (k,_,_) = k
tccFileTestCase  (_,t,_) = t
tccTextTestCase  (_,_,t) = t

pageDataCata
  course
  group
  assignment
  viewAssignment
  coursePreview
  groupPreview
  assignmentPreview
  p = case p of

  PD_Course timezone time courses tsType files ->
     course timezone time courses tsType files

  PD_Group  timezone time groups tsType files ->
     group  timezone time groups tsType files

  PD_Assignment timezone key asg tsType files testcase ->
     assignment timezone key asg tsType files testcase

  PD_ViewAssignment timezone key asg tsInfo testcase ->
     viewAssignment timezone key asg tsInfo testcase

  PD_Course_Preview timezone time courses tsType files assignment tccreation ->
     coursePreview  timezone time courses tsType files assignment tccreation

  PD_Group_Preview timezone time groups tsType files assignment tccreation ->
     groupPreview  timezone time groups tsType files assignment tccreation

  PD_Assignment_Preview timezone key asg tsType files testcase tcmod ->
     assignmentPreview  timezone key asg tsType files testcase tcmod

