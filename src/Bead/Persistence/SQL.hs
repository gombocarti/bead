{-# LANGUAGE CPP #-}
module Bead.Persistence.SQL (
    module Bead.Persistence.SQL.Assessment
  , module Bead.Persistence.SQL.Assignment
  , module Bead.Persistence.SQL.Comment
  , module Bead.Persistence.SQL.Course
  , module Bead.Persistence.SQL.Entities
  , module Bead.Persistence.SQL.Evaluation
  , module Bead.Persistence.SQL.Feedback
  , module Bead.Persistence.SQL.FileSystem
  , module Bead.Persistence.SQL.Group
  , module Bead.Persistence.SQL.MySQL
  , module Bead.Persistence.SQL.Moss
  , module Bead.Persistence.SQL.Notification
#ifndef SSO
  , module Bead.Persistence.SQL.Registration
#endif
  , module Bead.Persistence.SQL.Score
  , module Bead.Persistence.SQL.Submission
  , module Bead.Persistence.SQL.TestCase
  , module Bead.Persistence.SQL.TestJob
  , module Bead.Persistence.SQL.TestScript
  , module Bead.Persistence.SQL.User
#ifdef TEST
  , tests
#endif
  ) where

import Bead.Persistence.SQL.Assessment
import Bead.Persistence.SQL.Assignment
import Bead.Persistence.SQL.Comment
import Bead.Persistence.SQL.Course
import Bead.Persistence.SQL.Entities (Persist)
import Bead.Persistence.SQL.Evaluation
import Bead.Persistence.SQL.Feedback
import Bead.Persistence.SQL.FileSystem (testIncoming, testFeedbacks, finalizeTestFeedback, insertTestFeedback, uploadForMoss)
import Bead.Persistence.SQL.Group
import Bead.Persistence.SQL.MySQL
import Bead.Persistence.SQL.Moss
import Bead.Persistence.SQL.Notification
#ifndef SSO
import Bead.Persistence.SQL.Registration
#endif
import Bead.Persistence.SQL.Score
import Bead.Persistence.SQL.Submission
import Bead.Persistence.SQL.TestCase
import Bead.Persistence.SQL.TestJob
import Bead.Persistence.SQL.TestScript
import Bead.Persistence.SQL.User

#ifdef TEST
import Bead.Persistence.SQL.JSON (persistJSONConvertTests)

import Test.Tasty.TestSet (TestSet)
#endif

#ifdef TEST
tests = do
  persistJSONConvertTests
  administratedCourseTest
  groupTests
  testScriptTests
  assignmentTests
  assessmentTests
  testCaseTests
  submissionTests
  evaluationTests
  commentTests
  feedbackTests
#ifndef SSO
  userRegistrationTests
#endif
  scoreTests
#endif
