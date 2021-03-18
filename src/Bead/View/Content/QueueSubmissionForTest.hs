module Bead.View.Content.QueueSubmissionForTest
  ( queueSubmissionForTest
  , queueAllSubmissionsForTest
  ) where

import qualified Bead.Controller.Pages as Pages
import qualified Bead.Controller.UserStories as Story
import           Bead.View.Content

queueSubmissionForTest :: ModifyHandler
queueSubmissionForTest = ModifyHandler $ do
  sk <- getParameter submissionKeyPrm
  return $ Action $ do
    Story.queueSubmissionForTest sk
    return $ redirection $ Pages.evaluation sk ()

queueAllSubmissionsForTest :: ModifyHandler
queueAllSubmissionsForTest = ModifyHandler $ do
  ak <- getParameter assignmentKeyPrm
  return $ Action $ do
    Story.queueAllSubmissionsForTest ak
    ckGk <- Story.courseOrGroupOfAssignment ak
    return $ redirection $ either
      (\ck -> Pages.courseManagement ck Pages.AssignmentsContents ())
      (\gk -> Pages.groupOverview gk ())
      ckGk
