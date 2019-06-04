module Bead.View.Content.QueueSubmissionForTest
  ( queueSubmissionForTest
  , queueAllSubmissionsForTest
  ) where

import qualified Bead.Controller.UserStories as Story
import           Bead.View.Content

queueSubmissionForTest :: ModifyHandler
queueSubmissionForTest = ModifyHandler $ do
  sk <- getParameter submissionKeyPrm
  userStory $ Story.queueSubmissionForTest sk
  return NoUserAction

queueAllSubmissionsForTest :: ModifyHandler
queueAllSubmissionsForTest = ModifyHandler $ do
  ak <- getParameter assignmentKeyPrm
  userStory $ Story.queueAllSubmissionsForTest ak
  return NoUserAction
