{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.GetSubmission (
    getSubmission
  , submissionFilename
  ) where

import           Control.Monad.Trans (lift)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.UTF8 as BsUTF8 (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import           System.FilePath ((<.>))

import qualified Bead.Controller.UserStories as Story
import qualified Bead.Domain.Entity.Assignment as Assignment
import           Bead.View.Content
import qualified Bead.View.ContentHandler as CH

getSubmission :: DataHandler
getSubmission = DataHandler $ do
  sk <- getParameter submissionKeyPrm
  (s, description) <- userStory $ do
    Story.doesBlockSubmissionView sk
    Story.isAccessibleBallotBoxSubmission sk
    Story.getSubmission sk
  let submission = solution s
      (fname, ext) = submissionFilename description
      fileName = (T.pack $ fname <.> ext)
  submissionValue
    (downloadText fileName)
    (downloadStrict fileName CH.MimeZip)
    submission

-- | Returns a pair of filename and extension from a `SubmissionDesc`.
submissionFilename :: SubmissionDesc -> (String, String)
submissionFilename desc = (basename, ext)
    where
      basename = concat [eStudent desc, " (", uid id $ eUid desc, ")"]
      ext = if (Assignment.isZippedSubmissions . Assignment.aspects . eAssignment $ desc) then "zip" else "txt"
