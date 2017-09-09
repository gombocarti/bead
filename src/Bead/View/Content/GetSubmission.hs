{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.GetSubmission (
    getSubmission
  , submissionFilename
  ) where

import           Control.Monad.Trans (lift)
import           Data.String (fromString)
import qualified Data.ByteString.Lazy as LBs
import qualified Data.ByteString.Lazy.UTF8 as LBsUTF8 (fromString)
import           System.FilePath ((<.>))

import           Bead.Domain.Entities (usernameCata)
import qualified Bead.Domain.Entity.Assignment as Assignment
import qualified Bead.Controller.UserStories as Story
import           Bead.View.Content
import qualified Bead.View.ContentHandler as CH

getSubmission :: DataHandler
getSubmission = DataHandler $ do
  sk <- getParameter submissionKeyPrm
  (s, description) <- userStory (Story.getSubmission sk)
  let submission = solution s
      (fname, ext) = submissionFilename description
      downloadFile = CH.downloadFile (fname <.> ext)
  submissionValue
    (\s -> downloadFile (LBsUTF8.fromString s) CH.MimePlainText)
    (\s -> downloadFile (LBs.fromStrict s) CH.MimeZip)
    submission

-- | Returns a pair of filename and extension from a `SubmissionDesc`.
submissionFilename :: SubmissionDesc -> (String, String)
submissionFilename desc = (basename, ext)
    where
      basename = concat [eStudent desc, " (", uid id $ eUid desc, ")"]
      ext = if (Assignment.isZippedSubmissions . Assignment.aspects . eAssignment $ desc) then "zip" else "txt"

