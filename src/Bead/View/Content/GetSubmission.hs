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
import           Bead.View.Content
import qualified Bead.View.ContentHandler as CH

getSubmission :: DataHandler
getSubmission = DataHandler $ do
  sk <- getParameter submissionKeyPrm
  (s, user) <- userStory $ do
    Story.doesBlockSubmissionView sk
    Story.isAccessibleBallotBoxSubmission sk
    subm <- Story.getSubmission sk
    u <- Story.userOfSubmission sk
    return (subm, u)
  let submission = solution s
      (fname, ext) = submissionFilename user s
      fileName = (T.pack $ fname <.> ext)
  submissionValue
    (downloadText fileName)
    (downloadStrict fileName CH.MimeZip)
    submission
