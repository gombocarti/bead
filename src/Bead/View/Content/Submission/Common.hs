{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Submission.Common where

import           Data.Text (Text, pack)

import           Bead.View.Content

remainingTries :: I18N -> SubmissionLimit -> Maybe (Text, Text)
remainingTries msg =
  submissionLimit
    (const $ Nothing)
    (\n _ -> Just (msg $ msg_Submission_Remaining "Reamining:", pack $ show n))
    (const $ Just (msg $ msg_Submission_Remaining "Remaining:", msg $ msg_Submission_NoTriesLeft "No tries left."))
