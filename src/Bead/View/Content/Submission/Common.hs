{-# LANGUAGE OverloadedStrings #-}
module Bead.View.Content.Submission.Common where

import           Data.String (fromString)

import           Bead.View.Content

remainingTries :: I18N -> SubmissionLimit -> Maybe (String, String)
remainingTries msg =
  submissionLimit
    (const $ Nothing)
    (\n _ -> Just (msg $ msg_Submission_Remaining "Reamining:", show n))
    (const $ Just (msg $ msg_Submission_Remaining "Remaining:", msg $ msg_Submission_NoTriesLeft "No tries left."))
