module Bead.Domain.Entity.Assessment where

import Data.Text (Text)
import Data.Time (UTCTime)

import Bead.Domain.Shared.Evaluation

-- | Assessment for a student, without any submission
-- just an evaluation for it.
data Assessment = Assessment {
    title         :: Text
  , description   :: Text
  , created       :: UTCTime
  , evaluationCfg :: EvConfig
  , visible       :: Bool
  } deriving (Eq, Show)

assessment f (Assessment title desc creation cfg visible) = f title desc creation cfg visible

withAssessment a f = assessment f a

