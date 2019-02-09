{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
module Bead.Domain.Evaluation (
    binaryCata
  , binaryConfig
  , binaryEval
  , Binary(..)
  , binaryResult
  , EvalOrComment(..)
  , Evaluation(..)
  , evaluationCata
  , EvaluationData(..)
  , evaluationDataMap
  , EvConfig(..)
  , evConfigCata
  , EvResult
  , evResultCata
  , freeForm
  , FreeForm(..)
  , freeFormConfig
  , freeFormResult
  , score
  , Percentage(..)
  , percentageConfig
  , percentageResult
  , percentEval
  , percentValue
  , point
  , Result(..)
  , resultCata
  , Scores(..)
  , withEvaluation
  , withEvaluationData
  , withEvalOrComment
  , withEvConfig
  , withEvResult
  ) where

import           Data.Data (Data)
import           Data.Monoid

#ifdef TEST
import Control.Applicative
import Test.Tasty.Arbitrary
import Test.Tasty.TestSet hiding (shrink)
#endif

newtype Scores a = Scores { unScores :: [a] }
  deriving (Eq, Show, Data)

score :: a -> Scores a
score x = Scores [x]

newtype Percentage = Percentage (Scores Double)
  deriving (Eq, Show, Data)

point :: Percentage -> Maybe Double
point (Percentage (Scores [p])) = Just p
point (Percentage _) = Nothing

newtype EvResult = EvResult {
    evResult :: EvaluationData Binary Percentage FreeForm
  } deriving (Eq, Show, Data)

evResultCata
  binary
  percentage
  freeForm
  e = case e of
    (EvResult (BinEval b)) -> binary b
    (EvResult (PctEval p)) -> percentage p
    (EvResult (FreeEval f)) -> freeForm f

withEvResult result binary percentage freeForm
  = evResultCata binary percentage freeForm result

percentageResult :: Double -> EvResult
percentageResult d = EvResult (PctEval (Percentage (Scores { unScores = [ d ]})))

percentValue :: EvResult -> Maybe Double
percentValue (EvResult (PctEval (Percentage (Scores [p])))) = Just p
percentValue _ = Nothing

binaryResult :: Result -> EvResult
binaryResult r = EvResult (BinEval (Binary r))

freeFormResult :: String -> EvResult
freeFormResult = EvResult . FreeEval . FreeForm

newtype EvConfig = EvConfig {
    evConfig :: EvaluationData () Double ()
  } deriving (Eq, Show, Data)

evConfigCata
  binary
  percentage
  freeForm
  e = case e of
    (EvConfig (BinEval ())) -> binary
    (EvConfig (PctEval p))  -> percentage p
    (EvConfig (FreeEval ())) -> freeForm

withEvConfig e binary percentage freeForm
  = evConfigCata binary percentage freeForm e

newtype PctConfig = PctConfig { pLimit :: Double }

mkScores :: a -> Scores a
mkScores = Scores . (:[])

data Result = Passed | Failed
  deriving (Eq, Show, Data)

resultCata
  passed
  failed
  r = case r of
    Passed -> passed
    Failed -> failed

newtype Binary = Binary Result
  deriving (Eq, Show, Data)

binaryCata f (Binary x) = f x

percentageCata f (Percentage x) = f x

newtype FreeForm = FreeForm String
  deriving (Eq, Show, Data)

freeForm
  free
  x = case x of
    FreeForm f -> free f

percentageConfig :: Double -> EvConfig
percentageConfig = EvConfig . PctEval

binaryConfig :: EvConfig
binaryConfig = EvConfig (BinEval ())

freeFormConfig :: EvConfig
freeFormConfig = EvConfig (FreeEval ())

-- Command that can send from the evaluation page to the
-- server. It consists of a comment value, come from the
-- text field, or the value of the evaluation
data EvalOrComment
  = EvCmtComment
  | EvCmtResult EvResult

evalOrCommentCata
  comment
  result
  e = case e of
    EvCmtComment  -> comment
    EvCmtResult r -> result r

withEvalOrComment e comment result = evalOrCommentCata comment result e

-- Represents the evaluation type for an assignment
data EvaluationData b p f
  = BinEval b
  | PctEval p
  | FreeEval f
  deriving (Eq, Show, Data)

evaluationDataMap
  binEval
  pctEval
  freeEval
  e = case e of
    BinEval b -> binEval b
    PctEval p -> pctEval p
    FreeEval f -> freeEval f

withEvaluationData d binEval pctEval freeEval
  = evaluationDataMap binEval pctEval freeEval d

binaryEval :: EvaluationData b p f -> Maybe b
binaryEval (BinEval b) = Just b
binaryEval _           = Nothing

percentEval :: EvaluationData b p f -> Maybe p
percentEval (PctEval p) = Just p
percentEval _           = Nothing

#ifdef TEST

-- The arbitrary instances are defined here, because FAY does support
-- only the "#ifdef FAY"

instance Arbitrary Result where
  arbitrary = elements   [Passed, Failed]
  shrink    = resultCata [Failed] []

instance Arbitrary Binary where
  arbitrary = Binary <$> arbitrary
  shrink = fmap Binary . binaryCata shrink

instance Arbitrary Percentage where
  arbitrary = Percentage . mkScores <$> arbitrary
  shrink = percentageCata (fmap (Percentage . Scores) . shrink . unScores)

instance Arbitrary FreeForm where
  arbitrary = FreeForm <$> arbitrary
  shrink (FreeForm xs) = map FreeForm (shrink xs)

instance Arbitrary EvResult where
  arbitrary = EvResult <$> oneof [
      BinEval <$> arbitrary
    , PctEval <$> arbitrary
    , FreeEval <$> arbitrary
    ]
  shrink = fmap EvResult . evResultCata
    (fmap BinEval . shrink)
    (fmap PctEval . shrink)
    (fmap FreeEval . shrink)

instance Arbitrary EvConfig where
  arbitrary = EvConfig <$> oneof [
      BinEval <$> arbitrary
    , PctEval <$> arbitrary
    , FreeEval <$> arbitrary
    ]
  shrink = fmap EvConfig . evConfigCata
    []
    (fmap PctEval . shrink)
    []
#endif

-- | Template function for the evaluation
evaluationCata f (Evaluation result written) = f result written

-- | Template function with flipped parameter for the evaluation
withEvaluation e f = evaluationCata f e

-- | Evaluation of a submission
data Evaluation = Evaluation {
    evaluationResult  :: EvResult
  , writtenEvaluation :: String
  } deriving (Eq, Show, Data)
