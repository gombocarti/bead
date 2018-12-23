{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Bead.Domain.Evaluation (
    point
  , score
  , module Bead.Domain.Shared.Evaluation
  ) where

import Bead.Domain.Shared.Evaluation

#ifdef TEST
import Control.Applicative
import Test.Tasty.Arbitrary
import Test.Tasty.TestSet hiding (shrink)
#endif

score :: a -> Scores a
score x = Scores [x]

point :: Percentage -> Maybe Double
point (Percentage (Scores [p])) = Just p
point (Percentage _) = Nothing

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
