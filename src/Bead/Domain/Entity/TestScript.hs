{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Bead.Domain.Entity.TestScript (
    TestScript(..)
  , testScriptAppAna
  , testScriptCata
  , TestScriptType(..)
  , testScriptTypeCata
  , withTestScript
  ) where

import           Control.Applicative ((<*>))
import           Data.Data (Data)
import           Data.Functor ((<$>))

#ifdef TEST
import           Test.Tasty.Arbitrary
import           Test.Tasty.TestSet hiding (shrink)
#endif

-- Test Script Type represents a choice: The test cases for the
-- test script will be uploaded as plain text or a zip file
data TestScriptType
  = TestScriptSimple
  | TestScriptZipped
  deriving (Eq, Ord, Enum, Show, Read, Data)

-- Template function for the TestScriptType
testScriptTypeCata
  simple
  zipped
  t = case t of
    TestScriptSimple -> simple
    TestScriptZipped -> zipped

#ifdef TEST
instance Arbitrary TestScriptType where
  arbitrary = elements [TestScriptSimple, TestScriptZipped]
  shrink = testScriptTypeCata
    [TestScriptZipped]
    []
#endif

-- Test Script defines a scripts that can be integrated with the
-- testing framework for the given course.
data TestScript = TestScript {
    tsName :: String -- The name of the script
  , tsDescription :: String -- The short description of the script
  , tsNotes :: String -- The notes for the creator of the test cases, which are associated with the script
  , tsScript :: String -- The script itself that will be subsctituated to the test frameworks shell script
  , tsType :: TestScriptType -- The type of the test script
  } deriving (Eq, Show, Read)

-- Template function for the TestScript
testScriptCata
  tc -- Transformation of the test script type
  f
  (TestScript
    name
    description
    notes
    script
    type_)
  = f name description notes script (tc type_)

-- Template function for the TestScript with flipped parameters
withTestScript t tc f = testScriptCata tc f t

-- Applicative functor based TestScript value creation
testScriptAppAna name desc notes script type_
  = TestScript <$> name <*> desc <*> notes <*> script <*> type_
