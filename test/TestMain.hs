module Main where

import Test.Tasty
import Test.Tasty.TestSet

import qualified Test.Unit.Header
import qualified Test.Unit.Module
import qualified Test.Unit.Persistence
import qualified Test.Unit.UserStory
import qualified Test.View
import qualified Test.Property.Domain
import qualified Test.Property.Persistence
import qualified Test.Regression.UserStory

main = do
  Test.Tasty.defaultMain $ buildTestTree "" $ do
    Test.Unit.Header.tests
    Test.Unit.Module.tests
    Test.Unit.Persistence.tests
    Test.Unit.UserStory.tests
    Test.View.tests
    Test.Regression.UserStory.tests
    Test.Property.Domain.tests
    Test.Property.Persistence.tests

{-
main = do
  Test.Property.Persistence.createTestData 1
-}
