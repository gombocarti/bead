module Test.Property.Domain (
    tests
  ) where

import Bead.Domain.Entities

import qualified Data.Text as T
import System.Exit (ExitCode(ExitSuccess))
import qualified Test.Property.EntityGen as Gen
import Test.Property.Common (quick)
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Monadic (pick, run)
import Test.QuickCheck.Gen (Gen, suchThat, elements, listOf1)
import Test.Tasty.HUnit (testCase, assertEqual)
import Test.Tasty.TestSet (group, test, TestSet)

tests :: TestSet ()
tests = group "Domain property tests" $ do
  outputToMossScriptInvocationTests

outputToMossScriptInvocationTests :: TestSet ()
outputToMossScriptInvocationTests = test $ testCase "Moss script output interpretation tests" $ do
  quick 100 $ do
    exitCode <- pick Gen.exitCodesFailure
    output <- pick Gen.manyLines
    run $ assertEqual "outputToMossScriptInvocation returned incorrect result for unsuccessful exit code" (MossScriptInvocationFailure (T.pack output) exitCode) (outputToMossScriptInvocation exitCode output)

  quick 100 $ do
    (preUrl, url, postUrl) <- pick outputs
    let output = unlines [preUrl, url, postUrl]
    run $ assertEqual "outputToMossScriptInvocation returned incorrect result for successful exit code" (MossScriptInvocationSuccess (T.pack (preUrl ++ "\n")) (T.pack url)) (outputToMossScriptInvocation ExitSuccess output)

  quick 100 $ do
    (preUrl, _url, postUrl) <- pick outputs
    let output = unlines [preUrl, postUrl]
    run $ assertEqual "outputToMossScriptInvocation returned incorrect result for successful exit code without url" (MossScriptInvocationNotInterpretableOutput (T.pack output)) (outputToMossScriptInvocation ExitSuccess output)

  where
    outputs :: Gen (String, String, String)
    outputs = do
      preUrl <- Gen.manyLines
      url <- Gen.urls
      postUrl <- Gen.manyLines
      return (preUrl, url, postUrl)
