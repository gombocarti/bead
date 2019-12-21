module Test.Unit.AuthToken (
    encryptionDecryptionIsomorphism
  ) where

import qualified Bead.View.AuthToken as Auth

import           Test.Property.EntityGen (cookies)
import           Test.Tasty.TestSet (TestSet, test)

import           Control.Monad.IO.Class (liftIO)
import           Data.Either (either)

import           Test.HUnit hiding (Test(..), test, assert)
import           Test.Tasty.HUnit (testCase)
import           Test.QuickCheck (quickCheckResult)
import           Test.QuickCheck.Monadic (monadicIO, pick, assert, PropertyM)
import           Test.QuickCheck.Test (isSuccess)

encryptionDecryptionIsomorphism :: TestSet ()
encryptionDecryptionIsomorphism = test $ testCase "Encryption and decryption do not change cookie contents" $ do
  auth <- Auth.createAuthTokenManager
  result <- quickCheckResult $ monadicIO $ do
    c <- pick cookies
    encrypted <- getRight $ Auth.encryptCookie auth c
    decrypted <- getRight $ Auth.decryptCookie auth encrypted
    assert (decrypted == c)
  assertBool "Could not retrieve cookie from cypher text." $ isSuccess result
 where
   getRight :: Show a => IO (Either a b) -> PropertyM IO b
   getRight m = liftIO m >>= either (fail . show) return
