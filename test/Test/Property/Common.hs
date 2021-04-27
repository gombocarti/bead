module Test.Property.Common (
    check
  , quick
  , success
  ) where

import Test.QuickCheck (quickCheckWithResult, reason, output, Args, stdArgs, maxSuccess, chatty, Result(Success, Failure), Testable)
import Test.QuickCheck.Monadic (monadicIO, PropertyM)

quick :: Testable a => Int -> PropertyM IO a -> IO ()
quick n p = check (return ()) $ quickCheckWithResult (success n) $ monadicIO p

check :: IO a -> IO Result -> IO a
check cleanup m = do
  x <- m
  case x of
    s@(Success {}) -> cleanup
    f@(Failure {}) -> cleanup >> (fail $ reason f)
    other          -> cleanup >> (fail $ output other)

success :: Int -> Args
success n = stdArgs { maxSuccess = n, chatty = False }


