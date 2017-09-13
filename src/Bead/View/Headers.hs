{-# LANGUAGE CPP #-}
module Bead.View.Headers (
    getHeaders
#ifdef TEST
  , getHeadersTest
#endif
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.String (fromString)
import           Snap.Core (listHeaders, HasHeaders)
#ifdef TEST
import           Test.QuickCheck.Gen (Gen, listOf, listOf1)
import qualified Snap.Types.Headers as H
import qualified Test.Tasty.Arbitrary as Gen
import           Test.Tasty.TestSet (TestSet, assertProperty)
import qualified Data.Set as Set
#endif

getHeaders :: HasHeaders a => String -> a -> [BS.ByteString]
getHeaders fieldName request = concat [BS.split ',' v | (k, v) <- listHeaders request, fromString fieldName == k]

#ifdef TEST

getHeadersTest :: TestSet ()
getHeadersTest =
  assertProperty
    "getHeaders returns all the values associated with a key"
    (\keyValues ->
      let (key, values, rest) = keyValues
          header = insert ((key, values) : rest) H.empty
      in Set.fromList (getHeaders key header) == Set.fromList (map BS.pack values)
    )
    genKeyValues
    "getHeaders must return all values associated with a key"
  where
    insert :: [(String, [String])] -> H.Headers -> H.Headers
    insert keyValues header = foldr insertValues header keyValues

    insertValues :: (String, [String]) -> H.Headers -> H.Headers
    insertValues (key, values) header =
      foldr
        (\value h ->
           H.insert (fromString key) (fromString value) h
        )
        header
        values

    genKeyValues :: Gen (String, [String], [(String, [String])])
    genKeyValues = do
      key <- listOf1 Gen.alpha
      values <- listOf1 $ listOf1 Gen.alphaNum
      prefix <- listOf1 Gen.num
      rest <- listOf $ do
        k <- listOf1 Gen.alphaNum
        vs <- listOf1 $ listOf1 Gen.alphaNum
        return (prefix ++ k, vs)
      return (key, values, rest)
#endif
