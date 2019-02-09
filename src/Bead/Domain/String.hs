{-# LANGUAGE CPP #-}
module Bead.Domain.String
  ( CompareHu(..)
  , sortHu
  , removeAccents
#ifdef TEST
  , compareHuTests
#endif
  ) where

import           Control.Applicative ((<|>))
import           Data.Char (toUpper)
import           Data.List (sortBy, findIndex)
import qualified Data.Map as Map

#ifdef TEST
import           Test.Tasty.TestSet (Partition(Partition), eqPartitions, group)
#endif

-- Hungarian related charecter comparing, for special characters
-- uses the given list otherwise the normal comparism is called
-- capitals and non capitals are different characters
class CompareHu c where
  compareHu :: c -> c -> Ordering

instance CompareHu Char where
  compareHu c c' = maybe (compare c c') id
    ((compare <$> idxSmall   c <*> idxSmall   c') <|>
     (compare <$> idxCapital c <*> idxCapital c'))
    where
      idxSmall   x = findIndex (x==) huSmall
      idxCapital x = findIndex (x==) huCapital
      huSmall   = "aábcdeéfghiíjklmnoóöőpqrstuúüűvwxyz"
      huCapital = "AÁBCDEÉFGHIÍJKLMNOÓÖŐPQRSTUÚÜŰVWXYZ"

instance CompareHu c => CompareHu [c] where
  compareHu [] []    = EQ
  compareHu [] (_:_) = LT
  compareHu (_:_) [] = GT
  compareHu (x:xs) (y:ys)
    = case compareHu x y of
        EQ -> compareHu xs ys
        other -> other

sortHu :: [String] -> [String]
sortHu = sortBy compareHu

#ifdef TEST

compareHuTests = group "compareHu" $ eqPartitions compareHu'
  [ Partition "Small normal letters a-a" ('a', 'a') EQ ""
  , Partition "Small normal letters d-z" ('d', 'z') LT ""
  , Partition "Small normal letters z-a" ('z', 'a') GT ""
  , Partition "Capital normal letters A-A" ('A', 'A') EQ ""
  , Partition "Capital normal letters D-Z" ('D', 'Z') LT ""
  , Partition "Capital normal letters Z-A" ('Z', 'A') GT ""
  , Partition "Small accented letters á-á" ('á', 'á') EQ ""
  , Partition "Small accented letters é-ú" ('é', 'ú') LT ""
  , Partition "Small accented letters ű-á" ('ű', 'á') GT ""
  , Partition "Capital accented letters Á-Á" ('á', 'á') EQ ""
  , Partition "Capital accented letters É-Ú" ('É', 'Ú') LT ""
  , Partition "Capital accented letters Ű-Á" ('Ű', 'Á') GT ""
  ] where compareHu' = uncurry compareHu

#endif

-- | Converts hungarian accute accented letters into ones without accents,
-- in order to be compatible with zip managers and to avoid errors and
-- unreadable files.
removeAccents :: String -> String
removeAccents = map removeAccent
  where
      removeAccent :: Char -> Char
      removeAccent c = case Map.lookup c conversion of
                         Just latinLetter -> latinLetter
                         Nothing -> c

      conversion :: Map.Map Char Char
      conversion = Map.fromList (matching ++ upperCaseMatching)
        where
          matching :: [(Char, Char)]
          matching = [ ('á', 'a')
                     , ('é', 'e')
                     , ('í', 'i')
                     , ('ó', 'o')
                     , ('ö', 'o')
                     , ('ő', 'o')
                     , ('ú', 'u')
                     , ('ü', 'u')
                     , ('ű', 'u')
                     ]

          upperCaseMatching :: [(Char, Char)]
          upperCaseMatching = map (\(c1, c2) -> (toUpper c1, toUpper c2)) matching
