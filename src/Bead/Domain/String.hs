module Bead.Domain.String
  ( removeAccents
  ) where

import           Data.Char (toUpper)
import qualified Data.Map as Map

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
