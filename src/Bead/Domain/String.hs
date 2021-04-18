{-# LANGUAGE OverloadedStrings #-}

module Bead.Domain.String
  ( removeAccents
  , replaceSlash
  , porcelain
  , porcelainBS
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Char (toUpper, toLower, isSpace, isLetter, isAscii, isDigit)
import qualified Data.Map as Map
import           Data.String.Utils (replace)

-- | Converts hungarian accute accented letters into ones without accents,
-- in order to be compatible with zip managers and to avoid errors and
-- unreadable files.
removeAccents :: String -> String
removeAccents = map removeAccent

removeAccentsBS :: ByteString -> ByteString
removeAccentsBS = BC.map removeAccent

removeAccent :: Char -> Char
removeAccent c = case Map.lookup c conversion of
                   Just latinLetter -> latinLetter
                   Nothing -> c
  where
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

-- | Replaces slashes with underscores. Useful in creating zip
-- archives, making filenames zip-friendly.
replaceSlash :: String -> String
replaceSlash = replace "/" "_"

porcelain :: String -> String
porcelain = map toLower . filter isWhiteListChar . map replaceSpaces . removeAccents

porcelainBS :: ByteString -> ByteString
porcelainBS = BC.map toLower . BC.filter isWhiteListChar . BC.map replaceSpaces . removeAccentsBS

isWhiteListChar :: Char -> Bool
isWhiteListChar c = isAscii c && (isDigit c || isLetter c || c `elem` ("_-()" :: String))

replaceSpaces :: Char -> Char
replaceSpaces c
  | isSpace c = '_'
  | otherwise = c
