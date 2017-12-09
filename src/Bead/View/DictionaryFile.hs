module Bead.View.DictionaryFile (
    DictionaryFile(..)
  , DictionaryPatchFile(..)
  , dictionaryFileCata
  , Entry
  , (<|)
  ) where

import Bead.View.Translation.Base (Translation)

data DictionaryFile
  = DictionaryFile {
    langCode :: String -- The name of the language for a dictionary
  , langName :: String -- The displayable name of the language for a dictioanry
  , entries  :: [Entry] -- The entry list for a dictionary
  }

data DictionaryPatchFile
  = DictionaryPatchFile {
    parent   :: DictionaryFile  -- The dictionary to be patched
  , patches  :: [Entry]         -- List of entries
  }

dictionaryFileCata f (DictionaryFile langCode langName entries) =
  f langCode langName entries

-- A dictionary file entry is just a translation value,
-- which represents the translation key and the translated
-- value for that key
type Entry = Translation String

(<|) :: (String -> Translation String) -> String -> Translation String
(<|) = ($)

infixr 0 <|
