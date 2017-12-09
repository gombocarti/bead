{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
#ifdef TEST
{-# LANGUAGE LambdaCase #-}
#endif
module Bead.View.Dictionary (
    Entry(..)
  , I18N
  , Dictionary
  , Dictionaries -- A map from a language to dictionary and dictionary info
  , dictionariesCata -- The template function for the dictionaries values
  , DictionaryInfo(..) -- Information about a dictionary
  , dictionaryInfoCata -- The template function for the dictionaryInfo values
  , DictionaryFile(..) -- Represend a dictionary
  , Language(..) -- A language name for a dictionary
  , languageCata -- The template function for the language values
  , dictionary
  , dictionaries
  , unDictionary
  , dictionaryFromDFile -- Creates a Dictionary from the DictionaryFile structure
  , dictionaryFileToInfo -- Reads out the icon file name
  , idDictionary
  , defaultLanguage
  , defaultDictionary
  , defaultDictionaryInfo
#ifdef TEST
  , patchDictionariesTests
#endif
  ) where

-- Haskell imports

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Typeable
import Control.Arrow ((&&&))
import Data.Bifunctor (second)

-- Bead imports

import           Bead.Domain.Entities (Language(Language), languageCata)
import           Bead.View.DictionaryFile (DictionaryFile, DictionaryPatchFile, Entry, (<|))
import qualified Bead.View.DictionaryFile as DF
import           Bead.View.Translation
import           Bead.View.Translation.Language.DictionaryEn (defaultDictionary)
import           Bead.View.Translation.Language.DictionaryHu (dictionaryHu)

#ifdef TEST
import           Data.Map ((!))
import qualified Test.Tasty.TestSet as TS
import           Test.Tasty.TestSet
#endif

-- * Definitions

-- Dictionary is just a translation
newtype Dictionary = Dictionary { unDictionary :: I18N }

instance Show Dictionary where
  show _ = "Dictionary"

-- The dictionary info contains information about a
-- dictionary, it is only the name of the icon file
-- for a dictionary
newtype DictionaryInfo = DictionaryInfo { languageName :: String }

dictionaryInfoCata f (DictionaryInfo langName) = f langName

-- Dictionaries is a map from a given language to a Dictionary and
-- a dictionary info pair
type Dictionaries = Map Language (Dictionary, DictionaryInfo)

dictionariesCata f dictionaries = f dictionaries

makeDictionaries :: [DictionaryFile] -> Dictionaries
makeDictionaries = Map.fromList . map (second toValue . addKey)
  where
    addKey :: DictionaryFile -> (Language, DictionaryFile)
    addKey file = (Language (DF.langCode file), file)

    toValue :: DictionaryFile -> (Dictionary, DictionaryInfo)
    toValue = dictionaryFromDFile &&& dictionaryFileToInfo

dictionaries :: Dictionaries
dictionaries = makeDictionaries dictionaryFiles

defaultLanguage = DF.langCode defaultDictionary

defaultDictionaryInfo = DictionaryInfo {
    languageName = DF.langName defaultDictionary
  }

dictionaryFiles :: [DictionaryFile]
dictionaryFiles = [ defaultDictionary
                  , dictionaryHu
                  ]

patch :: DictionaryFile -> DictionaryPatchFile -> DictionaryFile
patch d p = d { DF.entries = process (DF.entries d) (DF.patches p) }
  where
    process :: [Entry] -> [Entry] -> [Entry]
    process eDict ePatch =
      map T . Map.toList . uncurry Map.union $
        both (Map.fromList . map unT) (ePatch, eDict)

    both :: (a -> b) -> (a, a) -> (b, b)
    both f (x, y) = (f x, f y)

-- Creates a new dictionary from the entries of the dictionary file,
-- if no translation key is found in the entries, the original value
-- would be used as the text of the translation
dictionaryFromDFile :: DictionaryFile -> Dictionary
dictionaryFromDFile = DF.dictionaryFileCata $ \ _langCode _langName entries ->
  Dictionary {
    unDictionary = \key ->
      let key' = createKey key
      in maybe (trans key) trans . Map.lookup key' . Map.fromList $ map (createKey &&& id) entries
  }
  where
    createKey :: Translation String -> Translation ()
    createKey (T (n,_)) = T (n,())

-- Reads out the icon file name into the dictionary info
dictionaryFileToInfo :: DictionaryFile -> DictionaryInfo
dictionaryFileToInfo = DF.dictionaryFileCata $ \ _langCode langName _entires ->
  DictionaryInfo langName

dictionary :: I18N -> Dictionary
dictionary = Dictionary

idDictionary :: Dictionary
idDictionary = Dictionary trans

#ifdef TEST
patchDictionariesTests = TS.group "Patch dictionaries" $ do
  satisfy "No dictionaries"
    Map.null
    []
    "Without dictionaries, there should be an empty map"
  satisfy "Single dictionary"
    (\ m -> fine m "sq" "Shqip")
    [ DF.DictionaryFile "sq" "Shqip" [] ]
    "A valid dictionary was not loaded"
  satisfy "Two different dictionaries"
    (\ m -> fine m "sq" "Shqip" && fine m "ae" "Avesta")
    [ DF.DictionaryFile "sq" "Shqip" []
    , DF.DictionaryFile "ae" "Avesta" []
    ]
    "Not all valid dictionaries were loaded when more of them are present"
  let d = DF.DictionaryFile "sq" "Shqip"
            [ msg_Login_Username <| "Username:"
            , msg_Login_Password <| "Password:"
            ]
  satisfy "Single dictionary with a patch"
    (\ m -> fine m "sq" "Shqip" &&
            translation m "sq" msg_Login_Username "Identifier:" &&
            translation m "sq" msg_Login_Password "Password:"
    )
    [ d `patch` (DF.DictionaryPatchFile d [ msg_Login_Username <| "Identifier:" ])
    ]
    "Could not apply a patch to a dictionary"
  satisfy "Single dictionary with two patches"
    (\ m -> fine m "sq" "Shqip" &&
            translation m "sq" msg_Login_Username "Identifier:" &&
            translation m "sq" msg_Login_Password "Passphrase:"
    )
    [ d `patch` (DF.DictionaryPatchFile d [ msg_Login_Username <| "Identifier:" ])
        `patch` (DF.DictionaryPatchFile d [ msg_Login_Password <| "Passphrase:" ])
    ]
    "Not all of the patches could be applied to a dictionary"
  satisfy "Single dictionary with overlapping patches"
    (\ m -> fine m "sq" "Shqip" &&
            translation m "sq" msg_Login_Username "Codename:" &&
            translation m "sq" msg_Login_Password "Password:"
    )
    [ d `patch` (DF.DictionaryPatchFile d [ msg_Login_Username <| "Identifier:" ])
        `patch` (DF.DictionaryPatchFile d [ msg_Login_Username <| "Codename:" ])
    ]
    "The last patch should be effective for overlapping patches"
  let d' = d { DF.entries =
                 [ msg_Login_Username <| "Identifier:"
                 , msg_Login_Password <| "Codename:"
                 ]
             }
  satisfy "Two repeated dictionaries"
    (\m -> fine m "sq" "Shqip" &&
           translation m "sq" msg_Login_Username "Identifier:" &&
           translation m "sq" msg_Login_Password "Codename:"
    )
    [ d, d' ]
    "An attempt to load multiple dictionaries of the same language should return the last dictionary for that language"
  let e = DF.DictionaryFile "ae" "Avesta"
            [ msg_Login_Username <| "Username:"
            , msg_Login_Password <| "Password:"
            ]
  satisfy "Two different dictionaries with a patch for one of them"
    (\ m -> fine m "sq" "Shqip" && fine m "ae" "Avesta" &&
            translation m "sq" msg_Login_Username "Identifier:" &&
            translation m "sq" msg_Login_Password "Password:" &&
            translation m "ae" msg_Login_Username "Username:" &&
            translation m "ae" msg_Login_Password "Password:"
    )
    [ d `patch` (DF.DictionaryPatchFile d [ msg_Login_Username <| "Identifier:" ])
    , e
    ]
    "Could not apply a patch to a dictionary when multiple dictionaries are present"
  satisfy "Two different dictionaries with corresponding patches"
    (\ m -> fine m "sq" "Shqip" && fine m "ae" "Avesta" &&
            translation m "sq" msg_Login_Username "Identifier:" &&
            translation m "ae" msg_Login_Password "Passphrase:"
    )
    [ d `patch` (DF.DictionaryPatchFile d [ msg_Login_Username <| "Identifier:" ])
    , e `patch` (DF.DictionaryPatchFile e [ msg_Login_Password <| "Passphrase:" ])
    ]
    "Could not apply all the patches to all of their corresponding dictionaries"
  satisfy "Two different dictionaries with overlapping patches for one of them"
    (\ m -> fine m "sq" "Shqip" && fine m "ae" "Avesta" &&
            translation m "sq" msg_Login_Username "Codename:" &&
            translation m "sq" msg_Login_Password "Password:"
    )
    [ d `patch` (DF.DictionaryPatchFile d [ msg_Login_Username <| "Identifier:" ])
        `patch` (DF.DictionaryPatchFile d [ msg_Login_Username <| "Codename:" ])
    , e
    ]
    "The last patch should be effective for overlapping patches, even in case of multiple dictionaries"
  where
    satisfy :: TestName -> (Dictionaries -> Bool) -> [DictionaryFile] -> Message -> TestSet ()
    satisfy s x y r = assertSatisfy s x (makeDictionaries y) r

    fine :: Dictionaries -> String -> String -> Bool
    fine m langShort langFull = languageName di == langFull
      where (_, di) = m ! (Language langShort)

    translation :: Dictionaries -> String -> (String -> Translation String) -> String -> Bool
    translation m langShort tr exp = unDictionary d (tr "") == exp
      where (d, _) = m ! (Language langShort)
#endif
