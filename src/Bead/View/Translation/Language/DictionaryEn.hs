module Bead.View.Translation.Language.DictionaryEn (defaultDictionary) where

import qualified Bead.View.DictionaryFile as DF
import           Bead.View.DictionaryFile ((<|))
import           Bead.View.Translation

defaultDictionary = DF.DictionaryFile {
    DF.langCode = "en"
  , DF.langName = "English"
  , DF.entries  = []
  }
