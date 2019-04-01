module Bead.View.Common (
    languageMenu
  ) where

import           Control.Monad (forM_)
import           Data.Monoid (mempty)

import           Bead.View.Dictionary (dictionaryInfoCata)
import           Bead.View.BeadContext (DictionaryInfos)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.RouteOf
import           Bead.View.Translation

import qualified Text.Blaze as M
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

languageMenu :: I18N -> DictionaryInfos -> H.Html
languageMenu msg languages@(_:_:_) = do
  Bootstrap.buttonGroupJustified $
    forM_ languages $ \(language,info) -> do
      Bootstrap.buttonLink (queryString changeLanguagePath [requestParam language])
         (dictionaryInfoCata M.toMarkup info)
languageMenu _ _ = mempty
