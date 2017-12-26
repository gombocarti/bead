module Bead.View.Common (
    languageMenu
  ) where

import           Data.Monoid (mempty)

import           Bead.View.Dictionary (dictionaryInfoCata)
import           Bead.View.BeadContext (DictionaryInfos)
import qualified Bead.View.Content.Bootstrap as Bootstrap
import           Bead.View.Headers.AcceptLanguage
import           Bead.View.RouteOf
import           Bead.View.Translation

import qualified Text.Blaze as M
import           Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

-- Set the default language in session if no information is found

languageMenu :: I18N -> DictionaryInfos -> H.Html
languageMenu msg languages@(_:_:_) = do
  Bootstrap.rowCol4Offset4 $ Bootstrap.buttonGroupJustified $
    Bootstrap.dropdown (msg $ msg_Login_SelectLanguage "Select a language") $
    for languages $ \(language,info) -> do
      link (queryString changeLanguagePath [requestParam language])
         (dictionaryInfoCata M.toMarkup info)
  where
    for = flip Prelude.map
    link ref text = H.a ! A.href ref $ text
languageMenu _ _ = mempty
