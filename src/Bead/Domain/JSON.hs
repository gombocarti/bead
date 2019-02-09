module Bead.Domain.JSON (
    encodeJSON
  , safeDecodeJSON
  , JSON.Result(..)
  ) where

import           Data.Data (Data)
import           Data.Text (Text)
import qualified Data.Text as T
import           Text.JSON (Result)
import qualified Text.JSON as JSON
import qualified Text.JSON.Generic as JSON (encodeJSON, fromJSON_generic)
import qualified Text.JSON.String as JSON (runGetJSON)

encodeJSON :: Data a => a -> Text
encodeJSON = T.pack . JSON.encodeJSON

safeDecodeJSON :: Data a => Text -> Result a
safeDecodeJSON t = either JSON.Error JSON.fromJSON_generic $ JSON.runGetJSON JSON.readJSValue (T.unpack t)
