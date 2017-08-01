module Bead.View.Headers (
  getHeaders
  ) where

import qualified Data.ByteString.Char8 as BS
import           Data.String (fromString)
import           Snap.Core (listHeaders, HasHeaders)

getHeaders :: HasHeaders a => String -> a -> [BS.ByteString]
getHeaders fieldName request = concat [BS.split ',' v | (k, v) <- listHeaders request, fromString fieldName == v]
