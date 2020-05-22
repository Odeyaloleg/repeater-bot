module Settings where

import Data.Map.Strict (Map)
import Data.ByteString.Char8 (ByteString)

class HasSettings a where
  setBotSettings :: Map ByteString ByteString -> Maybe a
