module App.Types where

import Protolude

data AppException =
  ConfigException Text
  deriving (Show)

instance Exception AppException

newtype Vincode =
  Vincode Text
  deriving (Show)

-- | Creates a Vincode. TODO verify vincode pattern
toVincode :: Text -> Vincode
toVincode = Vincode
