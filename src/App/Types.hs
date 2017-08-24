module App.Types where

import Protolude

data AppException = ConfigException Text deriving Show
instance Exception AppException
