{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types where

import Data.Aeson (FromJSON, ToJSON)
import Database.SQLite.Simple (FromRow, ToRow)
import qualified Database.SQLite.Simple as SQL

import Protolude

data CreateUserBody = CreateUserBody
  { name :: Text
  , email :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance ToRow CreateUserBody where
  toRow (CreateUserBody n e) = SQL.toRow (n, e)

data User = User
  { id :: Int
  , name :: Text
  , email :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromRow User where
  fromRow = User <$> SQL.field <*> SQL.field <*> SQL.field

instance ToRow User where
  toRow (User i n e) = SQL.toRow (i, n, e)
