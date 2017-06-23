{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module Api.Requests where

import Data.Aeson (FromJSON, ToJSON)

import Protolude

data CreateUserBody = CreateUserBody
  { name :: Text
  , email :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

