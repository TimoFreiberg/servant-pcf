{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Persist.Models where

import Data.Aeson
       (FromJSON, ToJSON, defaultOptions, genericToEncoding, toEncoding)

import Database.Persist.TH
       (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)

import Protolude

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
User
  name Text
  email Text
  deriving Eq Show Generic
|]

instance FromJSON User

instance ToJSON User where
  toEncoding = genericToEncoding defaultOptions
