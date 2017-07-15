{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import qualified Control.Monad.Logger as Log
import Data.Pool (Pool)
import Database.Persist.Sqlite (SqlBackend, createSqlitePool)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Logger as Log
import qualified Servant as Servant
import Servant ((:<|>)((:<|>)))

import Api
import Protolude

main :: IO ()
main = app

dbIdentifier :: Text
dbIdentifier = "db.sqlite3"

app :: IO ()
app = do
  let port = 8081 :: Int
  putText $ "starting server on port " <> show port
  (pool :: Pool SqlBackend) <-
    Log.runStdoutLoggingT (createSqlitePool dbIdentifier 10)
  runMigration pool
  Log.withStdoutLogger $ \logger -> do
    let settings =
          Warp.setPort port $ Warp.setLogger logger Warp.defaultSettings
    Warp.runSettings
      settings
      (Servant.serve
         (Proxy @UsersApi)
         (getAllUsers pool :<|> createUser pool :<|> getUser pool))
  -- Warp.run 8081 (Servant.serve (Proxy @GetAllUsers) (getAllUsers conn))
