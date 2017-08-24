{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main
  ( main
  ) where

import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Logger as Log
import qualified Servant as Servant
import Servant ((:>),(:<|>)((:<|>)))
import System.Environment(getEnv)
import Control.Monad.Catch(throwM)

import Protolude

import App

main :: IO ()
main = app

-- dbIdentifier :: Text
-- dbIdentifier = "db.sqlite3"
type HealthApi = "health" :> Servant.Get '[ Servant.JSON] Text
type KillApi = "kill" :> Servant.Get '[Servant.JSON] Text

app :: IO ()
app = do
  portString <- getEnv "PORT"
  port <- case readMaybe @Int portString of
    Just int -> pure int
    Nothing -> (throwM (ConfigException ("PORT must be a number, was "<> strConv Lenient portString)))

  putText $ "starting server on port " <> show port
  -- (pool :: Pool SqlBackend) <-
  --   Log.runStdoutLoggingT (createSqlitePool dbIdentifier 10)
  -- runMigration pool
  Log.withStdoutLogger $ \logger -> do
    let settings =
          Warp.setPort port $ Warp.setLogger logger Warp.defaultSettings
    Warp.runSettings
      settings
      (Servant.serve
         (Proxy @(HealthApi :<|> KillApi))
         (getHealth :<|> error "KILLED"))
  -- Warp.run 8081 (Servant.serve (Proxy @GetAllUsers) (getAllUsers conn))

getHealth :: Servant.Handler Text
getHealth = return "Up"
