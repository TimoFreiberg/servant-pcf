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
import Control.Monad.Catch(throwM)
import Servant ((:>),Get,JSON,Handler)
import System.Environment(getEnv)

import Protolude

import App

main :: IO ()
main = app

type HealthApi = "health" :> Get '[JSON] Text

healthHandler :: Handler Text
healthHandler = return "OK"

app :: IO ()
app = do
  portString <- getEnv "PORT"
  port <- case readMaybe @Int portString of
    Just int -> pure int
    Nothing -> (throwM (ConfigException ("PORT must be a number, was "<> strConv Lenient portString)))

  putText $ "starting server on port " <> show port

  Log.withStdoutLogger $ \logger -> do
    let settings =
          Warp.setPort port $ Warp.setLogger logger Warp.defaultSettings
    Warp.runSettings
      settings
      (Servant.serve
         (Proxy @HealthApi )
         healthHandler)
