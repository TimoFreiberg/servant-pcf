{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
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
import Servant ((:>),Get,JSON,Handler)
import System.Environment(getEnv)

import Protolude

main :: IO ()
main = app

type HealthApi = "health" :> Get '[JSON] Text

healthHandler :: Handler Text
healthHandler = return "OK"

defaultPort :: Int
defaultPort = 8080

app :: IO ()
app = do
  port <- fmap (readMaybe @Int) (getEnv "PORT") >>= \case
    Just x -> return x
    Nothing -> do
      putText ("defaulting to port " <> show defaultPort)
      return defaultPort
  putText $ "starting server on port " <> show port
  Log.withStdoutLogger $ \logger -> do
    let settings =
          Warp.setPort port $ Warp.setLogger logger Warp.defaultSettings
    Warp.runSettings
      settings
      (Servant.serve
         (Proxy @HealthApi )
         healthHandler)
