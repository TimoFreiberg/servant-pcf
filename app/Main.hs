{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main(main) where

import qualified Data.Time.Format as Time
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Logger as Log
import qualified Servant as Servant
import Servant ((:<|>)((:<|>)), (:>), Get, JSON, Post, ReqBody)
import Data.Pool (createPool)

import Protolude
import Api

main :: IO ()
main = app

-- type LogMessage = Log.WithSeverity Text
-- logHandler logger logMsg = do
--   let logText = map (Pprint.text . strConv Lenient) logMsg
--   let withSeverity = Log.renderWithSeverity identity logText
--   timestampMsg <- Log.timestamp withSeverity
--   let timestampDoc = Log.renderWithTimestamp timeFormat identity timestampMsg
--   logger $ timestampDoc
-- timeFormat =
--   Time.formatTime
--     Time.defaultTimeLocale
--     (Time.iso8601DateFormat (Just "%H:%M:%S"))

app :: IO ()
app = do
  let port = 8081 :: Int
  putText $ "starting server on port " <> show port
  runMigration
  Log.withStdoutLogger $ \logger -> do
    let settings =
          Warp.setPort port $ Warp.setLogger logger Warp.defaultSettings
    Warp.runSettings
      settings
      (Servant.serve (Proxy @UsersApi) (getAllUsers :<|> createUser :<|> getUser))
  -- Warp.run 8081 (Servant.serve (Proxy @GetAllUsers) (getAllUsers conn))
