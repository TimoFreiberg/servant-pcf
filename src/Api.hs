{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Api where

import Servant
       ((:<|>), (:>), Capture, Get, Handler, JSON, Post, ReqBody,
        ServantErr(errBody), err404, throwError)

import Data.Pool (Pool, withResource)
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Database.Persist.Sql (Entity)

import qualified Api.Requests as Api
import Persist.Models
import Protolude

type UsersApi = GetAllUsers :<|> CreateUser :<|> GetUser

type GetAllUsers = "users" :> Get '[ JSON] [Entity User]

type CreateUser
   = "users" :> ReqBody '[ JSON] Api.CreateUserBody :> Post '[ JSON] (Key User)

type GetUser = "users" :> Capture "id" (Key User) :> Get '[ JSON] User

runDB :: MonadIO m => Pool backend -> ReaderT backend IO a -> m a
runDB p dbAction = liftIO $ withResource p (runReaderT dbAction)

getAllUsers :: Pool DB.SqlBackend -> Handler [Entity User]
getAllUsers p = runDB p $ DB.selectList [] []

createUser :: Pool DB.SqlBackend -> Api.CreateUserBody -> Handler (Key User)
createUser p (Api.CreateUserBody name email) =
  runDB p $ DB.insert (User name email)

getUser :: Pool DB.SqlBackend -> Key User -> Handler User
getUser p id =
  (runDB p $ DB.get id) >>= \case
    Nothing ->
      throwError $ err404 {errBody = "No user found with id " <> show id}
    Just user -> return user

runMigration :: MonadIO m => Pool DB.SqlBackend -> m ()
runMigration p = runDB p (DB.runMigration migrateAll)
