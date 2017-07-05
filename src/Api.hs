{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Api where

import Servant
       ((:<|>), (:>), Capture, Get, Handler, JSON, Post, ReqBody,
        ServantErr(errBody), err404, throwError)

import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import Database.Persist.Sql (Entity)
import Database.Persist.Sqlite (runSqlite)

import qualified Api.Requests as Api
import Persist.Models
import Protolude

type UsersApi = GetAllUsers :<|> CreateUser :<|> GetUser

type GetAllUsers = "users" :> Get '[ JSON] [Entity User]

type CreateUser
   = "users" :> ReqBody '[ JSON] Api.CreateUserBody :> Post '[ JSON] (Key User)

type GetUser = "users" :> Capture "id" (Key User) :> Get '[ JSON] User

getAllUsers :: Handler [Entity User]
getAllUsers = liftIO . runSqlite ":memory:" . readOnly $ DB.selectList [] []

createUser :: Api.CreateUserBody -> Handler (Key User)
createUser (Api.CreateUserBody name email) =
  liftIO . runSqlite ":memory:" . readWrite $ DB.insert (User name email)

getUser :: Key User -> Handler User
getUser id =
  liftIO (runSqlite ":memory:" . readOnly $ DB.get id) >>= \case
    Nothing ->
      throwError $ err404 {errBody = "No user found with id " <> show id}
    Just user -> return user

readOnly :: ReaderT DB.SqlReadBackend m a -> ReaderT DB.SqlReadBackend m a
readOnly = identity

readWrite :: ReaderT DB.SqlWriteBackend m a -> ReaderT DB.SqlWriteBackend m a
readWrite = identity

runMigration :: IO ()
runMigration = runSqlite ":memory:" (DB.runMigration migrateAll)
