{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}

module Api where

import Data.Pool (Pool, withResource)
import qualified Servant as Servant
import Servant ((:<|>)((:<|>)), (:>), Get, JSON, Post, ReqBody)

import Protolude
import Api.Requests
import Persist.Models

type UsersApi = GetAllUsers :<|> CreateUser

type GetAllUsers = "users" :> Get '[ JSON] [User]

type CreateUser
   = "users" :> ReqBody '[ JSON] CreateUserBody :> Post '[ JSON] Int64

getAllUsers :: Pool SQL.Connection -> Servant.Server GetAllUsers
getAllUsers p =
  withResource p $ \conn -> liftIO $ do SQL.query_ conn "select * from users"

createUser :: Pool SQL.Connection -> Servant.Server CreateUser
createUser p user =
  withResource p $ \conn ->
    liftIO $ do
      SQL.withTransaction conn $ do
        SQL.execute conn "insert into users (name, email) values (?, ?)" user :: IO ()
        SQL.lastInsertRowId conn
