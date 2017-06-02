module Api where

import qualified Database.SQLite.Simple as SQL
import qualified Servant as Servant
import Servant ((:<|>)((:<|>)), (:>), Get, JSON, Post, ReqBody)
import Data.Pool (Pool, createPool, withResource)

type UsersApi = GetAllUsers :<|> CreateUser

type GetAllUsers = "users" :> Get '[ JSON] [User]

type CreateUser
   = "users" :> ReqBody '[ JSON] CreateUserBody :> Post '[ JSON] Int64

getAllUsers :: Pool SQL.Connection -> Servant.Server GetAllUsers
getAllUsers p = withResource p $ \conn ->
  liftIO $ do
    SQL.query_ conn "select * from users"

createUser :: Pool SQL.Connection -> Servant.Server CreateUser
createUser p user = withResource p $ \conn ->
  liftIO $ do
    SQL.withTransaction conn $ do
      SQL.execute conn "insert into users (name, email) values (?, ?)" user :: IO ()
      threadDelay 10000000
      SQL.lastInsertRowId conn
