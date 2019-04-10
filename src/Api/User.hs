{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.User where

import           AppContext                  (AppT (..))
import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Data.Int                    (Int64)
import           Data.Maybe                  (fromJust)
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time.Clock             (getCurrentTime)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey,
                                              getEntity, insert, selectList,
                                              toSqlKey)
import           Models                      (Key, User (User), runDb)
import           Servant
import           Types                       (BCrypt (..), NewUser (..),
                                              hashPassword)

type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "id" Int64 :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] NewUser :> Post '[JSON] Int64
    -- :<|> "events" :> Capture "id" Int64 :> ReqBody '[JSON] EventUpdate :> Put '[JSON] (Entity User)

userApi :: Proxy UserAPI
userApi = Proxy

-- | The server that runs the UserAPI
userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = listUsers :<|> getUser :<|> createUser
    -- :<|> updateUser

-- | Returns all users in the database.
listUsers :: MonadIO m => AppT m [Entity User]
listUsers = do
  increment "listUsers"
  logDebugNS "web" "listUsers"
  runDb (selectList [] [])

-- | Returns a user by id or throws a 404 error.
getUser :: MonadIO m => Int64 -> AppT m (Entity User)
getUser i = do
  increment "getUser"
  logDebugNS "web" "getUser"
  maybeUser <- runDb (getEntity (toSqlKey i :: Key User))
  case maybeUser of
    Nothing -> throwError err404
    Just u  -> return u

-- | Creates a user in the database.
createUser :: MonadIO m => NewUser -> AppT m Int64
createUser p = do
  increment "createUser"
  logDebugNS "web" "creating a user"
  currentTime <- liftIO $ getCurrentTime
  pw          <- liftIO $ hashPassword (password p)
  let pw' = fromJust pw
  newUser <- runDb
    ( insert
      ( User currentTime
             currentTime
             (emailAddress p)
             (name p)
             (BCrypt . decodeUtf8 $ pw')
      )
    )
  return $ fromSqlKey newUser

-- updateUser :: MonadIO m => Int64 -> UserUpdate -> AppT m (Entity User)
-- updateUser i e = do
--   increment "updateUser"
--   logDebugNS "web" "updateUser"
--   let k = toSqlKey i
--   runDb (update k e)
--   updated <- runDb (getEntity k)
--   case updated of
--     Nothing -> throwError err404
--     Just uu -> return uu
