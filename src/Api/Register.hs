{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Register where

import           AppContext                  (AppT (..))
import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (pack)
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time.Clock             (getCurrentTime)
import           Database.Persist.Postgresql (Entity (..), getBy, insertEntity)
import           Models                      (Unique (..), User (User), runDb)
import           Servant
import           Servant.Auth.Server         as SAS
import           Types                       (BCrypt (..), UserEmail (..),
                                              UserRegistration (..),
                                              hashPassword)

type RegisterAPI = "register" :> ReqBody '[JSON] UserRegistration :> Post '[JSON] (Entity User)

registerApi :: Proxy RegisterAPI
registerApi = Proxy

registerServer
  :: MonadIO m
  => CookieSettings
  -> JWTSettings
  -> UserRegistration
  -> AppT m (Entity User)
registerServer cs jwts u = register cs jwts u

register
  :: MonadIO m
  => CookieSettings
  -> JWTSettings
  -> UserRegistration
  -> AppT m (Entity User)
register _ _ (UserRegistration e n p) = do
  increment "register"
  logDebugNS "web" ("register" <> (pack $ show n) <> ": " <> (pack $ show e))
  u <- existingUser e
  case u of
    Just (Entity _ _) ->
      throwError err409 { errBody = "Conflicting registration." }
    Nothing -> do
      currentTime <- liftIO $ getCurrentTime
      pw          <- liftIO $ hashPassword p
      let pw' = fromJust pw
      newUser <- runDb
        ( insertEntity
          (User currentTime currentTime e n (BCrypt . decodeUtf8 $ pw'))
        )
      let k = entityKey newUser
      logDebugNS
        "web"
        (  "Registered user "
        <> (pack $ show k)
        <> ": "
        <> (pack $ show n)
        <> " "
        <> (pack $ show e)
        )
      pure newUser

existingUser :: MonadIO m => UserEmail -> AppT m (Maybe (Entity User))
existingUser e = do
  maybeUser <- runDb (getBy $ UniqueEmailAddress e)
  return maybeUser
