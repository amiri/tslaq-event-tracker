{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Register where

import           Api.Login                   (setLoginCookies, validateLogin)
import           AppContext                  (AppT (..))
import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Data.Maybe                  (fromJust)
import           Data.Text                   (pack)
import           Data.Text.Encoding          (decodeUtf8)
import           Data.Time.Clock             (getCurrentTime)
import           Database.Persist.Postgresql (Entity (..), getBy, insert,
                                              insertEntity)
import           Errors
import           Models                      (Unique (..), User (User),
                                              UserRole (..), runDb)
import           Servant
import           Servant.Auth.Server         as SAS
import           Types                       (AuthorizedUser (..), BCrypt (..),
                                              RoleName (..), UserEmail (..),
                                              UserRegistration (..),
                                              hashPassword)

type RegisterAPI
  = "register" :> ReqBody '[JSON] UserRegistration :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthorizedUser)

registerApi :: Proxy RegisterAPI
registerApi = Proxy

registerServer
  :: MonadIO m
  => CookieSettings
  -> JWTSettings
  -> UserRegistration
  -> AppT
       m
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header
             "Set-Cookie"
             SetCookie]
           AuthorizedUser
       )
registerServer cs jwts u = register cs jwts u

register
  :: MonadIO m
  => CookieSettings
  -> JWTSettings
  -> UserRegistration
  -> AppT
       m
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header
             "Set-Cookie"
             SetCookie]
           AuthorizedUser
       )
register cs jwts (UserRegistration e n p) = do
  increment "register"
  logDebugNS "web" ("register " <> (pack $ show n) <> ": " <> (pack $ show e))
  u <- existingUser e
  case u of
    Just (Entity _ _) -> throwError $ encodeJSONError
      (JSONError 409 "RegistrationConflict" "Conflicting registration.")
    Nothing -> do
      currentTime <- liftIO $ getCurrentTime
      pw          <- liftIO $ hashPassword p
      let pw' = fromJust pw
      newUser <- runDb
        (insertEntity
          (User currentTime currentTime e n (BCrypt . decodeUtf8 $ pw'))
        )
      let k = entityKey newUser
      normalRole <- runDb (getBy $ UniqueRole (RoleName "Normal"))
      let norm = fromJust normalRole
      _ <- runDb $ insert (UserRole k (entityKey norm))
      logDebugNS
        "web"
        (  "Registered user "
        <> (pack $ show k)
        <> ": "
        <> (pack $ show n)
        <> " "
        <> (pack $ show e)
        )
      maybeUser <- validateLogin e p
      setLoginCookies maybeUser cs jwts

existingUser :: MonadIO m => UserEmail -> AppT m (Maybe (Entity User))
existingUser e = do
  maybeUser <- runDb (getBy $ UniqueEmailAddress e)
  pure maybeUser
