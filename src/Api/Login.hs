{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Login where

import           AppContext                  (AppT (..))
import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Data.Text                   (Text, pack, unpack)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, getBy)
import           Models                      (Unique (..), runDb, userName,
                                              userPassword)
import           Servant
import           Servant.Auth.Server         as SAS
import           Types                       (AuthorizedUser (..), BCrypt (..),
                                              UserEmail, UserLogin (..),
                                              UserName (..), UserRole (..),
                                              passwordValid)

type LoginAPI = "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthorizedUser)

loginApi :: Proxy LoginAPI
loginApi = Proxy

loginServer
  :: MonadIO m
  => CookieSettings
  -> JWTSettings
  -> UserLogin
  -> AppT
       m
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header
             "Set-Cookie"
             SetCookie]
           AuthorizedUser
       )
loginServer cs jwts u = login cs jwts u

login
  :: MonadIO m
  => CookieSettings
  -> JWTSettings
  -> UserLogin
  -> AppT
       m
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header
             "Set-Cookie"
             SetCookie]
           AuthorizedUser
       )
login cs jwts (UserLogin e p) = do
  increment "login"
  logDebugNS "web" "login"
  maybeAuthUser <- validateLogin e p
  case maybeAuthUser of
    Nothing -> throwError err401 { errBody = "Invalid login." }
    Just u  -> do
      mApplyCookies <- liftIO $ SAS.acceptLogin cs jwts u
      case mApplyCookies of
        Nothing -> do
          throwError err401 { errBody = "acceptLogin failed." }
        Just applyCookies -> do
          logDebugNS "web" ((pack $ show (authUserId u)) <> " logged in")
          pure $ applyCookies u


validateLogin
  :: MonadIO m => UserEmail -> Text -> AppT m (Maybe (AuthorizedUser))
validateLogin e p = do
  maybeUser <- runDb (getBy $ UniqueEmailAddress e)
  case maybeUser of
    Nothing           -> return Nothing
    Just (Entity k v) -> do
      let uName = userName v
      let uId   = fromSqlKey k
      case passwordValid (unpack p) (unpack $ unBCrypt $ userPassword v) of
        True -> pure
          ( Just
            ( AuthorizedUser
              { authUserName = uName
              , authUserId   = uId
              , authUserRole = getUserRole (userName v)
              }
            )
          )
        False -> pure Nothing

getUserRole :: UserName -> UserRole
getUserRole n = if (n == (UserName "amiribarksdale")) then Admin else Normal
