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
import           Data.Int                    (Int64)
import           Data.Text                   (Text, unpack)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, getBy)
import           Models                      (Key (..), Unique (..),
                                              User (User), runDb, userName,
                                              userPassword)
import           Servant
import           Servant.Auth.Server         as SAS
import           Types                       (AuthorizedUser (..), BCrypt (..),
                                              NewUser, UserEmail,
                                              UserLogin (..), UserName (..),
                                              UserRole (..), passwordValid)

type LoginAPI = "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

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
           NoContent
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
           NoContent
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
          pure $ applyCookies NoContent


validateLogin
  :: MonadIO m => UserEmail -> Text -> AppT m (Maybe (AuthorizedUser))
validateLogin e p = do
  maybeUser <- runDb (getBy $ UniqueEmailAddress e)
  case maybeUser of
    Nothing           -> return Nothing
    Just (Entity k v) -> do
      let uName = userName v
      let uId   = k
      case passwordValid (unpack p) (unpack $ unBCrypt $ userPassword v) of
        True -> return
          ( Just
            ( AuthorizedUser
              { authUserName = uName
              , authUserId   = fromSqlKey k
              , authUserRole = getUserRole (userName v)
              }
            )
          )
        False -> return Nothing

getUserRole :: UserName -> UserRole
getUserRole n = if (n == (UserName "amiribarksdale")) then Admin else Normal
