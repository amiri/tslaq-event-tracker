{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Login where

import           AppContext                  (AppT (..))
import           Control.Monad.Except        (MonadError (..), MonadIO, liftIO)
import           Control.Monad.Logger        (MonadLogger (..), logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Data.Text                   (Text, pack, unpack)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, getBy)
import           Errors
import           Models                      (Unique (..), runDb, userName,
                                              userPassword)
import           Servant
import           Servant.Auth.Server         as SAS
import           Types                       (AuthorizedUser (..), BCrypt (..),
                                              UserEmail, UserLogin (..),
                                              UserName (..), UserRole (..),
                                              hashId, passwordValid)

type LoginAPI
  = "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthorizedUser)

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
  setLoginCookies maybeAuthUser cs jwts

setLoginCookies
  :: (MonadError ServerError m, MonadIO m, MonadLogger m)
  => (Maybe AuthorizedUser)
  -> CookieSettings
  -> JWTSettings
  -> m
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header
             "Set-Cookie"
             SetCookie]
           AuthorizedUser
       )
setLoginCookies u cs jwts = case u of
  Nothing -> throwError $ encodeJSONError
    (JSONError 401 "InvalidLogin" "Your login information is invalid.")
  Just u' -> do
    mApplyCookies <- liftIO $ SAS.acceptLogin cs jwts u'
    case mApplyCookies of
      Nothing -> do
        throwError $ encodeJSONError
          (JSONError 401
                     "AcceptLoginFailure"
                     "Your login information was not accepted."
          )
      Just applyCookies -> do
        logDebugNS "web" ((pack $ show (authUserId u')) <> " logged in")
        pure $ applyCookies u'


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
          (Just
            (AuthorizedUser { authUserName = uName
                            , authUserId   = hashId uId
                            , authUserRole = getUserRole (userName v)
                            }
            )
          )
        False -> pure Nothing

getUserRole :: UserName -> UserRole
getUserRole n = if (n == (UserName "amiribarksdale")) then Admin else Normal
