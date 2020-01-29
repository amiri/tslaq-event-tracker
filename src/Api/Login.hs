{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Login where

import           AppContext            (AppT (..))
import           Control.Monad.Except  (MonadError (..), MonadIO, liftIO)
import           Control.Monad.Logger  (MonadLogger (..), logDebugNS)
import           Control.Monad.Metrics (increment)
import           Data.Maybe            (catMaybes)
import           Data.Text             (Text, pack)
import           Database.Esqueleto
import           Errors
import           Models
import           Servant
import           Servant.Auth.Server   as SAS
import           Types                 (AuthorizedUser (..), RoleName (..),
                                        UserEmail, UserLogin (..),
                                        UserRoleName (..), hashId)

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


toUserRoleName :: RoleName -> UserRoleName
toUserRoleName (Types.RoleName n) = case n of
  "Admin"       -> Admin
  "Contributor" -> Contributor
  _             -> Normal

validateLogin
  :: MonadIO m => UserEmail -> Text -> AppT m (Maybe (AuthorizedUser))
validateLogin e _ = do
  userWithRoles <-
    runDb $ select $ from $ \(u `LeftOuterJoin` ur `LeftOuterJoin` r) -> do
      on $ r ?. RoleId ==. ur ?. UserRoleRoleId
      on $ just (u ^. UserId) ==. ur ?. UserRoleUserId
      where_ (u ^. UserEmailAddress ==. val e)
      pure (u, r)
  case userWithRoles of
    [] -> pure Nothing
    _  -> do
      let (users, roles) = unzip userWithRoles
      let ((Entity uk uv), rs) =
            ( (head users)
            , ( map (\r' -> toUserRoleName $ roleName $ entityVal r')
              $ catMaybes roles
              )
            )
      pure
        (Just
          (AuthorizedUser { authUserName  = userName uv
                          , authUserId    = hashId $ fromSqlKey uk
                          , authUserRoles = rs
                          }
          )
        )
