{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Login where

import           AppContext           (AppT (..))
import           Control.Monad.Except (MonadIO)
import           Servant
import           Servant.Auth.Server  as SAS
import           Types                (AuthorizedUser, NewUser, UserLogin)

type LoginAPI = "login" :> ReqBody '[JSON] UserLogin :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthorizedUser)
    :<|> "register" :> ReqBody '[JSON] NewUser :> Post '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] AuthorizedUser)

loginApi :: Proxy LoginAPI
loginApi = Proxy

loginServer
  :: MonadIO m => CookieSettings -> JWTSettings -> ServerT LoginAPI (AppT m)
loginServer cs jwts = login cs jwts :<|> register cs jwts

login = undefined

register = undefined
