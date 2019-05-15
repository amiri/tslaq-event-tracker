{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Logout where

import           AppContext           (AppContext (..), AppT (..))
import           Control.Monad.Logger (logDebugNS)
import           Control.Monad.Reader (MonadIO, asks)
import           Data.Text            (pack)
import           Servant
import           Servant.Auth.Server  as SAS
import           Types                (AuthorizedUser (..))

type LogoutAPI = "logout" :> Get '[JSON] (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] NoContent)

logoutApi :: Proxy LogoutAPI
logoutApi = Proxy

logoutServer
  :: MonadIO m
  => AuthorizedUser
  -> AppT
       m
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie]
           NoContent
       )
logoutServer u = logout u

logout
  :: MonadIO m
  => AuthorizedUser
  -> AppT
       m
       ( Headers
           '[Header "Set-Cookie" SetCookie, Header'
             '[Optional, Strict]
             "Set-Cookie"
             SetCookie]
           NoContent
       )
logout u = do
  cs <- asks ctxCookieSettings
  logDebugNS "web" ((pack $ show (authUserId u)) <> " logged out")
  pure $ SAS.clearSession cs NoContent
