{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Api.TSLAQ where

import           Api.Event
import           Api.Login
import           Api.Logout
import           Api.Metrics
import           Api.Prices
import           Api.ReadEvent
import           Api.Register
import           Api.User
import           AppContext             (AppT (..), Environment, S3Session,
                                         jsBucket, localJSFolder)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (MonadIO)
import           CustomAxios            (customAxios)
import qualified Data.ByteString        as B (writeFile)
import qualified Data.ByteString.Lazy   as LB (fromStrict)
import           Data.Digest.Pure.MD5   (md5)
import           Data.Text              (pack)
import           Data.Text.Encoding     (encodeUtf8)
import           Debug.Trace
import           Errors
import           Instances
import           Network.AWS            (send)
import           Network.AWS.Data.Body  (toBody)
import           Network.AWS.Easy       (withAWS)
import           Network.AWS.S3         (ObjectKey (..), putObject)
import           Servant
import           Servant.Auth.Server    as SAS
import           Servant.JS             (defAxiosOptions, jsForAPI,
                                         withCredentials)
import           Types                  (AuthorizedUser (..))

-- Servant type representation
type TSLAQAPI auths
  = (SAS.Auth auths AuthorizedUser :> ProtectedAPI) :<|> PublicAPI

type ProtectedAPI = UserAPI :<|> EventAPI :<|> MetricsAPI :<|> LogoutAPI

type PublicAPI = ReadEventAPI :<|> LoginAPI :<|> PricesAPI :<|> RegisterAPI

-- Servant API
tslaqApi :: Proxy (TSLAQAPI '[JWT, Cookie])
tslaqApi = Proxy

protectedApi :: Proxy ProtectedAPI
protectedApi = Proxy

publicApi :: Proxy PublicAPI
publicApi = Proxy

-- Servant servers
tslaqServer
  :: MonadIO m
  => SAS.CookieSettings
  -> SAS.JWTSettings
  -> ServerT (TSLAQAPI auths) (AppT m)
tslaqServer cs jwts = protectedServer :<|> publicServer cs jwts

protectedServer
  :: MonadIO m => AuthResult AuthorizedUser -> ServerT ProtectedAPI (AppT m)
protectedServer (SAS.Authenticated u) =
  userServer u :<|> eventServer u :<|> metricsServer u :<|> logoutServer u
protectedServer SAS.BadPassword = throwAll $ encodeJSONError
  (JSONError 401 "BadPassword" "You entered the wrong password.")
protectedServer SAS.NoSuchUser = throwAll
  $ encodeJSONError (JSONError 401 "NoSuchUser" "There is no such user.")
protectedServer SAS.Indefinite = throwAll $ encodeJSONError
  (JSONError 401 "Indefinite" "There has been an indefinite error.")

publicServer
  :: MonadIO m => CookieSettings -> JWTSettings -> ServerT PublicAPI (AppT m)
publicServer cs jwts =
  readEventServer cs jwts
    :<|> loginServer cs jwts
    :<|> pricesServer cs jwts
    :<|> registerServer cs jwts

-- | Generates JavaScript to query the User API.
writeJS :: Environment -> S3Session -> IO ()
writeJS e = withAWS $ do
  let js = encodeUtf8 $ jsForAPI tslaqApi $ customAxios
        e
        defAxiosOptions { withCredentials = True }
  let localFilename = (localJSFolder e) ++ "Api.js"
  liftIO $ B.writeFile localFilename js
