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
import           Api.Metrics
import           Api.Prices
import           Api.ReadEvent
import           Api.User
import           AppContext             (AppContext, AppT (..), Environment,
                                         S3Session, ctxCloudFrontSigningKey,
                                         jsBucket, localJSFolder, staticDomain)
import           Control.Monad          (void)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger   (MonadLogger, logDebugNS)
import           Control.Monad.Reader   (MonadIO, MonadReader, asks)
import           CustomAxios            (customAxios)
import qualified Data.ByteString        as B (writeFile)
import qualified Data.ByteString.Lazy   as LB (fromStrict)
import           Data.Digest.Pure.MD5   (md5)
import           Data.Text              (Text, pack, unpack)
import           Data.Text.Encoding     (encodeUtf8)
import           Instances
import           Network.AWS            (send)
import           Network.AWS.Data.Body  (toBody)
import           Network.AWS.Easy       (withAWS)
import           Network.AWS.S3         (ObjectKey (..), putObject)
import           Servant
import           Servant.Auth.Server    as SAS
import           Servant.JS             (defAxiosOptions, jsForAPI)
import           Types                  (AuthorizedUser (..), PriceUrl (..))

-- Servant type representation
type TSLAQAPI auths = (SAS.Auth auths AuthorizedUser :> ProtectedAPI) :<|> PublicAPI

type ProtectedAPI = UserAPI :<|> EventAPI :<|> MetricsAPI

type PublicAPI = ReadEventAPI :<|> LoginAPI :<|> PricesAPI

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
  userServer u :<|> eventServer u :<|> metricsServer u
protectedServer SAS.BadPassword = throwAll err401 { errBody = "Bad password." }
protectedServer SAS.NoSuchUser  = throwAll err401 { errBody = "No such user." }
protectedServer SAS.Indefinite =
  throwAll err401 { errBody = "Indefinite error." }

publicServer
  :: MonadIO m => CookieSettings -> JWTSettings -> ServerT PublicAPI (AppT m)
publicServer cs jwts =
  readEventServer cs jwts :<|> loginServer cs jwts :<|> pricesServer cs jwts

-- | Generates JavaScript to query the User API.
generateJavaScript :: Environment -> S3Session -> IO ()
generateJavaScript e = withAWS $ do
  let js =
        encodeUtf8
          $ jsForAPI (Proxy :: Proxy (TSLAQAPI '[JWT, Cookie]))
          $ customAxios defAxiosOptions
  let h             = md5 $ LB.fromStrict js
  let f             = "tslaq-api-" ++ (show h) ++ ".js"
  let f'            = pack f
  let localFilename = (localJSFolder e) ++ "api.js"
  liftIO $ B.writeFile localFilename js
  let body = toBody js
  void $ send (putObject jsBucket (ObjectKey f') body)
  -- pure f'

