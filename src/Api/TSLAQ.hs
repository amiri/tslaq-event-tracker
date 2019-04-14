{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Api.TSLAQ where

import           Api.Event
import           Api.Metrics
import           Api.ReadEvent
import           Api.User
import           AppContext            (AppT (..), S3Session, jsBucket,
                                        localJSFolder)
import           Control.Monad         (void)
import           Control.Monad.Except  (MonadIO, liftIO)
import qualified Data.ByteString       as B (writeFile)
import qualified Data.ByteString.Lazy  as LB (fromStrict)
import           Data.Digest.Pure.MD5  (md5)
import           Data.Text             (Text, pack)
import           Data.Text.Encoding    (encodeUtf8)
import           Instances
import           Network.AWS           (send)
import           Network.AWS.Data.Body (toBody)
import           Network.AWS.Easy      (withAWS)
import           Network.AWS.S3        (ObjectKey (..), putObject)
import           Servant
import           Servant.Auth.Server   as SAS
import           Servant.JS
import           System.Command
import           Types                 (AuthorizedUser (..))


type TSLAQAPI auths = (SAS.Auth auths AuthorizedUser :> ProtectedAPI) :<|> PublicAPI

tslaqApi :: Proxy (TSLAQAPI '[JWT, Cookie])
tslaqApi = Proxy

tslaqServer
  :: MonadIO m
  => SAS.CookieSettings
  -> SAS.JWTSettings
  -> ServerT (TSLAQAPI auths) (AppT m)
tslaqServer cs jwts = protectedServer :<|> publicServer cs jwts

type ProtectedAPI = UserAPI :<|> EventAPI :<|> MetricsAPI

protectedApi :: Proxy ProtectedAPI
protectedApi = Proxy

protectedServer
  :: MonadIO m => AuthResult AuthorizedUser -> ServerT ProtectedAPI (AppT m)
protectedServer (SAS.Authenticated u) =
  userServer u :<|> eventServer u :<|> metricsServer u
protectedServer _ = throwAll err401 { errBody = "Unauthorized" }

type PublicAPI = ReadEventAPI

publicApi :: Proxy PublicAPI
publicApi = Proxy

publicServer
  :: MonadIO m => CookieSettings -> JWTSettings -> ServerT PublicAPI (AppT m)
publicServer cs jwts = readEventServer cs jwts

-- | Generates JavaScript to query the User API.
generateJavaScript :: S3Session -> IO Text
generateJavaScript = withAWS $ do
  let
    js =
      encodeUtf8 $ jsForAPI (Proxy :: Proxy (TSLAQAPI '[JWT, Cookie])) vanillaJS
  let h             = md5 $ LB.fromStrict js
  let f             = "tslaq-api-" ++ (show h) ++ ".js"
  let f'            = pack f
  let localFilename = localJSFolder ++ f
  liftIO $ B.writeFile localFilename js
  let body = toBody js
  void $ send (putObject jsBucket (ObjectKey f') body)
  pure f'

getLatestPricesFile :: IO Text
getLatestPricesFile = do
  Stdout l <- command []
                      "/var/local/tslaq-prices/bin/tslaq-prices-exe"
                      ["--latest"]
  pure (pack l)
