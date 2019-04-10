{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

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
import           Network.AWS           (send)
import           Network.AWS.Data.Body (toBody)
import           Network.AWS.Easy      (withAWS)
import           Network.AWS.S3        (ObjectKey (..), putObject)
import           Servant
import           Servant.JS            (jsForAPI, vanillaJS)
import           System.Command


type TSLAQAPI = ProtectedAPI :<|> PublicAPI

tslaqApi :: Proxy TSLAQAPI
tslaqApi = Proxy

tslaqServer :: MonadIO m => ServerT TSLAQAPI (AppT m)
tslaqServer = protectedServer :<|> publicServer

type ProtectedAPI = UserAPI :<|> EventAPI :<|> MetricsAPI

protectedApi :: Proxy ProtectedAPI
protectedApi = Proxy

protectedServer :: MonadIO m => ServerT ProtectedAPI (AppT m)
protectedServer = userServer :<|> eventServer :<|> metricsServer

type PublicAPI = ReadEventAPI

publicApi :: Proxy PublicAPI
publicApi = Proxy

publicServer :: MonadIO m => ServerT PublicAPI (AppT m)
publicServer = readEventServer

-- | Generates JavaScript to query the User API.
generateJavaScript :: S3Session -> IO Text
generateJavaScript = withAWS $ do
  let js            = encodeUtf8 $ jsForAPI (Proxy :: Proxy TSLAQAPI) vanillaJS
  let h             = md5 $ LB.fromStrict js
  let f             = "tslaq-api-" ++ (show h) ++ ".js"
  let f'            = pack f
  let localFilename = localJSFolder ++ f
  liftIO $ B.writeFile localFilename js
  let body = toBody js
  void $ send (putObject jsBucket (ObjectKey f') body)
  return f'

getLatestPricesFile :: IO Text
getLatestPricesFile = do
  Stdout l <- command []
                      "/var/local/tslaq-prices/bin/tslaq-prices-exe"
                      ["--latest"]
  return (pack l)
