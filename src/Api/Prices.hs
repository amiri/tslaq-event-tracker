{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators    #-}


module Api.Prices where

import           AppContext             (AppContext (..), AppT (..),
                                         staticDomain)
import           Aws.CloudFront.Signer  (signCannedPolicyURL)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (MonadIO, asks)
import           Data.Text              (Text, pack, unpack)
import           Data.Time.Clock        (addUTCTime, getCurrentTime)
import           Servant
import           Servant.Auth.Server    as SAS
import           System.Command         (Stdout (..), command)
import           Types                  (PriceUrl (..))

type PricesAPI = "prices" :> Get '[JSON] PriceUrl

pricesApi :: Proxy PricesAPI
pricesApi = Proxy

pricesServer :: MonadIO m => CookieSettings -> JWTSettings -> AppT m PriceUrl
pricesServer = getPriceUrl

getPriceUrl :: MonadIO m => CookieSettings -> JWTSettings -> AppT m (PriceUrl)
getPriceUrl _ _ = do
  cfsk   <- asks ctxCloudFrontSigningKey
  latest <- liftIO $ getLatestPricesFile
  let u = staticDomain ++ (unpack latest)
  now <- liftIO $ getCurrentTime
  let oneHourHence = 3600 `addUTCTime` now
  let su           = signCannedPolicyURL cfsk oneHourHence u
  pure $ PriceUrl { url = (pack su) }

getLatestPricesFile :: IO Text
getLatestPricesFile = do
  Stdout l <- command [] "/var/local/tslaq-prices/bin/tslaq-prices" ["--latest"]
  pure (pack l)
