{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Metrics where

import           AppContext             (AppT (..))
import           Control.Lens           ((^.))
import           Control.Monad.Except   (MonadIO, liftIO)
import           Control.Monad.Logger   (logDebugNS)
import           Control.Monad.Metrics  (increment, metricsCounters)
import qualified Control.Monad.Metrics  as Metrics
import           Data.HashMap.Lazy      (HashMap)
import           Data.Int               (Int64)
import           Data.IORef             (readIORef)
import           Data.Text              (Text)
import           Servant
import qualified System.Metrics.Counter as Counter
import Types (AuthorizedUser(..))

type MetricsAPI = "metrics" :> Get '[JSON] (HashMap Text Int64)

metricsApi :: Proxy MetricsAPI
metricsApi = Proxy

-- | The server that runs the MetricsAPI
metricsServer :: MonadIO m => AuthorizedUser -> ServerT MetricsAPI (AppT m)
metricsServer = waiMetrics

-- | Return wai metrics as JSON
waiMetrics :: MonadIO m => AuthorizedUser -> AppT m (HashMap Text Int64)
waiMetrics u = do
  increment "metrics"
  logDebugNS "web" "metrics"
  metr <- Metrics.getMetrics
  liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)

