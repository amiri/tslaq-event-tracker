{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module App where

import           Api                                   (app)
import           Api.TSLAQ                             (writeJS)
import           AppContext                            (AppContext (..),
                                                        Environment (..),
                                                        defaultPgConnectInfo,
                                                        getAWSConfig,
                                                        getAppEnvironment,
                                                        getAuthConfig,
                                                        getCloudFrontSigningKey,
                                                        getCookieSettings,
                                                        getJWTSettings,
                                                        getJwtKey,
                                                        getMailGunKey,
                                                        getPgConnectInfo,
                                                        getPgConnectString,
                                                        makePool, s3Service,
                                                        secretsManagerService,
                                                        setLogger)
import           Control.Concurrent                    (killThread)
import           Control.Exception                     (bracket)
import           Control.Lens                          ((^.))
import qualified Control.Monad.Metrics                 as M
import           Data.Char                             (toLower)
import           Data.Maybe                            (fromJust, fromMaybe)
import qualified Data.Pool                             as Pool
import qualified Data.Text                             as T
import           Database.Persist.Postgresql           (runSqlPool)
import qualified Katip
import           Logger                                (defaultLogEnv)
import           Models                                (doMigrations)
import           Network.AWS.Easy                      (connect)
import           Network.Wai                           (Application)
import           Network.Wai.Handler.Warp              (run)
import           Network.Wai.Metrics                   (metrics,
                                                        registerNamedWaiMetrics,
                                                        registerWaiMetrics)
import           Network.Wai.Middleware.Cors           (cors, corsMethods,
                                                        corsOrigins,
                                                        corsRequestHeaders,
                                                        simpleCorsResourcePolicy)
import           Network.Wai.Middleware.Servant.Errors (errorMw)
import           Servant                               (JSON)
import           System.Remote.Monitoring              (forkServer,
                                                        serverMetricStore,
                                                        serverThreadId)

-- | An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
runApp :: IO ()
runApp = bracket acquireAppContext shutdownApp runApp'
  where runApp' ctx = run (ctxPort ctx) =<< initialize ctx

generate :: IO ()
generate = do
  env       <- getAppEnvironment
  c         <- getAWSConfig env
  s3Session <- connect c s3Service
  writeJS env s3Session


-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: AppContext -> IO Application
initialize ctx = do
  waiMetrics <- registerNamedWaiMetrics "TSLAQ"
                                        (ctxMetrics ctx ^. M.metricsStore)
  let logger = setLogger (ctxEnv ctx) (ctxLogEnv ctx)
  runSqlPool doMigrations (ctxPool ctx)
  -- wrap app in middleware
  ( pure
    . cors (const . Just $ corsPolicy)
    . errorMw @JSON @'["detail", "statusCode"]
    . logger
    . metrics waiMetrics
    . app
    )
    ctx where
  corsPolicy = simpleCorsResourcePolicy
    { corsOrigins        = Just
                             ( [ "http://localhost:7777"
                               , "https://www.tslaq-event-tracker.org"
                               , "https://prices.tslaq-event-tracker.org"
                               , "https://images.tslaq-event-tracker.org"
                               ]
                             , True
                             )
    , corsRequestHeaders = ["Authorization", "Content-Type", "X-XSRF-TOKEN"]
    , corsMethods        = ["GET", "HEAD", "POST", "PUT"]
    }

-- | Allocates resources for 'AppContext'
acquireAppContext :: IO AppContext
acquireAppContext = do
  let port = 8888
  env       <- getAppEnvironment
  logEnv    <- defaultLogEnv (T.pack $ map toLower $ show env)
  ekgServer <- forkServer "localhost" 8000
  let store = serverMetricStore ekgServer
  _              <- registerWaiMetrics store
  metr           <- M.initializeWith store
  c              <- getAWSConfig env
  secretsSession <- connect c secretsManagerService
  s3Session      <- connect c s3Service
  pgConnectInfo  <- case env of
    Production -> getPgConnectInfo "pgconnectinfo" secretsSession
    _          -> pure Nothing
  let pgConnectInfo'     = fromMaybe defaultPgConnectInfo pgConnectInfo
  let pgConnectionString = getPgConnectString pgConnectInfo'
  pool   <- makePool env pgConnectionString logEnv
  jwtKey <- getJwtKey "tslaq-jwt-key" secretsSession
  let j = fromJust jwtKey
  cloudFrontSigningKey <- getCloudFrontSigningKey "tslaq-cloudfront-key"
                                                  secretsSession
  let cfsk       = fromJust cloudFrontSigningKey
  let authConfig = getAuthConfig j env
  mailGunKey <- getMailGunKey "tslaq-mailgun-key" secretsSession
  let m = fromJust mailGunKey
  pure AppContext { ctxPool                 = pool
                  , ctxEnv                  = env
                  , ctxMetrics              = metr
                  , ctxLogEnv               = logEnv
                  , ctxPort                 = port
                  , ctxEkgServer            = serverThreadId ekgServer
                  , ctxSecretsSession       = secretsSession
                  , ctxS3Session            = s3Session
                  , ctxAuthConfig           = authConfig
                  , ctxJWTSettings          = getJWTSettings j
                  , ctxCookieSettings       = getCookieSettings env
                  , ctxCloudFrontSigningKey = cfsk
                  , ctxMailGunKey           = m
                  }

-- | Takes care of cleaning up 'AppContext' resources
shutdownApp :: AppContext -> IO ()
shutdownApp ctx = do
  _ <- Katip.closeScribes (ctxLogEnv ctx)
  Pool.destroyAllResources (ctxPool ctx)
  -- Monad.Metrics does not provide a function to destroy metrics store
  -- so, it'll hopefully get torn down when async exception gets thrown
  -- at metrics server process
  killThread (ctxEkgServer ctx)
  pure ()
