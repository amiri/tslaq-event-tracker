{-# LANGUAGE OverloadedStrings #-}

module App where

import           Api                         (app)
import           Api.TSLAQ                   (generateJavaScript,
                                              getLatestPricesFile)
import           AppContext                  (AppContext (..),
                                              defaultPgConnectInfo,
                                              getAWSConfig, getAuthConfig,
                                              getCookieSettings, getEnvironment,
                                              getJWTSettings, getJwtKey,
                                              getPgConnectInfo,
                                              getPgConnectString, makePool,
                                              s3Service, secretsManagerService,
                                              setLogger)
import           Control.Concurrent          (killThread)
import           Control.Exception           (bracket)
import           Control.Lens                ((^.))
import qualified Control.Monad.Metrics       as M
import           Data.Char                   (toLower)
import           Data.Maybe                  (fromJust, fromMaybe)
import qualified Data.Pool                   as Pool
import qualified Data.Text                   as T
import           Database.Persist.Postgresql (runSqlPool)
import qualified Katip
import           Logger                      (defaultLogEnv)
import           Models                      (doMigrations)
import           Network.AWS.Easy            (connect)
import           Network.HostName            (getHostName)
import           Network.Wai                 (Application)
import           Network.Wai.Handler.Warp    (run)
import           Network.Wai.Metrics         (metrics, registerNamedWaiMetrics,
                                              registerWaiMetrics)
import           System.Remote.Monitoring    (forkServer, serverMetricStore,
                                              serverThreadId)

-- | An action that creates a WAI 'Application' together with its resources,
--   runs it, and tears it down on exit
runApp :: IO ()
runApp = bracket acquireAppContext shutdownApp runApp'
  where runApp' ctx = run (ctxPort ctx) =<< initialize ctx

-- | The 'initialize' function accepts the required environment information,
-- initializes the WAI 'Application' and returns it
initialize :: AppContext -> IO Application
initialize ctx = do
  waiMetrics <- registerNamedWaiMetrics "TSLAQ"
                                        (ctxMetrics ctx ^. M.metricsStore)
  let logger = setLogger (ctxEnv ctx) (ctxLogEnv ctx)
  runSqlPool doMigrations (ctxPool ctx)
  -- wrap app in middleware
  (pure . logger . metrics waiMetrics . app) ctx

-- | Allocates resources for 'AppContext'
acquireAppContext :: IO AppContext
acquireAppContext = do
  let port = 8888
  env       <- getEnvironment
  logEnv    <- defaultLogEnv (T.pack $ map toLower $ show env)
  ekgServer <- forkServer "localhost" 8000
  let store = serverMetricStore ekgServer
  _              <- registerWaiMetrics store
  metr           <- M.initializeWith store
  c              <- getAWSConfig
  secretsSession <- connect c secretsManagerService
  s3Session      <- connect c s3Service
  hostname       <- getHostName
  pgConnectInfo  <- case hostname of
    "tslaq-event-tracker" -> getPgConnectInfo "pgconnectinfo" secretsSession
    _                     -> pure Nothing
  let pgConnectInfo'     = fromMaybe defaultPgConnectInfo pgConnectInfo
  let pgConnectionString = getPgConnectString pgConnectInfo'
  pool   <- makePool env pgConnectionString logEnv
  jwtKey <- getJwtKey "tslaq-jwt-key" secretsSession
  let j          = fromJust jwtKey
  let authConfig = getAuthConfig j
  latestJSFile     <- generateJavaScript s3Session
  latestPricesFile <- getLatestPricesFile
  pure AppContext
    { ctxPool             = pool
    , ctxEnv              = env
    , ctxMetrics          = metr
    , ctxLogEnv           = logEnv
    , ctxPort             = port
    , ctxEkgServer        = serverThreadId ekgServer
    , ctxSecretsSession   = secretsSession
    , ctxS3Session        = s3Session
    , ctxAuthConfig       = authConfig
    , ctxJWTSettings      = getJWTSettings j
    , ctxCookieSettings   = getCookieSettings
    , ctxLatestJSFile     = latestJSFile
    , ctxLatestPricesFile = latestPricesFile
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
