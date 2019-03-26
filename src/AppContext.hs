-- {-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module AppContext where

import           Control.Concurrent                   (ThreadId)
import           Control.Exception                    (throwIO)
import           Control.Lens                         ((&), (.~), (^.))
import           Control.Monad.Except                 (ExceptT, MonadError)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Logger                 (MonadLogger (..))
import           Control.Monad.Metrics                (Metrics, MonadMetrics,
                                                       getMetrics)
import           Control.Monad.Reader                 (MonadIO, MonadReader,
                                                       ReaderT, asks)
import           Control.Monad.Trans.AWS              (Credentials (..),
                                                       Region (..))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import           Data.Aeson                           (FromJSON, ToJSON, decode,
                                                       parseJSON, withObject,
                                                       (.:))
import qualified Data.ByteString.Char8                as BS
import           Data.Maybe                           (fromJust)
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text)
import           Data.Text.Lazy                       (fromStrict)
import           Data.Text.Lazy.Encoding              (encodeUtf8)
import           Database.Persist.Postgresql          (ConnectionPool,
                                                       ConnectionString,
                                                       createPostgresqlPool)
import           GHC.Generics                         (Generic)
import           Network.AWS                          (send)
import           Network.AWS.Easy                     (AWSConfig, Endpoint (..),
                                                       TypedSession, awsConfig,
                                                       awscCredentials, withAWS,
                                                       wrapAWSService)
import           Network.AWS.S3                       (s3)
import           Network.AWS.SecretsManager           (getSecretValue,
                                                       gsvrsSecretString,
                                                       secretsManager)
import           Network.HostName                     (getHostName)
import           Network.Wai                          (Middleware)
import           Network.Wai.Handler.Warp             (Port)
import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Servant                              (ServantErr)
import           System.Directory                     (doesFileExist)
import           System.Environment                   (lookupEnv)

import           Logger

wrapAWSService 's3 "S3Service" "S3Session"
wrapAWSService 'secretsManager "SMService" "SMSession"

appDir :: FilePath
appDir = "/var/local/tslaq-event-tracker/"

awsRegion :: Region
awsRegion = NorthVirginia

getCredentials :: Bool -> Credentials
getCredentials b = do
  case b of
    True  -> FromFile "tslaq-user" "/home/amiri/.aws/credentials"
    False -> Discover

getAWSConfig :: IO AWSConfig
getAWSConfig = do
  b <- doesFileExist "/home/amiri/.aws/credentials"
  let creds = getCredentials b
  let r     = AWSRegion awsRegion
  let c     = awsConfig r & awscCredentials .~ creds
  return c

defaultPgConnectInfo :: PGConnectInfo
defaultPgConnectInfo = PGConnectInfo
  { pgUsername             = "tslaq"
  , pgPassword             = "bk"
  , pgHost                 = "localhost"
  , pgPort                 = 5432
  , pgDbInstanceIdentifier = "tslaq-event-db"
  , pgSslmode              = Nothing
  , pgSslrootcert          = Nothing
  }

getPgConnectInfo :: Text -> SMSession -> IO (Maybe PGConnectInfo)
getPgConnectInfo s = withAWS $ do
  res <- send (getSecretValue s)
  let k  = res ^. gsvrsSecretString
  let k' = decode (encodeUtf8 $ fromStrict $ fromJust k) :: Maybe PGConnectInfo
  return k'

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT AppContext', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a
    = AppT
    { runApp :: ReaderT AppContext (ExceptT ServantErr m) a
    } deriving
    ( Functor, Applicative, Monad, MonadReader AppContext, MonadError ServantErr
    , MonadIO
    )

type App = AppT IO

-- | The AppContext for our application is (for now) the 'Environment' we're
-- running in and a Persistent 'ConnectionPool'.
data AppContext
    = AppContext
    { configPool           :: !ConnectionPool
    , configEnv            :: !Environment
    , configMetrics        :: !Metrics
    , configEkgServer      :: !ThreadId
    , configLogEnv         :: !LogEnv
    , configPort           :: !Port
    , configSecretsSession :: !(TypedSession SMService)
    , configS3Session      :: !(TypedSession S3Service)
    }

data PGConnectInfo = PGConnectInfo {
    pgUsername             :: Text
  , pgPassword             :: Text
  , pgHost                 :: Text
  , pgPort                 :: Int
  , pgDbInstanceIdentifier :: Text
  , pgSslmode              :: Maybe Text
  , pgSslrootcert          :: Maybe Text
  } deriving (Show, Generic)

instance ToJSON PGConnectInfo
instance FromJSON PGConnectInfo where
  parseJSON =
    withObject "PGConnectInfo" $ \obj -> do
      pgUsername <- obj .: "username"
      pgPassword <- obj .: "password"
      pgHost <- obj .: "host"
      pgPort <- obj .: "port"
      pgDbInstanceIdentifier <- obj .: "dbInstanceIdentifier"
      pgSslmode <- obj .: "sslmode"
      pgSslrootcert <- obj .: "sslrootcert"
      return PGConnectInfo {..}

instance Monad m => MonadMetrics (AppT m) where
    getMetrics = asks AppContext.configMetrics

-- | Katip instance for @AppT m@
instance MonadIO m => Katip (AppT m) where
    getLogEnv = asks configLogEnv
    localLogEnv = error "not implemented"

-- | MonadLogger instance to use within @AppT m@
instance MonadIO m => MonadLogger (AppT m) where
    monadLoggerLog = adapt logMsg

-- | MonadLogger instance to use in @makePool@
instance MonadIO m => MonadLogger (KatipT m) where
    monadLoggerLog = adapt logMsg

-- | Right now, we're distinguishing between three environments. We could
-- also add a @Staging@ environment if we needed to.
data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> Middleware
setLogger Test        = id
setLogger Development = logStdoutDev
setLogger Production  = logStdout

-- | Web request logger (currently unimplemented and unused). For inspiration
-- see ApacheLogger from wai-logger package.
katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
    -- todo: log proper request data
  logMsg "web" InfoS "todo: received some request"
  liftIO $ app req respond

getEnvironment :: IO Environment
getEnvironment = do
  getHostName >>= \case
    "tslaq-event-tracker" -> return Production
    _                     -> return Development


-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
makePool :: Environment -> LogEnv -> IO ConnectionPool
makePool Test env =
  runKatipT env (createPostgresqlPool (connStr "-test") (envPool Test))
makePool Development env =
  runKatipT env $ createPostgresqlPool (connStr "") (envPool Development)
makePool Production env = do
    -- This function makes heavy use of the 'MaybeT' monad transformer, which
    -- might be confusing if you're not familiar with it. It allows us to
    -- combine the effects from 'IO' and the effect of 'Maybe' into a single
    -- "big effect", so that when we bind out of @MaybeT IO a@, we get an
    -- @a@. If we just had @IO (Maybe a)@, then binding out of the IO would
    -- give us a @Maybe a@, which would make the code quite a bit more
    -- verbose.
  pool <- runMaybeT $ do
    let keys =
          [ "host="
          , "port="
          , "user="
          , "password="
          , "dbname="
          , "sslmode="
          , "sslrootcert="
          ]
    let envs = ["PGHOST", "PGPORT", "PGUSER", "PGPASS", "PGDATABASE"]
    envVars <- traverse (MaybeT . lookupEnv) envs
    let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
    lift $ runKatipT env $ createPostgresqlPool prodStr (envPool Production)
  case pool of
      -- If we don't have a correct database configuration, we can't
      -- handle that in the program, so we throw an IO exception. This is
      -- one example where using an exception is preferable to 'Maybe' or
      -- 'Either'.
    Nothing -> throwIO
      (userError "Database AppContexturation not present in environment.")
    Just a -> return a

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

-- | A basic 'ConnectionString' for local/test development. Pass in either
-- @""@ for 'Development' or @"test"@ for 'Test'.
connStr :: BS.ByteString -> ConnectionString
connStr sfx =
  "host=localhost dbname=perservant"
    <> sfx
    <> " user=test password=test port=5432"
