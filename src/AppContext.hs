{-# LANGUAGE DataKinds                  #-}
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

import           Control.Concurrent          (ThreadId)
import           Control.Lens                ((&), (.~), (^.))
import           Control.Monad.Except        (ExceptT, MonadError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (MonadLogger (..))
import           Control.Monad.Metrics       (Metrics, MonadMetrics, getMetrics)
import           Control.Monad.Reader        (MonadIO, MonadReader, ReaderT,
                                              asks)
import           Control.Monad.Trans.AWS     (Credentials (..), Region (..))
import           Crypto.JOSE.JWK             (JWK, fromRSA)
import           Data.Aeson                  (FromJSON, ToJSON, decode,
                                              parseJSON, withObject, (.:))
import qualified Data.ByteString             as BS
import           Data.Maybe                  (fromJust, fromMaybe)
import           Data.Monoid                 ((<>))
import           Data.PEM                    (pemParseBS)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Text.Lazy              (fromStrict)
import qualified Data.Text.Lazy.Encoding     as LE (encodeUtf8)
import           Data.X509                   (PrivKey (PrivKeyRSA))
import           Data.X509.Memory            (pemToKey)
import           Database.Persist.Postgresql (ConnectionPool, ConnectionString,
                                              createPostgresqlPool)
import           GHC.Generics                (Generic)
-- import           Katip.Core                  (logStr)
import           Logger
import           Network.AWS                 (send)
import           Network.AWS.Easy            (AWSConfig, Endpoint (..),
                                              TypedSession, awsConfig,
                                              awscCredentials, withAWS,
                                              wrapAWSService)
import           Network.AWS.S3              (BucketName (..), s3)
import           Network.AWS.SecretsManager  (getSecretValue, gsvrsSecretBinary,
                                              gsvrsSecretString, secretsManager)
import           Network.HostName            (getHostName)
import           Network.Wai                 (Middleware)
import           Network.Wai.Handler.Warp    (Port)
import           Servant                     (Context (..), ServantErr)
import           Servant.Auth.Server         (CookieSettings, JWTSettings,
                                              defaultCookieSettings,
                                              defaultJWTSettings)
import           System.Directory            (doesFileExist)
-- import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)

wrapAWSService 's3 "S3Service" "S3Session"
wrapAWSService 'secretsManager "SMService" "SMSession"

appDir :: FilePath
appDir = "/var/local/tslaq-event-tracker/"

localJSFolder :: FilePath
localJSFolder = appDir ++ "api-js/"

jsBucket :: BucketName
jsBucket = "tslaq-api-js"

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
  pure c

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
  let k = res ^. gsvrsSecretString
  let k' =
        decode (LE.encodeUtf8 $ fromStrict $ fromJust k) :: Maybe PGConnectInfo
  pure k'

getJwtKey :: Text -> SMSession -> IO (Maybe JWK)
getJwtKey s = withAWS $ do
  res <- send (getSecretValue s)
  let res' = fromJust $ res ^. gsvrsSecretBinary
  let k    = pemParseBS res'
  case k of
    Left  _ -> pure Nothing
    Right p -> do
      let k' = head $ pemToKey [] (head p)
      case k' of
        Just (PrivKeyRSA p') -> do
          pure $ Just (fromRSA p')
        _ -> pure Nothing

getAuthConfig :: JWK -> Context '[JWTSettings, CookieSettings]
getAuthConfig j = (getJWTSettings j) :. getCookieSettings :. EmptyContext

getJWTSettings :: JWK -> JWTSettings
getJWTSettings j = defaultJWTSettings j

getCookieSettings :: CookieSettings
getCookieSettings = defaultCookieSettings

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

-- | The AppContext for our application is a bunch of stuff
data AppContext
    = AppContext
    { ctxPool             :: !ConnectionPool
    , ctxEnv              :: !Environment
    , ctxMetrics          :: !Metrics
    , ctxEkgServer        :: !ThreadId
    , ctxLogEnv           :: !LogEnv
    , ctxPort             :: !Port
    , ctxSecretsSession   :: !(TypedSession SMService)
    , ctxS3Session        :: !(TypedSession S3Service)
    , ctxAuthConfig       :: !(Context '[JWTSettings, CookieSettings])
    , ctxJWTSettings      :: !(JWTSettings)
    , ctxCookieSettings   :: !(CookieSettings)
    , ctxLatestJSFile     :: !Text
    , ctxLatestPricesFile :: !Text
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
      pure PGConnectInfo {..}

instance Monad m => MonadMetrics (AppT m) where
    getMetrics = asks AppContext.ctxMetrics

-- | Katip instance for @AppT m@
instance MonadIO m => Katip (AppT m) where
    getLogEnv = asks ctxLogEnv
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
setLogger :: Environment -> LogEnv -> Middleware
setLogger Test        _  = id
setLogger Development le = katipLogger le
setLogger Production  le = katipLogger le

-- | Web request logger. For inspiration
-- see ApacheLogger from wai-logger package.
katipLogger :: LogEnv -> Middleware
katipLogger logEnv app req respond = runKatipT logEnv $ do
    -- todo: log proper request data
  logMsg "web" InfoS "todo: received some request"
  -- logMsg "web" InfoS (logStr (show req))
  liftIO $ app req respond

getEnvironment :: IO Environment
getEnvironment = do
  getHostName >>= \case
    "tslaq-event-tracker" -> pure Production
    _                     -> pure Development


getPgConnectString :: PGConnectInfo -> ConnectionString
getPgConnectString i = BS.intercalate " " $ zipWith (<>) pgKeys pgVals
 where
  PGConnectInfo { pgUsername = username, pgPassword = password, pgHost = host, pgPort = port, pgDbInstanceIdentifier = dbName, pgSslmode = sslMode, pgSslrootcert = sslRootCert }
    = i
  pgVals = map (encodeUtf8) $ filter
    (not . null . T.unpack)
    [ username
    , password
    , host
    , (T.pack $ show port)
    , dbName
    , (fromMaybe "" sslMode)
    , (fromMaybe "" sslRootCert)
    ]
  pgKeys =
    [ "user="
    , "password="
    , "host="
    , "port="
    , "dbname="
    , "sslmode="
    , "sslrootcert="
    ]


-- | This function creates a 'ConnectionPool' for the given environment.
-- For 'Development' and 'Test' environments, we use a stock and highly
-- insecure connection string. The 'Production' environment acquires the
-- information from environment variables that are set by the keter
-- deployment application.
makePool :: Environment -> ConnectionString -> LogEnv -> IO ConnectionPool
makePool Test s le = runKatipT le (createPostgresqlPool s (envPool Test))
makePool Development s le =
  runKatipT le $ createPostgresqlPool s (envPool Development)
makePool Production s le =
  runKatipT le $ createPostgresqlPool s (envPool Production)

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
