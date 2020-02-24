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

import           Aws.CloudFront.Signer       (CloudFrontSigningKey (..),
                                              parseRSAPrivateKeyDER)
import           Control.Concurrent          (ThreadId)
import           Control.Lens                ((&), (.~), (^.))
import           Control.Monad.Except        (ExceptT, MonadError)
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (MonadLogger (..))
import           Control.Monad.Metrics       (Metrics, MonadMetrics, getMetrics)
import           Control.Monad.Reader        (MonadIO, MonadReader, ReaderT,
                                              asks)
import           Control.Monad.Trans.AWS     (Credentials (..), Region (..))
import           Crypto.JOSE.JWK             (JWK, KeyMaterialGenParam (..),
                                              fromRSA, genJWK)
import           Data.Aeson                  (FromJSON, ToJSON, decode,
                                              parseJSON, withObject, (.:))
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as LBS
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
import           Katip.Core                  (logStr)
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
import           Servant
import           Servant.Auth.Server         (CookieSettings (..),
                                              IsSecure (..), JWTSettings,
                                              defaultCookieSettings,
                                              defaultJWTSettings)
import           System.Directory            (doesFileExist)
-- import           Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import           Errors
import           System.Environment          (getProgName, lookupEnv)
import           Types

wrapAWSService 's3 "S3Service" "S3Session"
wrapAWSService 'secretsManager "SMService" "SMSession"

appDir :: Environment -> FilePath
appDir Production = "/var/local/tslaq-event-tracker/"
appDir _          = ""

staticDomain :: String
staticDomain = "https://prices.tslaq-event-tracker.org/"

cloudFrontKeyPairId :: String
cloudFrontKeyPairId = "APKAJNXQDHWOTRTU7CRA"

localJSFolder :: Environment -> FilePath
localJSFolder e = (appDir e) <> "react/src/"

jsBucket :: BucketName
jsBucket = "tslaq-api-js"

imageDomain :: String
imageDomain = "https://images.tslaq-event-tracker.org/"

imageBucket :: BucketName
imageBucket = "tslaq-images"

awsRegion :: Region
awsRegion = NorthVirginia

mailGunDomain :: MailGunDomain
mailGunDomain =
  MailGunDomain "https://api.mailgun.net/v3/www.tslaq-event-tracker.org"

getCredentials :: Bool -> Environment -> Credentials
getCredentials b e = do
  case b of
    True  -> FromFile "tslaq-user" "/home/amiri/.aws/credentials"
    False -> do
      case e of
        Test   -> FromEnv "TQ_AK" "TQ_AS" Nothing (Just "NorthVirginia")
        Travis -> FromEnv "TQ_AK" "TQ_AS" Nothing (Just "NorthVirginia")
        _      -> Discover

getAWSConfig :: Environment -> IO AWSConfig
getAWSConfig e = do
  b <- doesFileExist "/home/amiri/.aws/credentials"
  let creds = getCredentials b e
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

getMailGunKey :: Text -> SMSession -> IO (Maybe MailGunKey)
getMailGunKey s = withAWS $ do
  res <- send (getSecretValue s)
  let k = res ^. gsvrsSecretString
  case k of
    Just t  -> pure $ Just (MailGunKey t)
    Nothing -> pure Nothing

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

getCloudFrontSigningKey :: Text -> SMSession -> IO (Maybe CloudFrontSigningKey)
getCloudFrontSigningKey s = withAWS $ do
  res <- send (getSecretValue s)
  let res' = LBS.fromStrict $ fromJust $ res ^. gsvrsSecretBinary
  let k    = parseRSAPrivateKeyDER res'
  case k of
    Left  _  -> pure Nothing
    Right pk -> pure $ Just $ CloudFrontSigningKey cloudFrontKeyPairId pk

getAuthConfig :: JWK -> Environment -> Context '[JWTSettings, CookieSettings]
getAuthConfig j e = (getJWTSettings j) :. (getCookieSettings e) :. EmptyContext

getJWTSettings :: JWK -> JWTSettings
getJWTSettings j = defaultJWTSettings j

testJWK :: IO JWK
testJWK = do
  jwk <- genJWK (RSAGenParam (4096 `div` 8))
  pure jwk

getCookieSettings :: Environment -> CookieSettings
getCookieSettings e = case e of
  Production -> defaultCookieSettings
  _          -> defaultCookieSettings { cookieIsSecure = NotSecure }

-- | This type represents the effects we want to have for our application.
-- We wrap the standard Servant monad with 'ReaderT AppContext', which gives us
-- access to the application configuration using the 'MonadReader'
-- interface's 'ask' function.
--
-- By encapsulating the effects in our newtype, we can add layers to the
-- monad stack without having to modify code that uses the current layout.
newtype AppT m a
    = AppT
    { runApp :: ReaderT AppContext (ExceptT ServerError m) a
    } deriving
    ( Functor, Applicative, Monad, MonadReader AppContext, MonadIO, MonadError ServerError
    )

type App = AppT IO

-- | The AppContext for our application is a bunch of stuff
data AppContext
    = AppContext
    { ctxPool                 :: !ConnectionPool
    , ctxEnv                  :: !Environment
    , ctxMetrics              :: !Metrics
    , ctxEkgServer            :: !ThreadId
    , ctxLogEnv               :: !LogEnv
    , ctxPort                 :: !Port
    , ctxSecretsSession       :: !(TypedSession SMService)
    , ctxS3Session            :: !(TypedSession S3Service)
    , ctxAuthConfig           :: !(Context '[JWTSettings, CookieSettings])
    , ctxJWTSettings          :: !(JWTSettings)
    , ctxCookieSettings       :: !(CookieSettings)
    , ctxCloudFrontSigningKey :: !CloudFrontSigningKey
    , ctxMailGunKey           :: !(MailGunKey)
    -- , ctxLatestJSFile     :: !Text
    -- , ctxLatestPricesFile :: !Text
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
  parseJSON = withObject "PGConnectInfo" $ \obj -> do
    pgUsername             <- obj .: "username"
    pgPassword             <- obj .: "password"
    pgHost                 <- obj .: "host"
    pgPort                 <- obj .: "port"
    pgDbInstanceIdentifier <- obj .: "dbInstanceIdentifier"
    pgSslmode              <- obj .: "sslmode"
    pgSslrootcert          <- obj .: "sslrootcert"
    pure PGConnectInfo { .. }

instance Monad m => MonadMetrics (AppT m) where
  getMetrics = asks AppContext.ctxMetrics

-- | Katip instance for @AppT m@
instance MonadIO m => Katip (AppT m) where
  getLogEnv   = asks ctxLogEnv
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
    | Travis
    | Production
    deriving (Eq, Show, Read)

-- | This returns a 'Middleware' based on the environment that we're in.
setLogger :: Environment -> LogEnv -> Middleware
setLogger Test        _  = Prelude.id
setLogger Travis      _  = Prelude.id
setLogger Development le = katipLogger le
setLogger Production  le = katipLogger le

-- | Web request logger. For inspiration
-- see ApacheLogger from wai-logger package.
katipLogger :: LogEnv -> Middleware
katipLogger logEnv app req respond = runKatipT logEnv $ do
    -- todo: log proper request data
  -- logMsg "web" InfoS "todo: received some request"
  logMsg "web" InfoS (logStr (show req))
  liftIO $ app req respond

getAppEnvironment :: IO Environment
getAppEnvironment = do
  getHostName >>= \case
    "tslaq-event-tracker" -> pure Production
    _                     -> do
      lookupEnv "TRAVIS" >>= \case
        Just "true" -> pure Travis
        _           -> do
          getProgName >>= \case
            "tslaq-event-tracker-test" -> pure Test
            _                          -> pure Development

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
makePool Test   s le = runKatipT le (createPostgresqlPool s (envPool Test))
makePool Travis s le = runKatipT le (createPostgresqlPool s (envPool Travis))
makePool Development s le =
  runKatipT le $ createPostgresqlPool s (envPool Development)
makePool Production s le =
  runKatipT le $ createPostgresqlPool s (envPool Production)

-- | The number of pools to use for a given environment.
envPool :: Environment -> Int
envPool Test        = 1
envPool Travis      = 1
envPool Development = 1
envPool Production  = 8

-- | A basic 'ConnectionString' for local/test development. Pass in either
-- @""@ for 'Development' or @"test"@ for 'Test'.
connStr :: BS.ByteString -> ConnectionString
connStr sfx =
  "host=localhost dbname=tslaq_event_db"
    <> sfx
    <> " user=test password=test port=5432"

checkAuthor :: Monad m => AuthorizedUser -> EditedEvent -> AppT m ()
checkAuthor u e =
  if (userRoleCheck u Admin || authUserId u == authorId (e :: EditedEvent))
    then pure ()
    else throwError $ encodeJSONError
      (JSONError 401 "WrongAuthor" "You are not the author of this item.")

userRoleCheck :: AuthorizedUser -> UserRoleName -> Bool
userRoleCheck u r = r `elem` (authUserRoles u)

userHasRole :: Monad m => AuthorizedUser -> UserRoleName -> AppT m ()
userHasRole u r = if userRoleCheck u r
  then pure ()
  else throwError $ encodeJSONError
    (JSONError 401
               "InsufficientAuthorization"
               "You have insufficient authorization for this action."
    )
