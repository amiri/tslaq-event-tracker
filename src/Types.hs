{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Types where

import           Control.Lens                ((^.))
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import           Data.Aeson                  (FromJSON, ToJSON, decode,
                                              parseJSON, withObject, (.:))
import           Data.ByteString
import           Data.Maybe                  (fromJust)
import           Data.Text                   (Text)
import           Data.Text.Lazy              (fromStrict)
import           Data.Text.Lazy.Encoding     (encodeUtf8)
import           Data.Time.Clock             (UTCTime)
import           Database.Persist.Postgresql
import qualified Database.Persist.TH         as PTH
import           GHC.Generics                (Generic)
import           Network.AWS                 (send)
import           Network.AWS.Data.Body       (toBody)
import           Network.AWS.Easy            (TypedSession, withAWS,
                                              wrapAWSService)
import           Network.AWS.S3              (s3)
import           Network.AWS.SecretsManager  (getSecretValue, gsvrsSecretString,
                                              secretsManager)
import           Network.HostName            (getHostName)
import           Servant
import           System.Log.Logger           (Logger)
import           System.Log.Logger           (Logger)

wrapAWSService 's3 "S3Service" "S3Session"
wrapAWSService 'secretsManager "SMService" "SMSession"

type AppM = ReaderT AppContext Handler

data AppContext = AppContext {
    envLog         :: !Logger
  , secretsSession :: !(TypedSession SMService)
  , s3Session      :: !(TypedSession S3Service)
  , pgConnectInfo  :: !PGConnectInfo
  }

data PGConnectInfo = PGConnectInfo {
    pgUsername             :: Text
  , pgPassword             :: Text
  , pgHost                 :: Text
  , pgPort                 :: Int
  , pgDbInstanceIdentifier :: Text
  , pgSslmode              :: Maybe Text
  , pgSslrootcert          :: Maybe Text
  } deriving (Show, Generic, ToJSON)

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

PTH.share [PTH.mkPersist PTH.sqlSettings { PTH.mpsGeneric = True , PTH.mpsPrefixFields = False , PTH.mpsEntityJSON = Just PTH.EntityJSON { entityToJSON = 'keyValueEntityToJSON , entityFromJSON = 'keyValueEntityFromJSON } , PTH.mpsGenerateLenses = True } , PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  User json sql=users
    registration_time UTCTime default=CURRENT_TIME
    email_address Text
    name Text
    password Text
    UniqueEmail email_address
    deriving Show Generic Read
  Event json sql=events
    create_time UTCTime default=CURRENT_TIME
    event_time UTCTime default=CURRENT_TIME
    title Text
    body Text
    UniqueTitle title
    deriving Show Generic Read
|]
