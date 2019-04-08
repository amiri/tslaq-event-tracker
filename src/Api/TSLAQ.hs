{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.TSLAQ where

import           AppContext                  (AppT (..), S3Session, jsBucket,
                                              localJSFolder)
import           Control.Lens                ((^.))
import           Control.Monad               (void)
import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment, metricsCounters)
import qualified Control.Monad.Metrics       as Metrics
import qualified Data.ByteString             as B (writeFile)
import qualified Data.ByteString.Lazy        as LB (fromStrict)
import           Data.Digest.Pure.MD5        (md5)
import           Data.HashMap.Lazy           (HashMap)
import           Data.Int                    (Int64)
import           Data.IORef                  (readIORef)
import           Data.Text                   (Text, pack)
import           Data.Text.Encoding          (encodeUtf8)
import           Data.Time.Clock             (getCurrentTime)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey,
                                              getEntity, insert, selectList,
                                              toSqlKey)
import           Models                      (Event (Event), Key, User (User),
                                              eventBody, eventEventTime,
                                              eventTitle, runDb,
                                              userEmailAddress, userName,
                                              userPassword)
import           Network.AWS                 (send)
import           Network.AWS.Data.Body       (toBody)
import           Network.AWS.Easy            (withAWS)
import           Network.AWS.S3              (ObjectKey (..), putObject)
import           Servant
import           Servant.JS                  (jsForAPI, vanillaJS)
import           System.Command
import qualified System.Metrics.Counter      as Counter


type TSLAQAPI = UserAPI :<|> EventAPI :<|> MetricsAPI

tslaqApi :: Proxy TSLAQAPI
tslaqApi = Proxy

tslaqServer :: MonadIO m => ServerT TSLAQAPI (AppT m)
tslaqServer = userServer :<|> eventServer :<|> metricsServer

type MetricsAPI = "metrics" :> Get '[JSON] (HashMap Text Int64)

metricsApi :: Proxy MetricsAPI
metricsApi = Proxy

-- | The server that runs the MetricsAPI
metricsServer :: MonadIO m => ServerT MetricsAPI (AppT m)
metricsServer = waiMetrics

type EventAPI =
         "events" :> Get '[JSON] [Entity Event]
    :<|> "events" :> Capture "id" Int64 :> Get '[JSON] (Entity Event)
    :<|> "events" :> ReqBody '[JSON] Event :> Post '[JSON] Int64
    -- :<|> "events" :> Capture "id" Int64 :> ReqBody '[JSON] UserUpdate :> Put '[JSON] (Entity Event)

eventApi :: Proxy EventAPI
eventApi = Proxy

-- | The server that runs the EventAPI
eventServer :: MonadIO m => ServerT EventAPI (AppT m)
eventServer = listEvents :<|> getEvent :<|> createEvent
    -- :<|> updateEvent

-- | Returns all events in the database.
listEvents :: MonadIO m => AppT m [Entity Event]
listEvents = do
  increment "listEvents"
  logDebugNS "web" "listEvents"
  runDb (selectList [] [])

-- | Returns a event by id or throws a 404 error.
getEvent :: MonadIO m => Int64 -> AppT m (Entity Event)
getEvent i = do
  increment "getEvent"
  logDebugNS "web" "getEvent"
  maybeEvent <- runDb (getEntity (toSqlKey i :: Key Event))
  case maybeEvent of
    Nothing -> throwError err404
    Just e  -> return e

-- | Creates a event in the database.
createEvent :: MonadIO m => Event -> AppT m Int64
createEvent p = do
  increment "createEvent"
  logDebugNS "web" "creating a event"
  currentTime <- liftIO $ getCurrentTime
  newEvent    <- runDb
    ( insert
      ( Event currentTime
              currentTime
              (eventEventTime p)
              (eventTitle p)
              (eventBody p)
      )
    )
  return $ fromSqlKey newEvent

-- -- | Returns a event by id or throws a 404 error.
-- updateEvent :: MonadIO m => Int64 -> EventUpdate -> AppT m (Entity Event)
-- updateEvent i e = do
--   increment "updateEvent"
--   logDebugNS "web" "updateEvent"
--   let k = toSqlKey i
--   runDb (update k e)
--   updated <- runDb (getEntity k)
--   case updated of
--     Nothing -> throwError err404
--     Just ue -> return ue


type UserAPI =
         "users" :> Get '[JSON] [Entity User]
    :<|> "users" :> Capture "id" Int64 :> Get '[JSON] (Entity User)
    :<|> "users" :> ReqBody '[JSON] User :> Post '[JSON] Int64
    -- :<|> "events" :> Capture "id" Int64 :> ReqBody '[JSON] EventUpdate :> Put '[JSON] (Entity User)

userApi :: Proxy UserAPI
userApi = Proxy

-- | The server that runs the UserAPI
userServer :: MonadIO m => ServerT UserAPI (AppT m)
userServer = listUsers :<|> getUser :<|> createUser
    -- :<|> updateUser

-- | Returns all users in the database.
listUsers :: MonadIO m => AppT m [Entity User]
listUsers = do
  increment "listUsers"
  logDebugNS "web" "listUsers"
  runDb (selectList [] [])

-- | Returns a user by id or throws a 404 error.
getUser :: MonadIO m => Int64 -> AppT m (Entity User)
getUser i = do
  increment "getUser"
  logDebugNS "web" "getUser"
  maybeUser <- runDb (getEntity (toSqlKey i :: Key User))
  case maybeUser of
    Nothing -> throwError err404
    Just u  -> return u

-- | Creates a user in the database.
createUser :: MonadIO m => User -> AppT m Int64
createUser p = do
  increment "createUser"
  logDebugNS "web" "creating a user"
  currentTime <- liftIO $ getCurrentTime
  newUser     <- runDb
    ( insert
      ( User currentTime
             currentTime
             (userEmailAddress p)
             (userName p)
             (userPassword p)
      )
    )
  return $ fromSqlKey newUser

-- updateUser :: MonadIO m => Int64 -> UserUpdate -> AppT m (Entity User)
-- updateUser i e = do
--   increment "updateUser"
--   logDebugNS "web" "updateUser"
--   let k = toSqlKey i
--   runDb (update k e)
--   updated <- runDb (getEntity k)
--   case updated of
--     Nothing -> throwError err404
--     Just uu -> return uu


-- | Return wai metrics as JSON
waiMetrics :: MonadIO m => AppT m (HashMap Text Int64)
waiMetrics = do
  increment "metrics"
  logDebugNS "web" "metrics"
  metr <- Metrics.getMetrics
  liftIO $ mapM Counter.read =<< readIORef (metr ^. metricsCounters)

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
