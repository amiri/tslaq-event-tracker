{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.ReadEvent where

import           AppContext                  (AppT (..))
import           Control.Monad.Except        (MonadIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), getEntity,
                                              selectList, toSqlKey)
import           Models                      (Event, Key, runDb)
import           Servant
import           Servant.Auth.Server         as SAS

type ReadEventAPI = "events" :> Get '[JSON] [Entity Event]
    :<|> "events" :> Capture "id" Int64 :> Get '[JSON] (Entity Event)

readEventApi :: Proxy ReadEventAPI
readEventApi = Proxy

readEventServer
  :: MonadIO m => CookieSettings -> JWTSettings -> ServerT ReadEventAPI (AppT m)
readEventServer _ _ = listEvents :<|> getEvent

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
    Just e  -> pure e
