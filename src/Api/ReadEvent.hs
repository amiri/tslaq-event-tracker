{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE FlexibleContexts #-}

module Api.ReadEvent where

import           AppContext                  (AppT (..), AppContext)
import           Control.Monad.Except        (MonadIO)
import           Control.Monad.Reader (MonadReader)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Data.Int                    (Int64)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (pack)
import           Data.Text.Lazy              (fromStrict)
import           Data.Text.Lazy.Encoding     (encodeUtf8)
import           Database.Esqueleto
import           Database.Persist.Postgresql (Entity (..), toSqlKey)
import           Models
import           Servant
import           Servant.Auth.Server         as SAS

type ReadEventAPI = "events" :> Get '[JSON] [(Event, [Category])]
    :<|> "events" :> Capture "id" Int64 :> Get '[JSON] (Event, [Category])

readEventApi :: Proxy ReadEventAPI
readEventApi = Proxy

readEventServer
  :: MonadIO m => CookieSettings -> JWTSettings -> ServerT ReadEventAPI (AppT m)
readEventServer _ _ = listEvents :<|> getEvent


keyValuesToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
keyValuesToMap = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))

eventsAndCategoriesToValues
  :: [(Entity Event, [Maybe (Entity Category)])] -> [(Event, [Category])]
eventsAndCategoriesToValues = map toValues
  where toValues (e, cs) = (entityVal e, map entityVal . catMaybes $ cs)

transformEventsAndCategories :: [(Entity Event, Maybe (Entity Category))] -> [(Event, [Category])]
transformEventsAndCategories = eventsAndCategoriesToValues . Map.toList . keyValuesToMap

getEvents :: (MonadReader AppContext m, MonadIO m) => Maybe Int64 -> m [(Entity Event, Maybe (Entity Category))]
getEvents (Just i) = do
  runDb $ select $ from $ \(e `LeftOuterJoin` ec `LeftOuterJoin` c) -> do
    on $ c ?. CategoryId ==. ec ?. EventCategoryCategoryId
    on $ just (e ^. EventId) ==. ec ?. EventCategoryEventId
    where_ (e ^. EventId ==. val (toSqlKey i))
    orderBy [asc (e ^. EventTime)]
    pure (e,c)
getEvents Nothing = do
  runDb $ select $ from $ \(e `LeftOuterJoin` ec `LeftOuterJoin` c) -> do
    on $ c ?. CategoryId ==. ec ?. EventCategoryCategoryId
    on $ just (e ^. EventId) ==. ec ?. EventCategoryEventId
    orderBy [asc (e ^. EventTime)]
    pure (e,c)

-- | Returns all events in the database.
listEvents :: MonadIO m => AppT m [(Event, [Category])]
listEvents = do
  increment "listEvents"
  logDebugNS "web" "listEvents"
  ecs <- getEvents Nothing
  let tes = transformEventsAndCategories ecs
  return tes

-- | Returns a event by id or throws a 404 error.
getEvent
  :: MonadIO m => Int64 -> AppT m (Event, [Category])
getEvent i = do
  increment "getEvent"
  logDebugNS "web" "getEvent"
  ecs <- getEvents (Just i)
  let te = transformEventsAndCategories ecs
  if length te > 0 then pure (head te) else throwError err404 { errBody = "Event " <> (encodeUtf8 $ fromStrict $ pack $ show i) <> " not found" }
