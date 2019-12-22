{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api.ReadEvent where

import           AppContext                  (AppContext, AppT (..))
import           Control.Monad.Except        (MonadIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Control.Monad.Reader        (MonadReader)
-- import           Data.Int                    (Int64)
import qualified Data.Map.Strict             as Map
import           Data.Maybe                  (catMaybes)
import           Data.Text                   (Text)
import           Database.Esqueleto
import           Database.Persist.Postgresql (Entity (..), toSqlKey)
import           Errors
import           Models
import           Servant
import           Servant.Auth.Server         as SAS
import           Types

type ReadEventAPI = "events" :> Get '[JSON] [EventDisplay]
    :<|> "events" :> Capture "id" Text :> Get '[JSON] EventDisplay

readEventApi :: Proxy ReadEventAPI
readEventApi = Proxy

readEventServer
  :: MonadIO m => CookieSettings -> JWTSettings -> ServerT ReadEventAPI (AppT m)
readEventServer _ _ = listEvents :<|> getEvent


keyValuesToMap :: (Ord k) => [(k, a)] -> Map.Map k [a]
keyValuesToMap = Map.fromListWith (++) . map (\(k, v) -> (k, [v]))

eventsAndCategoriesToDisplay
  :: [(Entity Event, [Maybe (Entity Category)])] -> [EventDisplay]
eventsAndCategoriesToDisplay = map toEventDisplay

toEventDisplay :: (Entity Event, [Maybe (Entity Category)]) -> EventDisplay
toEventDisplay (e, cs) = EventDisplay
  { body       = (eventBody $ entityVal e)
  , createTime = (eventCreateTime $ entityVal e)
  , id         = hashId $ fromSqlKey (entityKey e)
  , time       = (eventTime $ entityVal e)
  , title      = (eventTitle $ entityVal e)
  , updateTime = (eventUpdateTime $ entityVal e)
  , categories = (flattenCategories (catMaybes cs))
  }

toCategoryDisplay :: Entity Category -> CategoryDisplay
toCategoryDisplay c = CategoryDisplay
  { name       = (categoryName $ entityVal c)
  , createTime = (categoryCreateTime $ entityVal c)
  , id         = hashId $ fromSqlKey (entityKey c)
  , details    = categoryDetails $ entityVal c
  , updateTime = (categoryUpdateTime $ entityVal c)
  }

flattenCategories :: [Entity Category] -> Maybe [CategoryDisplay]
flattenCategories cs =
  let tcs = map (\c -> toCategoryDisplay c) cs
  in  if length tcs > 0 then Just (tcs) else Nothing

transformEventsAndCategories
  :: [(Entity Event, Maybe (Entity Category))] -> [EventDisplay]
transformEventsAndCategories =
  eventsAndCategoriesToDisplay . Map.toList . keyValuesToMap

getEvents :: (MonadReader AppContext m, MonadIO m) => Maybe Text -> m [(Entity Event, Maybe (Entity Category))]
getEvents (Just i) = do
  runDb $ select $ from $ \(e `LeftOuterJoin` ec `LeftOuterJoin` c) -> do
    on $ c ?. CategoryId ==. ec ?. EventCategoryCategoryId
    on $ just (e ^. EventId) ==. ec ?. EventCategoryEventId
    where_ (e ^. EventId ==. val (toSqlKey $ unhashId i))
    orderBy [asc (e ^. EventTime)]
    pure (e,c)
getEvents Nothing = do
  runDb $ select $ from $ \(e `LeftOuterJoin` ec `LeftOuterJoin` c) -> do
    on $ c ?. CategoryId ==. ec ?. EventCategoryCategoryId
    on $ just (e ^. EventId) ==. ec ?. EventCategoryEventId
    orderBy [asc (e ^. EventTime)]
    pure (e,c)

-- | Returns all events in the database.
listEvents :: MonadIO m => AppT m [EventDisplay]
listEvents = do
  increment "listEvents"
  logDebugNS "web" "listEvents"
  ecs <- getEvents Nothing
  let tes = transformEventsAndCategories ecs
  pure tes

-- | Returns a event by id or throws a 404 error.
getEvent :: MonadIO m => Text -> AppT m EventDisplay
getEvent i = do
  increment "getEvent"
  logDebugNS "web" "getEvent"
  ecs <- getEvents (Just i)
  let te = transformEventsAndCategories ecs
  if length te > 0
    then pure (head te)
    else throwError $ encodeJSONError
      (JSONError 404 "EventNotFound" ("Event " ++ (show i) ++ " not found"))
