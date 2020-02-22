{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Event where

import           Api.ReadEvent               (getEvent)
import           AppContext                  (AppT (..), userHasRole, checkAuthor)
import           Control.Lens
import           Control.Lens.Regex.Text
import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Data.Int                    (Int64)
import           Data.List.NonEmpty          (NonEmpty, toList)
import Data.Maybe (catMaybes)
import           Data.Text                   (Text(..), pack)
import           Data.Time.Clock             (getCurrentTime, UTCTime(..))
import           Database.Persist.Postgresql ((=.), Update(..), Entity (..), fromSqlKey, updateGet, getEntity, getBy,
                                              insert, insertEntity, toSqlKey)
import           Debug.Trace
import           Errors
import           Models                      (EntityField( EventBody, EventTitle, EventTime), Category (..), Event (..),
                                              EventCategory (..), Key,
                                              Unique (..), User, runDb)
import           Servant
import           Types                       (EventColumn(..), AuthorizedUser (..),
                                              CategoryName (..), EventDisplay,
                                              EventBody(..), EventTitle(..), NewEvent (..),
                                              UserRoleName (..), hashId, unhash,
                                              unhashId, EditedEvent(..))

type EventAPI
  = "events" :> ReqBody '[JSON] NewEvent :> Post '[JSON] EventDisplay :<|> "events" :> Capture "id" Text :> ReqBody '[JSON] EditedEvent :> Put '[JSON] EventDisplay
    -- :<|> "events" :> Capture "id" Int64 :> ReqBody '[JSON] UserUpdate :> Put '[JSON] (Entity Event)

eventApi :: Proxy EventAPI
eventApi = Proxy

-- | The server that runs the EventAPI
eventServer :: MonadIO m => AuthorizedUser -> ServerT EventAPI (AppT m)
eventServer u = createEvent u :<|> editEvent u

getIdFromCategory :: MonadIO m => Text -> AppT m Int64
getIdFromCategory c = do
  case has [regex|newcat-|] c of
    True -> do
      let matches = c ^.. [regex|newcat-(.*)|] . groups . ix 0
      case matches of
        [] -> throwError $ encodeJSONError
          (JSONError 400 "NoCategoryName" "You provided no category name.")
        (m : _) -> do
          traceM $ ("Making a new category " ++ (show m))
          let name = CategoryName m
          c' <- existingCategory name
          case c' of
            Just (Entity _ _) -> throwError $ encodeJSONError
              (JSONError 409
                         "CategoryConflict"
                         "There is already a category with that name."
              )
            Nothing -> do
              currentTime <- liftIO $ getCurrentTime
              traceM $ ("Category creation: " ++ (show $ CategoryName m))
              newCategory <- runDb
                (insert
                  (Category currentTime
                            currentTime
                            (CategoryName m)
                            Nothing
                            Nothing
                  )
                )
              pure $ fromSqlKey newCategory
    False -> do
      let cids = unhash c
      case cids of
        [] -> throwError $ encodeJSONError
          (JSONError 400
                     "InvalidCategoryId"
                     "You provided an invalid category id."
          )
        (cid : _) -> do
          traceM $ ("CID in non-empty branch: " ++ show cid)
          pure $ fromIntegral cid

findOrCreateCategories :: MonadIO m => [Text] -> AppT m [Int64]
findOrCreateCategories cs = mapM getIdFromCategory cs

createUpdate :: EventColumn a -> Maybe (Update Event)
createUpdate (BodyColumn b) = case b of
    Just t -> Just (Models.EventBody =. t)
    Nothing -> Nothing
createUpdate (TimeColumn tm) = case tm of
    Just (UTCTime d dt) -> Just (Models.EventTime =. UTCTime d dt)
    Nothing -> Nothing
createUpdate (TitleColumn t) = case t of
    Just t' -> Just (Models.EventTitle =. t')
    Nothing -> Nothing

editEvent :: MonadIO m => AuthorizedUser -> Text -> EditedEvent -> AppT m EventDisplay
editEvent u i e = do
  userHasRole u Contributor
  checkAuthor u e
  increment "editEvent"
  logDebugNS "web" ((pack $ show $ authUserId u) <> " editing event " <> (pack $ show i))
  traceM $ show e
  let EditedEvent { body = b, time = tm, title = t, categories = cs } = e
  traceM $ show tm
  let updates = catMaybes $ map createUpdate [BodyColumn b, TimeColumn tm, TitleColumn t]
  existing <- runDb (getEntity (toSqlKey (unhashId i) :: Key Event))
  case existing of
    Nothing -> throwError $ encodeJSONError (JSONError 404 "NoSuchEvent" "There is no such event.")
    Just evt -> do
      updated <- runDb (updateGet (entityKey evt) updates)  
      traceM $ show updated
      getEvent i

-- | Creates a event in the database.
createEvent :: MonadIO m => AuthorizedUser -> NewEvent -> AppT m EventDisplay
createEvent u (NewEvent b tm t cs) = do
  userHasRole u Contributor
  increment "createEvent"
  logDebugNS "web" ((pack $ show $ authUserId u) <> " creating an event")
  e <- existingEvent t
  case e of
    Just (Entity _ _) -> throwError $ encodeJSONError
      (JSONError 409
                 "EventConflict"
                 "There is already an event with that title."
      )
    Nothing -> do
      currentTime <- liftIO $ getCurrentTime
      newEvent    <- runDb
        (insertEntity
          (Event currentTime
                 currentTime
                 tm
                 t
                 b
                 (toSqlKey (unhashId $ authUserId u) :: Key User)
          )
        )
      let eventId = fromSqlKey . entityKey $ newEvent :: Int64
      traceM $ ("EventId: " ++ show eventId)
      cats <- findOrCreateCategories (toList cs)
      traceM $ ("Category input: " ++ show cs)
      traceM $ ("Categories: " ++ show cats)
      let hashedId = hashId $ fromIntegral eventId
      traceM $ ("HashedId: " ++ show hashedId)
      mapM_
        (\c' -> runDb $ insert $ EventCategory (toSqlKey eventId) (toSqlKey c'))
        cats
      getEvent hashedId

existingEvent :: MonadIO m => EventTitle -> AppT m (Maybe (Entity Event))
existingEvent t = do
  maybeEvent <- runDb (getBy $ UniqueTitle t)
  pure maybeEvent

existingCategory
  :: MonadIO m => CategoryName -> AppT m (Maybe (Entity Category))
existingCategory n = do
  maybeCategory <- runDb (getBy $ UniqueName n)
  pure maybeCategory
