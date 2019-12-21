{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Event where

import           AppContext                  (userHasRole, AppT (..))
import           Control.Monad.Except        (MonadIO, liftIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Data.Int                    (Int64)
import           Data.Text                   (pack)
import           Data.Time.Clock             (getCurrentTime)
import           Database.Persist.Postgresql (toSqlKey, fromSqlKey, insert)
import           Models                      (Key, User, Event(..), runDb)
import           Servant
import           Types                       (NewEvent (..), unhashId, AuthorizedUser (..), UserRole(..))

type EventAPI = "events" :> ReqBody '[JSON] NewEvent :> Post '[JSON] Int64
    -- :<|> "events" :> Capture "id" Int64 :> ReqBody '[JSON] UserUpdate :> Put '[JSON] (Entity Event)

eventApi :: Proxy EventAPI
eventApi = Proxy

-- | The server that runs the EventAPI
eventServer :: MonadIO m => AuthorizedUser -> ServerT EventAPI (AppT m)
eventServer u = createEvent u
    -- listEvents
    -- :<|> getEvent
    -- :<|> updateEvent

-- | Creates a event in the database.
createEvent :: MonadIO m => AuthorizedUser -> NewEvent -> AppT m Int64
createEvent u p = do
  userHasRole u Admin
  increment "createEvent"
  logDebugNS "web" ((pack $ show $ authUserId u) <> " creating an event")
  logDebugNS "web" ("new event: " <> (pack $ show $ p))
  currentTime <- liftIO $ getCurrentTime
  newEvent    <- runDb
    ( insert
      ( Event currentTime
              currentTime
              (time p)
              (title p)
              (body p)
              (toSqlKey (unhashId $ authUserId u) :: Key User)
      )
    )
  pure $ fromSqlKey newEvent

-- -- | Returns a event by id or throws a 404 error.
-- updateEvent :: MonadIO m => Int64 -> EventUpdate -> AppT m (Entity Event)
-- updateEvent i e = do
--   increment "updateEvent"
--   logDebugNS "web" "updateEvent"
--   let k = toSqlKey i
--   runDb (update k e)
--   updated <- runDb (getEntity k)
--   case updated of
--     Nothing -> throwError $ encodeJSONError (JSONError 404 "NoSuchEvent" "There is no such event.")
--     Just ue -> pure ue
