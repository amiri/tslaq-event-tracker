{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
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

module Models where

import           AppContext           (AppT (..), ctxPool)
import           Control.Monad.Reader (MonadIO, asks, liftIO)
import           Data.Ord             (comparing)
import           Data.Time.Clock      (UTCTime, getCurrentTime)
import           Database.Persist.Sql (SqlPersistT, insertBy, runMigration,
                                       runSqlPool)
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)
import           Types                (BCrypt, CategoryDetails, CategoryName,
                                       EventBody, EventTitle, RoleName,
                                       UserEmail, UserName)
import qualified Types                as TS (RoleName (..))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User json sql=users
    createTime UTCTime sql=create_time sqltype=timestamptz default=CURRENT_TIMESTAMP
    updateTime UTCTime sql=update_time sqltype=timestamptz default=CURRENT_TIMESTAMP
    emailAddress UserEmail sql=email_address sqltype=text
    name UserName sql=user_name sqltype=text
    password BCrypt sqltype=bytea
    UniqueEmailAddress emailAddress
    deriving Eq Show Read
  Event json sql=events
    createTime UTCTime sql=create_time sqltype=timestamptz default=CURRENT_TIMESTAMP
    updateTime UTCTime sql=update_time sqltype=timestamptz default=CURRENT_TIMESTAMP
    time UTCTime sql=event_time sqltype=timestamptz default=CURRENT_TIMESTAMP
    title EventTitle sql=event_title sqltype=text
    body EventBody sql=event_body sqltype=jsonb
    authorId UserId
    UniqueTitle title
    deriving Eq Show Read
  Category json sql=category
    createTime UTCTime sql=create_time sqltype=timestamptz default=CURRENT_TIMESTAMP
    updateTime UTCTime sql=update_time sqltype=timestamptz default=CURRENT_TIMESTAMP
    name CategoryName
    details CategoryDetails Maybe
    parentId CategoryId Maybe
    UniqueName name
    deriving Eq Show Read
  EventCategory json sql=event_category
    eventId EventId sql=event_id
    categoryId CategoryId sql=category_id
  Role json sql=role
    createTime UTCTime sql=create_time sqltype=timestamptz default=CURRENT_TIMESTAMP
    updateTime UTCTime sql=update_time sqltype=timestamptz default=CURRENT_TIMESTAMP
    name RoleName
    UniqueRole name
    deriving Eq Show Read
  UserRole json sql=user_role
    userId UserId sql=user_id
    roleId RoleId sql=role_id
|]

instance Ord Event where
  compare = comparing eventTime

doMigrations :: SqlPersistT IO ()
doMigrations = do
  runMigration migrateAll
  currentTime <- liftIO $ getCurrentTime
  _           <- insertBy $ Role currentTime currentTime (TS.RoleName "Admin")
  _           <- insertBy $ Role currentTime currentTime (TS.RoleName "Contributor")
  _           <- insertBy $ Role currentTime currentTime (TS.RoleName "Normal")
  pure ()

runDb :: MonadIO m => SqlPersistT IO b -> AppT m b
runDb query = do
  pool <- asks ctxPool
  liftIO $ runSqlPool query pool
