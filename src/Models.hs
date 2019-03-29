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

import           AppContext           (AppContext, ctxPool)
import           Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import           Data.Text            (Text)
import           Data.Time.Clock      (UTCTime)
import           Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import           Database.Persist.TH  (mkMigrate, mkPersist, persistLowerCase,
                                       share, sqlSettings)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  User json sql=users
    registration_time UTCTime default=CURRENT_TIMESTAMP
    email_address Text
    name Text
    password Text
    UniqueEmailAddress email_address
    deriving Show Read
  Event json sql=events
    create_time UTCTime default=CURRENT_TIMESTAMP
    event_time UTCTime default=CURRENT_TIMESTAMP
    title Text
    body Text
    UniqueTitle title
    deriving Show Read
|]

doMigrations :: SqlPersistT IO ()
doMigrations = runMigration migrateAll

runDb :: (MonadReader AppContext m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks ctxPool
  liftIO $ runSqlPool query pool
