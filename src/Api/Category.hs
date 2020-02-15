{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}

module Api.Category where

import           Api.ReadCategory            (categoryNameExists, getCategories)
import           AppContext                  (AppT (..), userHasRole)
import           Control.Monad.Except        (MonadIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
import           Control.Monad.Reader        (liftIO)
import           Data.Text                   (Text, pack)
import           Data.Time.Clock             (getCurrentTime)
import           Database.Persist.Postgresql (Key (..), insert, toSqlKey)
import           Errors
import           Models                      (Category (..), runDb)
import           Servant
import           Types                       (AuthorizedUser (..),
                                              CategoryExists (..),
                                              CategoryName (..),
                                              CategoryTree (..),
                                              NewCategory (..),
                                              UserRoleName (..), unhashId)

type CategoryAPI
  = "categories" :> ReqBody '[JSON] NewCategory :> Post '[JSON] [CategoryTree]

categoryApi :: Proxy CategoryAPI
categoryApi = Proxy

categoryServer :: MonadIO m => AuthorizedUser -> ServerT CategoryAPI (AppT m)
categoryServer u = createCategory u

extractParentId :: Maybe Text -> Maybe (Key Category)
extractParentId (Just x) = case x of
  "" -> Nothing
  _  -> Just (toSqlKey (unhashId x) :: Key Category)
extractParentId Nothing = Nothing

createCategory
  :: MonadIO m => AuthorizedUser -> NewCategory -> AppT m [CategoryTree]
createCategory u (NewCategory (CategoryName n) p) = do
  userHasRole u Contributor
  increment "createCategory"
  logDebugNS "web" ((pack $ show $ authUserId u) <> " creating a category")
  (CategoryExists e) <- categoryNameExists n
  logDebugNS "web" ("category " <> n <> " exists: " <> (pack $ show e))
  logDebugNS "web" ("parentId for category " <> n <> ": " <> (pack $ show p))
  case e of
    False -> do
      let p' = extractParentId p
      currentTime <- liftIO $ getCurrentTime
      _           <- runDb
        (insert $ Category currentTime currentTime (CategoryName n) Nothing p')
      cs <- getCategories
      pure $ cs
    True -> throwError $ encodeJSONError
      (JSONError 409
                 "CategoryConflict"
                 "There is already a category with that name"
      )
