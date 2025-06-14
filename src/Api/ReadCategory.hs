{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeOperators         #-}

module Api.ReadCategory where

import           AppContext            (AppT (..), ctxPool)
import           Control.Monad.Except  (MonadIO)
import           Control.Monad.Logger  (logDebugNS)
import           Control.Monad.Metrics (increment)
import           Control.Monad.Reader  (ReaderT, asks, liftIO)
-- import           Data.Int                    (Int64)
import           Data.Text             (Text)
import           Database.Esqueleto
import           Errors
import           Models
import           Servant
import           Servant.Auth.Server   as SAS
import           Text.RawString.QQ
import           Types

type ReadCategoryAPI
  = "categories" :> Get '[JSON] [CategoryTree] :<|> "categories" :> Capture "id" Text :> Get '[JSON] CategoryTree :<|> "categories" :> "name" :> Capture "name" Text :> Get '[JSON] CategoryExists


readCategoryApi :: Proxy ReadCategoryAPI
readCategoryApi = Proxy

readCategoryServer
  :: MonadIO m
  => CookieSettings
  -> JWTSettings
  -> ServerT ReadCategoryAPI (AppT m)
readCategoryServer _ _ = getCategories :<|> getCategory :<|> categoryNameExists

categoriesWithParents :: Text
categoriesWithParents = [r|
WITH RECURSIVE categories AS (
  SELECT
    id,
    create_time,
    update_time,
    name,
    name AS full_name,
    details,
    parent_id,
    CAST(NULL AS varchar) AS parents
  FROM
    category
  WHERE
    parent_id IS NULL
  UNION ALL
  SELECT
    subcategories.id,
    subcategories.create_time,
    subcategories.update_time,
    subcategories.name,
    (parent_categories.full_name || ' > ' || subcategories.name) AS full_name,
    subcategories.details,
    subcategories.parent_id,
    CASE WHEN parent_categories.parents IS NULL THEN
      subcategories.parent_id::varchar
    ELSE
      (parent_categories.parents || ',' || subcategories.parent_id)
    END AS parents
  FROM
    category AS subcategories
    INNER JOIN categories AS parent_categories ON (subcategories.parent_id = parent_categories.id))
SELECT
 ??,
 ??,
 ??,
 ??,
 ??,
 ??,
 ??,
 ??
FROM
  categories
ORDER BY
  ARRAY_LENGTH(REGEXP_SPLIT_TO_ARRAY(parents, ','), 1) DESC
|]

getCategoriesWithParents :: MonadIO m => ReaderT SqlBackend m [CategoryTree]
getCategoriesWithParents = do
  cs <- rawSql categoriesWithParents []
  pure cs

getCategories :: MonadIO m => AppT m [CategoryTree]
getCategories = do
  increment "getCategories"
  logDebugNS "web" "getCategories"
  pool <- asks ctxPool
  liftIO $ runSqlPersistMPool getCategoriesWithParents pool

getCategory :: MonadIO m => Text -> AppT m CategoryTree
getCategory i = do
  increment "getCategory"
  logDebugNS "web" "getCategory"
  cats <- getCategories
  let cat = filter (\CategoryTree { id = tid } -> tid == i) cats
  case cat of
    [] -> throwError $ encodeJSONError
      (JSONError 404 "NoSuchCategory" "There is no such category.")
    cs -> pure $ head cs

categoryNameExists :: MonadIO m => Text -> AppT m CategoryExists
categoryNameExists n = do
  increment "categoryNameExists"
  logDebugNS "web" "categoryNameExists"
  maybeCategory <- runDb (getBy $ UniqueName (Types.CategoryName n))
  case maybeCategory of
    Nothing -> pure $ CategoryExists False
    Just _  -> pure $ CategoryExists True
