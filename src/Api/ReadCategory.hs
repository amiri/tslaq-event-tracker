{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Api.ReadCategory where

import           AppContext                  (AppT (..))
import           Control.Monad.Except        (MonadIO)
import           Control.Monad.Logger        (logDebugNS)
import           Control.Monad.Metrics       (increment)
-- import           Data.Int                    (Int64)
import           Api.ReadEvent               (toCategoryDisplay)
import           Data.Text                   (Text)
import           Database.Esqueleto
import           Database.Persist.Postgresql (selectList, toSqlKey)
import           Errors
import           Models
import           Servant
import           Servant.Auth.Server         as SAS
import           Types

type ReadCategoryAPI
  = "categories" :> Get '[JSON] [CategoryDisplay] :<|> "categories" :> Capture "id" Text :> Get '[JSON] CategoryDisplay

readCategoryApi :: Proxy ReadCategoryAPI
readCategoryApi = Proxy

readCategoryServer
  :: MonadIO m
  => CookieSettings
  -> JWTSettings
  -> ServerT ReadCategoryAPI (AppT m)
readCategoryServer _ _ = getCategories :<|> getCategory

getCategories :: MonadIO m => AppT m [CategoryDisplay]
getCategories = do
  increment "getCategories"
  logDebugNS "web" "getCategories"
  cs <- runDb (selectList [] [])
  pure $ map toCategoryDisplay cs

getCategory :: MonadIO m => Text -> AppT m CategoryDisplay
getCategory i = do
  increment "getCategory"
  logDebugNS "web" "getCategory"
  maybeCategory <- runDb (getEntity (toSqlKey (unhashId i) :: Key Category))
  case maybeCategory of
    Nothing -> throwError $ encodeJSONError
      (JSONError 404 "NoSuchCategory" "There is no such category.")
    Just c -> pure $ toCategoryDisplay c
