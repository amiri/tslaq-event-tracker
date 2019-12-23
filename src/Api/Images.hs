{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Api.Images where

import           AppContext           (AppContext (..), AppT (..),
                                       S3Session (..), awsRegion, imageBucket)
import           Control.Lens         ((&), (.~))
import           Control.Monad.Except (MonadIO, liftIO)
import           Control.Monad.Reader (asks)
import           Data.Time.Clock      (getCurrentTime)
import           Network.AWS.Easy     (Session (..))
import           Network.AWS.Env      (Env (..))
import           Network.AWS.Presign
import           Servant
import           Types                (AuthorizedUser (..), ImageName (..),
                                       ImageUpload (..), PresignedUrl (..))
-- import Network.AWS.Presign (presignURL)
import           Data.Text.Encoding   (decodeUtf8)
import           Network.AWS.S3       (ObjectKey (..), poContentType, putObject)

type ImageAPI
  = "sign" :> ReqBody '[JSON] ImageUpload :> Post '[JSON] PresignedUrl

imageApi :: Proxy ImageAPI
imageApi = Proxy

-- | The server that runs the ImageAPI
imageServer :: MonadIO m => AuthorizedUser -> ServerT ImageAPI (AppT m)
imageServer u = getPresignedUrl u

getPresignedUrl
  :: MonadIO m => AuthorizedUser -> ImageUpload -> AppT m PresignedUrl
getPresignedUrl _ (ImageUpload (ImageName n) ct) = do
  (S3Session sess) <- asks ctxS3Session
  let Session { _sEnv = env } = sess
  let Env { _envAuth = a }    = env
  now <- liftIO $ getCurrentTime
  let req = putObject imageBucket (ObjectKey n) "" & poContentType .~ (Just ct)
  u <- presignURL a awsRegion now 60 req
  pure $ PresignedUrl (decodeUtf8 u)
