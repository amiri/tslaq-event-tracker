{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}
module Lib
  ( app
    ) where

import           Control.Lens
import           Control.Monad              (void)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Aeson                 (FromJSON, ToJSON)
import           GHC.Generics               (Generic)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Types




-- startApp :: (MonadReader Types.AppContext m, MonadIO m) => m ()
-- startApp = do
--   env <- ask
--   return run 8080 app

nt :: AppContext -> AppM a -> Handler a
nt ctx x = runReaderT x ctx

app :: AppContext -> Application
app ctx = serve api $ hoistServer api (nt ctx) server

type API = "events" :> Get '[JSON] [Maybe Event]

api :: Proxy API
api = Proxy

server :: ServerT API AppM
server = undefined

events :: [Maybe Event]
events = [Nothing]
