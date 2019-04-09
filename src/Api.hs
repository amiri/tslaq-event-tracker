{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import           Control.Monad.Reader (runReaderT)
import           Servant              (Server, serveWithContext)
import           Servant.Server

import           Api.TSLAQ            (TSLAQAPI, tslaqApi, tslaqServer)
import           AppContext           (AppContext (..), AppT (..))

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: AppContext -> Server TSLAQAPI
appToServer ctx = hoistServer tslaqApi (convertApp ctx) tslaqServer

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application.
convertApp :: AppContext -> AppT IO a -> Handler a
convertApp ctx appt = Handler $ runReaderT (runApp appt) ctx

-- | Finally, this function takes a configuration and runs our 'TSLAQAPI'
-- alongside the 'Raw' endpoint that serves all of our files.
app :: AppContext -> Application
app ctx = serveWithContext tslaqApi (ctxAuthConfig ctx) (appToServer ctx)
