{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import           Control.Monad.Reader (runReaderT)
import           Servant              (Proxy (..), Server, serveWithContext)
import           Servant.Auth.Server  as SAS
import           Servant.Server

import           Api.TSLAQ            (TSLAQAPI, tslaqApi, tslaqServer)
import           AppContext           (AppContext (..), AppT (..))

-- | This functions tells Servant how to run the 'App' monad with our
-- 'server' function.
appToServer :: AppContext -> Server (TSLAQAPI '[JWT, Cookie])
appToServer ctx = hoistServerWithContext
  tslaqApi
  (Proxy :: Proxy '[SAS.JWTSettings, SAS.CookieSettings])
  (convertApp ctx)
  (tslaqServer (ctxCookieSettings ctx) (ctxJWTSettings ctx))

-- | This function converts our @'AppT' m@ monad into the @ExceptT ServantErr
-- m@ monad that Servant's 'enter' function needs in order to run the
-- application.
convertApp :: AppContext -> AppT IO a -> Handler a
convertApp ctx appt = Handler $ runReaderT (runApp appt) ctx

-- | Finally, this function takes a configuration and runs our 'TSLAQAPI'
app :: AppContext -> Application
app ctx = serveWithContext tslaqApi (ctxAuthConfig ctx) (appToServer ctx)
