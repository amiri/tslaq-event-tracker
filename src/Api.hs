{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api (app) where

import           Control.Monad.Reader (runReaderT)
import           Servant              ((:<|>) ((:<|>)), Proxy (Proxy), Raw,
                                       Server, serve, serveDirectoryFileServer)
import           Servant.Server

import           Api.TSLAQ             (TSLAQAPI, tslaqApi, tslaqServer)
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

-- | Since we also want to provide a minimal front end, we need to give
-- Servant a way to serve a directory with HTML and JavaScript. This
-- function creates a WAI application that just serves the files out of the
-- given directory.
files :: Server Raw
files = serveDirectoryFileServer "assets"

-- | Just like a normal API type, we can use the ':<|>' combinator to unify
-- two different APIs and applications. This is a powerful tool for code
-- reuse and abstraction! We need to put the 'Raw' endpoint last, since it
-- always succeeds.
type AppAPI = TSLAQAPI :<|> Raw

appApi :: Proxy AppAPI
appApi = Proxy

-- | Finally, this function takes a configuration and runs our 'TSLAQAPI'
-- alongside the 'Raw' endpoint that serves all of our files.
app :: AppContext -> Application
app ctx = serve appApi (appToServer ctx :<|> files)
