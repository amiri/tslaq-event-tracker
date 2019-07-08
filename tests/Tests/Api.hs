{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Tests.Api
  ( tests
  )
where

import           Api
import           Api.TSLAQ
import           App                                       (acquireAppContext)
import           AppContext
import           Servant
import           Servant.Auth
import           Servant.Client
import           Servant.QuickCheck
import           Servant.QuickCheck.Internal               (HasGenRequest,
                                                            serverDoesntSatisfy)
import           Servant.QuickCheck.Internal.HasGenRequest
import           Servant.Server
import           System.IO.Unsafe                          (unsafePerformIO)
import           Test.Tasty                                (TestTree,
                                                            localOption,
                                                            testGroup)
import           Test.Tasty.Hspec
import           Types

args :: Args
args = defaultArgs

withCtx :: IO AppContext
withCtx = do
  appCtx <- acquireAppContext
  pure appCtx

getServer = do
  ctx <- withCtx
  pure (appToServer ctx)

tests :: TestTree
tests =
  localOption Success $ testGroup "Api"
    $ [unsafePerformIO (testSpec "servant-quickcheck" spec_servantQuickCheck)]

-- instance (HasGenRequest a) => HasGenRequest (Servant.Auth.Auth '[Servant.Auth.JWT, Servant.Auth.Cookie] Types.AuthorizedUser :> ProtectedAPI) where
--     genRequest _ = genRequest (Proxy :: Proxy a)

-- spec_servantQuickCheck :: SpecWith ()
spec_servantQuickCheck = do
  before withCtx $ do
    it "API demonstrates best practices" $ \(ctx) -> do
      pendingWith "Need instance for HasGenRequest"
  --     withServantServerAndContext tslaqApi (ctxAuthConfig ctx) getServer
  --       $ \burl -> serverSatisfies
  --           tslaqApi
  --           burl
  --           args
  --           (   unauthorizedContainsWWWAuthenticate
  --           <%> not500
  --           <%> onlyJsonObjects
  --           <%> mempty
  --           )
  --   it "API doesn't have these things implemented yet" $ \(ctx) -> do
  --     pendingWith "Need instance for HasGenRequest"
  --     withServantServerAndContext tslaqApi (ctxAuthConfig ctx) getServer
  --       $ \burl -> serverDoesntSatisfy
  --           tslaqApi
  --           burl
  --           args
  --           (   getsHaveCacheControlHeader
  --           <%> notAllowedContainsAllowHeader
  --           <%> mempty
  --           )

