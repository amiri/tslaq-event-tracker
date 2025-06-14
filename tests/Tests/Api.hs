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
import           Crypto.JOSE.JWK             (JWK, fromRSA, genJWK, KeyMaterialGenParam(..))
import           Servant
import           Servant.Auth
import           Servant.Auth.Server                       (CookieSettings (..),
                                                            JWTSettings (..))
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
import Debug.Trace
import System.Environment

args :: Args
args = defaultArgs

getServer = do
  j <- testJWK
  -- ctx <- basicAuthCtx j
  ctx <- acquireAppContext
  pure (appToServer ctx)

basicAuthCtx :: JWK -> Context '[JWTSettings, CookieSettings]
basicAuthCtx j =
  (getJWTSettings j) :. (getCookieSettings Test) :. EmptyContext

tests :: TestTree
tests =
  localOption Success
    $ testGroup "Api"
    $ [unsafePerformIO (testSpec "servant-quickcheck" spec_servantQuickCheck)]

-- instance (HasGenRequest a) => HasGenRequest (Servant.Auth.Auth '[Servant.Auth.JWT, Servant.Auth.Cookie] Types.AuthorizedUser :> ProtectedAPI) where
--     genRequest _ = genRequest (Proxy :: Proxy a)

-- spec_servantQuickCheck :: SpecWith ()
spec_servantQuickCheck = do
  before testJWK $ do
    it "API demonstrates best practices" $ \(j) -> do
      pendingWith "No instance for (HasGenRequest (Auth '[JWT, Cookie] AuthorizedUser :> ProtectedAPI))"
      -- withServantServerAndContext tslaqApi (basicAuthCtx j) getServer $ \burl ->
      --   serverSatisfies
      --     tslaqApi
      --     burl
      --     args
      --     (   unauthorizedContainsWWWAuthenticate
      --     <%> not500
      --     <%> onlyJsonObjects
      --     <%> mempty
      --     )
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

