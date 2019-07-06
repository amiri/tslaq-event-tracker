{-# LANGUAGE OverloadedStrings #-}

module Main
  where

import qualified  Tests.Api
import           Test.Tasty

main :: IO ()
main = defaultMain $ testGroup "All Tests" [Tests.Api.tests]
