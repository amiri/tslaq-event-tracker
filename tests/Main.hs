{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Tasty
import qualified Tests.Api

main :: IO ()
main = defaultMain $ testGroup "All Tests" [Tests.Api.tests]
