{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens              ((&), (.~))
import           Control.Monad.Reader
import           Control.Monad.Trans.AWS   (Credentials (..), Region (..))
import           Data.Maybe
import           Lib
import           Network.AWS.Easy          (AWSConfig, Endpoint (..), awsConfig,
                                            awscCredentials, connect)
import           Network.HostName          (getHostName)
import           System.Directory          (doesFileExist)
import           System.Log.Formatter      (simpleLogFormatter)
import           System.Log.Handler        (setFormatter)
import           System.Log.Handler.Simple (fileHandler)
import           System.Log.Logger         (Logger, Priority (..), addHandler,
                                            getLogger, removeAllHandlers,
                                            setLevel, updateGlobalLogger)
import           Types

appDir = "/var/local/tslaq-event-tracker/"

tslaqEventsLogger :: IO Logger
tslaqEventsLogger = do
  l <- getLogger "main"
  h <- fileHandler (appDir ++ "debug.log") DEBUG >>= \lh ->
    return $ setFormatter
      lh
      (simpleLogFormatter "[$time - $loggername - $prio] $msg")
  updateGlobalLogger "main" (setLevel DEBUG)
  updateGlobalLogger "main" (addHandler h)
  getLogger "main"

awsRegion :: Region
awsRegion = NorthVirginia

getCredentials :: Bool -> Credentials
getCredentials b = do
  case b of
    True  -> FromFile "tslaq-user" "/home/amiri/.aws/credentials"
    False -> Discover

getAWSConfig :: IO AWSConfig
getAWSConfig = do
  b <- doesFileExist "/home/amiri/.aws/credentials"
  let creds = getCredentials b
  let r     = AWSRegion awsRegion
  let c     = awsConfig r & awscCredentials .~ creds
  return c

main :: IO ()
main = do
  l              <- tslaqEventsLogger
  c              <- getAWSConfig
  secretsSession <- connect c secretsManagerService
  s3Session      <- connect c s3Service
  hostname       <- getHostName
  pgConnectInfo  <- case hostname of
    "tslaq-event-tracker" -> getPgConnectInfo "pgconnectinfo" secretsSession
    _                     -> return Nothing
  let pgConnectInfo' = fromMaybe defaultPgConnectInfo pgConnectInfo
  let ctx = AppContext
        { envLog         = l
        , secretsSession = secretsSession
        , s3Session      = s3Session
        , pgConnectInfo  = pgConnectInfo'
        }
  -- runReaderT startApp ctx
  removeAllHandlers
