{-# LANGUAGE OverloadedStrings #-}

module Mail where

import           AppContext            (AppT (..), mailGunReplyTo)
import           Control.Lens          ((&), (?~))
import           Control.Monad.Except  (MonadIO, liftIO)
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Text             (Text, unpack)
import           Network.Wreq
import           Types

email
  :: MonadIO m
  => MailGunDomain
  -> MailGunKey
  -> Text
  -> ByteString
  -> ByteString
  -> UserEmail
  -> AppT m ()
email (MailGunDomain domain) (MailGunKey apiKey) subject textMessage htmlMessage (UserEmail to)
  = do
    let replyTo   = mailGunReplyTo
    let recipient = pack (unpack to)
    -- traceM $ ("Hailgun replyTo: " ++ (show replyTo))
    -- traceM $ ("Hailgun recipient: " ++ (show recipient))
    let opts = defaults & auth ?~ basicAuth "api" (pack $ unpack apiKey)
    _ <- liftIO $ postWith
      opts
      (unpack domain)
      [ partText "subject" subject
      , partBS "text" textMessage
      , partBS "html" htmlMessage
      , partBS "from" replyTo
      , partBS "to"   recipient
      ]
    -- traceM $ show (res ^? responseBody)
    pure ()
