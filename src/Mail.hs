{-# LANGUAGE OverloadedStrings #-}

module Mail where

import           AppContext            (AppT (..), mailGunReplyTo)
import           Control.Lens                ((&), (^?), (?~))
import           Control.Monad.Except  (MonadIO, liftIO)
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Text             (Text, unpack)
-- import           Errors
-- import           Servant               (throwError)
import           Types
import Debug.Trace
import Network.Wreq

email
  :: MonadIO m
  => MailGunDomain
  -> MailGunKey
  -> Text
  -> ByteString
  -> UserEmail
  -> AppT m ()
email (MailGunDomain domain) (MailGunKey apiKey) subject message (UserEmail to)
  = do
    let replyTo = mailGunReplyTo
    let recipient = pack (unpack to)
    traceM $ ("Hailgun replyTo: " ++ (show replyTo))
    traceM $ ("Hailgun recipient: " ++ (show recipient))
    let opts = defaults & auth ?~ basicAuth "api" (pack $ unpack apiKey)
    res <- liftIO $ postWith opts (unpack domain) ["subject" := subject, "text" := message, "from" := replyTo, "to" := recipient]
    traceM $ show (res ^? responseBody)

    -- let context = HailgunContext (unpack domain) (unpack apiKey) Nothing
    -- let msg = hailgunMessage
    --       subject
    --       (TextOnly message)
    --       replyTo
    --       (emptyMessageRecipients { recipientsTo = [recipient] })
    --       []
    -- traceM $ ("Hailgun message: " ++ (show msg))
    -- case msg of
    --   Left err -> do
    --     traceM $ ("Hailgun creation error: " ++ (show err))
    --     throwError $ encodeJSONError (JSONError 502 "EmailCreationError" err)
    --   Right msg' -> do
    --     result <- liftIO $ sendEmail context msg'
    --     traceM $ ("Hailgun sending result: " ++ (show result))
    --     case result of
    --       Left err' -> do
    --         traceM $ ("Hailgun error: " ++ (show err'))
    --         traceM $ ("Hailgun error 2: " ++ (show (herMessage err')))
    --         throwError $ encodeJSONError
    --           (JSONError 502 "EmailSendError" (herMessage err'))
    --       Right _ -> pure ()
