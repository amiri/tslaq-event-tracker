{-# LANGUAGE OverloadedStrings #-}

module Mail where

import           AppContext            (AppT (..))
import           Control.Monad.Except  (MonadIO, liftIO)
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Text             (Text, unpack)
import           Debug.Trace
import           Errors
import           Mail.Hailgun
import           Servant               (throwError)
import           Types

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
    let replyTo = "tslaq@tslaq-event-tracker.org"
    let context = HailgunContext (unpack domain) (unpack apiKey) Nothing
    let msg = hailgunMessage
          subject
          (TextOnly message)
          replyTo
          (emptyMessageRecipients { recipientsTo = [(pack $ unpack to)] })
          []
    -- traceM $ show msg
    case msg of
      Left err ->
        throwError $ encodeJSONError (JSONError 502 "EmailCreationError" err)
      Right msg' -> do
        result <- liftIO $ sendEmail context msg'
        -- traceM $ show result
        case result of
          Left err' ->
            throwError $ encodeJSONError
              (JSONError 502 "EmailSendError" (herMessage err'))
          Right _ -> pure ()
