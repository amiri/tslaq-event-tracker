{-# LANGUAGE OverloadedStrings #-}

module Mail where

import           AppContext            (AppT (..))
import           Control.Monad.Except  (MonadIO, liftIO)
import           Data.ByteString.Char8 (ByteString, pack)
import           Data.Text             (Text, unpack)
import           Errors
import           Mail.Hailgun
import           Servant               (throwError)
import           Types
import Debug.Trace

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
    let replyTo = "$TSLAQ Event Tracker <tslaq@tslaq-event-tracker.org>"
    let context = HailgunContext (unpack domain) (unpack apiKey) Nothing
    let msg = hailgunMessage
          subject
          (TextOnly message)
          replyTo
          (emptyMessageRecipients { recipientsTo = [(pack $ unpack to)] })
          []
    traceM $ ("Hailgun message: " ++ (show msg))
    case msg of
      Left err ->
        throwError $ encodeJSONError (JSONError 502 "EmailCreationError" err)
      Right msg' -> do
        result <- liftIO $ sendEmail context msg'
        traceM $ ("Hailgun sending result: " ++ (show result))
        case result of
          Left err' -> do
            traceM $ ("Hailgun error: " ++ (show err'))
            traceM $ ("Hailgun error 2: " ++ (show (herMessage err')))
            throwError $ encodeJSONError
              (JSONError 502 "EmailSendError" (herMessage err'))
          Right _ -> pure ()
