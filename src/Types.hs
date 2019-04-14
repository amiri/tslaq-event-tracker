{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module Types where

import           Control.Monad.Except  (MonadIO, liftIO)
import           Crypto.BCrypt         (hashPasswordUsingPolicy,
                                        slowerBcryptHashingPolicy,
                                        validatePassword)
import           Data.Aeson
import           Data.ByteString       (ByteString)
import           Data.ByteString.Char8 (pack)
import           Data.Int              (Int64)
import           Data.Text             (Text)
import           Data.Text.Encoding    (encodeUtf8)
import           Database.Persist.Sql
import           GHC.Generics
import           Servant.Auth.Server   as SAS

data UserRole = Normal | Admin deriving (Show, Eq, Generic, Read)
instance ToJSON UserRole where
instance FromJSON UserRole where

data NewUser = NewUser {
    emailAddress :: UserEmail
  , name         :: UserName
  , password     :: Text
  } deriving (Show, Eq, Generic, Read)

instance ToJSON NewUser where

instance FromJSON NewUser where

data UserLogin = UserLogin {
    emailAddress :: UserEmail
  , password     :: Text
  } deriving (Show, Eq, Generic, Read)

data AuthorizedUser = AuthorizedUser {
    userName :: UserName
  , userId   :: Int64
  , role     :: UserRole
  } deriving (Show, Eq, Generic, Read)

instance ToJSON AuthorizedUser
instance FromJSON AuthorizedUser
instance SAS.ToJWT AuthorizedUser
instance SAS.FromJWT AuthorizedUser

instance ToJSON UserLogin where

instance FromJSON UserLogin where

newtype BCrypt = BCrypt { unBCrypt :: Text} deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype UserEmail = UserEmail Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype UserName = UserName Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype EventTitle = EventTitle Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype EventBody = EventBody Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)

hashPassword :: MonadIO m => Text -> m (Maybe ByteString)
hashPassword p =
  liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 p)

passwordValid :: String -> String -> Bool
passwordValid p h = validatePassword (pack h) (pack p)
