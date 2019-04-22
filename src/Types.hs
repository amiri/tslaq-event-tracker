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
import           Data.Time.Clock       (UTCTime)
import           Database.Persist.Sql
import           GHC.Generics
import           Servant.Auth.Server   as SAS



data UserRole = Normal | Admin deriving (Show, Eq, Generic, Read)
instance ToJSON UserRole where
instance FromJSON UserRole where

data EventDisplay = EventDisplay {
    body       :: EventBody
  , createTime :: UTCTime
  , eventId    :: Int64
  , time       :: UTCTime
  , title      :: EventTitle
  , updateTime :: UTCTime
  , categories :: Maybe [CategoryDisplay]
  } deriving (Show, Eq, Generic, Read)

data CategoryDisplay = CategoryDisplay {
    name    :: CategoryName
  , details :: Maybe CategoryDetails
  } deriving (Show, Eq, Generic, Read)

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
    authUserName :: UserName
  , authUserId   :: Int64
  , authUserRole :: UserRole
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
newtype CategoryName = CategoryName Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype CategoryDetails = CategoryDetails Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype EventBody = EventBody Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)

hashPassword :: MonadIO m => Text -> m (Maybe ByteString)
hashPassword p =
  liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 p)

passwordValid :: String -> String -> Bool
passwordValid p h = validatePassword (pack h) (pack p)
