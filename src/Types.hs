{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GADTs #-}
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

data PriceUrl = PriceUrl {
  url :: Text
  } deriving (Show, Eq, Generic, Read)
instance ToJSON PriceUrl
instance FromJSON PriceUrl

data UserRole = Normal | Admin deriving (Show, Eq, Generic, Read)
instance ToJSON UserRole
instance FromJSON UserRole

data EventDisplay = EventDisplay {
    body       :: !EventBody
  , createTime :: !UTCTime
  , id         :: !Int64
  , time       :: !UTCTime
  , title      :: !EventTitle
  , updateTime :: !UTCTime
  , categories :: !(Maybe [CategoryDisplay])
  } deriving (Show, Eq, Generic, Read)
instance ToJSON EventDisplay
instance FromJSON EventDisplay

data CategoryDisplay = CategoryDisplay {
    name       :: CategoryName
  , id         :: Int64
  , createTime :: UTCTime
  , updateTime :: UTCTime
  , details    :: Maybe CategoryDetails
  } deriving (Show, Eq, Generic, Read)
instance ToJSON CategoryDisplay
instance FromJSON CategoryDisplay

data NewUser = NewUser {
    emailAddress :: UserEmail
  , name         :: UserName
  , password     :: Text
  } deriving (Show, Eq, Generic, Read)
instance ToJSON NewUser
instance FromJSON NewUser

data UserLogin = UserLogin {
    emailAddress :: UserEmail
  , password     :: Text
  } deriving (Show, Eq, Generic, Read)
instance ToJSON UserLogin
instance FromJSON UserLogin

data AuthorizedUser = AuthorizedUser {
    authUserName :: UserName
  , authUserId   :: Int64
  , authUserRole :: UserRole
  } deriving (Show, Eq, Generic, Read)
instance ToJSON AuthorizedUser
instance FromJSON AuthorizedUser
instance SAS.ToJWT AuthorizedUser
instance SAS.FromJWT AuthorizedUser

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
