{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GADTs                      #-}
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
import           Data.List.NonEmpty    (NonEmpty)
import           Data.Text             (Text)
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)
import           Data.Time.Clock       (UTCTime)
import           Database.Persist.Sql
import           GHC.Generics
import           Servant.Auth.Server   as SAS
import qualified Web.Hashids           as Hash

data PriceUrl = PriceUrl {
  url :: Text
  } deriving (Show, Eq, Generic, Read)
instance ToJSON PriceUrl
instance FromJSON PriceUrl

data UserRole = Normal | Contributor | Admin deriving (Show, Eq, Generic, Read)
instance ToJSON UserRole
instance FromJSON UserRole

data EventDisplay = EventDisplay {
    body       :: !EventBody
  , createTime :: !UTCTime
  , id         :: !Text
  , time       :: !UTCTime
  , title      :: !EventTitle
  , updateTime :: !UTCTime
  , categories :: !(Maybe [CategoryDisplay])
  } deriving (Show, Eq, Generic, Read)
instance ToJSON EventDisplay
instance FromJSON EventDisplay

data CategoryDisplay = CategoryDisplay {
    name       :: !CategoryName
  , id         :: !Text
  , createTime :: !UTCTime
  , updateTime :: !UTCTime
  , details    :: !(Maybe CategoryDetails)
  } deriving (Show, Eq, Generic, Read)
instance ToJSON CategoryDisplay
instance FromJSON CategoryDisplay

data NewEvent = NewEvent {
    body :: !EventBody
  , time :: !UTCTime
  , title :: !EventTitle
  , categories :: !(NonEmpty Text)
  } deriving (Show, Eq, Generic, Read)
instance ToJSON NewEvent
instance FromJSON NewEvent

data UserRegistration = UserRegistration {
    emailAddress :: UserEmail
  , name         :: UserName
  , password     :: Text
  } deriving (Show, Eq, Generic, Read)
instance ToJSON UserRegistration
instance FromJSON UserRegistration

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
    authUserName :: !UserName
  , authUserId   :: !Text
  , authUserRole :: !UserRole
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

hashSalt :: ByteString
hashSalt = "firesale"

hashContext :: Hash.HashidsContext
hashContext = Hash.hashidsMinimum hashSalt 4

hashId :: Int64 -> Text
hashId = decodeUtf8 . Hash.encode hashContext . fromIntegral

unhashId :: Text -> Int64
unhashId = fromIntegral . head . Hash.decode hashContext . encodeUtf8
