{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
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
import           Data.Text             (Text, splitOn, unpack)
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

data UserRoleName = Normal | Contributor | Admin deriving (Show, Eq, Generic, Read)
instance ToJSON UserRoleName
instance FromJSON UserRoleName

data ImageUpload = ImageUpload {
    name        :: !ImageName
  , contentType :: !Text
} deriving (Show, Eq, Generic, Read)
instance ToJSON ImageUpload
instance FromJSON ImageUpload

data PresignedUrl = PresignedUrl {
    url :: !Text
} deriving (Show, Eq, Generic, Read)
instance ToJSON PresignedUrl
instance FromJSON PresignedUrl

data EventProcess = EventProcess {
    body       :: !EventBody
  , createTime :: !UTCTime
  , id         :: !Text
  , time       :: !UTCTime
  , title      :: !EventTitle
  , updateTime :: !UTCTime
  , author     :: !UserName
  , authorId   :: !Text
} deriving (Show, Eq, Generic, Read)
instance ToJSON EventProcess
instance FromJSON EventProcess

instance Ord EventProcess where
  compare (EventProcess _ _ _ t1 _ _ _ _) (EventProcess _ _ _ t2 _ _ _ _) =
    t1 `compare` t2

data EventDisplay = EventDisplay {
    body       :: !EventBody
  , createTime :: !UTCTime
  , id         :: !Text
  , time       :: !UTCTime
  , title      :: !EventTitle
  , updateTime :: !UTCTime
  , author     :: !UserName
  , authorId   :: !Text
  , categories :: !(Maybe [CategoryTree])
  } deriving (Show, Eq, Generic, Read)
instance ToJSON EventDisplay
instance FromJSON EventDisplay

data CategoryTree = CategoryTree {
    id         :: !Text
  , createTime :: !UTCTime
  , updateTime :: !UTCTime
  , name       :: !Text
  , fullName   :: !Text
  , details    :: !(Maybe CategoryDetails)
  , parentId   :: !(Maybe Text)
  , parents    :: !(Maybe [Text])
  } deriving (Show, Eq, Generic, Read)
instance ToJSON CategoryTree
instance FromJSON CategoryTree


instance RawSql CategoryTree where
  rawSqlCols _ _ =
    ( 8
    , [ "id"
      , "create_time"
      , "update_time"
      , "name"
      , "full_name"
      , "details"
      , "parent_id"
      , "parents"
      ]
    )
  rawSqlColCountReason _ =
    "A category with tree information contains 8 columns"
  rawSqlProcessRow xs =
    let PersistInt64   i  = xs !! 0
        PersistUTCTime ct = xs !! 1
        PersistUTCTime ut = xs !! 2
        PersistText    n  = xs !! 3
        PersistText    fn = xs !! 4
        d                 = case xs !! 5 of
          PersistText d' -> Just (CategoryDetails d')
          _              -> Nothing
        p = case xs !! 6 of
          PersistInt64 p' -> Just (hashId p')
          _               -> Nothing
        ps = case xs !! 7 of
          PersistText ps' -> Just (explodeParentIds ps')
          _               -> Nothing
    in  Right
          (CategoryTree { id         = hashId i
                        , createTime = ct
                        , updateTime = ut
                        , name       = n
                        , fullName   = fn
                        , details    = d
                        , parentId   = p
                        , parents    = ps
                        }
          )

explodeParentIds :: Text -> [Text]
explodeParentIds ids =
  fmap (hashId . (read :: String -> Int64) . unpack) $ splitOn "," ids

data CategoryDisplay = CategoryDisplay {
    name       :: !CategoryName
  , id         :: !Text
  , createTime :: !UTCTime
  , updateTime :: !UTCTime
  , details    :: !(Maybe CategoryDetails)
  } deriving (Show, Eq, Generic, Read)
instance ToJSON CategoryDisplay
instance FromJSON CategoryDisplay

data EditedEvent = EditedEvent {
    authorId   :: !Text
  , body       :: !(Maybe EventBody)
  , time       :: !(Maybe UTCTime)
  , title      :: !(Maybe EventTitle)
  , categories :: !(Maybe (NonEmpty Text))
  } deriving (Show, Eq, Generic, Read)
instance ToJSON EditedEvent
instance FromJSON EditedEvent

data NewEvent = NewEvent {
    body       :: !EventBody
  , time       :: !UTCTime
  , title      :: !EventTitle
  , categories :: !(NonEmpty Text)
  } deriving (Show, Eq, Generic, Read)
instance ToJSON NewEvent
instance FromJSON NewEvent

data NewCategory = NewCategory {
    name     :: !CategoryName
  , parentId :: !(Maybe Text)
  } deriving (Show, Eq, Generic, Read)
instance ToJSON NewCategory
instance FromJSON NewCategory

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
    authUserName  :: !UserName
  , authUserId    :: !Text
  , authUserRoles :: ![UserRoleName]
  } deriving (Show, Eq, Generic, Read)
instance ToJSON AuthorizedUser
instance FromJSON AuthorizedUser
instance SAS.ToJWT AuthorizedUser
instance SAS.FromJWT AuthorizedUser

data CategoryExists = CategoryExists {
  exists :: !Bool
  } deriving (Show, Eq, Generic, Read)
instance ToJSON CategoryExists
instance FromJSON CategoryExists

newtype BCrypt = BCrypt { unBCrypt :: Text} deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype UserEmail = UserEmail Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype UserName = UserName Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype EventTitle = EventTitle Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype CategoryId = CategoryId Int64 deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype CategoryName = CategoryName Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype CategoryDetails = CategoryDetails Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype EventBody = EventBody Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype ImageName = ImageName Text deriving (Eq, FromJSON, ToJSON, Show, Read)
newtype RoleId = RoleId Int64 deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)
newtype RoleName = RoleName Text deriving (Eq, PersistField, PersistFieldSql, FromJSON, ToJSON, Show, Read)

hashPassword :: MonadIO m => Text -> m (Maybe ByteString)
hashPassword p =
  liftIO $ hashPasswordUsingPolicy slowerBcryptHashingPolicy (encodeUtf8 p)

passwordValid :: String -> String -> Bool
passwordValid p h = validatePassword (pack h) (pack p)

hashSalt :: ByteString
hashSalt = "firesale"

hashContext :: Hash.HashidsContext
hashContext = Hash.hashidsMinimum hashSalt 4

hashEncode' :: Int -> ByteString
hashEncode' = Hash.encode hashContext

hashDecode' :: ByteString -> [Int]
hashDecode' = Hash.decode hashContext

hashId :: Int64 -> Text
hashId = decodeUtf8 . hashEncode' . fromIntegral

unhash :: Text -> [Int]
unhash = hashDecode' . encodeUtf8

unhashId :: Text -> Int64
unhashId = fromIntegral . head . hashDecode' . encodeUtf8
