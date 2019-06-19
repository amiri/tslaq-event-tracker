{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

module Errors where

import           Data.Aeson (ToJSON, encode)
import           Data.ByteString.Char8 (pack)
import           Data.CaseInsensitive  (mk)
import           GHC.Generics (Generic)
import qualified Servant as S

data JSONError = JSONError
    { statusCode :: Int
    , title :: String
    , detail :: String
    } deriving (Generic, Show)

instance ToJSON JSONError

encodeJSONError :: JSONError -> S.ServantErr
encodeJSONError jsonError = err {S.errBody = jsonBody, S.errHeaders = [jsonHeader]}
  where
    err        = getErrorFromCode $ statusCode jsonError
    jsonBody   = encode jsonError
    jsonHeader = ((mk $ pack "Content-Type"),
                  (pack "application/json;charset=utf-8"))

-- Non-exhaustive. Trivial to add more, just only adding the ones I
-- am currently using.
getErrorFromCode :: Int -> S.ServantErr
getErrorFromCode 401 = S.err401
getErrorFromCode 404 = S.err404
getErrorFromCode 409 = S.err409
