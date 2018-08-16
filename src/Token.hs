{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Token where

import Prelude ()
import Prelude.Compat
import Data.Time.Clock
import GHC.Generics
import           Data.ByteString.Char8          ( pack )
import qualified Data.ByteString.Base64 as B64

data Token = Token String

data Role = Subscriber | Publisher | Moderator
instance Show Role where
  show Subscriber = "subscriber"
  show Publisher = "publisher"
  show Moderator = "moderator"

data TokenOptions = TokenOptions {
  role :: Role,
  expireTime :: Maybe UTCTime,
  tokenData :: Maybe String,
  initialLayoutClassList :: Maybe [String]
} deriving (Show, Generic)

-- |Default token options
tokenOpts :: TokenOptions
tokenOpts = TokenOptions {
  role = Publisher,
  expireTime = Nothing,
  tokenData = Nothing,
  initialLayoutClassList = Nothing
}

sanitize :: String -> String
sanitize s = map (\c -> case () of
    _ | c == '-' -> '+'
      | c == '_' -> '\\'
      | otherwise -> c
  ) s

-- data SessionComponents = SessionComponents {
--   _apiKey :: APIKey,
--   _location :: String,
--   _create_time :: String,
-- }

-- decodeSessionId :: SessionId -> APIKey
-- decodeSessionId id =
--   let withOutSent = drop 2 id
--   let sanitized = sanitize withOutSent
--   let encoded = pack $ B64.encode sanitized



-- function decodeSessionId(sessionId) {
--   var fields;
--   // remove sentinal (e.g. '1_', '2_')
--   sessionId = sessionId.substring(2);
--   // replace invalid base64 chars
--   sessionId = sessionId.replace(/-/g, '+').replace(/_/g, '/');
--   // base64 decode
--   if (typeof Buffer.from === 'function') {
--     sessionId = Buffer.from(sessionId, 'base64').toString('ascii');
--   } else {
--     sessionId = new Buffer(sessionId, 'base64').toString('ascii');
--   }
--   // separate fields
--   fields = sessionId.split('~');
--   return {
--     apiKey: fields[1],
--     location: fields[2],
--     create_time: new Date(fields[3])
--   };
-- }


-- encodeToken :: APIKey -> APISecret -> TokenOptions -> Token
-- encodeToken key opts =
--   let
