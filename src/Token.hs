{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Token where

import           Prelude                        ( )
import           Prelude.Compat
import qualified Codec.Binary.Base64.String    as B64 ( decode )
import           Control.Lens.Fold
import           Control.Lens.Combinators
import qualified Data.ByteString.Char8      as C8 ( ByteString, pack , unpack )
import qualified Data.ByteString.Lazy.Char8 as L8 ( pack, fromStrict )
import           Data.Digest.Pure.SHA
import           Data.Semigroup                 ( (<>) )
import           Data.Time.Clock
import           Data.UUID                      ( )
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.Text                     as T
import           GHC.Generics
import           Network.HTTP.Types             ( renderQuery )
-- import qualified Data.String.UTF8 as UTF8 ( fromString )
import           Error
import           Util
import           Types


data Role = Subscriber | Publisher | Moderator
instance Show Role where
  show Subscriber = "subscriber"
  show Publisher = "publisher"
  show Moderator = "moderator"

data TokenOptions = TokenOptions {
  role :: Role,
  expireTime :: Maybe UTCTime,
  connectionData :: Maybe String,
  initialLayoutClassList :: Maybe [String]
} deriving (Show, Generic)

-- |Default token options
tokenOpts :: TokenOptions
tokenOpts = TokenOptions
  { role                   = Publisher
  , expireTime             = Nothing
  , connectionData         = Nothing
  , initialLayoutClassList = Nothing
  }

-- |Replace invalid base64 chars
sanitize :: String -> String
sanitize = map
  (\c -> case () of
    _ | c == '-'  -> '+'
      | c == '_'  -> '\\'
      | otherwise -> c
  )

-- data SessionComponents = SessionComponents {
--   _apiKey :: APIKey,
--   _location :: String,
--   _create_time :: String,
-- }

-- |Extract an API key from a session Id and compare against provided key
validSessionId :: SessionId -> APIKey -> Bool
validSessionId sessionId key =
  let sanitized  = (sanitize . drop 2) sessionId
      components = (T.splitOn "~" . T.pack . B64.decode) sanitized
      maybeKey   = fmap T.unpack $ components ^? element 1
  in  maybe False (== key) maybeKey

-- |Validate token options
validExpireTime :: TokenOptions -> IO Bool
validExpireTime opts = case expireTime opts of
  Nothing     -> pure True
  Just expire -> do
    now <- getCurrentTime
    let maxExpire = addUTCTime (30 * 86400) now
    pure $ expire >= now && expire <= maxExpire





-- function validSessionId(sessionId) {
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


encodeToken :: APIKey -> APISecret -> TokenOptions -> IO Token
encodeToken key secret opts = do
  now   <- getCurrentTime
  nonce <- nextRandom
  let tokenSentinel = "T1=="
  let expire = maybe (utcToBS $ addUTCTime 86400 now) utcToBS (expireTime opts)
  let
    options =
      [ ("create_time"    , Just $ utcToBS now)
      , ("expire_time"    , Just expire)
      , ("nonce"          , Just $ C8.pack $ show nonce)
      , ("role"           , Just $ C8.pack $ show $ role opts)
      , ("connection_data", fmap C8.pack $ connectionData opts)
      ] :: [(C8.ByteString, Maybe C8.ByteString)]
  let query = renderQuery False options
  let signed = hmacSha256 (L8.pack secret) (L8.fromStrict query)
  let decoded = "partner_id=" <> key <> "&sig=" <> showDigest signed <> ":" <> C8.unpack query :: String
  print $ showDigest signed
  pure $ tokenSentinel ++ B64.decode decoded

-- // Prevent mutating value passed in
--   tokenData = _.clone(tokenData);

--   _.defaults(tokenData, {
--     create_time: Math.round(timestamp.now()),
--     expire_time: Math.round(timestamp.now('1d')),
--     nonce: nonce(),
--     role: 'publisher'
--   });

--   var dataString = querystring.stringify(tokenData),
--       sig = signString(dataString, apiSecret),
--       decoded = new Buffer("partner_id="+apiKey+"&sig="+sig+":"+dataString, 'utf8');
--   return TOKEN_SENTINEL + decoded.toString('base64');

createToken
  :: APIKey
  -> APISecret
  -> SessionId
  -> TokenOptions
  -> IO (Either OTError Token)
createToken key secret sessionId opts = do
  let sessionIdValid = validSessionId sessionId key
  expirationValid <- validExpireTime opts
  case (sessionIdValid, expirationValid) of
    (False, _) -> pure $ Left $ error "Failed to validate provided SessionId"
    (_, False) -> pure $ Left $ error
      "Token expireTime must be between now and 30 days from now"
    (_, _) -> fmap Right (encodeToken key secret opts)


