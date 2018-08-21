{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Token where

import           Prelude                        ( )
import           Prelude.Compat
import qualified Data.Digest.Pure.SHA as SHA
import qualified Codec.Binary.Base64.String    as B64
                                                ( encode
                                                , decode
                                                )
import           Control.Lens.Fold
import           Control.Lens.Combinators
import           Data.Maybe
import qualified Data.ByteString.Char8         as C8
                                                ( ByteString
                                                , pack
                                                , unpack
                                                )
import qualified Data.ByteString.Lazy.Char8    as L8
                                                ( pack,
                                                  fromStrict )
import           Data.Semigroup                 ( (<>) )
import           Data.Time.Clock
import           Data.UUID                      ( )
import           Data.UUID.V4                   ( nextRandom )
import qualified Data.Text                     as T
import           GHC.Generics
import           Network.HTTP.Types             ( renderQuery )
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

-- | Default token options
tokenOpts :: TokenOptions
tokenOpts = TokenOptions
  { role                   = Publisher
  , expireTime             = Nothing
  , connectionData         = Nothing
  , initialLayoutClassList = Nothing
  }

-- | Replace invalid base64 chars
sanitize :: String -> String
sanitize = map
  (\c -> case () of
    _ | c == '-'  -> '+'
      | c == '_'  -> '\\'
      | otherwise -> c
  )

-- | Extract an API key from a session Id and compare against provided key
validSessionId :: SessionId -> APIKey -> Bool
validSessionId sessionId key =
  let sanitized  = (sanitize . drop 2) sessionId
      components = (T.splitOn "~" . T.pack . B64.decode) sanitized
      maybeKey   = T.unpack <$> components ^? element 1
  in  maybeKey == Just key

-- | Validate token options
validExpireTime :: TokenOptions -> IO Bool
validExpireTime opts = case expireTime opts of
  Nothing     -> pure True
  Just expire -> do
    now <- getCurrentTime
    let maxExpire = addUTCTime (30 * 86400) now
    pure $ expire >= now && expire <= maxExpire

-- | Remove pairs with Nothing values before creating query string
cleanTokenOptions :: [(a, Maybe b)] -> [(a, Maybe b)]
cleanTokenOptions = filter (\(_, v) -> isJust v)

-- | Remove new-line characters from the final token
formatToken :: String -> String
formatToken = filter (/= '\n')

-- | Create a SHA1 encoded token
encodeToken :: APIKey -> APISecret -> SessionId -> TokenOptions -> IO Token
encodeToken key secret sessionId opts = do
  now   <- getCurrentTime
  nonce <- nextRandom
  let tokenSentinel = "T1=="
  let expire = maybe (utcToBS $ addUTCTime 86400 now) utcToBS (expireTime opts)
  let
    options =
      [ ("session_id"     , Just $ C8.pack sessionId)
      , ("create_time"    , Just $ utcToBS now)
      , ("expire_time"    , Just expire)
      , ("nonce"          , Just $ C8.pack $ show nonce)
      , ("role"           , Just $ C8.pack $ show $ role opts)
      , ("connection_data", C8.pack <$> connectionData opts)
      ] :: [(C8.ByteString, Maybe C8.ByteString)]
  let dataString = renderQuery False $ cleanTokenOptions options
  let sig  = SHA.showDigest $ SHA.hmacSha1 (L8.pack secret) (L8.fromStrict dataString)
  let decoded =
        "partner_id=" <> key <> "&sig=" <> sig <> ":" <> C8.unpack dataString :: String
  pure $ formatToken $ tokenSentinel <> B64.encode decoded

-- | Generate a new token for an OpenTok session using the provided token options
generate
  :: APIKey
  -> APISecret
  -> SessionId
  -> TokenOptions
  -> IO (Either OTError Token)
generate key secret sessionId opts = do
  let sessionIdValid = validSessionId sessionId key
  expirationValid <- validExpireTime opts
  case (sessionIdValid, expirationValid) of
    (False, _) -> pure $ Left $ error "Failed to validate provided SessionId"
    (_, False) -> pure $ Left $ error "Token expireTime must be between now and 30 days from now"
    (_, _) -> Right <$> encodeToken key secret sessionId opts
