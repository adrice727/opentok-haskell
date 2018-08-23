{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module OpenTok.Client
  ( Client(Client, _apiKey, _secret)
  , ClientError(statusCode, message)
  , Path
  , emptyOptions
  , post
  , postWithBody
  )
where

import           Prelude                        ( )
import           Prelude.Compat
import           Control.Arrow                  ( left )
import           Control.Lens.Combinators
import           Control.Lens.Operators
-- import           Control.Monad.Trans.Either     ( eitherT )
import           Control.Monad.Time
import           Control.Monad.Trans.Except
import           Crypto.JWT
import           Data.Either.Combinators        ( mapLeft )
import           Data.List                      ( isInfixOf )
import           Data.Semigroup                 ( (<>) )
import           Data.UUID                      ( toText )
import           Data.UUID.V4
import           Data.Aeson                     ( decode
                                                , encode
                                                , eitherDecode
                                                , FromJSON
                                                , ToJSON
                                                )
import           Data.Aeson.Types
import qualified Data.ByteString.Char8         as C8
                                                ( pack )
import           Data.ByteString.Lazy           ( toStrict )
import           Data.HashMap.Strict           as HM
import           Data.Time.Clock
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Client     hiding ( responseStatus )
import           Network.HTTP.Client.TLS        ( tlsManagerSettings )
import           Network.HTTP.Simple            ( getResponseStatusCode )
import           Network.HTTP.Types.Header      ( RequestHeaders )

import           OpenTok.Util

type Path = String

data APIError = APIError {
  _code :: Int,
  _status :: Maybe String,
  _message :: String
} deriving (Generic, Show)

instance FromJSON APIError where
  parseJSON = genericParseJSON $ defaultOptions { omitNothingFields = True, fieldLabelModifier = drop 1 }

data ClientError = ClientError {
  statusCode :: Int,
  message :: String
} deriving (Generic, Show)

jwtError :: ClientError
jwtError = ClientError 0 "Failed to create JWT"

decodeError :: Int -> ClientError
decodeError sc = ClientError sc "Failed to decode API response"

expireTime :: UTCTime -> UTCTime
expireTime t = epochToUTC $ utcToEpoch t + 60

data Client = Client {
  _apiKey :: String,
  _secret :: String
}

-- | Create claims for a JWT
mkClaims :: String -> IO ClaimsSet
mkClaims projectKey = do
  now  <- currentTime
  uuid <- nextRandom
  let now'  = epochToUTC $ utcToEpoch now
  let later = expireTime now
  pure
    $  emptyClaimsSet
    &  claimIss .~ preview stringOrUri (projectKey :: String)
    &  claimIat ?~ NumericDate now'
    &  claimExp ?~ NumericDate later
    &  claimJti ?~ toText uuid
    &  unregisteredClaims %~ HM.insert "ist" "project"

-- | Sign JWT claims
signJWT :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
signJWT key claims =
  runExceptT $ signClaims key (newJWSHeader ((), HS256)) claims

data EmptyOptions = EmptyOptions deriving (Generic)
instance ToJSON EmptyOptions

-- | A dummy type used when not passing any options to request
emptyOptions :: Maybe EmptyOptions
emptyOptions = Nothing

createJWT :: Client -> IO (Either ClientError SignedJWT)
createJWT client = do
  claims <- mkClaims (_apiKey client)
  let key = fromOctets (C8.pack $ _secret client)
  eitherSigned <- signJWT key claims
  pure $ mapLeft (const jwtError) eitherSigned

-- | Build headers for a Request
buildHeaders :: SignedJWT -> RequestHeaders
buildHeaders jwt =
  [
    ( "Content-Type", "application/json")
  , ("Accept"        , "application/json")
  , ("X-OPENTOK-AUTH", toStrict $ encodeCompact jwt)
  ]

-- | Build headers for a Request to an API v1 endpoint (without Content-Type)
buildV1Headers :: SignedJWT -> RequestHeaders
buildV1Headers jwt = drop 1 $ buildHeaders jwt


-- | Execute an API request
execute :: (FromJSON a) => Request -> IO (Either ClientError a)
execute req = do
  manager  <- newManager tlsManagerSettings
  response <- httpLbs req manager
  let body = responseBody response
  let sc   = getResponseStatusCode response
  case sc of
    200 -> pure $ left (\_ -> decodeError sc) (eitherDecode body)
    _   -> pure $ Left $ maybe (decodeError sc)
                               (ClientError sc . _message)
                               (decode body :: Maybe APIError)

buildRequest :: Path -> SignedJWT -> IO Request
buildRequest p jwt = do
  initialRequest <- parseRequest $ "https://api.opentok.com" <> p
  let buildFn = if "v2" `isInfixOf` p then buildHeaders else buildV1Headers
  pure $ initialRequest { method = "POST", requestHeaders = buildFn jwt }

-- | Make a POST request
post :: (FromJSON a) => Client -> Path -> IO (Either ClientError a)
post client p = do
  eitherJWT <- createJWT client
  case eitherJWT of
    Left e -> pure $ Left e
    Right jwt -> do
      request <- buildRequest p jwt
      execute request

-- | Make a POST request with a body
postWithBody :: (ToJSON a, FromJSON b) => Client -> Path -> a -> IO (Either ClientError b)
postWithBody client p b = do
  eitherJWT <- createJWT client
  case eitherJWT of
    Left e -> pure $ Left $ e
    Right jwt -> do
      request <- buildRequest p jwt
      execute $ request { requestBody = RequestBodyLBS $ encode b }

