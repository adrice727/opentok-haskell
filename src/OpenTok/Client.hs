{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}

module OpenTok.Client(
  Client(Client, _apiKey, _secret),
  ClientError(statusCode, message),
  Path,
  request
) where

import           Prelude                        ( )
import           Prelude.Compat
import           Control.Arrow                  ( left )
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Monad.Time
import           Control.Monad.Trans.Except
import           Crypto.JWT
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

request
  :: (ToJSON a, FromJSON b) => Client -> Path -> a -> IO (Either ClientError b)
request client p opts = do
  claims <- mkClaims (_apiKey client)
  let key = fromOctets (C8.pack $ _secret client)
  eitherJWT <- signJWT key claims
  case eitherJWT of
    Left  _         -> pure $ Left $ jwtError
    Right signedJWT -> do
      initialRequest <- parseRequest $ "https://api.opentok.com" <> p
      let req = initialRequest
            { method         = "POST"
            , requestBody    = RequestBodyLBS $ encode opts
            , requestHeaders = [ ("Content-Type", "application/json")
                               , ("Accept"      , "application/json")
                               , ( "X-OPENTOK-AUTH"
                                 , toStrict $ encodeCompact signedJWT
                                 )
                               ]
            }
      manager  <- newManager tlsManagerSettings
      response <- httpLbs req manager
      let body = responseBody response
      let sc   = getResponseStatusCode response
      case sc of
        200 -> pure $ left (\_ -> decodeError sc) (eitherDecode body)
        _   -> pure $ Left $ maybe (decodeError sc)
                                   (ClientError sc . _message)
                                   (decode body :: Maybe APIError)
