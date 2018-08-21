{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Prelude                        ( )
import           Prelude.Compat
-- import           Control.Arrow                  ( left )
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Monad.Time
import           Control.Monad.Trans.Except
import           Crypto.JWT
import           Data.Semigroup                 ( (<>) )
import           Data.UUID                      ( toText )
import           Data.UUID.V4
import           Data.Aeson                     ( encode
                                                , eitherDecode
                                                , FromJSON
                                                , ToJSON
                                                )
import           Data.ByteString.Char8          ( pack )
import           Data.ByteString.Lazy           ( toStrict
                                                , ByteString
                                                )
import           Data.HashMap.Strict           as HM
import           Data.Time.Clock
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status      ( statusCode )

import           Types
import           Util

type Path = String

data APIError = APIError {
  code :: Int,
  message :: String,
  description :: String
} deriving (Show, Generic)

instance FromJSON APIError

errorMessage :: ByteString -> String
errorMessage b = do
  let attempt = eitherDecode b :: Either String APIError
  case attempt of
    Left  _ -> ""
    Right e -> message e

apiBase :: String
apiBase = "https://api.opentok.com"

expireTime :: UTCTime -> UTCTime
expireTime t = epochToUTC $ utcToEpoch t + 60

data Client = Client {
  _apiKey :: String,
  _secret :: String
}

data RequestType = CreateSession | StartArchive | StopArchive

mkClaims :: String -> IO ClaimsSet
mkClaims projectKey = do
  now  <- currentTime
  uuid <- nextRandom
  let now'  = epochToUTC $ utcToEpoch now
  let later = expireTime now
  pure
    $  emptyClaimsSet
    &  claimIss
    .~ preview stringOrUri (projectKey :: String)
    &  claimIat
    ?~ NumericDate now'
    &  claimExp
    ?~ NumericDate later
    &  claimJti
    ?~ toText uuid
    &  unregisteredClaims
    %~ HM.insert "ist" "project"

signJWT :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
signJWT key claims =
  runExceptT $ signClaims key (newJWSHeader ((), HS256)) claims

request :: (ToJSON a) => Client -> Path -> a -> IO (Either OTError ByteString)
request client p opts = do
  claims <- mkClaims (_apiKey client)
  let key = fromOctets (pack $ _secret client)
  eitherJWT <- signJWT key claims
  case eitherJWT of
    Left  _         -> pure $ Left "Failed to create JSON Web Token"
    Right signedJWT -> do
      initialRequest <- parseRequest $ "https://api.opentok.com" <> p
      manager        <- newManager tlsManagerSettings
      let req = initialRequest
            { method         = "POST"
            , requestBody    = RequestBodyLBS $ encode opts
            , requestHeaders = [ ("Accept", "application/json")
                               , ( "X-OPENTOK-AUTH"
                                 , toStrict $ encodeCompact signedJWT
                                 )
                               ]
            }
      response <- httpLbs req manager
      let body = responseBody response
      case statusCode $ responseStatus response of
        200 -> pure $ Right body
        403 -> pure $  Left $  "An authentication error occurred: " <> errorMessage body
        500 -> pure $ Left $ "A server error occurred: " <> errorMessage body
        _   -> pure $ Left "An unrecognized error occurred."







