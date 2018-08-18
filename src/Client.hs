{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

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
import           Data.Aeson                     ( encode, eitherDecode, FromJSON)
import           Data.ByteString.Char8          ( pack )
import           Data.ByteString.Lazy           ( toStrict, ByteString )
import           Data.HashMap.Strict           as HM
import           Data.Time.Clock
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status      ( statusCode )
import           Session
import           Error
import           Util

data APIError = APIError {
  code :: Int,
  message :: String,
  description :: String
} deriving (Show, Generic)

errorMessage :: ByteString -> String
errorMessage b = do
  let attempt = eitherDecode b :: Either String APIError
  case attempt of
    Left _ -> ""
    Right e -> message e

instance FromJSON APIError

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
    &  claimIss .~ preview stringOrUri (projectKey :: String)
    &  claimIat ?~ NumericDate now'
    &  claimExp ?~ NumericDate later
    &  claimJti ?~ toText uuid
    &  unregisteredClaims %~ HM.insert "ist" "project"

signJWT :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
signJWT key claims = runExceptT $ signClaims key (newJWSHeader ((), HS256)) claims

decodeBody :: ByteString -> Either OTError [SessionProperties]
decodeBody b = do
  let attempt = eitherDecode b :: Either String [SessionProperties]
  left (\failure -> otError $ "Failed to decode create session response" <> failure) attempt

createSession :: Client -> SessionOptions -> IO (Either OTError SessionProperties)
createSession c opts = do
  claims <- mkClaims (_apiKey c)
  let key = fromOctets (pack $ _secret c)
  eitherJWT <- signJWT key claims
  case eitherJWT of
    Left  _         -> pure $ Left $ otError "Failed to create JSON Web Token"
    Right signedJWT -> do
      initialRequest <- parseRequest "https://api.opentok.com/session/create"
      manager        <- newManager tlsManagerSettings
      let request = initialRequest
            { method         = "POST"
            , requestBody    = RequestBodyLBS $ encode opts
            , requestHeaders = [ ("Accept", "application/json")
                               , ( "X-OPENTOK-AUTH" , toStrict $ encodeCompact signedJWT)
                               ]
            }
      response <- httpLbs request manager
      let body = responseBody response
      case statusCode $ responseStatus response of
        200 -> pure $ fmap head $ decodeBody body
        -- 200 -> pure $ fmap (\r -> r ^? element 1) $ decodeBody body   ^^^ avoid using head
        403 -> pure $ Left $ otError $ "An authentication error occurred: " <> errorMessage body
        500 -> pure $ Left $ otError $ "A server error occurred: " <> errorMessage body
        _ -> pure $ Left $ otError "An unrecognized error occurred."







