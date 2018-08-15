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
import           Data.UUID                      ( toText )
import           Data.UUID.V4
import           Data.Aeson                     ( encode, eitherDecode, FromJSON)
import qualified Data.ByteString.Char8         as C8
import           Data.ByteString.Lazy          as L8 ( toStrict, ByteString )
import           Data.Convertible               ( convert )
import           Data.HashMap.Strict           as HM
import           Data.Time.Clock
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status      ( statusCode )
import           System.Posix.Types             ( EpochTime )
import           Session
import           Error

data APIError = APIError {
  code :: Int,
  message :: String,
  description :: String
} deriving (Show, Generic)

errorMessage :: L8.ByteString -> String
errorMessage b = do
  let attempt = eitherDecode b :: Either String APIError
  case attempt of
    Left _ -> ""
    Right e -> message e

instance FromJSON APIError

apiBase :: String
apiBase = "https://api.opentok.com"

maybeToEither :: a -> Maybe b -> Either a b
maybeToEither e = maybe (Left e) (\x -> Right x)

utcToEpoch :: UTCTime -> EpochTime
utcToEpoch = convert

epochToUTC :: EpochTime -> UTCTime
epochToUTC = convert

expireTime :: UTCTime -> UTCTime
expireTime t = epochToUTC $ (utcToEpoch t) + 60

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
    &  claimIat .~ Just (NumericDate now')
    &  claimExp .~ Just (NumericDate later)
    &  claimJti .~ Just (toText uuid)
    &  unregisteredClaims %~ HM.insert "ist" "project"



signJWT :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
signJWT key claims = runExceptT $ do
  signClaims key (newJWSHeader ((), HS256)) claims

decodeBody :: L8.ByteString -> (Either OTError [SessionProperties])
decodeBody b = do
  let attempt = (eitherDecode b) :: Either String [SessionProperties]
  left (\failure -> otError $ "Failed to decode create session response" <> failure) attempt

createSession :: Client -> SessionOptions -> IO (Either OTError SessionProperties)
createSession c opts = do
  claims <- mkClaims (_apiKey c)
  let key = fromOctets (C8.pack $ _secret c)
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
      case (statusCode $ responseStatus response) of
        200 -> pure $ fmap head $ decodeBody body
        403 -> pure $ Left $ otError $ "An authentication error occurred: " <> (errorMessage body)
        500 -> pure $ Left $ otError $ "A server error occurred: " <> (errorMessage body)
        _ -> pure $ Left $ otError "An unrecognized error occurred."







