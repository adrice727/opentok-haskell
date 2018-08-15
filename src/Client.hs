{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Prelude                        ( )
import           Prelude.Compat          hiding ( exp )
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
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Convertible               ( convert )
import           Data.HashMap.Strict           as HM
import           Data.Time.Clock
import           GHC.Generics
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status      ( statusCode )
import           Session
import           System.Posix.Types             ( EpochTime )
-- import           OTError

data APIError = APIError {
  code :: Int,
  message :: String,
  description :: String
} deriving (Show, Generic)

errorMessage :: Response body -> String
errorMessage r = message $ reponseBody r

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
  apiKey :: String,
  secret :: String
}

data Request = CreateSession | StartArchive | StopArchive

mkClaims :: String -> IO ClaimsSet
mkClaims projectKey = do
  now  <- currentTime
  uuid <- nextRandom
  let now'  = epochToUTC $ utcToEpoch now
  let later = expireTime now
  pure
    $  emptyClaimsSet
    &  claimIss .~ preview stringOrUri (projectKey ++ "3" :: String)
    &  claimIat .~ Just (NumericDate now')
    &  claimExp .~ Just (NumericDate later)
    &  claimJti .~ Just (toText uuid)
    &  unregisteredClaims %~ HM.insert "ist" "project"

signJWT :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
signJWT key claims = runExceptT $ do
  signClaims key (newJWSHeader ((), HS256)) claims

decodeResponse :: Response body -> Either Error CreateSessionResponse
decodeResponse r = do
  let attempt = eitherDecode (responseBody r) :: Either Error CreateSessionResponse
  left (\decodeFailure -> errorWithoutStackTrace "Failed to decode create session response" <> decodeFailure) attempt


createSession :: Client -> SessionOptions -> IO (Either Error CreateSessionResponse)
createSession c opts = do
  claims <- mkClaims (Client.apiKey c)
  let key = fromOctets (C8.pack $ Client.secret c)
  eitherJWT <- signJWT key claims
  case eitherJWT of
    Left  _         -> pure $ Left $ errorWithoutStackTrace "Failed to create JSON Web Token"
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
      case (statusCode $ responseStatus response) of
        200 -> pure $ decodeResponse response
        403 -> pure $ Left $ errorWithoutStackTrace $ "An authentication error occurred: " <> (errorMessage response)
        500 -> pure $ Left $ errorWithoutStackTrace $ "A server error occurred: " <> (errorMessage response)







