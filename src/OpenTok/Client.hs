{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module OpenTok.Client where

import           Prelude                        ( )
import           Prelude.Compat
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
import    qualified       Data.ByteString.Char8  as C8        ( pack )
import           Data.ByteString.Lazy           ( toStrict
                                                , ByteString
                                                )
import           Data.HashMap.Strict           as HM
import           Data.Time.Clock
import           GHC.Generics                   ( Generic )
import           Network.HTTP.Client
import           Network.HTTP.Simple            ( httpJSONEither )

import           OpenTok.Types
import           OpenTok.Util

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

request :: (ToJSON a, FromJSON b) => Client -> Path -> a -> IO (Either OTError b)
request client p opts = do
  claims <- mkClaims (_apiKey client)
  let key = fromOctets (C8.pack $ _secret client)
  eitherJWT <- signJWT key claims
  case eitherJWT of
    Left  _         -> pure $ Left "Failed to create JSON Web Token"
    Right signedJWT -> do
      initialRequest <- parseRequest $ "https://api.opentok.com" <> p
      -- manager        <- newManager tlsManagerSettings
      let req = initialRequest
            { method         = "POST"
            , requestBody    = RequestBodyLBS $ encode opts
            , requestHeaders = [
                                 ("Content-Type", "application/json")
                               , ("Accept", "application/json")
                               , ("X-OPENTOK-AUTH" , toStrict $ encodeCompact signedJWT)
                               ]
            }
      response <- httpJSONEither req
      case responseBody response of
        Left ex -> do
          print ex
          -- decode response body as APIError ?
          pure $ Left $ "Some errorMessages"
        Right b -> pure $ Right b



      -- case statusCode (responseStatus response) of
      --   200 -> pure $ responseBody body
      --   _ -> pure $ left show (responseBody response)





        -- case responseBody response of
      --   Right b -> pure $ Right b
      --   Left ex -> do
      --     print $ responseBody ex
      --     pure $ Left $ "Some errorMessages"




      -- let body = responseBody response
      -- print body
      -- case statusCode $ responseStatus response of
      --   200 -> pure $ Right body
      --   403 -> pure $  Left $ "An authentication error occurred: " <> errorMessage body
      --   404 -> pure $  Left $ "Resource not found: " <> errorMessage body
      --   490 -> pure $  Left $ "Resource conflict: " <> errorMessage body
      --   500 -> pure $ Left $ "A server error occurred: " <> errorMessage body
      --   _   -> pure $ Left "An unrecognized error occurred."







