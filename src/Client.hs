{-# LANGUAGE OverloadedStrings #-}

module Client where

import           Prelude                        ( )
import           Prelude.Compat          hiding ( exp )
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Monad.Time
import           Control.Monad.Trans.Except
import           Data.UUID                      ( toText )
import           Data.UUID.V4
import           Data.Aeson                     ( encode, decode)
import qualified Data.ByteString.Char8         as C8
import           Data.ByteString.Lazy           ( toStrict )
import           Data.Convertible               ( convert )
import           Data.HashMap.Strict           as HM
import           Data.Time.Clock
import           System.Posix.Types             ( EpochTime )
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status      ( statusCode )
import           Crypto.JWT
import           Session
import           OTError

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
    &  claimIss .~ preview stringOrUri (projectKey :: String)
    &  claimIat .~ Just (NumericDate now')
    &  claimExp .~ Just (NumericDate later)
    &  claimJti .~ Just (toText uuid)
    &  unregisteredClaims %~ HM.insert "ist" "project"

signJWT :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
signJWT key claims = runExceptT $ do
  signClaims key (newJWSHeader ((), HS256)) claims

createSession :: Client -> SessionOptions -> IO (Either OTError CreateSessionResponse)
createSession c opts = do
  claims <- mkClaims (Client.apiKey c)
  let key = fromOctets (C8.pack $ Client.secret c)
  eitherJWT <- signJWT key claims
  case eitherJWT of
    Left  _         -> pure $ Left $ OTError "oh no"
    Right signedJWT -> do
      initialRequest <- parseRequest "https://api.opentok.com/session/create"
      manager        <- newManager tlsManagerSettings
      let request = initialRequest
            { method         = "POST"
            , requestBody    = RequestBodyLBS $ encode opts
            , requestHeaders = [ ("Accept", "application/json")
                               , ( "X-OPENTOK-AUTH"
                                 , toStrict $ encodeCompact signedJWT
                                 )
                               ]
            }
      response <- httpLbs request manager
      putStrLn $ "The status code was: " ++ show (statusCode $ responseStatus response)
      pure $ maybeToEither (OTError "oh no") (decode (responseBody response) :: Maybe CreateSessionResponse)






