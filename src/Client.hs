{-# LANGUAGE OverloadedStrings #-}

module Client where

import            Prelude ()
import            Prelude.Compat hiding (exp)
import            Control.Lens.Combinators
import            Control.Lens.Operators
import            Control.Monad.Time
import            Control.Monad.Trans.Except
import            Data.Aeson                 (encode, object, (.=))
import            Data.Aeson.Types
import qualified  Data.ByteString.Lazy.Char8 as L8
import qualified  Data.ByteString.Char8 as C8
import            Data.HashMap.Strict as HM
import            Data.Time.Clock
-- import qualified  Data.Text as T
-- import            Data.Map (Map)
import            Data.Maybe
import            qualified Data.Map as Map
import            Network.HTTP.Client
import            Network.HTTP.Client.TLS
import            Network.HTTP.Types.Status  (statusCode)
import            Crypto.JWT
import            Session

apiBase :: String
apiBase = "https://api.opentok.com"

data Client = Client {
  apiKey :: String,
  secret :: String
}

-- data Request = CreateSession | GenerateToken

-- -- request :: Client -> Request ->

manager :: IO Manager
manager = newManager tlsManagerSettings

data Request = CreateSession | StartArchive | StopArchive


-- Make some claims
mkClaims :: String -> IO ClaimsSet
mkClaims projectKey = do
  now <- currentTime
  let later = addUTCTime nominalDay now
  pure $ emptyClaimsSet
    & claimIss .~ preview stringOrUri (projectKey :: String)
    & claimIat .~ Just (NumericDate now)
    & claimExp .~ Just (NumericDate later)
    & claimJti .~ Just ("jwt_nonce")
    & unregisteredClaims %~ HM.insert "ist" "project"



signJWT :: JWK -> ClaimsSet -> IO (Either JWTError SignedJWT)
signJWT secret claims = runExceptT $ do
  alg <- H256 secret
  signClaims secret (newJWSHeader ((), alg)) claims

createSession :: Client -> SessionOptions -> IO (Either OTError Session)
createSession c opts = do

  -- currentTime <- getCurrentTime
  claims <- mkClaims (Client.apiKey c)
  jwt <- signJWT (Client.secret c) $ claims
  case jwt of
    Left e ->
      pure $ Left $ OTError "Failed to create session. Unable to generate credentials."
    Right token ->
      setRequestBodyJSON
      initialRequest <- parseRequest "http://httpbin.org/post"
      let request = initialRequest
              { method = "POST"
              , requestBody = RequestBodyLBS $ encode opts
              , requestHeaders =
                  [ ( "Content-Type", "application/json; charset=utf-8"),
                    ( "X-OPENTOK-AUTH", jwt)
                  ]
              }

      response <- httpLbs request manager
      putStrLn $ "The status code was: "
              ++ show (statusCode $ responseStatus response)
      L8.putStrLn $ responseBody response





