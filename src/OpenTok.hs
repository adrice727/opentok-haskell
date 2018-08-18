{-# LANGUAGE OverloadedStrings #-}

module OpenTok
  ( opentok
  , OpenTok.createSession
  , OpenTok.generateToken
  )
where

import           Prelude                        ( )
import           Prelude.Compat
import           Data.Semigroup                 ( (<>) )
import           Session
import           Client
import           Error
import           Token
import           Types

data OpenTok = OpenTok {
  apiKey :: String,
  secret :: String
}

instance Show OpenTok where
  show ot = "OpenTok { APIKey: " <> apiKey ot <> ", Secret: *_*_*_*_*_*  }"

-- |Get your key and secret from https://tokbox.com/account/
opentok :: APIKey -> APISecret -> OpenTok
opentok = OpenTok

-- |Generate a new OpenTok session
createSession :: OpenTok -> SessionOptions -> IO (Either OTError Session)
createSession ot opts = do
  let client = Client.Client (apiKey ot) (secret ot)
  sessionProps <- Client.createSession client opts
  pure $ fmap (fromProps opts) sessionProps

-- |Generate a token. Use the Role value appropriate for the user.
generateToken :: OpenTok -> SessionId -> TokenOptions -> IO (Either OTError Token)
generateToken ot = createToken (apiKey ot) (secret ot)




  -- generateToken :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- startArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- stopArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- deleteArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- listArchives :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- forceDisconnect :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- dial :: (OpenTok ot) => Maybe SessionOptions -> Session
