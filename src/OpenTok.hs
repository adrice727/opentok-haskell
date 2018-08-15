{-# LANGUAGE OverloadedStrings #-}

module OpenTok
  ( opentok
  , OpenTok.createSession
  )
where

import           Prelude                        ( )
import           Prelude.Compat
import           Session
import           Client
import           Error

data OpenTok = OpenTok {
  apiKey :: String,
  secret :: String
}

instance Show OpenTok where
  show ot = "OpenTok { APIKey: " <> (apiKey ot) <> ", Secret: *_*_*_*_*_*  }"

opentok :: String -> String -> OpenTok
opentok k s = OpenTok k s

createSession :: OpenTok -> SessionOptions -> IO (Either OTError Session)
createSession ot opts = do
  let client = Client.Client (apiKey ot) (secret ot)
  sessionProps <- Client.createSession client opts
  pure $ fmap (\props -> fromProps opts props) sessionProps


  -- generateToken :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- startArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- stopArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- deleteArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- listArchives :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- forceDisconnect :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- dial :: (OpenTok ot) => Maybe SessionOptions -> Session
