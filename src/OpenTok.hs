module OpenTok (opentok, createSession) where

import Session
-- import OTError

-- import qualified Request
import qualified Client
-- import Archive

data OpenTok = OpenTok {
  apiKey :: String,
  secret :: String
}

instance Show OpenTok where
  show ot = "OpenTok { APIKey: " ++ (apiKey ot) ++ ", Secret: " ++ (unwords $ fmap (\_ -> "*") $ secret ot) ++ " }"

opentok :: String -> String -> OpenTok
opentok k s = OpenTok k s

createSession :: OpenTok -> SessionOptions -> IO (Either String CreateSessionResponse)
createSession ot = do
  let client = Client.Client (apiKey ot) (secret ot)
  Client.createSession client


  -- generateToken :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- startArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- stopArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- deleteArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- listArchives :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- forceDisconnect :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- dial :: (OpenTok ot) => Maybe SessionOptions -> Session