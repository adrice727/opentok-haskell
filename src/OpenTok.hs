module OpenTok (opentok) where

import Session

-- import qualified Request
import qualified Client
-- import Archive

data OpenTok = OpenTok {
  apiKey :: String,
  secret :: String
}

opentok :: String -> String -> OpenTok
opentok key secret = Opentok key secret

-- run :: IO ()
-- run = Request.send

createSession :: OpenTok -> SessionOptions -> IO Session
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