{-# LANGUAGE OverloadedStrings #-}

module OpenTok
  ( opentok
  , Session
  , OpenTok
  , createSession
  , generateToken
  )
where

import           Prelude                        ( )
import           Prelude.Compat
import           Data.Semigroup                 ( (<>) )
import           Session
import           Client
import           Token
import           Types

-- | Represents an OpenTok project.
--
-- Get your project key and secret from https://tokbox.com/account/
data OpenTok = OpenTok {
  apiKey :: APIKey,
  secret :: APISecret,
  client :: Client
}

instance Show OpenTok where
  show ot = "OpenTok { APIKey: " <> apiKey ot <> ", Secret: *_*_*_*_*_*  }"

-- | Get an OpenTok project
--
-- > ot = opentok "my_api_key" "my_api_secret"
--
opentok :: APIKey -> APISecret -> OpenTok
opentok k s = OpenTok k s (Client k s)

-- | Generate a new OpenTok Session
--
-- @
-- options = sessionOpts { mediaMode = Routed }
-- session <- createSession ot sessionOpts
-- @
--
createSession :: OpenTok -> SessionOptions -> IO (Either OTError Session)
createSession ot = Session.create (client ot)

-- | Generate a token.
--
-- @
-- let options = tokenOpts { connectionData = "name:tim" }
-- token <- generateToken ot options
-- @
--
generateToken :: OpenTok -> SessionId -> TokenOptions -> IO (Either OTError Token)
generateToken ot = Token.generate (apiKey ot) (secret ot)



  -- startArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- stopArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- deleteArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- listArchives :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- forceDisconnect :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- dial :: (OpenTok ot) => Maybe SessionOptions -> Session
