{-# LANGUAGE OverloadedStrings #-}

module OpenTok
  ( module OpenTok
  , module OpenTok.Session
  , module OpenTok.Archive
  , module OpenTok.Token
  , module OpenTok.Types
  )
where

import           Prelude                        ( )
import           Prelude.Compat
import           Data.Semigroup                 ( (<>) )

import           OpenTok.Archive
import           OpenTok.Session
import           OpenTok.Token
import           OpenTok.Client
import           OpenTok.Types

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
opentok k s = OpenTok k s (OpenTok.Client.Client k s)

-- | Generate a new OpenTok Session
--
-- @
-- options = sessionOpts { mediaMode = Routed }
-- session <- createSession ot sessionOpts
-- @
--
createSession :: OpenTok -> SessionOptions -> IO (Either OTError Session)
createSession ot = OpenTok.Session.create (client ot)

-- | Generate a token.
--
-- @
-- let options = tokenOpts { connectionData = "name:tim" }
-- token <- generateToken ot options
-- @
--
generateToken :: OpenTok -> SessionId -> TokenOptions -> IO (Either OTError Token)
generateToken ot = OpenTok.Token.generate (apiKey ot) (secret ot)

-- | Start recording an archive of an OpenTok session
--
-- > startArchive ot archiveOpts { sessionId = "your_session_id" }
--
startArchive :: OpenTok -> ArchiveOptions -> IO (Either OTError ArchiveResponse)
startArchive ot = OpenTok.Archive.start (client ot)

  -- startArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- stopArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- deleteArchive :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- listArchives :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- forceDisconnect :: (OpenTok ot) => Maybe SessionOptions -> Session
  -- dial :: (OpenTok ot) => Maybe SessionOptions -> Session
