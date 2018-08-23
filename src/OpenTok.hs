{-# LANGUAGE OverloadedStrings #-}

module OpenTok
  ( OpenTok(apiKey, secret)
  , opentok
  , createSession
  , generateToken
  , Token
  , TokenOptions
  , tokenOpts
  , startArchive
  , stopArchive
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
  show ot = "OpenTok { APIKey: " <> OpenTok.apiKey ot <> ", Secret: *_*_*_*_*_*  }"

-- | Get an OpenTok project
--
-- > ot = opentok "my_api_key" "my_api_secret"
--
opentok :: APIKey -> APISecret -> OpenTok
opentok k s = OpenTok k s (OpenTok.Client.Client k s)

-- | Create a new OpenTok Session
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
generateToken ot = OpenTok.Token.generate (OpenTok.apiKey ot) (OpenTok.secret ot)

-- | Start recording an archive of an OpenTok session
--
-- > startArchive ot archiveOpts { sessionId = "your_session_id" }
--
startArchive :: OpenTok -> ArchiveOptions -> IO (Either OTError Archive)
startArchive ot = OpenTok.Archive.start (client ot)


-- | Stop recording an archive of an OpenTok session
--
-- > stopArchive ot "your_session_id"
--
stopArchive :: OpenTok -> ArchiveId -> IO (Either OTError Archive)
stopArchive ot = OpenTok.Archive.stop (client ot)
