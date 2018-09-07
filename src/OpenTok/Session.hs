{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module OpenTok.Session
  (
    ArchiveMode(..)
  , MediaMode(..)
  , SessionOptions(..)
  , Session(..)
  , sessionOpts
  , create
  )
where

import           Prelude                        ( )
import           Prelude.Compat
import           Data.Aeson
import           Data.Aeson.Casing              ( snakeCase )
import           Data.Aeson.Types
import           Data.Aeson.TH
import           Data.Data
import           Data.IP                        ( IPv4 )
import           Data.Strings                   ( strToLower )
import           GHC.Generics

import           OpenTok.Client
import           OpenTok.Types

-- | Relayed sessions will attempt to use peer-to-peer (p2p) connections.
--
-- Routed sessions will use the <https://tokbox.com/platform/multi-party OpenTok Media Router>
data MediaMode = Relayed | Routed deriving (Data, Eq, Generic, Typeable)

instance Show MediaMode where
  show = strToLower . showConstr . toConstr

deriveJSON defaultOptions { constructorTagModifier = strToLower } ''MediaMode

-- | Manual, as it implies, requires archives to be manually started and stopped.
-- Always means that archives will automatically be created.
data ArchiveMode = Manual | Always deriving (Data, Eq, Generic, Typeable)

instance Show ArchiveMode where
  show = strToLower . showConstr . toConstr

deriveJSON defaultOptions { constructorTagModifier = strToLower } ''ArchiveMode

-- | Defines options for an OpenTok Session
--
-- 'MediaMode' specifies how clients in the session will send audio
-- and video streams.
--
-- 'ArchiveMode' specifies how archives will be created.
--
-- An 'IPv4' address may be provided as a location hint which will
-- be used in selecting an OpenTok Media Router for the session.
data SessionOptions = SessionOptions {
  _mediaMode :: MediaMode,
  _archiveMode :: ArchiveMode,
  _location :: Maybe IPv4
} deriving(Show, Generic)

instance ToJSON IPv4 where
  toJSON = genericToJSON defaultOptions


instance ToJSON SessionOptions where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True, fieldLabelModifier = drop 1 }

{-|
Default Session options

@
SessionOptions
  { _mediaMode: 'Relayed'
  , _archiveMode: 'Manual'
  , _location: Nothing
}
@

-}
sessionOpts :: SessionOptions
sessionOpts = SessionOptions Relayed Manual Nothing

{-|

An OpenTok Session

@
Session {
  apiKey :: 'String',
  sessionId :: 'String',
  mediaMode :: 'MediaMode',
  archiveMode :: 'ArchiveMode'
}
@

-}
data Session = Session {
  apiKey :: String,
  sessionId :: String,
  mediaMode :: MediaMode,
  archiveMode :: ArchiveMode,
  location :: Maybe IPv4
} deriving (Show)

data SessionProperties = SessionProperties {
  _sessionId :: String,
  _projectId :: String,
  _createDt :: String,
  _mediaServerUrl :: Maybe String
} deriving (Generic, Show)

instance FromJSON SessionProperties where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = snakeCase . drop 1 }

-- | Create a session using the session options and the properties
-- returned by the API call.
fromProps :: SessionOptions -> SessionProperties -> Session
fromProps opts props = Session
  { apiKey      = _projectId props
  , sessionId   = _sessionId props
  , mediaMode   = _mediaMode opts
  , archiveMode = _archiveMode opts
  , location    = _location opts
  }

-- | Create a new OpenTok Session
create :: Client -> SessionOptions -> IO (Either OTError Session)
create client opts = do
  response <- postWithBody client "/session/create/" (Just opts) :: IO (Either ClientError [SessionProperties])
  case response of
    Right propsArray -> pure $ Right $ (fromProps opts . head) propsArray
    Left  e          -> pure $ Left $ message e
