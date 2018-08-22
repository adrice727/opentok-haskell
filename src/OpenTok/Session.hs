{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenTok.Session
  ( Session
  , SessionOptions(_mediaMode, _archiveMode, _location)
  , MediaMode(Relayed, Routed)
  , create
  , sessionOpts
  )
where

import           Prelude                        ( )
import           Prelude.Compat
import           Data.Aeson
import           Data.Aeson.Casing              ( snakeCase )
import           Data.Aeson.Types
import           Data.Aeson.TH
import           Data.Data
import           Data.Strings                   ( strToLower )
import           GHC.Generics

import           OpenTok.Client
import           OpenTok.Types

-- | Relayed sessions will attempt to use peer-to-peer (p2p) connections.
--
-- Routed sessions will use the <https://tokbox.com/platform/multi-party OpenTok Media Router>
data MediaMode = Relayed | Routed deriving (Data, Generic, Typeable)

instance Show MediaMode where
  show = strToLower . showConstr . toConstr

deriveJSON defaultOptions { constructorTagModifier = strToLower } ''MediaMode

-- | Manual, as it implies, requires archives to be manually started and stopped.
-- Always means that archives will automatically be created.
data ArchiveMode = Manual | Always deriving (Data, Generic, Typeable)

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
-- An 'IPAddress' may be provided as a location hint which will
-- be when choosing an OpenTok Media Router for the session.
data SessionOptions = SessionOptions {
  _mediaMode :: MediaMode,
  _archiveMode :: ArchiveMode,
  _location :: Maybe IPAddress
} deriving(Show, Generic)


instance ToJSON SessionOptions where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True, fieldLabelModifier = drop 1 }

-- | Default options for Session creation
--
-- mediaMode: 'Relayed'
--
-- archiveMode: 'Manual'
--
-- location: Nothing
--
sessionOpts :: SessionOptions
sessionOpts = SessionOptions Relayed Manual Nothing

-- | Represents an OpenTok Session
data Session = Session {
  apiKey :: String,
  sessionId :: String,
  mediaMode :: MediaMode,
  archiveMode :: ArchiveMode
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
  }

-- | Create a new OpenTok Session
create :: Client -> SessionOptions -> IO (Either OTError Session)
create client opts = do
  response <-
    request client "/session/create/" opts :: IO
      (Either ClientError [SessionProperties])
  case response of
    Right propsArray -> pure $ Right $ (fromProps opts . head) propsArray
    Left  e          -> pure $ Left $ message e
