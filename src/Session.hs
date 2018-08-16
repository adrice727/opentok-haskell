{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Session
  ( CreateSessionResponse
  , Session
  , SessionId
  , SessionOptions
  , sessionOpts
  , SessionProperties
  , fromProps
  )
where

import           Prelude                        ()
import           Prelude.Compat
import           Archive
import           Data.Aeson
import           Data.Aeson.Casing              ( snakeCase )
import           GHC.Generics

type SessionId = String

data MediaMode = Routed | Relayed
instance Show MediaMode where
  show Relayed = "relayed"
  show Routed = "routed"

instance ToJSON MediaMode where
  toJSON Relayed = object [ "value" .= String "relayed" ]
  toJSON Routed  = object [ "value" .= String "routed" ]

type IPAddress = String

data SessionOptions = SessionOptions {
  _mediaMode :: MediaMode,
  _archiveMode :: ArchiveMode,
  _location :: Maybe IPAddress
} deriving(Show, Generic)


instance ToJSON SessionOptions where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True, fieldLabelModifier = drop 1 }

sessionOpts :: SessionOptions
sessionOpts = SessionOptions Relayed Manual Nothing

data Session = Session {
  apiKey :: String,
  sessionId :: String,
  mediaMode :: MediaMode,
  archiveMode :: ArchiveMode
} deriving (Show)

fromProps :: SessionOptions -> SessionProperties -> Session
fromProps opts props = Session
  { apiKey      = _projectId props
  , sessionId   = _sessionId props
  , mediaMode   = _mediaMode opts
  , archiveMode = _archiveMode opts
  }

data SessionProperties = SessionProperties {
  _sessionId :: String,
  _projectId :: String,
  _createDt :: String,
  _mediaServerUrl :: Maybe String
} deriving (Generic, Show)

instance FromJSON SessionProperties where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = snakeCase . drop 1 }

data CreateSessionResponse = CreateSessionResponse [SessionProperties] deriving (Show, Generic)
instance FromJSON CreateSessionResponse
