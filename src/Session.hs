{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}


module Session (CreateSessionResponse, Session, SessionOptions, sessionOpts, fromProps) where

import Archive
import Data.Aeson
import Data.Aeson.Casing (snakeCase)
import GHC.Generics

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

sessionOpts:: SessionOptions
sessionOpts = SessionOptions Routed Manual Nothing

data Session = Session {
  apiKey :: String,
  sessionId :: String,
  mediaMode :: MediaMode,
  archiveMode :: ArchiveMode
} deriving (Show)

fromProps :: SessionOptions -> SessionProperties -> Session
fromProps opts props = Session {
  apiKey =  _projectId props,
  sessionId = _sessionId props,
  mediaMode = _mediaMode opts,
  archiveMode = _archiveMode opts
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
