{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Session (CreateSessionResponse, Session, SessionOptions, sessionOpts) where

import Archive
import Data.Aeson
import Data.Aeson.Casing (aesonPrefix, camelCase)
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
  mediaMode :: MediaMode,
  archiveMode :: ArchiveMode,
  location :: Maybe IPAddress
} deriving(Show, Generic)

instance ToJSON SessionOptions where
  toJSON = genericToJSON defaultOptions
    { omitNothingFields = True }

sessionOpts:: SessionOptions
sessionOpts = SessionOptions Relayed Manual Nothing

data Session = Session { apiKey :: String, sessionId :: String } deriving (Show)

data CreateSessionProperties = CreateSessionProperties {
  session_id :: String,
  project_id :: String,
  create_dt :: String,
  media_server_url :: String
} deriving (Generic, Show)

instance FromJSON CreateSessionProperties where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

data CreateSessionResponse = CreateSessionResponse [CreateSessionProperties] deriving (Show, Generic)
instance FromJSON CreateSessionResponse
