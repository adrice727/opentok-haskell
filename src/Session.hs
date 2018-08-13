{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Session (Session, SessionOptions, defaultSessionOptions) where

import Archive
import Data.Aeson
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

defaultSessionOptions :: SessionOptions
defaultSessionOptions = SessionOptions Relayed Manual Nothing

data Session = Session { apiKey :: String, sessionId :: String } deriving (Show)

