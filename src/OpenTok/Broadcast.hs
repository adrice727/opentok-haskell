{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenTok.Broadcast(
  BroadcastLayoutType(..),
  RtmpStreamConfig(..),
  BroadcastOutputs(..),
  BroadcastLayout(..),
  BroadcastOptions(..),
  BroadcastUrls(..),
  RtmpStream(..),
  Broadcast(..),
  start
) where

import           Prelude ()
import           Prelude.Compat
import           Data.Aeson
import           Data.Aeson.TH
import           Data.Data
import qualified Data.HashMap.Strict            as HM
import           Data.Semigroup                 ( (<>) )
import           Data.Strings                   ( strToLower )
import           GHC.Generics

import           OpenTok.Client
import           OpenTok.Types

type BroadcastResolution = Resolution

data BroadcastLayoutType = BestFit | Custom | HorizontalPresentation | PIP | VerticalPresentation
  deriving (Data, Generic, Typeable)

instance Show BroadcastLayoutType where
  show = strToLower . showConstr . toConstr

deriveJSON defaultOptions { constructorTagModifier = strToLower } ''BroadcastLayoutType

data RtmpStreamConfig = RtmpStreamConfig {
  _id :: String,
  _serverUrl :: String, -- Change to strict url
  _streamName :: String
} deriving (Generic, Show)

instance ToJSON RtmpStreamConfig where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = drop 1 }

data BroadcastOutputs = BroadcastOutputs {
  _hls :: Bool,
  _rtmp :: Maybe [RtmpStreamConfig]
} deriving (Generic, Show)

instance ToJSON BroadcastOutputs where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = drop 1 }

data BroadcastLayout = BroadcastLayout {
  _layoutType :: BroadcastLayoutType,
  _stylesheet :: Maybe String
} deriving (Generic, Show)

instance ToJSON BroadcastLayout where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = drop 1 }

data BroadcastOptions = BroadcastOptions {
  _sessionId :: SessionId,
  _layout :: Maybe BroadcastLayout,
  _maxDuration :: Maybe Int,
  _outputs :: BroadcastOutputs,
  _resolution :: Maybe BroadcastResolution
} deriving (Generic, Show)

instance ToJSON BroadcastOptions where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = drop 1 }

data BroadcastStatus = Started | Stopped | Available deriving (Generic, Data)

instance Show BroadcastStatus where
  show = strToLower . showConstr . toConstr

deriveJSON defaultOptions { constructorTagModifier = strToLower } ''BroadcastStatus

data RtmpStream = RtmpStream {
  serverUrl :: String, -- Change to strict url
  streamName :: String
} deriving (Generic, Show)

instance FromJSON RtmpStream

data BroadcastUrls = BroadcastUrls {
  hls :: String, -- change to url ?
  rtmp :: HM.HashMap String RtmpStream
} deriving (Generic, Show)

instance FromJSON BroadcastUrls

data Broadcast = Broadcast {
  id :: String,
  sessionId :: SessionId,
  projectId :: APIKey,
  createdAt :: Integer,
  updatedAt :: Integer,
  resolution :: Resolution,
  status :: BroadcastStatus,
  broadcastUrls :: BroadcastUrls
} deriving (Generic, Show)

instance FromJSON Broadcast

start :: Client -> BroadcastOptions -> IO (Either OTError Broadcast)
start c opts = do
  let path = "/v2/project/" <> _apiKey c <> "/broadcast"
  response <- postWithBody c path (Just opts) :: IO (Either ClientError Broadcast)
  case response of
    Right archive -> pure $ Right archive
    Left  e       -> pure $ Left $ "Failed to start archive: " <> message e
