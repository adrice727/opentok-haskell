{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module OpenTok.Archive
  ( ArchiveOptions
    ( _hasAudio
    , _hasAudio
    , _name
    , _outputMode
    , _resolution
    , _sessionId
    )
  , ArchiveResolution(SD, HD)
  , Archive
    ( id
    , status
    , createdAt
    , size
    , partnerId
    , url
    , resolution
    , outputMode
    , hasAudio
    , hasVideo
    , reason
    , name
    , updatedAt
    , duration
    , sessionId
    )
  , OutputMode(Composed, Individual)
  , archiveOpts
  , start
  , stop
  )
where

import           Prelude                        ( )
import           Prelude.Compat
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH
import           Data.Data
import           Data.Semigroup                 ( (<>) )
import           Data.Strings                   ( strToLower )
import           GHC.Generics

import           OpenTok.Client
import           OpenTok.Types



-- | Composed means that streams will be composed into a single file
--
-- Individual means that an individual file will be created for each stream
--
data OutputMode = Composed | Individual deriving (Data, Generic, Typeable)

instance Show OutputMode where
  show = strToLower . showConstr . toConstr

deriveJSON defaultOptions { constructorTagModifier = strToLower } ''OutputMode

-- | SD (Standard Definition 640 x 480)
--
-- | HD (High Definition 1280 x 720)
--
data ArchiveResolution = SD | HD

instance Show ArchiveResolution where
  show SD = "640x480 (SD)"
  show HD = "1280x720 (HD)"

instance ToJSON ArchiveResolution where
  toJSON SD = String "640x480"
  toJSON HD = String "1280x720"

instance FromJSON ArchiveResolution where
  parseJSON (String s) = case s of
    "640x480" -> pure SD
    "1280x720" -> pure HD
    _ -> typeMismatch "Could not parse ArchiveResolution" (String s)
  parseJSON x = typeMismatch "Expected String" x

-- | Defines options for an Archive
--
-- sessionId: The session to be archived
--
-- hasAudio: Whether the archive will record audio
--
-- hasVideo: Whether the archive will record video
--
-- name: The name of the archive
--
-- Whether all streams in the archive are recorded to a
-- single file ('Composed') or to individual files ('Individual')
--
-- The resolution of the archive, either 'SD' (the default, 640 x 480),
-- or 'HD' (1280 x 720)
--
data ArchiveOptions =  ArchiveOptions {
  _hasAudio :: Bool,
  _hasVideo :: Bool,
  _name :: Maybe String,
  _outputMode :: OutputMode,
  _resolution :: ArchiveResolution,
  _sessionId :: SessionId
} deriving (Generic, Show)

instance ToJSON ArchiveOptions where
  toJSON = genericToJSON defaultOptions { omitNothingFields = True, fieldLabelModifier = drop 1 }

-- | Default Archive options
archiveOpts :: ArchiveOptions
archiveOpts = ArchiveOptions
  { _hasAudio   = True
  , _hasVideo   = True
  , _name       = Nothing
  , _outputMode = Composed
  , _resolution = SD
  , _sessionId  = ""
  }

-- | Status of an OpenTok `Archive`
data ArchiveStatus = Available | Expired | Failed | Paused | Started | Stopped | Uploaded deriving (Data, Generic, Typeable)
deriveJSON defaultOptions { constructorTagModifier = strToLower } ''ArchiveStatus


-- | Represents an OpenTok Archive
data Archive = Archive {
  id :: String,
  status :: String,
  createdAt :: Integer,
  size :: Float,
  partnerId :: Int,
  url :: Maybe String,
  resolution :: ArchiveResolution,
  outputMode :: OutputMode,
  hasAudio :: Bool,
  hasVideo :: Bool,
  reason :: String,
  name :: String,
  updatedAt :: Integer,
  duration :: Float,
  sessionId :: String
} deriving (Show, Generic)

instance FromJSON Archive where
  parseJSON = genericParseJSON defaultOptions

-- [("event",String "archive"),("status",String "started"),("createdAt",Number 1.534898598419e12),("size",Number 0.0),("partnerId",Number 4.5759482e7),("url",Null),("resolution",String "640x480"),("outputMode",String "composed"),("hasAudio",Bool True),("reason",String ""),("name",Null),("password",String ""),("id",String "e4efbf78-244f-47e7-ae89-640988cac725"),("updatedAt",Number 1.53489859849e12),("sha256sum",String ""),("projectId",Number 4.5759482e7),("sessionId",String "2_MX40NTc1OTQ4Mn5-MTUzNDg5NzQ0MDgwMX44QjZFVFQ4VkhWL05YYUpvRUtlNGFUQkh-fg"),("hasVideo",Bool True),("duration",Number 0.0)]

start :: Client -> ArchiveOptions -> IO (Either OTError Archive)
start c opts = do
  let path = "/v2/project/" <> _apiKey c <> "/archive"
  response <- postWithBody c path (Just opts) :: IO (Either ClientError Archive)
  case response of
    Right archive -> pure $ Right archive
    Left  e       -> pure $ Left $ "Failed to start archive: " <> message e

stop :: Client -> ArchiveId -> IO (Either OTError Archive)
stop c aId = do
  let path = "/v2/project/" <> _apiKey c <> "/archive/" <> aId <> "/stop"
  response <- post c path :: IO (Either ClientError Archive)
  case response of
    Right archive -> pure $ Right archive
    Left  e       -> pure $ Left $ "An error occurred in attempting to stop an archive: " <> message e
