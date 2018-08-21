{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module OpenTok.Archive
  ( Archive
  , ArchiveOptions(hasAudio, hasAudio, name, outputMode, resolution, sessionId)
  , ArchiveResolution(SD, HD)
  , ArchiveStatus(Avilable, Expired, Failed, Paused, Started, Stopped, Uploaded)
  , OutputMode(Composed, Individual)
  , archiveOpts
  , start
  )
where

import           Prelude                        ( )
import           Prelude.Compat
import           Data.Aeson
import           Data.Aeson.Casing              ( aesonPrefix
                                                , camelCase
                                                , snakeCase
                                                , pascalCase
                                                )
import           Data.Aeson.Types
import           Data.Semigroup                 ( (<>) )
import           Data.Text                      ( unpack
                                                , Text
                                                )
import           Data.Time.Clock
import           GHC.Generics

import           OpenTok.Client
import           OpenTok.Types



-- | Composed means that streams will be composed into a single file
--
-- Individual means that an individual file will be created for each stream
--
data OutputMode = Composed | Individual deriving (Generic, Show)

instance ToJSON OutputMode where
  toJSON Composed = String "composed"
  toJSON Individual = String "individual"

instance FromJSON OutputMode where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase


-- | SD (Standard Definition 640 x 480)
--
-- | HD (High Definition 1280 x 720)
--
data ArchiveResolution = SD | HD

instance Show ArchiveResolution where
  show SD = "640 x 480 (SD)"
  show HD = "1280 x 720 (HD)"

instance ToJSON ArchiveResolution where
  toJSON SD = String "640x480"
  toJSON HD = String "1280x720"

instance FromJSON ArchiveResolution where
  parseJSON = withObject "ArchiveResolution" $ \o -> do
      res <- o .: "value"
      case res :: Text of
          "640x480" -> pure SD
          "1280x720" -> pure HD
          _ -> fail $ "Expected one of 640x480 or 1280x720 for ArchiveResolution, got: " <> unpack res

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
  hasAudio :: Bool,
  hasVideo :: Bool,
  name :: Maybe String,
  outputMode :: OutputMode,
  resolution :: ArchiveResolution,
  sessionId :: SessionId
} deriving (Generic, Show)

instance ToJSON ArchiveOptions

-- | Default Archive options
archiveOpts :: ArchiveOptions
archiveOpts = ArchiveOptions
  { hasAudio   = True
  , hasVideo   = True
  , name       = Nothing
  , outputMode = Composed
  , resolution = SD
  , sessionId  = ""
  }

-- | Status of an OpenTok `Archive`
data ArchiveStatus = Avilable | Expired | Failed | Paused | Started | Stopped | Uploaded deriving (Generic, Show)

instance ToJSON ArchiveStatus where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON ArchiveStatus where
  parseJSON = genericParseJSON $ aesonPrefix pascalCase

-- | Represents an OpenTok Archive
data Archive = Archive {
  _createdAt :: UTCTime,
  _duration :: Int,
  _hasAudio :: Bool,
  _hasVideo :: Bool,
  _id :: String,
  _name :: String,
  _outputMode :: OutputMode,
  _projectId :: APIKey,
  _reason :: String,
  _resolution :: ArchiveResolution,
  _sessionId :: SessionId,
  _size :: Int,
  _status :: ArchiveStatus,
  _url :: String
} deriving (Show, Generic)

instance FromJSON Archive where
  parseJSON = genericParseJSON $ defaultOptions { fieldLabelModifier = snakeCase . drop 1 }

start :: Client -> ArchiveOptions -> IO (Either OTError Archive)
start c opts = do
  let path = "/v2/project/" <> _apiKey c <> "/archive"
  response <- request c path opts :: Either OTError Archive
  case response of
    Right bs -> pure $ eitherDecode bs
    Left  e  -> pure $ Left $ "Failed to start archive: " <> e



