module OpenTokSDK (opentok) where

  import OpenTok

  opentok :: OpenTok
  opentok = OpenTok

-- Expose OpenTok in this module so new instances can be created



-- type SessionId = String
-- type ArchiveId = String
-- type SipUri = String
-- type IPAddress = String

-- data SipOptions = SipOptions {
--   something :: String
-- }

-- data ArchiveOptions = ArchiveOptions {
--   name :: String
-- } deriving (Show)

-- data SessionOptions = SessionOptions {
--   locationHint :: IPAddress
-- } deriving (Show)

-- type Role = Moderator | Publisher | Subscriber

-- data TokenOptions = TokenOptions {
--   role :: Maybe Role
--   expireTime :: Maybe Date
--   data :: Maybe String,
--   initialLayoutClassList :: Maybe [String]
-- }

-- data OpenTok = OpenTok APIKey Secret
--   createSession :: (Maybe SessionOptions) -> Session
--   generateToken :: (Maybe TokenOptions) -> String
--   startArchive :: SessionId -> ArchiveOptions -> Archive
--   stopArchive :: Archive
--   deleteArchive :: Unit
--   listArchives :: [Archive]
--   forceDisconnect :: Unit
--   dial :: SessionId -> Token -> SipUri -> SipOptions

-- data Session = Session
--   generateToken

-- data Archive = Archive {
--   archiveId :: String
-- }
--   stop :: Archive
--   delete :: Unit