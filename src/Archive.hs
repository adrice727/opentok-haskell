{-# LANGUAGE OverloadedStrings #-}

module Archive where

import           Prelude                        ( )
import           Prelude.Compat
import           Data.Aeson

-- | Manual, as it implies, requires archives to be manually started and stopped.
-- Always means that archives will automatically be created.
data ArchiveMode = Manual | Always

instance Show ArchiveMode where
  show Manual = "manual"
  show Always = "always"

instance ToJSON ArchiveMode where
  toJSON Manual = object [ "value" .= String "manual" ]
  toJSON Always  = object [ "value" .= String "always" ]
