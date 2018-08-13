{-# LANGUAGE OverloadedStrings #-}

module Archive where

import Data.Aeson

data ArchiveMode = Manual | Always
instance Show ArchiveMode where
  show Manual = "manual"
  show Always = "always"
instance ToJSON ArchiveMode where
  toJSON Manual = object [ "value" .= String "manual" ]
  toJSON Always  = object [ "value" .= String "always" ]