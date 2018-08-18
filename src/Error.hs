module Error where

newtype OTError = OTError String deriving (Show)

otError :: String -> OTError
otError = OTError
