module Error where

data OTError = OTError String deriving (Show)

otError :: String -> OTError
otError a = OTError a
