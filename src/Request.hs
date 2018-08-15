{-# LANGUAGE OverloadedStrings #-}

module Request where

import           Prelude                        ( )
import           Prelude.Compat
import           Data.Aeson                     ( encode
                                                , object
                                                , (.=)
                                                )
import qualified Data.ByteString.Lazy.Char8    as L8
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Status      ( statusCode )

send :: IO ()
send = do
  manager <- newManager tlsManagerSettings

  let requestObject =
        object ["name" .= ("Alice" :: String), "age" .= (35 :: Int)]
  initialRequest <- parseRequest "http://httpbin.org/post"
  let
    request = initialRequest
      { method         = "POST"
      , requestBody    = RequestBodyLBS $ encode requestObject
      , requestHeaders = [("Content-Type", "application/json; charset=utf-8")]
      }

  response <- httpLbs request manager
  putStrLn $ "The status code was: " <> show
    (statusCode $ responseStatus response)
  L8.putStrLn $ responseBody response
