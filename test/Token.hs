{-# LANGUAGE OverloadedStrings #-}

module Token(spec) where

import qualified  Codec.Binary.Base64.String  as B64
import            Control.Arrow
import            Control.Lens
import            Control.Lens.Extras         ( is )
import            Data.Either.Combinators     ( fromRight )
import            Data.List.Split
import            Test.Hspec

import            Config
import            OpenTok
import            OpenTok.Session
import            OpenTok.Types

ot :: OpenTok
ot = opentok testKey testSecret

sessionRequest :: IO (Either OTError Session)
sessionRequest = createSession ot sessionOpts

tokenParts :: Either OTError Token -> [String]
tokenParts et = fromRight [] (right (splitOn "&" . B64.decode . drop 4) et)

extractValue :: String -> Maybe String
extractValue p = splitOn "=" p ^? element 1

spec :: Spec
spec = do
  defaultTokenSpec
  customTokenSpec

defaultTokenSpec :: Spec
defaultTokenSpec = describe "A token created with default options" $ do
  it "should have default connection properties" $ do
    eSession <- sessionRequest
    eToken   <- generateToken ot (fromRight "" (right sessionId eSession)) tokenOpts
    let parts = tokenParts eToken
    let role = (parts ^? element 5) >>= extractValue
    let connectionData = (parts ^? element 6) >>= extractValue
    role `shouldBe` Just "publisher"
    connectionData `shouldBe` Nothing

customTokenSpec :: Spec
customTokenSpec = describe "A token created with custom options" $ do
  it "should have those properties encoded" $ do
    let opts = tokenOpts { _role = Moderator, _connectionData = Just ("foo" :: String) }
    eSession <- sessionRequest
    eToken   <- generateToken ot (fromRight "" (right sessionId eSession)) opts
    let parts = tokenParts eToken
    let role = (parts ^? element 5) >>= extractValue
    let connectionData = (parts ^? element 6) >>= extractValue
    role `shouldBe` Just "moderator"
    connectionData `shouldBe` Just "foo"


