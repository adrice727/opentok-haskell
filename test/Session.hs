{-# LANGUAGE OverloadedStrings #-}

module Session(spec) where

import Control.Arrow
import Control.Lens
import Control.Lens.Extras (is)
import Data.IP
import Test.Hspec

import Config
import OpenTok
import OpenTok.Session
import OpenTok.Types

ot :: OpenTok
ot = opentok testKey testSecret

spec :: Spec
spec = do
  defaultSessionSpec
  customSessionSpec
  invalidCredentialsSpec

defaultSessionSpec :: Spec
defaultSessionSpec = describe "A session created with default options" $ do
  it "should have default session properties" $ do
    session <- createSession ot sessionOpts
    session `shouldSatisfy` is _Right
    right mediaMode session `shouldBe` Right Relayed
    right archiveMode session `shouldBe` Right Manual
    right location session `shouldBe` Right Nothing

customSessionSpec :: Spec
customSessionSpec = describe "A session created with custom options" $ do
  it "should have matching session properties" $ do
    let loc = Just $ ipv4 "10.1.200.30"
    session <- createSession ot sessionOpts { _mediaMode = Routed, _archiveMode = Always, _location = loc }
    session `shouldSatisfy` is _Right
    right mediaMode session `shouldBe` Right Routed
    right archiveMode session `shouldBe` Right Always
    right location session `shouldBe` Right loc

invalidCredentialsSpec :: Spec
invalidCredentialsSpec = describe "A session created with invalid credentials" $ do
  it "should return an error" $ do
    let invalidOT = opentok "123456" "123456789123456789123456789"
    session <- createSession invalidOT sessionOpts
    session `shouldSatisfy` is _Left


