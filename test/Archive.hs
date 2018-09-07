{-# LANGUAGE OverloadedStrings #-}

module Archive(spec) where

import            Control.Arrow
import            Control.Lens
import            Control.Lens.Extras         ( is )
import            Data.Either.Combinators     ( fromRight )
import            Data.List.Split
import            Test.Hspec

import            Config
import            OpenTok
import            OpenTok.Archive
import            OpenTok.Types

ot :: OpenTok
ot = opentok testKey testSecret

spec :: Spec
spec = do
  listArchivesSpec

listArchivesSpec :: Spec
listArchivesSpec = describe "listArchives" $ do
  it "should return an archive collection" $ do
    eArchives <- listArchives ot listArchiveOpts
    eArchives `shouldSatisfy` is _Right


