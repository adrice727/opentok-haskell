import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Session
import Token


main :: IO ()
main = do
  unitTests <- unitTestsIO
  defaultMain $ testGroup "Tests" [unitTests]

unitTestsIO :: IO TestTree
unitTestsIO = do
  session <- testSpec "Session" Session.spec
  token <- testSpec "Token" Token.spec
  return $ testGroup "Unit tests" [session, token]