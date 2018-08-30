import Test.Tasty
import Test.Tasty.Hspec
import Test.Tasty.QuickCheck

import Session


main :: IO ()
main = do
  unitTests <- unitTestsIO
  defaultMain $ testGroup "Tests" [unitTests]

unitTestsIO :: IO TestTree
unitTestsIO = do
  session <- testSpec "Session" Session.spec
  return $ testGroup "Unit tests" [session]