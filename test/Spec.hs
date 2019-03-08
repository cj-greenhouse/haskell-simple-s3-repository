module Main where
--
import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Spec.GenerateIndex as GenerateIndex

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "hackage-simple" [
    GenerateIndex.tests
    ]
