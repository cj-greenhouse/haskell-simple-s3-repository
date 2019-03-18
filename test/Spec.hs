module Main where
--
import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Spec.Distribution.Simple.Index as Index

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "hackage-simple" [
    Index.tests
    ]
