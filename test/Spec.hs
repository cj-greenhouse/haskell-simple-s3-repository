module Main where
--
import Test.Tasty (TestTree, testGroup, defaultMain)

import qualified Spec.Distribution.Simple.Index as Index
import qualified Spec.Distribution.Simple.PackageStore as PackageStore

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "hackage-simple" [
    Index.tests,
    PackageStore.tests
    ]
