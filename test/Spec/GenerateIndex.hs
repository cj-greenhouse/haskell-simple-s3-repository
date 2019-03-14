module Spec.GenerateIndex (tests) where

import Spec.Prelude

import Control.Monad.Trans.Writer
import Data.Monoid (Sum (..))
import Data.Text (Text)


tests :: TestTree
tests = testGroup "GenerateIndex" [

    testCase "flow" $ do

        let
            -- pkg1 = ("cool-lib", "0.1.1.0")
            -- pkg2 = ("lame-lib", "3.0.0.2")
            -- tars = (\(n, v) -> n ++ "-" ++ v) <$> [pkg1,pkg2]

            actual = generate

        captured actual === Sum 1



    ]

---------------------------------------------------------------
-- Actual

generate :: (Monad m, PackageStore m) => m ()
generate = listPackages >> pure ()

data Item = Item Text Text deriving (Show, Eq) -- let's use names and PVP ver
data Entry = Entry [Text] deriving (Show, Eq, Ord)

class PackageStore m where
    listPackages :: m [Text]



----------------------------------------------------------------
-- Test

type Capture = Sum Integer
type Test = Writer Capture

captured :: Test a -> Capture
captured = snd . runWriter

instance PackageStore Test where
    listPackages = tell (Sum 1) >> pure mempty


