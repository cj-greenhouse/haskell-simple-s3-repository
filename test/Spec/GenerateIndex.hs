module Spec.GenerateIndex (tests) where

import Spec.Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.Set (Set, fromList, singleton)
import Data.Text (Text)


tests :: TestTree
tests = testGroup "GenerateIndex" [

    testCase "fetches every package" $ do

        let
        -- given
            packages = ["foo", "bar"]

        -- when
            actual = generate

        -- then
        captured packages actual === fromList packages


    ]

---------------------------------------------------------------
-- Actual

generate :: (Monad m, PackageStore m) => m ()
generate = listPackages >>= mapM_ fetchPackage

class PackageStore m where
    listPackages :: m [Text]
    fetchPackage :: Text -> m ()

----------------------------------------------------------------
-- Test

type Capture = Set Text
type Env = [Text]
type Test = ReaderT Env (Writer Capture)

captured :: Env -> Test a -> Capture
captured env = snd . runWriter . flip runReaderT env

instance PackageStore Test where
    listPackages =  ask
    fetchPackage = lift . tell . singleton
