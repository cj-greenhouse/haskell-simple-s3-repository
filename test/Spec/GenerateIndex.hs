module Spec.GenerateIndex (tests) where

import Spec.Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.List (find)
import Data.Set (Set, fromList)
import Data.Text (Text)


tests :: TestTree
tests = testGroup "GenerateIndex" [

    testGroup ("flow") [

        testCase "writes index" $ do
            let
            -- given
                packages = [
                    Pkg "config1" (PkgInfo "foo" "1.1.1"),
                    Pkg "config2" (PkgInfo "bar" "0.1.1.3")]

            -- when
                actual = generate

            -- then
            captured packages actual === [fromList packages]

        ]
    ]

---------------------------------------------------------------
-- Actual

type Cabal = Text
data Package = Pkg {_cabal :: Cabal, _info :: PackageInfo} deriving (Eq, Show, Ord)
data PackageInfo = PkgInfo {_name :: Text, _version :: Text} deriving (Eq, Show, Ord)
type Index = Set Package


generate :: (Monad m, PackageStore m) => m ()
generate = do
    infos <- listPackages
    cabals <- mapM fetchPackage infos
    let packages = zipWith Pkg cabals infos
    storeIndex $ fromList packages

class PackageStore m where
    listPackages :: m [PackageInfo]
    fetchPackage :: PackageInfo -> m Cabal
    storeIndex :: Index -> m ()

----------------------------------------------------------------
-- Test

type Capture = [Index]
type Env = [Package]
type Test = ReaderT Env (Writer Capture)

captured :: Env -> Test a -> Capture
captured env = snd . runWriter . flip runReaderT env

instance PackageStore Test where
    listPackages =  ask >>= pure . fmap _info
    fetchPackage info = do
        ask >>= pure . _cabal . maybe undefined id . find (\a -> (_info a == info))
    storeIndex  = lift . tell . (: [])

