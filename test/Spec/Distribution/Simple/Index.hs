{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.Distribution.Simple.Index (tests) where

import Spec.Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Data.List (find)
import Data.Set (fromList)

import Distribution.Simple.PackageStore
import Distribution.Types.PackageId (PackageIdentifier (..))
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.Version (mkVersion)

import Distribution.Simple.Index

tests :: TestTree
tests = testGroup "Index" [

    testGroup ("flow") [

        testCase "writes index" $ do
            let
            -- given
                packages = [
                    Pkg "config1" (PackageIdentifier (mkPackageName "foo") (mkVersion [12,1,3,4])),
                    Pkg "config1" (PackageIdentifier (mkPackageName "bar") (mkVersion [0,1,3]))
                    ]

            -- when
                actual = generate

            -- then
            captured packages actual === [fromList packages]

        ]
    ]


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

