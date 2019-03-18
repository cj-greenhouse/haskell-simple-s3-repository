{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.Distribution.Simple.PackageStore (tests) where

import Spec.Prelude

import Control.Monad.Trans.Reader
import Data.Text (Text, unpack, pack)
import Distribution.Pretty (prettyShow)
import Distribution.Types.PackageId (PackageIdentifier (..), PackageId)
import Distribution.Types.PackageName (mkPackageName)
import Distribution.Types.Version (mkVersion)

import Distribution.Simple.PackageStore

tests :: TestTree
tests = testGroup "PackageStore" [

    testGroup "object store impl" [
        testCase "filter and parse object names" $ do
            let
            -- given
                n1 = "mylib"; v1 = [0,1,3,2]
                n2 = "other"; v2 = [3,2,1]
                n3 = "why"; v3 = [2]
                e1 = pid n1 v1
                e2 = pid n2 v2
                e3 = pid n3 v3
                listing = [
                    pidt e1 <> ".tar.gz",
                    "99.33.2",
                    pidt e2 <> ".tar.gz",
                    "other",
                    "index.tar.gz",
                    pidt e3 <> ".tar.gz",
                    "more"]

            -- when
                actual = listPackagesUsingObjectStore

            -- then
            value listing actual === [e1, e2, e3]
        -- ,
        -- testCase "fetch package" $ do
        --     let
        --     -- given
        --         n = "thelib"; v = "0.1.1.1.1.1"
        --         pkg =
        --     -- when

        --     -- then

        ]


    ]


pid :: Text -> [Int] -> PackageId
pid n v = PackageIdentifier (mkPackageName (unpack n)) (mkVersion v)

pidt :: PackageId -> Text
pidt = pack . prettyShow

type Env = [Text]
type Test = Reader Env

value :: Env -> Test a -> a
value = flip runReader

instance ObjectStore Test where
    listObjectNames = ask

