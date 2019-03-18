{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.Distribution.Simple.PackageStore (tests) where

import Spec.Prelude

import Control.Monad.Trans.Reader
import Data.Text (Text, unpack)
import Distribution.Text (simpleParse)

import Distribution.Simple.PackageStore

tests :: TestTree
tests = testGroup "PackageStore" [

    testCase "filter object names" $ do
        let
            n1 = "mylib"; v1 = "0.1.3.2"
            n2 = "other"; v2 = "3.2.1"
            n3 = "why"; v3 = "why"
            listing = [
                "wot", n1 <> "-" <> v1 <> ".tar.gz",
                "99.33.2", n2 <> "-" <> v2 <> ".tar.gz",
                "other",
                "index.tar.gz",
                n3 <> "-" <> v3 <> ".tar.gz",
                "more"]

            actual = listPackagesUsingObjectStore

            e1 = maybe undefined id $ simpleParse . unpack $ n1 <> "-" <> v1
            e2 = maybe undefined id $ simpleParse . unpack $ n2 <> "-" <> v2
            e3 = maybe undefined id $ simpleParse . unpack $ n3 <> "-" <> v3
        value listing actual === [e1, e2, e3]

    ]

type Env = [Text]
type Test = Reader Env

value :: Env -> Test a -> a
value = flip runReader

instance ObjectStore Test where
    listObjectNames = ask

