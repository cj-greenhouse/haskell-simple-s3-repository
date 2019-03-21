{-# OPTIONS_GHC -fno-warn-orphans #-}

module Spec.Distribution.Simple.PackageStore (tests) where

import Spec.Prelude

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Codec.Archive.Tar (write)
import Codec.Archive.Tar.Entry (Entry, fromTarPath, toTarPath, fileEntry, directoryEntry)
import Codec.Compression.GZip (compress)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Map as Map (fromList, Map, (!))
import Data.Set as Set (fromList, toList)
import Data.Text (Text, unpack, pack)
import Data.Text.Encoding (encodeUtf8)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Cabal (Cabal)
import Distribution.Simple.ObjectStore (ObjectStore (..))
import Distribution.Types.PackageId (PackageIdentifier (..), PackageId)
import Distribution.Types.PackageName (mkPackageName, unPackageName)
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
                p1 = pid n1 v1
                p2 = pid n2 v2
                p3 = pid n3 v3
                listing = [
                    pidt p1 <> ".tar.gz",
                    "99.33.2",
                    pidt p2 <> ".tar.gz",
                    "other",
                    "index.tar.gz",
                    pidt p3 <> ".tar.gz",
                    "more"]

            -- when
                actual = listPackagesUsingObjectStore

            -- then
                env = (listing, mempty)
            value env actual === [p1, p2, p3]
        ,
        testCase "fetch package" $ do
            let
            -- given
                name = "thelib"; version = [0,1,2,2,1,1,1,1]
                p = pid name version
                c = "cabal text"
                obj = mkPackageTarball p c

            -- when
                actual = fetchPackageUsingObjectStore p

            -- then
                fetch = Map.fromList [(pidt p <> ".tar.gz", obj)]
            value (mempty, fetch) actual === c
        ]
        ,
        testCase "write index" $ do
            let
            -- given
                pkgId1 = pid "lib-a" [0,1,2]
                pkgId2 = pid "lib-b" [2,1,0]
                pkgId3 = pid "lib-b" [2,2,1]
                pkg1 = Pkg "spec1" pkgId1
                pkg2 = Pkg "spec2" pkgId2
                pkg3 = Pkg "spec2" pkgId3
                index = Set.fromList [pkg3, pkg1, pkg2]

            -- when
                actual = storeIndexUsingObjectStore index

            -- then
            captured (mempty, mempty) actual === [("index.tar.gz",mkIndexTarball index)]

    ]

indexEntry :: Package -> Entry
indexEntry pkg =
    let name = (unPackageName . pkgName . _info) pkg
        version = prettyShow $ _info pkg
        path = either undefined id $ toTarPath False $ name <> "/" <> version <> "/" <> name <> ".cabal"
    in fileEntry path ((fromStrict . encodeUtf8 . _cabal) pkg)


mkIndexTarball :: Index -> ByteString
mkIndexTarball index = compress $ write . fmap indexEntry . toList $ index

-- create a tarball containing the cabal file using the correct name
mkPackageTarball :: PackageId -> Cabal -> ByteString
mkPackageTarball pkgId spec =
    let bpath = either undefined id $ toTarPath True $ prettyShow pkgId
        dentry = directoryEntry bpath
        cpath = either undefined id $ toTarPath False (fromTarPath bpath ++ (unPackageName $ pkgName pkgId) ++ ".cabal")
        centry = fileEntry cpath (fromStrict $ encodeUtf8 spec)
    in compress $ write [dentry, centry]

pid :: Text -> [Int] -> PackageId
pid n v = PackageIdentifier (mkPackageName (unpack n)) (mkVersion v)

pidt :: PackageId -> Text
pidt = pack . prettyShow

type Env = ([Text], Map Text ByteString)
type Capture = [(Text,ByteString)]
type Test = ReaderT Env (Writer Capture)

run :: Env -> Test a -> (a, Capture)
run env = runWriter . flip runReaderT env

value :: Env -> Test a -> a
value env = fst . run env

captured :: Env -> Test a -> Capture
captured env = snd . run env

instance ObjectStore Test where
    listObjectNames = fst <$> ask
    fetchObject key = do
        cfg <- snd <$> ask
        pure $ cfg ! key
    storeObject key obj = lift $ tell [(key,obj)]

