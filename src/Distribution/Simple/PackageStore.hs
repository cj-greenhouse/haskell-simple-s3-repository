module Distribution.Simple.PackageStore where

import Codec.Archive.Tar as TAR (Entries, read, foldlEntries, write)
import Codec.Archive.Tar.Entry as TAR (Entry (..), fromTarPath, toTarPath, EntryContent (..), fileEntry)
import Codec.Compression.GZip (decompress, compress)
import Data.List (find)
import Data.Set (toList)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Cabal (Cabal)
import Distribution.Simple.ObjectStore (ObjectStore (..))
import Distribution.Text as C (simpleParse)
import Distribution.Types.PackageId (PackageId, pkgName)
import Distribution.Types.PackageName (unPackageName)
import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Text (Text, stripSuffix, unpack, pack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)

data Package = Pkg {_cabal :: Cabal, _info :: PackageId} deriving (Eq, Show, Ord)
-- data PackageInfo = PkgInfo {_name :: Text, _version :: Text} deriving (Eq, Show, Ord)
type Index = Set Package


class PackageStore m where
    listPackages :: m [PackageId]
    fetchPackage :: PackageId -> m Cabal
    storeIndex :: Index -> m ()

parseName :: Text -> Maybe PackageId
parseName n =
    if n == "index.tar.gz"
        then Nothing
        else stripSuffix ".tar.gz" n >>= simpleParse . unpack

listPackagesUsingObjectStore :: (Monad m, ObjectStore m) => m [PackageId]
listPackagesUsingObjectStore = fmap (maybe undefined id) . filter isJust . fmap parseName <$> listObjectNames

unEntries :: Entries err -> Maybe [Entry]
unEntries entries = either (const Nothing) Just $ foldlEntries (\es e -> e : es) mempty entries

fetchPackageUsingObjectStore :: (Monad m, ObjectStore m) => PackageId ->  m Cabal
fetchPackageUsingObjectStore pkgId = do
    let
        key = (pack $ prettyShow pkgId) <> ".tar.gz"
    tarball <- fetchObject key
    let entries = (TAR.read . decompress) tarball
        maybeCabal = fmap textContent (unEntries entries >>= find isCabal)
    maybe undefined pure maybeCabal
    where
        isCabal entry =
            let want = prettyShow pkgId ++ "/" ++ (unPackageName . pkgName) pkgId ++ ".cabal"
            in (fromTarPath . entryTarPath) entry == want
        textContent entry = case entryContent entry of
            NormalFile blob _ -> (decodeUtf8 . toStrict) blob
            _ -> undefined

storeIndexUsingObjectStore :: (Monad m, ObjectStore m) => Index -> m ()
storeIndexUsingObjectStore index = do
    storeObject "index.tar.gz" tarball
    where
        tarball = compress . write . fmap entry . toList $ index
        entry :: Package -> Entry
        entry pkg = fileEntry path spec
            where
                path = either undefined id $ toTarPath False $ name <> "/" <> version <> "/" <> name <> ".cabal"
                name = unPackageName . pkgName . _info $ pkg
                version = prettyShow . _info $ pkg
                spec = fromStrict . encodeUtf8 . _cabal $ pkg

