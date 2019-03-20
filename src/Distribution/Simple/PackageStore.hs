module Distribution.Simple.PackageStore where

import Codec.Archive.Tar as TAR (Entries, read, foldlEntries)
import Codec.Archive.Tar.Entry as TAR (Entry (..), fromTarPath, EntryContent (..))
import Codec.Compression.GZip (decompress)
import Data.List (find)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Cabal (Cabal)
import Distribution.Text as C (simpleParse)
import Distribution.Types.PackageId (PackageId, pkgName)
import Distribution.Types.PackageName (unPackageName)
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Text (Text, stripSuffix, unpack, pack)
import Data.Text.Encoding (decodeUtf8)

data Package = Pkg {_cabal :: Cabal, _info :: PackageId} deriving (Eq, Show, Ord)
-- data PackageInfo = PkgInfo {_name :: Text, _version :: Text} deriving (Eq, Show, Ord)
type Index = Set Package


class PackageStore m where
    listPackages :: m [PackageId]
    fetchPackage :: PackageId -> m Cabal
    storeIndex :: Index -> m ()

class ObjectStore m where
    listObjectNames :: m [Text]
    fetchObject :: Text -> m ByteString

parseName :: Text -> Maybe PackageId
parseName n =
    if n == "index.tar.gz"
        then Nothing
        else stripSuffix ".tar.gz" n >>= simpleParse . unpack

listPackagesUsingObjectStore :: (Monad m, ObjectStore m) => m [PackageId]
listPackagesUsingObjectStore = fmap (maybe undefined id) . filter isJust . fmap parseName <$> listObjectNames

unEntries :: Entries err -> Maybe [Entry]
unEntries entries = either (const Nothing) Just $ foldlEntries (\es e -> e : es) mempty entries

isCabal :: PackageId -> Entry -> Bool
isCabal pkgId entry =
    let want = prettyShow pkgId ++ "/" ++ (unPackageName . pkgName) pkgId ++ ".cabal"
    in (fromTarPath . entryTarPath) entry == want

cabal :: Entry -> Cabal
cabal entry = case entryContent entry of
    NormalFile blob _ -> decodeUtf8 . toStrict $ blob
    _ -> undefined

fetchPackageUsingObjectStore :: (Monad m, ObjectStore m) => PackageId ->  m Cabal
fetchPackageUsingObjectStore pkgId = do
    let
        key = (pack $ prettyShow pkgId) <> ".tar.gz"
    tarball <- fetchObject key
    let entries = (TAR.read . decompress) tarball
        maybeCabal = fmap cabal (unEntries entries >>= find (isCabal pkgId))
    maybe undefined pure maybeCabal


