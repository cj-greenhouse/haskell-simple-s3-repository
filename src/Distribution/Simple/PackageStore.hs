module Distribution.Simple.PackageStore where

import Distribution.Text as C (simpleParse)
import Distribution.Types.PackageId (PackageId)
import Data.Maybe (isJust)
import Data.Set (Set)
import Data.Text (Text, stripSuffix, unpack)
import Distribution.Simple.Cabal (Cabal)

data Package = Pkg {_cabal :: Cabal, _info :: PackageInfo} deriving (Eq, Show, Ord)
data PackageInfo = PkgInfo {_name :: Text, _version :: Text} deriving (Eq, Show, Ord)
type Index = Set Package


class PackageStore m where
    listPackages :: m [PackageInfo]
    fetchPackage :: PackageInfo -> m Cabal
    storeIndex :: Index -> m ()

class ObjectStore m where
    listObjectNames :: m [Text]

parseName :: Text -> Maybe PackageId
parseName n = do
    if n == "index.tar.gz"
        then Nothing
        else stripSuffix ".tar.gz" n >>= simpleParse . unpack

listPackagesUsingObjectStore :: (Monad m, ObjectStore m) => m [PackageId]
listPackagesUsingObjectStore = do
    names <- listObjectNames
    pure $ fmap (maybe undefined id) $ filter isJust $ fmap parseName  names
