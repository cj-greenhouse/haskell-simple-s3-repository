module Distribution.Simple.PackageStore where

import Data.Set (Set)
import Data.Text (Text)
import Distribution.Simple.Cabal (Cabal)

data Package = Pkg {_cabal :: Cabal, _info :: PackageInfo} deriving (Eq, Show, Ord)
data PackageInfo = PkgInfo {_name :: Text, _version :: Text} deriving (Eq, Show, Ord)
type Index = Set Package


class PackageStore m where
    listPackages :: m [PackageInfo]
    fetchPackage :: PackageInfo -> m Cabal
    storeIndex :: Index -> m ()

