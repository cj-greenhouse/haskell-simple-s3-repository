module Distribution.Simple.Index where

import Data.Set (Set, fromList)
import Data.Text (Text)

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
