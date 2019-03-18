module Distribution.Simple.Index where

import Data.Set (fromList)
import Distribution.Simple.PackageStore (PackageStore (..), Package (..))

generate :: (Monad m, PackageStore m) => m ()
generate = do
    infos <- listPackages
    cabals <- mapM fetchPackage infos
    let packages = zipWith Pkg cabals infos
    storeIndex $ fromList packages

