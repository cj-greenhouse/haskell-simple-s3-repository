module Distribution.Simple.ObjectStore where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

class ObjectStore m where
    listObjectNames :: m [Text]
    fetchObject :: Text -> m ByteString
    storeObject :: Text -> ByteString -> m ()

