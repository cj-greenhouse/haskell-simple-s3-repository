module Spec.Distribution.Simple.ObjectStoreS3 (tests) where

import Spec.Prelude

import Control.Lens
import Control.Monad.Trans.Reader
import Data.Text (Text)
import Data.Time
import Network.AWS (AWSRequest (..))
import Network.AWS.S3

tests :: TestTree
tests = testGroup "ObjectStoreS3" [
    testCase "simple listing" $ do
        let
        -- given
            k1 = "object1"
            k2 = "object2"
            repo = "store-repo"
            os = (\n -> object' (ETag "") 0 (ObjectKey n) OSCStandard (UTCTime (toEnum 0) 1093)) <$> [k1,k2]
            req = listObjectsV2 $ BucketName repo
            resp = listObjectsV2Response 200 & lovrsContents .~ os

        -- when
            actual = listObjectNamesUsingS3 repo

        -- then
        value [(req, resp)] actual === [k1,k2]
    ]


type Env = [(ListObjectsV2, ListObjectsV2Response)]
type Test = Reader Env

value :: Env -> Test a -> a
value = flip runReader

instance AWS ListObjectsV2 Test where
    aws r = maybe undefined id . lookup r <$> ask

-------------------------------------------------------------------
--- actual

class AWS r m where
    aws :: r -> m (Rs r)

listObjectNamesUsingS3 :: (Monad m, AWS ListObjectsV2 m) => Text -> m [Text]
listObjectNamesUsingS3 repo = do
    rs <- aws $ listObjectsV2 $ BucketName repo
    pure $ (\(ObjectKey k) -> k) . view oKey <$> (rs ^. lovrsContents)

