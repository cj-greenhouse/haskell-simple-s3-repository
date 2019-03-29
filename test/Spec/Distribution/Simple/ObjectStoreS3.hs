module Spec.Distribution.Simple.ObjectStoreS3 (tests) where

import Spec.Prelude

import Control.Lens
import Control.Monad.Trans.Reader
import  Data.List (sort)
import Data.Text (Text)
import Data.Time
import Network.AWS (AWSRequest (..))
import Network.AWS.S3
import Network.AWS.Pager (page)

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
    ,
    testCase "big listing" $ do
        let
        -- given
            k1 = "object1"
            k2 = "object2"
            k3 = "object3"
            k4 = "object4"
            k5 = "object5"
            ns = [k1, k2, k3, k4, k5]
            repo = "store-repo"
            tok1 = "foo"
            tok2 = "bar"
            os = (\n -> object' (ETag "") 0 (ObjectKey n) OSCStandard (UTCTime (toEnum 0) 1093)) <$> ns
            req1 = listObjectsV2 $ BucketName repo
            resp1 = listObjectsV2Response 200
                    & lovrsContents .~ (take 2 os)
                    & lovrsNextContinuationToken .~ (Just tok1)
                    & lovrsIsTruncated .~ Just True
            req2 = maybe undefined id $ page req1 resp1
            resp2 = listObjectsV2Response 200
                    & lovrsContents .~ (take 1 (drop 2 os))
                    & lovrsNextContinuationToken .~ (Just tok2)
                    & lovrsIsTruncated .~ Just True
            req3 = maybe undefined id $ page req2 resp2
            resp3 = listObjectsV2Response 200
                    & lovrsContents .~ (drop 3 os)

        -- when
            actual = listObjectNamesUsingS3 repo

        -- then
        sort (value [(req1, resp1),(req2, resp2),(req3, resp3)] actual) === sort (ns)


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
    rss <- accum [] (listObjectsV2 (BucketName repo))
    let os = concat $ view lovrsContents <$> rss
        ks = (\(ObjectKey k) -> k) . view oKey <$> os
    pure ks
    where
        -- let's refactor this to paginate (Conduit)
        accum rac rq = do
            rs <- aws rq
            let nrac = (rs:rac)
            case page rq rs of
                Nothing -> pure nrac
                Just next -> accum nrac next

