module Spec.Distribution.Simple.ObjectStoreS3 (tests) where

import Spec.Prelude

import Conduit
import Control.Lens
import Control.Monad.Trans.Reader
import Data.ByteString (ByteString)
import Data.List (sort)
import Data.Text (Text)
import Data.Time
import Network.AWS (AWSRequest (..))
import Network.AWS.Data.Body (RsBody (..))
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
        value ([(req, resp)], mempty) actual === [k1,k2]
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
        sort (value ([(req1, resp1),(req2, resp2),(req3, resp3)], mempty) actual) === sort (ns)
    ,
    testCase "fetch object" $ do
        let
        -- given
            key = "okey"
            repo = "repo99"
            obj = "blob"
            etag = Just $ ETag "blobhash"
            req = getObject (BucketName repo) (ObjectKey key)
            resp = getObjectResponse 200 (RsBody $ yieldMany []) & gorsETag .~ etag
            extract rs = pure $ if (rs ^. gorsETag == etag)
                            then obj
                            else "NOT OBJECT"

        -- when
            actual = fetchObjectUsingS3 extract repo key

        -- then
        value (mempty, [(req,resp)]) actual === obj


    ]

type Env = ([(ListObjectsV2, ListObjectsV2Response)], [(GetObject, GetObjectResponse)])
type Test = Reader Env

value :: Env -> Test a -> a
value = flip runReader

instance AWS ListObjectsV2 Test where
    aws r = maybe undefined id . lookup r . fst <$> ask

instance AWS GetObject Test where
    aws r = maybe undefined id . lookup r . snd <$> ask

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

-- this function requires extraction configuration because the Amazonka
-- get object response contains an IO-coupled conduit and we can't perform
-- pure testing with it AFAICT (research this)
fetchObjectUsingS3 :: (Monad m, AWS GetObject m) => (GetObjectResponse -> m ByteString) -> Text -> Text -> m ByteString
fetchObjectUsingS3 extract repo key = do
    resp <- aws $ getObject (BucketName repo) (ObjectKey key)
    extract resp
