{-# LANGUAGE UndecidableInstances #-}
module Effect.AWS (
    AWSOrqGetObjectResponse (..),
    AWSORqPutObject (..),
) where

import Control.Lens
import Data.ByteString.Lazy as BSL (ByteString)
import Data.Text (Text)
import Network.AWS (AWSRequest (..))
import Network.AWS.Prelude
import Network.AWS.S3
import Network.AWS.S3.Types

import Effect.AWS.Lens

class AWSOp' r m where
    withAWS' :: r -> m (Rs r)

class AWSOpRequest r where
    type AWSORq r = (rq :: *) | rq -> r
    type AWSORq r = r

class AWSOpResponse r where
    type AWSORs r = (rs :: *) | rs -> r
    type AWSORs r = (Rs r)


class AWSOp r m where
    withAWS :: AWSORq r -> m (AWSORs r)

instance (AWSORq r ~ r, AWSORs r ~ Rs r, AWSOp' r m) => AWSOp r m where
    withAWS = withAWS'



class ProjectEq a where
    type PEQ a = (peq :: *) | peq -> a
    projectEq :: a -> PEQ a

instance (ProjectEq a, Eq (PEQ a)) => Eq a where
    x == y = projectEq x == projectEq y




instance ProjectEq AWSOrqGetObjectResponse where
    type PEQ AWSOrqGetObjectResponse = (
        Maybe RequestCharged,
        Maybe Int,
        Maybe ETag,
        Maybe ObjectVersionId,
        Maybe Integer,
        Maybe UTCTime,
        Maybe Text,
        Maybe Text,
        Maybe Bool,
        Maybe Text,
        Maybe Int,
        Maybe Int,
        Maybe Text,
        Maybe Text,
        Maybe StorageClass,
        Maybe Text,
        Maybe Text,
        Maybe Text,
        HashMap Text Text,
        Maybe ReplicationStatus,
        Maybe Text,
        Maybe Text,
        Maybe UTCTime,
        Maybe Text,
        Maybe Text,
        Maybe ServerSideEncryption,
        Maybe Text,
        Int,
        BSL.ByteString
        )
    projectEq a = (
        a ^. aogoResponse . gorsRequestCharged,
        a ^. aogoResponse . gorsPartsCount,
        a ^. aogoResponse . gorsETag,
        a ^. aogoResponse . gorsVersionId,
        a ^. aogoResponse . gorsContentLength,
        a ^. aogoResponse . gorsExpires,
        a ^. aogoResponse . gorsRestore,
        a ^. aogoResponse . gorsExpiration,
        a ^. aogoResponse . gorsDeleteMarker,
        a ^. aogoResponse . gorsSSECustomerAlgorithm,
        a ^. aogoResponse . gorsTagCount,
        a ^. aogoResponse . gorsMissingMeta,
        a ^. aogoResponse . gorsWebsiteRedirectLocation,
        a ^. aogoResponse . gorsAcceptRanges,
        a ^. aogoResponse . gorsStorageClass,
        a ^. aogoResponse . gorsSSECustomerKeyMD5,
        a ^. aogoResponse . gorsSSEKMSKeyId,
        a ^. aogoResponse . gorsContentEncoding,
        a ^. aogoResponse . gorsMetadata,
        a ^. aogoResponse . gorsReplicationStatus,
        a ^. aogoResponse . gorsCacheControl,
        a ^. aogoResponse . gorsContentLanguage,
        a ^. aogoResponse . gorsLastModified,
        a ^. aogoResponse . gorsContentDisposition,
        a ^. aogoResponse . gorsContentRange,
        a ^. aogoResponse . gorsServerSideEncryption,
        a ^. aogoResponse . gorsContentType,
        a ^. aogoResponse . gorsResponseStatus,
        a ^. aogoData
        )

instance AWSOpResponse GetObject where
    type AWSORs GetObject = AWSOrqGetObjectResponse


instance ProjectEq AWSORqPutObject where
    type PEQ AWSORqPutObject = (
        Maybe Integer,
        Maybe UTCTime,
        Maybe Text,
        Maybe Text,
        Maybe Text,
        Maybe RequestPayer,
        Maybe Text,
        Maybe Text,
        Maybe Text,
        Maybe StorageClass,
        Maybe Text,
        Maybe Text,
        Maybe Text,
        Maybe Text,
        Maybe Text,
        Maybe Text,
        HashMap Text Text,
        Maybe Text,
        Maybe Text,
        Maybe ObjectCannedACL,
        Maybe Text,
        Maybe ServerSideEncryption,
        Maybe Text,
        BucketName,
        ObjectKey,
        BSL.ByteString
        )
    projectEq a = (
        a ^. aopoRequest . poContentLength,
        a ^. aopoRequest . poExpires,
        a ^. aopoRequest . poGrantReadACP,
        a ^. aopoRequest . poSSECustomerAlgorithm,
        a ^. aopoRequest . poSSECustomerKey,
        a ^. aopoRequest . poRequestPayer,
        a ^. aopoRequest . poGrantWriteACP,
        a ^. aopoRequest . poWebsiteRedirectLocation,
        a ^. aopoRequest . poGrantRead,
        a ^. aopoRequest . poStorageClass,
        a ^. aopoRequest . poSSECustomerKeyMD5,
        a ^. aopoRequest . poSSEKMSKeyId,
        a ^. aopoRequest . poGrantFullControl,
        a ^. aopoRequest . poContentEncoding,
        a ^. aopoRequest . poTagging,
        a ^. aopoRequest . poContentMD5,
        a ^. aopoRequest . poMetadata,
        a ^. aopoRequest . poCacheControl,
        a ^. aopoRequest . poContentLanguage,
        a ^. aopoRequest . poACL,
        a ^. aopoRequest . poContentDisposition,
        a ^. aopoRequest . poServerSideEncryption,
        a ^. aopoRequest . poContentType,
        a ^. aopoRequest . poBucket,
        a ^. aopoRequest . poKey,
        a ^. aopoData
        )

instance AWSOpRequest PutObject where
    type AWSORq PutObject = AWSORqPutObject



