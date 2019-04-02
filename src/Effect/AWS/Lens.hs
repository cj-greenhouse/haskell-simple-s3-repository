{-# LANGUAGE TemplateHaskell #-}

module Effect.AWS.Lens where

import Control.Lens.TH
import Data.ByteString.Lazy as BSL (ByteString)
import Network.AWS.S3

data AWSORqPutObject = AWSOrqPutObject {
    _aopoRequest :: PutObject,
    _aopoData :: BSL.ByteString
    }

makeLenses ''AWSORqPutObject


data AWSOrqGetObjectResponse = AWSOrqGetObjectResponse {
    _aogoResponse :: GetObjectResponse,
    _aogoData :: BSL.ByteString
    }

makeLenses ''AWSOrqGetObjectResponse
