{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.ListBuckets
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of all buckets owned by the authenticated sender of the
-- request.
module Network.AWS.S3.V2006_03_01.ListBuckets where

import           Data.ByteString     (ByteString)
import           Data.Default
import           Data.HashMap.Strict (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Request.RestS3
import           Network.AWS.Types   hiding (Error)
import           Network.AWS.S3.V2006_03_01.Types
import           Prelude             hiding (head)

-- | Smart constructor utilising default fields to
-- specify the minimum viable ListBuckets request.
listBuckets :: ListBuckets
listBuckets = ListBuckets

data ListBuckets = ListBuckets
    deriving (Eq, Show, Generic)

instance ToPath ListBuckets where
    toPath = const "/"

instance ToQuery ListBuckets

instance ToHeaders ListBuckets

instance ToBody ListBuckets

instance AWSRequest ListBuckets where
    type Sv ListBuckets = S3

    request  = get
    response = undefined

data instance Rs ListBuckets = ListBucketsResponse
    { lboBuckets :: [Bucket]
    , lboOwner :: Maybe Owner
    } deriving (Eq, Show, Generic)
