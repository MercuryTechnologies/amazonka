{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Lightsail.GetBuckets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about one or more Amazon Lightsail buckets.
--
-- For more information about buckets, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/buckets-in-amazon-lightsail Buckets in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/..
module Amazonka.Lightsail.GetBuckets
  ( -- * Creating a Request
    GetBuckets (..),
    newGetBuckets,

    -- * Request Lenses
    getBuckets_bucketName,
    getBuckets_includeConnectedResources,
    getBuckets_pageToken,

    -- * Destructuring the Response
    GetBucketsResponse (..),
    newGetBucketsResponse,

    -- * Response Lenses
    getBucketsResponse_nextPageToken,
    getBucketsResponse_buckets,
    getBucketsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBuckets' smart constructor.
data GetBuckets = GetBuckets'
  { -- | The name of the bucket for which to return information.
    --
    -- When omitted, the response includes all of your buckets in the AWS
    -- Region where the request is made.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value that indicates whether to include Lightsail instances
    -- that were given access to the bucket using the
    -- SetResourceAccessForBucket action.
    includeConnectedResources :: Prelude.Maybe Prelude.Bool,
    -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetBuckets@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBuckets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'getBuckets_bucketName' - The name of the bucket for which to return information.
--
-- When omitted, the response includes all of your buckets in the AWS
-- Region where the request is made.
--
-- 'includeConnectedResources', 'getBuckets_includeConnectedResources' - A Boolean value that indicates whether to include Lightsail instances
-- that were given access to the bucket using the
-- SetResourceAccessForBucket action.
--
-- 'pageToken', 'getBuckets_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBuckets@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
newGetBuckets ::
  GetBuckets
newGetBuckets =
  GetBuckets'
    { bucketName = Prelude.Nothing,
      includeConnectedResources = Prelude.Nothing,
      pageToken = Prelude.Nothing
    }

-- | The name of the bucket for which to return information.
--
-- When omitted, the response includes all of your buckets in the AWS
-- Region where the request is made.
getBuckets_bucketName :: Lens.Lens' GetBuckets (Prelude.Maybe Prelude.Text)
getBuckets_bucketName = Lens.lens (\GetBuckets' {bucketName} -> bucketName) (\s@GetBuckets' {} a -> s {bucketName = a} :: GetBuckets)

-- | A Boolean value that indicates whether to include Lightsail instances
-- that were given access to the bucket using the
-- SetResourceAccessForBucket action.
getBuckets_includeConnectedResources :: Lens.Lens' GetBuckets (Prelude.Maybe Prelude.Bool)
getBuckets_includeConnectedResources = Lens.lens (\GetBuckets' {includeConnectedResources} -> includeConnectedResources) (\s@GetBuckets' {} a -> s {includeConnectedResources = a} :: GetBuckets)

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetBuckets@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getBuckets_pageToken :: Lens.Lens' GetBuckets (Prelude.Maybe Prelude.Text)
getBuckets_pageToken = Lens.lens (\GetBuckets' {pageToken} -> pageToken) (\s@GetBuckets' {} a -> s {pageToken = a} :: GetBuckets)

instance Core.AWSRequest GetBuckets where
  type AWSResponse GetBuckets = GetBucketsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBucketsResponse'
            Prelude.<$> (x Core..?> "nextPageToken")
            Prelude.<*> (x Core..?> "buckets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBuckets where
  hashWithSalt salt' GetBuckets' {..} =
    salt' `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` includeConnectedResources
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData GetBuckets where
  rnf GetBuckets' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf includeConnectedResources

instance Core.ToHeaders GetBuckets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetBuckets" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetBuckets where
  toJSON GetBuckets' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("bucketName" Core..=) Prelude.<$> bucketName,
            ("includeConnectedResources" Core..=)
              Prelude.<$> includeConnectedResources,
            ("pageToken" Core..=) Prelude.<$> pageToken
          ]
      )

instance Core.ToPath GetBuckets where
  toPath = Prelude.const "/"

instance Core.ToQuery GetBuckets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBucketsResponse' smart constructor.
data GetBucketsResponse = GetBucketsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetBuckets@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe buckets.
    buckets :: Prelude.Maybe [Bucket],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getBucketsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetBuckets@ request
-- and specify the next page token using the @pageToken@ parameter.
--
-- 'buckets', 'getBucketsResponse_buckets' - An array of objects that describe buckets.
--
-- 'httpStatus', 'getBucketsResponse_httpStatus' - The response's http status code.
newGetBucketsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketsResponse
newGetBucketsResponse pHttpStatus_ =
  GetBucketsResponse'
    { nextPageToken =
        Prelude.Nothing,
      buckets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetBuckets@ request
-- and specify the next page token using the @pageToken@ parameter.
getBucketsResponse_nextPageToken :: Lens.Lens' GetBucketsResponse (Prelude.Maybe Prelude.Text)
getBucketsResponse_nextPageToken = Lens.lens (\GetBucketsResponse' {nextPageToken} -> nextPageToken) (\s@GetBucketsResponse' {} a -> s {nextPageToken = a} :: GetBucketsResponse)

-- | An array of objects that describe buckets.
getBucketsResponse_buckets :: Lens.Lens' GetBucketsResponse (Prelude.Maybe [Bucket])
getBucketsResponse_buckets = Lens.lens (\GetBucketsResponse' {buckets} -> buckets) (\s@GetBucketsResponse' {} a -> s {buckets = a} :: GetBucketsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getBucketsResponse_httpStatus :: Lens.Lens' GetBucketsResponse Prelude.Int
getBucketsResponse_httpStatus = Lens.lens (\GetBucketsResponse' {httpStatus} -> httpStatus) (\s@GetBucketsResponse' {} a -> s {httpStatus = a} :: GetBucketsResponse)

instance Prelude.NFData GetBucketsResponse where
  rnf GetBucketsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf buckets
