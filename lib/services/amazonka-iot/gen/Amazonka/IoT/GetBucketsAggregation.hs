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
-- Module      : Amazonka.IoT.GetBucketsAggregation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Aggregates on indexed data with search queries pertaining to particular
-- fields.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetBucketsAggregation>
-- action.
module Amazonka.IoT.GetBucketsAggregation
  ( -- * Creating a Request
    GetBucketsAggregation (..),
    newGetBucketsAggregation,

    -- * Request Lenses
    getBucketsAggregation_queryVersion,
    getBucketsAggregation_indexName,
    getBucketsAggregation_queryString,
    getBucketsAggregation_aggregationField,
    getBucketsAggregation_bucketsAggregationType,

    -- * Destructuring the Response
    GetBucketsAggregationResponse (..),
    newGetBucketsAggregationResponse,

    -- * Response Lenses
    getBucketsAggregationResponse_buckets,
    getBucketsAggregationResponse_totalCount,
    getBucketsAggregationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBucketsAggregation' smart constructor.
data GetBucketsAggregation = GetBucketsAggregation'
  { -- | The version of the query.
    queryVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the index to search.
    indexName :: Prelude.Maybe Prelude.Text,
    -- | The search query string.
    queryString :: Prelude.Text,
    -- | The aggregation field.
    aggregationField :: Prelude.Text,
    -- | The basic control of the response shape and the bucket aggregation type
    -- to perform.
    bucketsAggregationType :: BucketsAggregationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketsAggregation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryVersion', 'getBucketsAggregation_queryVersion' - The version of the query.
--
-- 'indexName', 'getBucketsAggregation_indexName' - The name of the index to search.
--
-- 'queryString', 'getBucketsAggregation_queryString' - The search query string.
--
-- 'aggregationField', 'getBucketsAggregation_aggregationField' - The aggregation field.
--
-- 'bucketsAggregationType', 'getBucketsAggregation_bucketsAggregationType' - The basic control of the response shape and the bucket aggregation type
-- to perform.
newGetBucketsAggregation ::
  -- | 'queryString'
  Prelude.Text ->
  -- | 'aggregationField'
  Prelude.Text ->
  -- | 'bucketsAggregationType'
  BucketsAggregationType ->
  GetBucketsAggregation
newGetBucketsAggregation
  pQueryString_
  pAggregationField_
  pBucketsAggregationType_ =
    GetBucketsAggregation'
      { queryVersion =
          Prelude.Nothing,
        indexName = Prelude.Nothing,
        queryString = pQueryString_,
        aggregationField = pAggregationField_,
        bucketsAggregationType = pBucketsAggregationType_
      }

-- | The version of the query.
getBucketsAggregation_queryVersion :: Lens.Lens' GetBucketsAggregation (Prelude.Maybe Prelude.Text)
getBucketsAggregation_queryVersion = Lens.lens (\GetBucketsAggregation' {queryVersion} -> queryVersion) (\s@GetBucketsAggregation' {} a -> s {queryVersion = a} :: GetBucketsAggregation)

-- | The name of the index to search.
getBucketsAggregation_indexName :: Lens.Lens' GetBucketsAggregation (Prelude.Maybe Prelude.Text)
getBucketsAggregation_indexName = Lens.lens (\GetBucketsAggregation' {indexName} -> indexName) (\s@GetBucketsAggregation' {} a -> s {indexName = a} :: GetBucketsAggregation)

-- | The search query string.
getBucketsAggregation_queryString :: Lens.Lens' GetBucketsAggregation Prelude.Text
getBucketsAggregation_queryString = Lens.lens (\GetBucketsAggregation' {queryString} -> queryString) (\s@GetBucketsAggregation' {} a -> s {queryString = a} :: GetBucketsAggregation)

-- | The aggregation field.
getBucketsAggregation_aggregationField :: Lens.Lens' GetBucketsAggregation Prelude.Text
getBucketsAggregation_aggregationField = Lens.lens (\GetBucketsAggregation' {aggregationField} -> aggregationField) (\s@GetBucketsAggregation' {} a -> s {aggregationField = a} :: GetBucketsAggregation)

-- | The basic control of the response shape and the bucket aggregation type
-- to perform.
getBucketsAggregation_bucketsAggregationType :: Lens.Lens' GetBucketsAggregation BucketsAggregationType
getBucketsAggregation_bucketsAggregationType = Lens.lens (\GetBucketsAggregation' {bucketsAggregationType} -> bucketsAggregationType) (\s@GetBucketsAggregation' {} a -> s {bucketsAggregationType = a} :: GetBucketsAggregation)

instance Core.AWSRequest GetBucketsAggregation where
  type
    AWSResponse GetBucketsAggregation =
      GetBucketsAggregationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBucketsAggregationResponse'
            Prelude.<$> (x Core..?> "buckets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "totalCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketsAggregation where
  hashWithSalt salt' GetBucketsAggregation' {..} =
    salt' `Prelude.hashWithSalt` bucketsAggregationType
      `Prelude.hashWithSalt` aggregationField
      `Prelude.hashWithSalt` queryString
      `Prelude.hashWithSalt` indexName
      `Prelude.hashWithSalt` queryVersion

instance Prelude.NFData GetBucketsAggregation where
  rnf GetBucketsAggregation' {..} =
    Prelude.rnf queryVersion
      `Prelude.seq` Prelude.rnf bucketsAggregationType
      `Prelude.seq` Prelude.rnf aggregationField
      `Prelude.seq` Prelude.rnf queryString
      `Prelude.seq` Prelude.rnf indexName

instance Core.ToHeaders GetBucketsAggregation where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetBucketsAggregation where
  toJSON GetBucketsAggregation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("queryVersion" Core..=) Prelude.<$> queryVersion,
            ("indexName" Core..=) Prelude.<$> indexName,
            Prelude.Just ("queryString" Core..= queryString),
            Prelude.Just
              ("aggregationField" Core..= aggregationField),
            Prelude.Just
              ( "bucketsAggregationType"
                  Core..= bucketsAggregationType
              )
          ]
      )

instance Core.ToPath GetBucketsAggregation where
  toPath = Prelude.const "/indices/buckets"

instance Core.ToQuery GetBucketsAggregation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBucketsAggregationResponse' smart constructor.
data GetBucketsAggregationResponse = GetBucketsAggregationResponse'
  { -- | The main part of the response with a list of buckets. Each bucket
    -- contains a @keyValue@ and a @count@.
    --
    -- @keyValue@: The aggregation field value counted for the particular
    -- bucket.
    --
    -- @count@: The number of documents that have that value.
    buckets :: Prelude.Maybe [Bucket],
    -- | The total number of documents that fit the query string criteria and
    -- contain a value for the Aggregation field targeted in the request.
    totalCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketsAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'buckets', 'getBucketsAggregationResponse_buckets' - The main part of the response with a list of buckets. Each bucket
-- contains a @keyValue@ and a @count@.
--
-- @keyValue@: The aggregation field value counted for the particular
-- bucket.
--
-- @count@: The number of documents that have that value.
--
-- 'totalCount', 'getBucketsAggregationResponse_totalCount' - The total number of documents that fit the query string criteria and
-- contain a value for the Aggregation field targeted in the request.
--
-- 'httpStatus', 'getBucketsAggregationResponse_httpStatus' - The response's http status code.
newGetBucketsAggregationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketsAggregationResponse
newGetBucketsAggregationResponse pHttpStatus_ =
  GetBucketsAggregationResponse'
    { buckets =
        Prelude.Nothing,
      totalCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The main part of the response with a list of buckets. Each bucket
-- contains a @keyValue@ and a @count@.
--
-- @keyValue@: The aggregation field value counted for the particular
-- bucket.
--
-- @count@: The number of documents that have that value.
getBucketsAggregationResponse_buckets :: Lens.Lens' GetBucketsAggregationResponse (Prelude.Maybe [Bucket])
getBucketsAggregationResponse_buckets = Lens.lens (\GetBucketsAggregationResponse' {buckets} -> buckets) (\s@GetBucketsAggregationResponse' {} a -> s {buckets = a} :: GetBucketsAggregationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The total number of documents that fit the query string criteria and
-- contain a value for the Aggregation field targeted in the request.
getBucketsAggregationResponse_totalCount :: Lens.Lens' GetBucketsAggregationResponse (Prelude.Maybe Prelude.Int)
getBucketsAggregationResponse_totalCount = Lens.lens (\GetBucketsAggregationResponse' {totalCount} -> totalCount) (\s@GetBucketsAggregationResponse' {} a -> s {totalCount = a} :: GetBucketsAggregationResponse)

-- | The response's http status code.
getBucketsAggregationResponse_httpStatus :: Lens.Lens' GetBucketsAggregationResponse Prelude.Int
getBucketsAggregationResponse_httpStatus = Lens.lens (\GetBucketsAggregationResponse' {httpStatus} -> httpStatus) (\s@GetBucketsAggregationResponse' {} a -> s {httpStatus = a} :: GetBucketsAggregationResponse)

instance Prelude.NFData GetBucketsAggregationResponse where
  rnf GetBucketsAggregationResponse' {..} =
    Prelude.rnf buckets
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf totalCount
