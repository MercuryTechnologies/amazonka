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
-- Module      : Amazonka.EC2.DescribeMovingAddresses
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes your Elastic IP addresses that are being moved to the EC2-VPC
-- platform, or that are being restored to the EC2-Classic platform. This
-- request does not return information about any other Elastic IP addresses
-- in your account.
--
-- This operation returns paginated results.
module Amazonka.EC2.DescribeMovingAddresses
  ( -- * Creating a Request
    DescribeMovingAddresses (..),
    newDescribeMovingAddresses,

    -- * Request Lenses
    describeMovingAddresses_filters,
    describeMovingAddresses_publicIps,
    describeMovingAddresses_nextToken,
    describeMovingAddresses_dryRun,
    describeMovingAddresses_maxResults,

    -- * Destructuring the Response
    DescribeMovingAddressesResponse (..),
    newDescribeMovingAddressesResponse,

    -- * Response Lenses
    describeMovingAddressesResponse_movingAddressStatuses,
    describeMovingAddressesResponse_nextToken,
    describeMovingAddressesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeMovingAddresses' smart constructor.
data DescribeMovingAddresses = DescribeMovingAddresses'
  { -- | One or more filters.
    --
    -- -   @moving-status@ - The status of the Elastic IP address
    --     (@MovingToVpc@ | @RestoringToClassic@).
    filters :: Prelude.Maybe [Filter],
    -- | One or more Elastic IP addresses.
    publicIps :: Prelude.Maybe [Prelude.Text],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return for the request in a single
    -- page. The remaining results of the initial request can be seen by
    -- sending another request with the returned @NextToken@ value. This value
    -- can be between 5 and 1000; if @MaxResults@ is given a value outside of
    -- this range, an error is returned.
    --
    -- Default: If no value is provided, the default is 1000.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMovingAddresses' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeMovingAddresses_filters' - One or more filters.
--
-- -   @moving-status@ - The status of the Elastic IP address
--     (@MovingToVpc@ | @RestoringToClassic@).
--
-- 'publicIps', 'describeMovingAddresses_publicIps' - One or more Elastic IP addresses.
--
-- 'nextToken', 'describeMovingAddresses_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'describeMovingAddresses_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'describeMovingAddresses_maxResults' - The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value outside of
-- this range, an error is returned.
--
-- Default: If no value is provided, the default is 1000.
newDescribeMovingAddresses ::
  DescribeMovingAddresses
newDescribeMovingAddresses =
  DescribeMovingAddresses'
    { filters = Prelude.Nothing,
      publicIps = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | One or more filters.
--
-- -   @moving-status@ - The status of the Elastic IP address
--     (@MovingToVpc@ | @RestoringToClassic@).
describeMovingAddresses_filters :: Lens.Lens' DescribeMovingAddresses (Prelude.Maybe [Filter])
describeMovingAddresses_filters = Lens.lens (\DescribeMovingAddresses' {filters} -> filters) (\s@DescribeMovingAddresses' {} a -> s {filters = a} :: DescribeMovingAddresses) Prelude.. Lens.mapping Lens.coerced

-- | One or more Elastic IP addresses.
describeMovingAddresses_publicIps :: Lens.Lens' DescribeMovingAddresses (Prelude.Maybe [Prelude.Text])
describeMovingAddresses_publicIps = Lens.lens (\DescribeMovingAddresses' {publicIps} -> publicIps) (\s@DescribeMovingAddresses' {} a -> s {publicIps = a} :: DescribeMovingAddresses) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
describeMovingAddresses_nextToken :: Lens.Lens' DescribeMovingAddresses (Prelude.Maybe Prelude.Text)
describeMovingAddresses_nextToken = Lens.lens (\DescribeMovingAddresses' {nextToken} -> nextToken) (\s@DescribeMovingAddresses' {} a -> s {nextToken = a} :: DescribeMovingAddresses)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeMovingAddresses_dryRun :: Lens.Lens' DescribeMovingAddresses (Prelude.Maybe Prelude.Bool)
describeMovingAddresses_dryRun = Lens.lens (\DescribeMovingAddresses' {dryRun} -> dryRun) (\s@DescribeMovingAddresses' {} a -> s {dryRun = a} :: DescribeMovingAddresses)

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value outside of
-- this range, an error is returned.
--
-- Default: If no value is provided, the default is 1000.
describeMovingAddresses_maxResults :: Lens.Lens' DescribeMovingAddresses (Prelude.Maybe Prelude.Natural)
describeMovingAddresses_maxResults = Lens.lens (\DescribeMovingAddresses' {maxResults} -> maxResults) (\s@DescribeMovingAddresses' {} a -> s {maxResults = a} :: DescribeMovingAddresses)

instance Core.AWSPager DescribeMovingAddresses where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeMovingAddressesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeMovingAddressesResponse_movingAddressStatuses
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeMovingAddresses_nextToken
          Lens..~ rs
          Lens.^? describeMovingAddressesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeMovingAddresses where
  type
    AWSResponse DescribeMovingAddresses =
      DescribeMovingAddressesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeMovingAddressesResponse'
            Prelude.<$> ( x Core..@? "movingAddressStatusSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeMovingAddresses where
  hashWithSalt salt' DescribeMovingAddresses' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` publicIps
      `Prelude.hashWithSalt` filters

instance Prelude.NFData DescribeMovingAddresses where
  rnf DescribeMovingAddresses' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf publicIps

instance Core.ToHeaders DescribeMovingAddresses where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeMovingAddresses where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeMovingAddresses where
  toQuery DescribeMovingAddresses' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("DescribeMovingAddresses" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        Core.toQuery
          (Core.toQueryList "PublicIp" Prelude.<$> publicIps),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newDescribeMovingAddressesResponse' smart constructor.
data DescribeMovingAddressesResponse = DescribeMovingAddressesResponse'
  { -- | The status for each Elastic IP address.
    movingAddressStatuses :: Prelude.Maybe [MovingAddressStatus],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeMovingAddressesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'movingAddressStatuses', 'describeMovingAddressesResponse_movingAddressStatuses' - The status for each Elastic IP address.
--
-- 'nextToken', 'describeMovingAddressesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeMovingAddressesResponse_httpStatus' - The response's http status code.
newDescribeMovingAddressesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeMovingAddressesResponse
newDescribeMovingAddressesResponse pHttpStatus_ =
  DescribeMovingAddressesResponse'
    { movingAddressStatuses =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status for each Elastic IP address.
describeMovingAddressesResponse_movingAddressStatuses :: Lens.Lens' DescribeMovingAddressesResponse (Prelude.Maybe [MovingAddressStatus])
describeMovingAddressesResponse_movingAddressStatuses = Lens.lens (\DescribeMovingAddressesResponse' {movingAddressStatuses} -> movingAddressStatuses) (\s@DescribeMovingAddressesResponse' {} a -> s {movingAddressStatuses = a} :: DescribeMovingAddressesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
describeMovingAddressesResponse_nextToken :: Lens.Lens' DescribeMovingAddressesResponse (Prelude.Maybe Prelude.Text)
describeMovingAddressesResponse_nextToken = Lens.lens (\DescribeMovingAddressesResponse' {nextToken} -> nextToken) (\s@DescribeMovingAddressesResponse' {} a -> s {nextToken = a} :: DescribeMovingAddressesResponse)

-- | The response's http status code.
describeMovingAddressesResponse_httpStatus :: Lens.Lens' DescribeMovingAddressesResponse Prelude.Int
describeMovingAddressesResponse_httpStatus = Lens.lens (\DescribeMovingAddressesResponse' {httpStatus} -> httpStatus) (\s@DescribeMovingAddressesResponse' {} a -> s {httpStatus = a} :: DescribeMovingAddressesResponse)

instance
  Prelude.NFData
    DescribeMovingAddressesResponse
  where
  rnf DescribeMovingAddressesResponse' {..} =
    Prelude.rnf movingAddressStatuses
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
