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
-- Module      : Amazonka.EC2.SearchLocalGatewayRoutes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for routes in the specified local gateway route table.
--
-- This operation returns paginated results.
module Amazonka.EC2.SearchLocalGatewayRoutes
  ( -- * Creating a Request
    SearchLocalGatewayRoutes (..),
    newSearchLocalGatewayRoutes,

    -- * Request Lenses
    searchLocalGatewayRoutes_filters,
    searchLocalGatewayRoutes_nextToken,
    searchLocalGatewayRoutes_dryRun,
    searchLocalGatewayRoutes_maxResults,
    searchLocalGatewayRoutes_localGatewayRouteTableId,

    -- * Destructuring the Response
    SearchLocalGatewayRoutesResponse (..),
    newSearchLocalGatewayRoutesResponse,

    -- * Response Lenses
    searchLocalGatewayRoutesResponse_routes,
    searchLocalGatewayRoutesResponse_nextToken,
    searchLocalGatewayRoutesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchLocalGatewayRoutes' smart constructor.
data SearchLocalGatewayRoutes = SearchLocalGatewayRoutes'
  { -- | One or more filters.
    filters :: Prelude.Maybe [Filter],
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, make another call with the returned @nextToken@
    -- value.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The ID of the local gateway route table.
    localGatewayRouteTableId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchLocalGatewayRoutes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'searchLocalGatewayRoutes_filters' - One or more filters.
--
-- 'nextToken', 'searchLocalGatewayRoutes_nextToken' - The token for the next page of results.
--
-- 'dryRun', 'searchLocalGatewayRoutes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'maxResults', 'searchLocalGatewayRoutes_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
--
-- 'localGatewayRouteTableId', 'searchLocalGatewayRoutes_localGatewayRouteTableId' - The ID of the local gateway route table.
newSearchLocalGatewayRoutes ::
  -- | 'localGatewayRouteTableId'
  Prelude.Text ->
  SearchLocalGatewayRoutes
newSearchLocalGatewayRoutes
  pLocalGatewayRouteTableId_ =
    SearchLocalGatewayRoutes'
      { filters =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        localGatewayRouteTableId =
          pLocalGatewayRouteTableId_
      }

-- | One or more filters.
searchLocalGatewayRoutes_filters :: Lens.Lens' SearchLocalGatewayRoutes (Prelude.Maybe [Filter])
searchLocalGatewayRoutes_filters = Lens.lens (\SearchLocalGatewayRoutes' {filters} -> filters) (\s@SearchLocalGatewayRoutes' {} a -> s {filters = a} :: SearchLocalGatewayRoutes) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next page of results.
searchLocalGatewayRoutes_nextToken :: Lens.Lens' SearchLocalGatewayRoutes (Prelude.Maybe Prelude.Text)
searchLocalGatewayRoutes_nextToken = Lens.lens (\SearchLocalGatewayRoutes' {nextToken} -> nextToken) (\s@SearchLocalGatewayRoutes' {} a -> s {nextToken = a} :: SearchLocalGatewayRoutes)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
searchLocalGatewayRoutes_dryRun :: Lens.Lens' SearchLocalGatewayRoutes (Prelude.Maybe Prelude.Bool)
searchLocalGatewayRoutes_dryRun = Lens.lens (\SearchLocalGatewayRoutes' {dryRun} -> dryRun) (\s@SearchLocalGatewayRoutes' {} a -> s {dryRun = a} :: SearchLocalGatewayRoutes)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, make another call with the returned @nextToken@
-- value.
searchLocalGatewayRoutes_maxResults :: Lens.Lens' SearchLocalGatewayRoutes (Prelude.Maybe Prelude.Int)
searchLocalGatewayRoutes_maxResults = Lens.lens (\SearchLocalGatewayRoutes' {maxResults} -> maxResults) (\s@SearchLocalGatewayRoutes' {} a -> s {maxResults = a} :: SearchLocalGatewayRoutes)

-- | The ID of the local gateway route table.
searchLocalGatewayRoutes_localGatewayRouteTableId :: Lens.Lens' SearchLocalGatewayRoutes Prelude.Text
searchLocalGatewayRoutes_localGatewayRouteTableId = Lens.lens (\SearchLocalGatewayRoutes' {localGatewayRouteTableId} -> localGatewayRouteTableId) (\s@SearchLocalGatewayRoutes' {} a -> s {localGatewayRouteTableId = a} :: SearchLocalGatewayRoutes)

instance Core.AWSPager SearchLocalGatewayRoutes where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchLocalGatewayRoutesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchLocalGatewayRoutesResponse_routes
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchLocalGatewayRoutes_nextToken
          Lens..~ rs
          Lens.^? searchLocalGatewayRoutesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest SearchLocalGatewayRoutes where
  type
    AWSResponse SearchLocalGatewayRoutes =
      SearchLocalGatewayRoutesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          SearchLocalGatewayRoutesResponse'
            Prelude.<$> ( x Core..@? "routeSet" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (x Core..@? "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchLocalGatewayRoutes where
  hashWithSalt salt' SearchLocalGatewayRoutes' {..} =
    salt'
      `Prelude.hashWithSalt` localGatewayRouteTableId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchLocalGatewayRoutes where
  rnf SearchLocalGatewayRoutes' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf localGatewayRouteTableId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders SearchLocalGatewayRoutes where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath SearchLocalGatewayRoutes where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchLocalGatewayRoutes where
  toQuery SearchLocalGatewayRoutes' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("SearchLocalGatewayRoutes" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        Core.toQuery
          (Core.toQueryList "Filter" Prelude.<$> filters),
        "NextToken" Core.=: nextToken,
        "DryRun" Core.=: dryRun,
        "MaxResults" Core.=: maxResults,
        "LocalGatewayRouteTableId"
          Core.=: localGatewayRouteTableId
      ]

-- | /See:/ 'newSearchLocalGatewayRoutesResponse' smart constructor.
data SearchLocalGatewayRoutesResponse = SearchLocalGatewayRoutesResponse'
  { -- | Information about the routes.
    routes :: Prelude.Maybe [LocalGatewayRoute],
    -- | The token to use to retrieve the next page of results. This value is
    -- @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchLocalGatewayRoutesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routes', 'searchLocalGatewayRoutesResponse_routes' - Information about the routes.
--
-- 'nextToken', 'searchLocalGatewayRoutesResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
--
-- 'httpStatus', 'searchLocalGatewayRoutesResponse_httpStatus' - The response's http status code.
newSearchLocalGatewayRoutesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchLocalGatewayRoutesResponse
newSearchLocalGatewayRoutesResponse pHttpStatus_ =
  SearchLocalGatewayRoutesResponse'
    { routes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the routes.
searchLocalGatewayRoutesResponse_routes :: Lens.Lens' SearchLocalGatewayRoutesResponse (Prelude.Maybe [LocalGatewayRoute])
searchLocalGatewayRoutesResponse_routes = Lens.lens (\SearchLocalGatewayRoutesResponse' {routes} -> routes) (\s@SearchLocalGatewayRoutesResponse' {} a -> s {routes = a} :: SearchLocalGatewayRoutesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
searchLocalGatewayRoutesResponse_nextToken :: Lens.Lens' SearchLocalGatewayRoutesResponse (Prelude.Maybe Prelude.Text)
searchLocalGatewayRoutesResponse_nextToken = Lens.lens (\SearchLocalGatewayRoutesResponse' {nextToken} -> nextToken) (\s@SearchLocalGatewayRoutesResponse' {} a -> s {nextToken = a} :: SearchLocalGatewayRoutesResponse)

-- | The response's http status code.
searchLocalGatewayRoutesResponse_httpStatus :: Lens.Lens' SearchLocalGatewayRoutesResponse Prelude.Int
searchLocalGatewayRoutesResponse_httpStatus = Lens.lens (\SearchLocalGatewayRoutesResponse' {httpStatus} -> httpStatus) (\s@SearchLocalGatewayRoutesResponse' {} a -> s {httpStatus = a} :: SearchLocalGatewayRoutesResponse)

instance
  Prelude.NFData
    SearchLocalGatewayRoutesResponse
  where
  rnf SearchLocalGatewayRoutesResponse' {..} =
    Prelude.rnf routes
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
