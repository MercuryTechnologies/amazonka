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
-- Module      : Amazonka.RobOMaker.ListWorlds
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists worlds.
--
-- This operation returns paginated results.
module Amazonka.RobOMaker.ListWorlds
  ( -- * Creating a Request
    ListWorlds (..),
    newListWorlds,

    -- * Request Lenses
    listWorlds_filters,
    listWorlds_nextToken,
    listWorlds_maxResults,

    -- * Destructuring the Response
    ListWorldsResponse (..),
    newListWorldsResponse,

    -- * Response Lenses
    listWorldsResponse_nextToken,
    listWorldsResponse_worldSummaries,
    listWorldsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RobOMaker.Types

-- | /See:/ 'newListWorlds' smart constructor.
data ListWorlds = ListWorlds'
  { -- | Optional filters to limit results. You can use @status@.
    filters :: Prelude.Maybe (Prelude.NonEmpty Filter),
    -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListWorlds@ again and
    -- assign that token to the request object\'s @nextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | When this parameter is used, @ListWorlds@ only returns @maxResults@
    -- results in a single page along with a @nextToken@ response element. The
    -- remaining results of the initial request can be seen by sending another
    -- @ListWorlds@ request with the returned @nextToken@ value. This value can
    -- be between 1 and 100. If this parameter is not used, then @ListWorlds@
    -- returns up to 100 results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorlds' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listWorlds_filters' - Optional filters to limit results. You can use @status@.
--
-- 'nextToken', 'listWorlds_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorlds@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
--
-- 'maxResults', 'listWorlds_maxResults' - When this parameter is used, @ListWorlds@ only returns @maxResults@
-- results in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @ListWorlds@ request with the returned @nextToken@ value. This value can
-- be between 1 and 100. If this parameter is not used, then @ListWorlds@
-- returns up to 100 results and a @nextToken@ value if applicable.
newListWorlds ::
  ListWorlds
newListWorlds =
  ListWorlds'
    { filters = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Optional filters to limit results. You can use @status@.
listWorlds_filters :: Lens.Lens' ListWorlds (Prelude.Maybe (Prelude.NonEmpty Filter))
listWorlds_filters = Lens.lens (\ListWorlds' {filters} -> filters) (\s@ListWorlds' {} a -> s {filters = a} :: ListWorlds) Prelude.. Lens.mapping Lens.coerced

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorlds@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
listWorlds_nextToken :: Lens.Lens' ListWorlds (Prelude.Maybe Prelude.Text)
listWorlds_nextToken = Lens.lens (\ListWorlds' {nextToken} -> nextToken) (\s@ListWorlds' {} a -> s {nextToken = a} :: ListWorlds)

-- | When this parameter is used, @ListWorlds@ only returns @maxResults@
-- results in a single page along with a @nextToken@ response element. The
-- remaining results of the initial request can be seen by sending another
-- @ListWorlds@ request with the returned @nextToken@ value. This value can
-- be between 1 and 100. If this parameter is not used, then @ListWorlds@
-- returns up to 100 results and a @nextToken@ value if applicable.
listWorlds_maxResults :: Lens.Lens' ListWorlds (Prelude.Maybe Prelude.Int)
listWorlds_maxResults = Lens.lens (\ListWorlds' {maxResults} -> maxResults) (\s@ListWorlds' {} a -> s {maxResults = a} :: ListWorlds)

instance Core.AWSPager ListWorlds where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listWorldsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listWorldsResponse_worldSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listWorlds_nextToken
          Lens..~ rs
          Lens.^? listWorldsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListWorlds where
  type AWSResponse ListWorlds = ListWorldsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWorldsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "worldSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWorlds where
  hashWithSalt salt' ListWorlds' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters

instance Prelude.NFData ListWorlds where
  rnf ListWorlds' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders ListWorlds where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListWorlds where
  toJSON ListWorlds' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("filters" Core..=) Prelude.<$> filters,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListWorlds where
  toPath = Prelude.const "/listWorlds"

instance Core.ToQuery ListWorlds where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListWorldsResponse' smart constructor.
data ListWorldsResponse = ListWorldsResponse'
  { -- | If the previous paginated request did not return all of the remaining
    -- results, the response object\'s @nextToken@ parameter value is set to a
    -- token. To retrieve the next set of results, call @ListWorlds@ again and
    -- assign that token to the request object\'s @nextToken@ parameter. If
    -- there are no remaining results, the previous response object\'s
    -- NextToken parameter is set to null.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Summary information for worlds.
    worldSummaries :: Prelude.Maybe [WorldSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWorldsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listWorldsResponse_nextToken' - If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorlds@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
--
-- 'worldSummaries', 'listWorldsResponse_worldSummaries' - Summary information for worlds.
--
-- 'httpStatus', 'listWorldsResponse_httpStatus' - The response's http status code.
newListWorldsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWorldsResponse
newListWorldsResponse pHttpStatus_ =
  ListWorldsResponse'
    { nextToken = Prelude.Nothing,
      worldSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the previous paginated request did not return all of the remaining
-- results, the response object\'s @nextToken@ parameter value is set to a
-- token. To retrieve the next set of results, call @ListWorlds@ again and
-- assign that token to the request object\'s @nextToken@ parameter. If
-- there are no remaining results, the previous response object\'s
-- NextToken parameter is set to null.
listWorldsResponse_nextToken :: Lens.Lens' ListWorldsResponse (Prelude.Maybe Prelude.Text)
listWorldsResponse_nextToken = Lens.lens (\ListWorldsResponse' {nextToken} -> nextToken) (\s@ListWorldsResponse' {} a -> s {nextToken = a} :: ListWorldsResponse)

-- | Summary information for worlds.
listWorldsResponse_worldSummaries :: Lens.Lens' ListWorldsResponse (Prelude.Maybe [WorldSummary])
listWorldsResponse_worldSummaries = Lens.lens (\ListWorldsResponse' {worldSummaries} -> worldSummaries) (\s@ListWorldsResponse' {} a -> s {worldSummaries = a} :: ListWorldsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listWorldsResponse_httpStatus :: Lens.Lens' ListWorldsResponse Prelude.Int
listWorldsResponse_httpStatus = Lens.lens (\ListWorldsResponse' {httpStatus} -> httpStatus) (\s@ListWorldsResponse' {} a -> s {httpStatus = a} :: ListWorldsResponse)

instance Prelude.NFData ListWorldsResponse where
  rnf ListWorldsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf worldSummaries
