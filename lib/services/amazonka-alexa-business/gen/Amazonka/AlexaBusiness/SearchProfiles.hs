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
-- Module      : Amazonka.AlexaBusiness.SearchProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches room profiles and lists the ones that meet a set of filter
-- criteria.
--
-- This operation returns paginated results.
module Amazonka.AlexaBusiness.SearchProfiles
  ( -- * Creating a Request
    SearchProfiles (..),
    newSearchProfiles,

    -- * Request Lenses
    searchProfiles_filters,
    searchProfiles_sortCriteria,
    searchProfiles_nextToken,
    searchProfiles_maxResults,

    -- * Destructuring the Response
    SearchProfilesResponse (..),
    newSearchProfilesResponse,

    -- * Response Lenses
    searchProfilesResponse_profiles,
    searchProfilesResponse_nextToken,
    searchProfilesResponse_totalCount,
    searchProfilesResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSearchProfiles' smart constructor.
data SearchProfiles = SearchProfiles'
  { -- | The filters to use to list a specified set of room profiles. Supported
    -- filter keys are ProfileName and Address. Required.
    filters :: Prelude.Maybe [Filter],
    -- | The sort order to use in listing the specified set of room profiles.
    -- Supported sort keys are ProfileName and Address.
    sortCriteria :: Prelude.Maybe [Sort],
    -- | An optional token returned from a prior request. Use this token for
    -- pagination of results from this action. If this parameter is specified,
    -- the response includes only results beyond the token, up to the value
    -- specified by @MaxResults@.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in the response. If more
    -- results exist than the specified @MaxResults@ value, a token is included
    -- in the response so that the remaining results can be retrieved.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchProfiles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'searchProfiles_filters' - The filters to use to list a specified set of room profiles. Supported
-- filter keys are ProfileName and Address. Required.
--
-- 'sortCriteria', 'searchProfiles_sortCriteria' - The sort order to use in listing the specified set of room profiles.
-- Supported sort keys are ProfileName and Address.
--
-- 'nextToken', 'searchProfiles_nextToken' - An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
--
-- 'maxResults', 'searchProfiles_maxResults' - The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
newSearchProfiles ::
  SearchProfiles
newSearchProfiles =
  SearchProfiles'
    { filters = Prelude.Nothing,
      sortCriteria = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The filters to use to list a specified set of room profiles. Supported
-- filter keys are ProfileName and Address. Required.
searchProfiles_filters :: Lens.Lens' SearchProfiles (Prelude.Maybe [Filter])
searchProfiles_filters = Lens.lens (\SearchProfiles' {filters} -> filters) (\s@SearchProfiles' {} a -> s {filters = a} :: SearchProfiles) Prelude.. Lens.mapping Lens.coerced

-- | The sort order to use in listing the specified set of room profiles.
-- Supported sort keys are ProfileName and Address.
searchProfiles_sortCriteria :: Lens.Lens' SearchProfiles (Prelude.Maybe [Sort])
searchProfiles_sortCriteria = Lens.lens (\SearchProfiles' {sortCriteria} -> sortCriteria) (\s@SearchProfiles' {} a -> s {sortCriteria = a} :: SearchProfiles) Prelude.. Lens.mapping Lens.coerced

-- | An optional token returned from a prior request. Use this token for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only results beyond the token, up to the value
-- specified by @MaxResults@.
searchProfiles_nextToken :: Lens.Lens' SearchProfiles (Prelude.Maybe Prelude.Text)
searchProfiles_nextToken = Lens.lens (\SearchProfiles' {nextToken} -> nextToken) (\s@SearchProfiles' {} a -> s {nextToken = a} :: SearchProfiles)

-- | The maximum number of results to include in the response. If more
-- results exist than the specified @MaxResults@ value, a token is included
-- in the response so that the remaining results can be retrieved.
searchProfiles_maxResults :: Lens.Lens' SearchProfiles (Prelude.Maybe Prelude.Natural)
searchProfiles_maxResults = Lens.lens (\SearchProfiles' {maxResults} -> maxResults) (\s@SearchProfiles' {} a -> s {maxResults = a} :: SearchProfiles)

instance Core.AWSPager SearchProfiles where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? searchProfilesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? searchProfilesResponse_profiles Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& searchProfiles_nextToken
          Lens..~ rs
          Lens.^? searchProfilesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest SearchProfiles where
  type
    AWSResponse SearchProfiles =
      SearchProfilesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SearchProfilesResponse'
            Prelude.<$> (x Core..?> "Profiles" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "TotalCount")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SearchProfiles where
  hashWithSalt salt' SearchProfiles' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortCriteria
      `Prelude.hashWithSalt` filters

instance Prelude.NFData SearchProfiles where
  rnf SearchProfiles' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortCriteria

instance Core.ToHeaders SearchProfiles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.SearchProfiles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SearchProfiles where
  toJSON SearchProfiles' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("SortCriteria" Core..=) Prelude.<$> sortCriteria,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath SearchProfiles where
  toPath = Prelude.const "/"

instance Core.ToQuery SearchProfiles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSearchProfilesResponse' smart constructor.
data SearchProfilesResponse = SearchProfilesResponse'
  { -- | The profiles that meet the specified set of filter criteria, in sort
    -- order.
    profiles :: Prelude.Maybe [ProfileData],
    -- | The token returned to indicate that there is more data available.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The total number of room profiles returned.
    totalCount :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchProfilesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'profiles', 'searchProfilesResponse_profiles' - The profiles that meet the specified set of filter criteria, in sort
-- order.
--
-- 'nextToken', 'searchProfilesResponse_nextToken' - The token returned to indicate that there is more data available.
--
-- 'totalCount', 'searchProfilesResponse_totalCount' - The total number of room profiles returned.
--
-- 'httpStatus', 'searchProfilesResponse_httpStatus' - The response's http status code.
newSearchProfilesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SearchProfilesResponse
newSearchProfilesResponse pHttpStatus_ =
  SearchProfilesResponse'
    { profiles = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      totalCount = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The profiles that meet the specified set of filter criteria, in sort
-- order.
searchProfilesResponse_profiles :: Lens.Lens' SearchProfilesResponse (Prelude.Maybe [ProfileData])
searchProfilesResponse_profiles = Lens.lens (\SearchProfilesResponse' {profiles} -> profiles) (\s@SearchProfilesResponse' {} a -> s {profiles = a} :: SearchProfilesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token returned to indicate that there is more data available.
searchProfilesResponse_nextToken :: Lens.Lens' SearchProfilesResponse (Prelude.Maybe Prelude.Text)
searchProfilesResponse_nextToken = Lens.lens (\SearchProfilesResponse' {nextToken} -> nextToken) (\s@SearchProfilesResponse' {} a -> s {nextToken = a} :: SearchProfilesResponse)

-- | The total number of room profiles returned.
searchProfilesResponse_totalCount :: Lens.Lens' SearchProfilesResponse (Prelude.Maybe Prelude.Int)
searchProfilesResponse_totalCount = Lens.lens (\SearchProfilesResponse' {totalCount} -> totalCount) (\s@SearchProfilesResponse' {} a -> s {totalCount = a} :: SearchProfilesResponse)

-- | The response's http status code.
searchProfilesResponse_httpStatus :: Lens.Lens' SearchProfilesResponse Prelude.Int
searchProfilesResponse_httpStatus = Lens.lens (\SearchProfilesResponse' {httpStatus} -> httpStatus) (\s@SearchProfilesResponse' {} a -> s {httpStatus = a} :: SearchProfilesResponse)

instance Prelude.NFData SearchProfilesResponse where
  rnf SearchProfilesResponse' {..} =
    Prelude.rnf profiles
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf totalCount
      `Prelude.seq` Prelude.rnf nextToken
