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
-- Module      : Amazonka.DataBrew.ListProjects
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the DataBrew projects that are defined.
--
-- This operation returns paginated results.
module Amazonka.DataBrew.ListProjects
  ( -- * Creating a Request
    ListProjects (..),
    newListProjects,

    -- * Request Lenses
    listProjects_nextToken,
    listProjects_maxResults,

    -- * Destructuring the Response
    ListProjectsResponse (..),
    newListProjectsResponse,

    -- * Response Lenses
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projects,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataBrew.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListProjects' smart constructor.
data ListProjects = ListProjects'
  { -- | The token returned by a previous call to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProjects' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProjects_nextToken' - The token returned by a previous call to retrieve the next set of
-- results.
--
-- 'maxResults', 'listProjects_maxResults' - The maximum number of results to return in this request.
newListProjects ::
  ListProjects
newListProjects =
  ListProjects'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token returned by a previous call to retrieve the next set of
-- results.
listProjects_nextToken :: Lens.Lens' ListProjects (Prelude.Maybe Prelude.Text)
listProjects_nextToken = Lens.lens (\ListProjects' {nextToken} -> nextToken) (\s@ListProjects' {} a -> s {nextToken = a} :: ListProjects)

-- | The maximum number of results to return in this request.
listProjects_maxResults :: Lens.Lens' ListProjects (Prelude.Maybe Prelude.Natural)
listProjects_maxResults = Lens.lens (\ListProjects' {maxResults} -> maxResults) (\s@ListProjects' {} a -> s {maxResults = a} :: ListProjects)

instance Core.AWSPager ListProjects where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProjectsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listProjectsResponse_projects) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProjects_nextToken
          Lens..~ rs
          Lens.^? listProjectsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListProjects where
  type AWSResponse ListProjects = ListProjectsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProjectsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Projects" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListProjects where
  hashWithSalt salt' ListProjects' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListProjects where
  rnf ListProjects' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListProjects where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListProjects where
  toPath = Prelude.const "/projects"

instance Core.ToQuery ListProjects where
  toQuery ListProjects' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListProjectsResponse' smart constructor.
data ListProjectsResponse = ListProjectsResponse'
  { -- | A token that you can use in a subsequent call to retrieve the next set
    -- of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of projects that are defined .
    projects :: [Project]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProjectsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProjectsResponse_nextToken' - A token that you can use in a subsequent call to retrieve the next set
-- of results.
--
-- 'httpStatus', 'listProjectsResponse_httpStatus' - The response's http status code.
--
-- 'projects', 'listProjectsResponse_projects' - A list of projects that are defined .
newListProjectsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProjectsResponse
newListProjectsResponse pHttpStatus_ =
  ListProjectsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      projects = Prelude.mempty
    }

-- | A token that you can use in a subsequent call to retrieve the next set
-- of results.
listProjectsResponse_nextToken :: Lens.Lens' ListProjectsResponse (Prelude.Maybe Prelude.Text)
listProjectsResponse_nextToken = Lens.lens (\ListProjectsResponse' {nextToken} -> nextToken) (\s@ListProjectsResponse' {} a -> s {nextToken = a} :: ListProjectsResponse)

-- | The response's http status code.
listProjectsResponse_httpStatus :: Lens.Lens' ListProjectsResponse Prelude.Int
listProjectsResponse_httpStatus = Lens.lens (\ListProjectsResponse' {httpStatus} -> httpStatus) (\s@ListProjectsResponse' {} a -> s {httpStatus = a} :: ListProjectsResponse)

-- | A list of projects that are defined .
listProjectsResponse_projects :: Lens.Lens' ListProjectsResponse [Project]
listProjectsResponse_projects = Lens.lens (\ListProjectsResponse' {projects} -> projects) (\s@ListProjectsResponse' {} a -> s {projects = a} :: ListProjectsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListProjectsResponse where
  rnf ListProjectsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf projects
      `Prelude.seq` Prelude.rnf httpStatus
