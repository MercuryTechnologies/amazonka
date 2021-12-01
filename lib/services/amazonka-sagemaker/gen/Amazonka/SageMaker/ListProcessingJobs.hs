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
-- Module      : Amazonka.SageMaker.ListProcessingJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists processing jobs that satisfy various filters.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListProcessingJobs
  ( -- * Creating a Request
    ListProcessingJobs (..),
    newListProcessingJobs,

    -- * Request Lenses
    listProcessingJobs_nameContains,
    listProcessingJobs_lastModifiedTimeBefore,
    listProcessingJobs_creationTimeAfter,
    listProcessingJobs_nextToken,
    listProcessingJobs_sortOrder,
    listProcessingJobs_lastModifiedTimeAfter,
    listProcessingJobs_creationTimeBefore,
    listProcessingJobs_statusEquals,
    listProcessingJobs_maxResults,
    listProcessingJobs_sortBy,

    -- * Destructuring the Response
    ListProcessingJobsResponse (..),
    newListProcessingJobsResponse,

    -- * Response Lenses
    listProcessingJobsResponse_nextToken,
    listProcessingJobsResponse_httpStatus,
    listProcessingJobsResponse_processingJobSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListProcessingJobs' smart constructor.
data ListProcessingJobs = ListProcessingJobs'
  { -- | A string in the processing job name. This filter returns only processing
    -- jobs whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only processing jobs modified before the specified
    -- time.
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only processing jobs created after the specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | If the result of the previous @ListProcessingJobs@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of processing jobs, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only processing jobs modified after the specified
    -- time.
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only processing jobs created after the specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that retrieves only processing jobs with a specific status.
    statusEquals :: Prelude.Maybe ProcessingJobStatus,
    -- | The maximum number of processing jobs to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe SortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProcessingJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameContains', 'listProcessingJobs_nameContains' - A string in the processing job name. This filter returns only processing
-- jobs whose name contains the specified string.
--
-- 'lastModifiedTimeBefore', 'listProcessingJobs_lastModifiedTimeBefore' - A filter that returns only processing jobs modified before the specified
-- time.
--
-- 'creationTimeAfter', 'listProcessingJobs_creationTimeAfter' - A filter that returns only processing jobs created after the specified
-- time.
--
-- 'nextToken', 'listProcessingJobs_nextToken' - If the result of the previous @ListProcessingJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of processing jobs, use the token in the next request.
--
-- 'sortOrder', 'listProcessingJobs_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'lastModifiedTimeAfter', 'listProcessingJobs_lastModifiedTimeAfter' - A filter that returns only processing jobs modified after the specified
-- time.
--
-- 'creationTimeBefore', 'listProcessingJobs_creationTimeBefore' - A filter that returns only processing jobs created after the specified
-- time.
--
-- 'statusEquals', 'listProcessingJobs_statusEquals' - A filter that retrieves only processing jobs with a specific status.
--
-- 'maxResults', 'listProcessingJobs_maxResults' - The maximum number of processing jobs to return in the response.
--
-- 'sortBy', 'listProcessingJobs_sortBy' - The field to sort results by. The default is @CreationTime@.
newListProcessingJobs ::
  ListProcessingJobs
newListProcessingJobs =
  ListProcessingJobs'
    { nameContains = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A string in the processing job name. This filter returns only processing
-- jobs whose name contains the specified string.
listProcessingJobs_nameContains :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.Text)
listProcessingJobs_nameContains = Lens.lens (\ListProcessingJobs' {nameContains} -> nameContains) (\s@ListProcessingJobs' {} a -> s {nameContains = a} :: ListProcessingJobs)

-- | A filter that returns only processing jobs modified before the specified
-- time.
listProcessingJobs_lastModifiedTimeBefore :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_lastModifiedTimeBefore = Lens.lens (\ListProcessingJobs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListProcessingJobs' {} a -> s {lastModifiedTimeBefore = a} :: ListProcessingJobs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only processing jobs created after the specified
-- time.
listProcessingJobs_creationTimeAfter :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_creationTimeAfter = Lens.lens (\ListProcessingJobs' {creationTimeAfter} -> creationTimeAfter) (\s@ListProcessingJobs' {} a -> s {creationTimeAfter = a} :: ListProcessingJobs) Prelude.. Lens.mapping Core._Time

-- | If the result of the previous @ListProcessingJobs@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of processing jobs, use the token in the next request.
listProcessingJobs_nextToken :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.Text)
listProcessingJobs_nextToken = Lens.lens (\ListProcessingJobs' {nextToken} -> nextToken) (\s@ListProcessingJobs' {} a -> s {nextToken = a} :: ListProcessingJobs)

-- | The sort order for results. The default is @Ascending@.
listProcessingJobs_sortOrder :: Lens.Lens' ListProcessingJobs (Prelude.Maybe SortOrder)
listProcessingJobs_sortOrder = Lens.lens (\ListProcessingJobs' {sortOrder} -> sortOrder) (\s@ListProcessingJobs' {} a -> s {sortOrder = a} :: ListProcessingJobs)

-- | A filter that returns only processing jobs modified after the specified
-- time.
listProcessingJobs_lastModifiedTimeAfter :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_lastModifiedTimeAfter = Lens.lens (\ListProcessingJobs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListProcessingJobs' {} a -> s {lastModifiedTimeAfter = a} :: ListProcessingJobs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only processing jobs created after the specified
-- time.
listProcessingJobs_creationTimeBefore :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.UTCTime)
listProcessingJobs_creationTimeBefore = Lens.lens (\ListProcessingJobs' {creationTimeBefore} -> creationTimeBefore) (\s@ListProcessingJobs' {} a -> s {creationTimeBefore = a} :: ListProcessingJobs) Prelude.. Lens.mapping Core._Time

-- | A filter that retrieves only processing jobs with a specific status.
listProcessingJobs_statusEquals :: Lens.Lens' ListProcessingJobs (Prelude.Maybe ProcessingJobStatus)
listProcessingJobs_statusEquals = Lens.lens (\ListProcessingJobs' {statusEquals} -> statusEquals) (\s@ListProcessingJobs' {} a -> s {statusEquals = a} :: ListProcessingJobs)

-- | The maximum number of processing jobs to return in the response.
listProcessingJobs_maxResults :: Lens.Lens' ListProcessingJobs (Prelude.Maybe Prelude.Natural)
listProcessingJobs_maxResults = Lens.lens (\ListProcessingJobs' {maxResults} -> maxResults) (\s@ListProcessingJobs' {} a -> s {maxResults = a} :: ListProcessingJobs)

-- | The field to sort results by. The default is @CreationTime@.
listProcessingJobs_sortBy :: Lens.Lens' ListProcessingJobs (Prelude.Maybe SortBy)
listProcessingJobs_sortBy = Lens.lens (\ListProcessingJobs' {sortBy} -> sortBy) (\s@ListProcessingJobs' {} a -> s {sortBy = a} :: ListProcessingJobs)

instance Core.AWSPager ListProcessingJobs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listProcessingJobsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listProcessingJobsResponse_processingJobSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listProcessingJobs_nextToken
          Lens..~ rs
          Lens.^? listProcessingJobsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListProcessingJobs where
  type
    AWSResponse ListProcessingJobs =
      ListProcessingJobsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListProcessingJobsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "ProcessingJobSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListProcessingJobs where
  hashWithSalt salt' ListProcessingJobs' {..} =
    salt' `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` statusEquals
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` nameContains

instance Prelude.NFData ListProcessingJobs where
  rnf ListProcessingJobs' {..} =
    Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf statusEquals
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore

instance Core.ToHeaders ListProcessingJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListProcessingJobs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListProcessingJobs where
  toJSON ListProcessingJobs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NameContains" Core..=) Prelude.<$> nameContains,
            ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("StatusEquals" Core..=) Prelude.<$> statusEquals,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListProcessingJobs where
  toPath = Prelude.const "/"

instance Core.ToQuery ListProcessingJobs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListProcessingJobsResponse' smart constructor.
data ListProcessingJobsResponse = ListProcessingJobsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of processing jobs, use it in the subsequent
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of @ProcessingJobSummary@ objects, each listing a processing
    -- job.
    processingJobSummaries :: [ProcessingJobSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListProcessingJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listProcessingJobsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of processing jobs, use it in the subsequent
-- request.
--
-- 'httpStatus', 'listProcessingJobsResponse_httpStatus' - The response's http status code.
--
-- 'processingJobSummaries', 'listProcessingJobsResponse_processingJobSummaries' - An array of @ProcessingJobSummary@ objects, each listing a processing
-- job.
newListProcessingJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListProcessingJobsResponse
newListProcessingJobsResponse pHttpStatus_ =
  ListProcessingJobsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      processingJobSummaries = Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of processing jobs, use it in the subsequent
-- request.
listProcessingJobsResponse_nextToken :: Lens.Lens' ListProcessingJobsResponse (Prelude.Maybe Prelude.Text)
listProcessingJobsResponse_nextToken = Lens.lens (\ListProcessingJobsResponse' {nextToken} -> nextToken) (\s@ListProcessingJobsResponse' {} a -> s {nextToken = a} :: ListProcessingJobsResponse)

-- | The response's http status code.
listProcessingJobsResponse_httpStatus :: Lens.Lens' ListProcessingJobsResponse Prelude.Int
listProcessingJobsResponse_httpStatus = Lens.lens (\ListProcessingJobsResponse' {httpStatus} -> httpStatus) (\s@ListProcessingJobsResponse' {} a -> s {httpStatus = a} :: ListProcessingJobsResponse)

-- | An array of @ProcessingJobSummary@ objects, each listing a processing
-- job.
listProcessingJobsResponse_processingJobSummaries :: Lens.Lens' ListProcessingJobsResponse [ProcessingJobSummary]
listProcessingJobsResponse_processingJobSummaries = Lens.lens (\ListProcessingJobsResponse' {processingJobSummaries} -> processingJobSummaries) (\s@ListProcessingJobsResponse' {} a -> s {processingJobSummaries = a} :: ListProcessingJobsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListProcessingJobsResponse where
  rnf ListProcessingJobsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf processingJobSummaries
      `Prelude.seq` Prelude.rnf httpStatus
