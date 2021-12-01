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
-- Module      : Amazonka.IoT.ListJobExecutionsForJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for a job.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListJobExecutionsForJob>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListJobExecutionsForJob
  ( -- * Creating a Request
    ListJobExecutionsForJob (..),
    newListJobExecutionsForJob,

    -- * Request Lenses
    listJobExecutionsForJob_status,
    listJobExecutionsForJob_nextToken,
    listJobExecutionsForJob_maxResults,
    listJobExecutionsForJob_jobId,

    -- * Destructuring the Response
    ListJobExecutionsForJobResponse (..),
    newListJobExecutionsForJobResponse,

    -- * Response Lenses
    listJobExecutionsForJobResponse_executionSummaries,
    listJobExecutionsForJobResponse_nextToken,
    listJobExecutionsForJobResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJobExecutionsForJob' smart constructor.
data ListJobExecutionsForJob = ListJobExecutionsForJob'
  { -- | The status of the job.
    status :: Prelude.Maybe JobExecutionStatus,
    -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The unique identifier you assigned to this job when it was created.
    jobId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobExecutionsForJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listJobExecutionsForJob_status' - The status of the job.
--
-- 'nextToken', 'listJobExecutionsForJob_nextToken' - The token to retrieve the next set of results.
--
-- 'maxResults', 'listJobExecutionsForJob_maxResults' - The maximum number of results to be returned per request.
--
-- 'jobId', 'listJobExecutionsForJob_jobId' - The unique identifier you assigned to this job when it was created.
newListJobExecutionsForJob ::
  -- | 'jobId'
  Prelude.Text ->
  ListJobExecutionsForJob
newListJobExecutionsForJob pJobId_ =
  ListJobExecutionsForJob'
    { status = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      jobId = pJobId_
    }

-- | The status of the job.
listJobExecutionsForJob_status :: Lens.Lens' ListJobExecutionsForJob (Prelude.Maybe JobExecutionStatus)
listJobExecutionsForJob_status = Lens.lens (\ListJobExecutionsForJob' {status} -> status) (\s@ListJobExecutionsForJob' {} a -> s {status = a} :: ListJobExecutionsForJob)

-- | The token to retrieve the next set of results.
listJobExecutionsForJob_nextToken :: Lens.Lens' ListJobExecutionsForJob (Prelude.Maybe Prelude.Text)
listJobExecutionsForJob_nextToken = Lens.lens (\ListJobExecutionsForJob' {nextToken} -> nextToken) (\s@ListJobExecutionsForJob' {} a -> s {nextToken = a} :: ListJobExecutionsForJob)

-- | The maximum number of results to be returned per request.
listJobExecutionsForJob_maxResults :: Lens.Lens' ListJobExecutionsForJob (Prelude.Maybe Prelude.Natural)
listJobExecutionsForJob_maxResults = Lens.lens (\ListJobExecutionsForJob' {maxResults} -> maxResults) (\s@ListJobExecutionsForJob' {} a -> s {maxResults = a} :: ListJobExecutionsForJob)

-- | The unique identifier you assigned to this job when it was created.
listJobExecutionsForJob_jobId :: Lens.Lens' ListJobExecutionsForJob Prelude.Text
listJobExecutionsForJob_jobId = Lens.lens (\ListJobExecutionsForJob' {jobId} -> jobId) (\s@ListJobExecutionsForJob' {} a -> s {jobId = a} :: ListJobExecutionsForJob)

instance Core.AWSPager ListJobExecutionsForJob where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForJobResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForJobResponse_executionSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listJobExecutionsForJob_nextToken
          Lens..~ rs
          Lens.^? listJobExecutionsForJobResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListJobExecutionsForJob where
  type
    AWSResponse ListJobExecutionsForJob =
      ListJobExecutionsForJobResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobExecutionsForJobResponse'
            Prelude.<$> ( x Core..?> "executionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobExecutionsForJob where
  hashWithSalt salt' ListJobExecutionsForJob' {..} =
    salt' `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListJobExecutionsForJob where
  rnf ListJobExecutionsForJob' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders ListJobExecutionsForJob where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListJobExecutionsForJob where
  toPath ListJobExecutionsForJob' {..} =
    Prelude.mconcat
      ["/jobs/", Core.toBS jobId, "/things"]

instance Core.ToQuery ListJobExecutionsForJob where
  toQuery ListJobExecutionsForJob' {..} =
    Prelude.mconcat
      [ "status" Core.=: status,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListJobExecutionsForJobResponse' smart constructor.
data ListJobExecutionsForJobResponse = ListJobExecutionsForJobResponse'
  { -- | A list of job execution summaries.
    executionSummaries :: Prelude.Maybe [JobExecutionSummaryForJob],
    -- | The token for the next set of results, or __null__ if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobExecutionsForJobResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionSummaries', 'listJobExecutionsForJobResponse_executionSummaries' - A list of job execution summaries.
--
-- 'nextToken', 'listJobExecutionsForJobResponse_nextToken' - The token for the next set of results, or __null__ if there are no
-- additional results.
--
-- 'httpStatus', 'listJobExecutionsForJobResponse_httpStatus' - The response's http status code.
newListJobExecutionsForJobResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobExecutionsForJobResponse
newListJobExecutionsForJobResponse pHttpStatus_ =
  ListJobExecutionsForJobResponse'
    { executionSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of job execution summaries.
listJobExecutionsForJobResponse_executionSummaries :: Lens.Lens' ListJobExecutionsForJobResponse (Prelude.Maybe [JobExecutionSummaryForJob])
listJobExecutionsForJobResponse_executionSummaries = Lens.lens (\ListJobExecutionsForJobResponse' {executionSummaries} -> executionSummaries) (\s@ListJobExecutionsForJobResponse' {} a -> s {executionSummaries = a} :: ListJobExecutionsForJobResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or __null__ if there are no
-- additional results.
listJobExecutionsForJobResponse_nextToken :: Lens.Lens' ListJobExecutionsForJobResponse (Prelude.Maybe Prelude.Text)
listJobExecutionsForJobResponse_nextToken = Lens.lens (\ListJobExecutionsForJobResponse' {nextToken} -> nextToken) (\s@ListJobExecutionsForJobResponse' {} a -> s {nextToken = a} :: ListJobExecutionsForJobResponse)

-- | The response's http status code.
listJobExecutionsForJobResponse_httpStatus :: Lens.Lens' ListJobExecutionsForJobResponse Prelude.Int
listJobExecutionsForJobResponse_httpStatus = Lens.lens (\ListJobExecutionsForJobResponse' {httpStatus} -> httpStatus) (\s@ListJobExecutionsForJobResponse' {} a -> s {httpStatus = a} :: ListJobExecutionsForJobResponse)

instance
  Prelude.NFData
    ListJobExecutionsForJobResponse
  where
  rnf ListJobExecutionsForJobResponse' {..} =
    Prelude.rnf executionSummaries
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
