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
-- Module      : Amazonka.IoT.ListJobExecutionsForThing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the job executions for the specified thing.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListJobExecutionsForThing>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListJobExecutionsForThing
  ( -- * Creating a Request
    ListJobExecutionsForThing (..),
    newListJobExecutionsForThing,

    -- * Request Lenses
    listJobExecutionsForThing_status,
    listJobExecutionsForThing_namespaceId,
    listJobExecutionsForThing_nextToken,
    listJobExecutionsForThing_maxResults,
    listJobExecutionsForThing_thingName,

    -- * Destructuring the Response
    ListJobExecutionsForThingResponse (..),
    newListJobExecutionsForThingResponse,

    -- * Response Lenses
    listJobExecutionsForThingResponse_executionSummaries,
    listJobExecutionsForThingResponse_nextToken,
    listJobExecutionsForThingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListJobExecutionsForThing' smart constructor.
data ListJobExecutionsForThing = ListJobExecutionsForThing'
  { -- | An optional filter that lets you search for jobs that have the specified
    -- status.
    status :: Prelude.Maybe JobExecutionStatus,
    -- | The namespace used to indicate that a job is a customer-managed job.
    --
    -- When you specify a value for this parameter, Amazon Web Services IoT
    -- Core sends jobs notifications to MQTT topics that contain the value in
    -- the following format.
    --
    -- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
    --
    -- The @namespaceId@ feature is in public preview.
    namespaceId :: Prelude.Maybe Prelude.Text,
    -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The thing name.
    thingName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobExecutionsForThing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listJobExecutionsForThing_status' - An optional filter that lets you search for jobs that have the specified
-- status.
--
-- 'namespaceId', 'listJobExecutionsForThing_namespaceId' - The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
--
-- 'nextToken', 'listJobExecutionsForThing_nextToken' - The token to retrieve the next set of results.
--
-- 'maxResults', 'listJobExecutionsForThing_maxResults' - The maximum number of results to be returned per request.
--
-- 'thingName', 'listJobExecutionsForThing_thingName' - The thing name.
newListJobExecutionsForThing ::
  -- | 'thingName'
  Prelude.Text ->
  ListJobExecutionsForThing
newListJobExecutionsForThing pThingName_ =
  ListJobExecutionsForThing'
    { status =
        Prelude.Nothing,
      namespaceId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      thingName = pThingName_
    }

-- | An optional filter that lets you search for jobs that have the specified
-- status.
listJobExecutionsForThing_status :: Lens.Lens' ListJobExecutionsForThing (Prelude.Maybe JobExecutionStatus)
listJobExecutionsForThing_status = Lens.lens (\ListJobExecutionsForThing' {status} -> status) (\s@ListJobExecutionsForThing' {} a -> s {status = a} :: ListJobExecutionsForThing)

-- | The namespace used to indicate that a job is a customer-managed job.
--
-- When you specify a value for this parameter, Amazon Web Services IoT
-- Core sends jobs notifications to MQTT topics that contain the value in
-- the following format.
--
-- @$aws\/things\/THING_NAME\/jobs\/JOB_ID\/notify-namespace-NAMESPACE_ID\/@
--
-- The @namespaceId@ feature is in public preview.
listJobExecutionsForThing_namespaceId :: Lens.Lens' ListJobExecutionsForThing (Prelude.Maybe Prelude.Text)
listJobExecutionsForThing_namespaceId = Lens.lens (\ListJobExecutionsForThing' {namespaceId} -> namespaceId) (\s@ListJobExecutionsForThing' {} a -> s {namespaceId = a} :: ListJobExecutionsForThing)

-- | The token to retrieve the next set of results.
listJobExecutionsForThing_nextToken :: Lens.Lens' ListJobExecutionsForThing (Prelude.Maybe Prelude.Text)
listJobExecutionsForThing_nextToken = Lens.lens (\ListJobExecutionsForThing' {nextToken} -> nextToken) (\s@ListJobExecutionsForThing' {} a -> s {nextToken = a} :: ListJobExecutionsForThing)

-- | The maximum number of results to be returned per request.
listJobExecutionsForThing_maxResults :: Lens.Lens' ListJobExecutionsForThing (Prelude.Maybe Prelude.Natural)
listJobExecutionsForThing_maxResults = Lens.lens (\ListJobExecutionsForThing' {maxResults} -> maxResults) (\s@ListJobExecutionsForThing' {} a -> s {maxResults = a} :: ListJobExecutionsForThing)

-- | The thing name.
listJobExecutionsForThing_thingName :: Lens.Lens' ListJobExecutionsForThing Prelude.Text
listJobExecutionsForThing_thingName = Lens.lens (\ListJobExecutionsForThing' {thingName} -> thingName) (\s@ListJobExecutionsForThing' {} a -> s {thingName = a} :: ListJobExecutionsForThing)

instance Core.AWSPager ListJobExecutionsForThing where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForThingResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listJobExecutionsForThingResponse_executionSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listJobExecutionsForThing_nextToken
          Lens..~ rs
          Lens.^? listJobExecutionsForThingResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListJobExecutionsForThing where
  type
    AWSResponse ListJobExecutionsForThing =
      ListJobExecutionsForThingResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobExecutionsForThingResponse'
            Prelude.<$> ( x Core..?> "executionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobExecutionsForThing where
  hashWithSalt salt' ListJobExecutionsForThing' {..} =
    salt' `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` namespaceId
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListJobExecutionsForThing where
  rnf ListJobExecutionsForThing' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf namespaceId

instance Core.ToHeaders ListJobExecutionsForThing where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListJobExecutionsForThing where
  toPath ListJobExecutionsForThing' {..} =
    Prelude.mconcat
      ["/things/", Core.toBS thingName, "/jobs"]

instance Core.ToQuery ListJobExecutionsForThing where
  toQuery ListJobExecutionsForThing' {..} =
    Prelude.mconcat
      [ "status" Core.=: status,
        "namespaceId" Core.=: namespaceId,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListJobExecutionsForThingResponse' smart constructor.
data ListJobExecutionsForThingResponse = ListJobExecutionsForThingResponse'
  { -- | A list of job execution summaries.
    executionSummaries :: Prelude.Maybe [JobExecutionSummaryForThing],
    -- | The token for the next set of results, or __null__ if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListJobExecutionsForThingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executionSummaries', 'listJobExecutionsForThingResponse_executionSummaries' - A list of job execution summaries.
--
-- 'nextToken', 'listJobExecutionsForThingResponse_nextToken' - The token for the next set of results, or __null__ if there are no
-- additional results.
--
-- 'httpStatus', 'listJobExecutionsForThingResponse_httpStatus' - The response's http status code.
newListJobExecutionsForThingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobExecutionsForThingResponse
newListJobExecutionsForThingResponse pHttpStatus_ =
  ListJobExecutionsForThingResponse'
    { executionSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of job execution summaries.
listJobExecutionsForThingResponse_executionSummaries :: Lens.Lens' ListJobExecutionsForThingResponse (Prelude.Maybe [JobExecutionSummaryForThing])
listJobExecutionsForThingResponse_executionSummaries = Lens.lens (\ListJobExecutionsForThingResponse' {executionSummaries} -> executionSummaries) (\s@ListJobExecutionsForThingResponse' {} a -> s {executionSummaries = a} :: ListJobExecutionsForThingResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token for the next set of results, or __null__ if there are no
-- additional results.
listJobExecutionsForThingResponse_nextToken :: Lens.Lens' ListJobExecutionsForThingResponse (Prelude.Maybe Prelude.Text)
listJobExecutionsForThingResponse_nextToken = Lens.lens (\ListJobExecutionsForThingResponse' {nextToken} -> nextToken) (\s@ListJobExecutionsForThingResponse' {} a -> s {nextToken = a} :: ListJobExecutionsForThingResponse)

-- | The response's http status code.
listJobExecutionsForThingResponse_httpStatus :: Lens.Lens' ListJobExecutionsForThingResponse Prelude.Int
listJobExecutionsForThingResponse_httpStatus = Lens.lens (\ListJobExecutionsForThingResponse' {httpStatus} -> httpStatus) (\s@ListJobExecutionsForThingResponse' {} a -> s {httpStatus = a} :: ListJobExecutionsForThingResponse)

instance
  Prelude.NFData
    ListJobExecutionsForThingResponse
  where
  rnf ListJobExecutionsForThingResponse' {..} =
    Prelude.rnf executionSummaries
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
