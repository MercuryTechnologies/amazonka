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
-- Module      : Amazonka.Batch.DescribeJobDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a list of job definitions. You can specify a @status@ (such as
-- @ACTIVE@) to only return job definitions that match that status.
--
-- This operation returns paginated results.
module Amazonka.Batch.DescribeJobDefinitions
  ( -- * Creating a Request
    DescribeJobDefinitions (..),
    newDescribeJobDefinitions,

    -- * Request Lenses
    describeJobDefinitions_status,
    describeJobDefinitions_jobDefinitionName,
    describeJobDefinitions_jobDefinitions,
    describeJobDefinitions_nextToken,
    describeJobDefinitions_maxResults,

    -- * Destructuring the Response
    DescribeJobDefinitionsResponse (..),
    newDescribeJobDefinitionsResponse,

    -- * Response Lenses
    describeJobDefinitionsResponse_jobDefinitions,
    describeJobDefinitionsResponse_nextToken,
    describeJobDefinitionsResponse_httpStatus,
  )
where

import Amazonka.Batch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the parameters for @DescribeJobDefinitions@.
--
-- /See:/ 'newDescribeJobDefinitions' smart constructor.
data DescribeJobDefinitions = DescribeJobDefinitions'
  { -- | The status used to filter job definitions.
    status :: Prelude.Maybe Prelude.Text,
    -- | The name of the job definition to describe.
    jobDefinitionName :: Prelude.Maybe Prelude.Text,
    -- | A list of up to 100 job definitions. Each entry in the list can either
    -- be an ARN of the form
    -- @arn:aws:batch:${Region}:${Account}:job-definition\/${JobDefinitionName}:${Revision}@
    -- or a short version using the form @${JobDefinitionName}:${Revision}@.
    jobDefinitions :: Prelude.Maybe [Prelude.Text],
    -- | The @nextToken@ value returned from a previous paginated
    -- @DescribeJobDefinitions@ request where @maxResults@ was used and the
    -- results exceeded the value of that parameter. Pagination continues from
    -- the end of the previous results that returned the @nextToken@ value.
    -- This value is @null@ when there are no more results to return.
    --
    -- This token should be treated as an opaque identifier that\'s only used
    -- to retrieve the next items in a list and not for other programmatic
    -- purposes.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results returned by @DescribeJobDefinitions@ in
    -- paginated output. When this parameter is used, @DescribeJobDefinitions@
    -- only returns @maxResults@ results in a single page and a @nextToken@
    -- response element. The remaining results of the initial request can be
    -- seen by sending another @DescribeJobDefinitions@ request with the
    -- returned @nextToken@ value. This value can be between 1 and 100. If this
    -- parameter isn\'t used, then @DescribeJobDefinitions@ returns up to 100
    -- results and a @nextToken@ value if applicable.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeJobDefinitions_status' - The status used to filter job definitions.
--
-- 'jobDefinitionName', 'describeJobDefinitions_jobDefinitionName' - The name of the job definition to describe.
--
-- 'jobDefinitions', 'describeJobDefinitions_jobDefinitions' - A list of up to 100 job definitions. Each entry in the list can either
-- be an ARN of the form
-- @arn:aws:batch:${Region}:${Account}:job-definition\/${JobDefinitionName}:${Revision}@
-- or a short version using the form @${JobDefinitionName}:${Revision}@.
--
-- 'nextToken', 'describeJobDefinitions_nextToken' - The @nextToken@ value returned from a previous paginated
-- @DescribeJobDefinitions@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return.
--
-- This token should be treated as an opaque identifier that\'s only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
--
-- 'maxResults', 'describeJobDefinitions_maxResults' - The maximum number of results returned by @DescribeJobDefinitions@ in
-- paginated output. When this parameter is used, @DescribeJobDefinitions@
-- only returns @maxResults@ results in a single page and a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @DescribeJobDefinitions@ request with the
-- returned @nextToken@ value. This value can be between 1 and 100. If this
-- parameter isn\'t used, then @DescribeJobDefinitions@ returns up to 100
-- results and a @nextToken@ value if applicable.
newDescribeJobDefinitions ::
  DescribeJobDefinitions
newDescribeJobDefinitions =
  DescribeJobDefinitions'
    { status = Prelude.Nothing,
      jobDefinitionName = Prelude.Nothing,
      jobDefinitions = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The status used to filter job definitions.
describeJobDefinitions_status :: Lens.Lens' DescribeJobDefinitions (Prelude.Maybe Prelude.Text)
describeJobDefinitions_status = Lens.lens (\DescribeJobDefinitions' {status} -> status) (\s@DescribeJobDefinitions' {} a -> s {status = a} :: DescribeJobDefinitions)

-- | The name of the job definition to describe.
describeJobDefinitions_jobDefinitionName :: Lens.Lens' DescribeJobDefinitions (Prelude.Maybe Prelude.Text)
describeJobDefinitions_jobDefinitionName = Lens.lens (\DescribeJobDefinitions' {jobDefinitionName} -> jobDefinitionName) (\s@DescribeJobDefinitions' {} a -> s {jobDefinitionName = a} :: DescribeJobDefinitions)

-- | A list of up to 100 job definitions. Each entry in the list can either
-- be an ARN of the form
-- @arn:aws:batch:${Region}:${Account}:job-definition\/${JobDefinitionName}:${Revision}@
-- or a short version using the form @${JobDefinitionName}:${Revision}@.
describeJobDefinitions_jobDefinitions :: Lens.Lens' DescribeJobDefinitions (Prelude.Maybe [Prelude.Text])
describeJobDefinitions_jobDefinitions = Lens.lens (\DescribeJobDefinitions' {jobDefinitions} -> jobDefinitions) (\s@DescribeJobDefinitions' {} a -> s {jobDefinitions = a} :: DescribeJobDefinitions) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ value returned from a previous paginated
-- @DescribeJobDefinitions@ request where @maxResults@ was used and the
-- results exceeded the value of that parameter. Pagination continues from
-- the end of the previous results that returned the @nextToken@ value.
-- This value is @null@ when there are no more results to return.
--
-- This token should be treated as an opaque identifier that\'s only used
-- to retrieve the next items in a list and not for other programmatic
-- purposes.
describeJobDefinitions_nextToken :: Lens.Lens' DescribeJobDefinitions (Prelude.Maybe Prelude.Text)
describeJobDefinitions_nextToken = Lens.lens (\DescribeJobDefinitions' {nextToken} -> nextToken) (\s@DescribeJobDefinitions' {} a -> s {nextToken = a} :: DescribeJobDefinitions)

-- | The maximum number of results returned by @DescribeJobDefinitions@ in
-- paginated output. When this parameter is used, @DescribeJobDefinitions@
-- only returns @maxResults@ results in a single page and a @nextToken@
-- response element. The remaining results of the initial request can be
-- seen by sending another @DescribeJobDefinitions@ request with the
-- returned @nextToken@ value. This value can be between 1 and 100. If this
-- parameter isn\'t used, then @DescribeJobDefinitions@ returns up to 100
-- results and a @nextToken@ value if applicable.
describeJobDefinitions_maxResults :: Lens.Lens' DescribeJobDefinitions (Prelude.Maybe Prelude.Int)
describeJobDefinitions_maxResults = Lens.lens (\DescribeJobDefinitions' {maxResults} -> maxResults) (\s@DescribeJobDefinitions' {} a -> s {maxResults = a} :: DescribeJobDefinitions)

instance Core.AWSPager DescribeJobDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeJobDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeJobDefinitionsResponse_jobDefinitions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeJobDefinitions_nextToken
          Lens..~ rs
          Lens.^? describeJobDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest DescribeJobDefinitions where
  type
    AWSResponse DescribeJobDefinitions =
      DescribeJobDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeJobDefinitionsResponse'
            Prelude.<$> (x Core..?> "jobDefinitions" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeJobDefinitions where
  hashWithSalt salt' DescribeJobDefinitions' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` jobDefinitions
      `Prelude.hashWithSalt` jobDefinitionName
      `Prelude.hashWithSalt` status

instance Prelude.NFData DescribeJobDefinitions where
  rnf DescribeJobDefinitions' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf jobDefinitions
      `Prelude.seq` Prelude.rnf jobDefinitionName

instance Core.ToHeaders DescribeJobDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeJobDefinitions where
  toJSON DescribeJobDefinitions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("status" Core..=) Prelude.<$> status,
            ("jobDefinitionName" Core..=)
              Prelude.<$> jobDefinitionName,
            ("jobDefinitions" Core..=)
              Prelude.<$> jobDefinitions,
            ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath DescribeJobDefinitions where
  toPath = Prelude.const "/v1/describejobdefinitions"

instance Core.ToQuery DescribeJobDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeJobDefinitionsResponse' smart constructor.
data DescribeJobDefinitionsResponse = DescribeJobDefinitionsResponse'
  { -- | The list of job definitions.
    jobDefinitions :: Prelude.Maybe [JobDefinition],
    -- | The @nextToken@ value to include in a future @DescribeJobDefinitions@
    -- request. When the results of a @DescribeJobDefinitions@ request exceed
    -- @maxResults@, this value can be used to retrieve the next page of
    -- results. This value is @null@ when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeJobDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDefinitions', 'describeJobDefinitionsResponse_jobDefinitions' - The list of job definitions.
--
-- 'nextToken', 'describeJobDefinitionsResponse_nextToken' - The @nextToken@ value to include in a future @DescribeJobDefinitions@
-- request. When the results of a @DescribeJobDefinitions@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
--
-- 'httpStatus', 'describeJobDefinitionsResponse_httpStatus' - The response's http status code.
newDescribeJobDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeJobDefinitionsResponse
newDescribeJobDefinitionsResponse pHttpStatus_ =
  DescribeJobDefinitionsResponse'
    { jobDefinitions =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of job definitions.
describeJobDefinitionsResponse_jobDefinitions :: Lens.Lens' DescribeJobDefinitionsResponse (Prelude.Maybe [JobDefinition])
describeJobDefinitionsResponse_jobDefinitions = Lens.lens (\DescribeJobDefinitionsResponse' {jobDefinitions} -> jobDefinitions) (\s@DescribeJobDefinitionsResponse' {} a -> s {jobDefinitions = a} :: DescribeJobDefinitionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ value to include in a future @DescribeJobDefinitions@
-- request. When the results of a @DescribeJobDefinitions@ request exceed
-- @maxResults@, this value can be used to retrieve the next page of
-- results. This value is @null@ when there are no more results to return.
describeJobDefinitionsResponse_nextToken :: Lens.Lens' DescribeJobDefinitionsResponse (Prelude.Maybe Prelude.Text)
describeJobDefinitionsResponse_nextToken = Lens.lens (\DescribeJobDefinitionsResponse' {nextToken} -> nextToken) (\s@DescribeJobDefinitionsResponse' {} a -> s {nextToken = a} :: DescribeJobDefinitionsResponse)

-- | The response's http status code.
describeJobDefinitionsResponse_httpStatus :: Lens.Lens' DescribeJobDefinitionsResponse Prelude.Int
describeJobDefinitionsResponse_httpStatus = Lens.lens (\DescribeJobDefinitionsResponse' {httpStatus} -> httpStatus) (\s@DescribeJobDefinitionsResponse' {} a -> s {httpStatus = a} :: DescribeJobDefinitionsResponse)

instance
  Prelude.NFData
    DescribeJobDefinitionsResponse
  where
  rnf DescribeJobDefinitionsResponse' {..} =
    Prelude.rnf jobDefinitions
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
