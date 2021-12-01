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
-- Module      : Amazonka.IoTAnalytics.ListDatasetContents
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists information about dataset contents that have been created.
--
-- This operation returns paginated results.
module Amazonka.IoTAnalytics.ListDatasetContents
  ( -- * Creating a Request
    ListDatasetContents (..),
    newListDatasetContents,

    -- * Request Lenses
    listDatasetContents_nextToken,
    listDatasetContents_scheduledBefore,
    listDatasetContents_maxResults,
    listDatasetContents_scheduledOnOrAfter,
    listDatasetContents_datasetName,

    -- * Destructuring the Response
    ListDatasetContentsResponse (..),
    newListDatasetContentsResponse,

    -- * Response Lenses
    listDatasetContentsResponse_datasetContentSummaries,
    listDatasetContentsResponse_nextToken,
    listDatasetContentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTAnalytics.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDatasetContents' smart constructor.
data ListDatasetContents = ListDatasetContents'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter to limit results to those dataset contents whose creation is
    -- scheduled before the given time. See the field @triggers.schedule@ in
    -- the @CreateDataset@ request. (timestamp)
    scheduledBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of results to return in this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter to limit results to those dataset contents whose creation is
    -- scheduled on or after the given time. See the field @triggers.schedule@
    -- in the @CreateDataset@ request. (timestamp)
    scheduledOnOrAfter :: Prelude.Maybe Core.POSIX,
    -- | The name of the dataset whose contents information you want to list.
    datasetName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetContents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDatasetContents_nextToken' - The token for the next set of results.
--
-- 'scheduledBefore', 'listDatasetContents_scheduledBefore' - A filter to limit results to those dataset contents whose creation is
-- scheduled before the given time. See the field @triggers.schedule@ in
-- the @CreateDataset@ request. (timestamp)
--
-- 'maxResults', 'listDatasetContents_maxResults' - The maximum number of results to return in this request.
--
-- 'scheduledOnOrAfter', 'listDatasetContents_scheduledOnOrAfter' - A filter to limit results to those dataset contents whose creation is
-- scheduled on or after the given time. See the field @triggers.schedule@
-- in the @CreateDataset@ request. (timestamp)
--
-- 'datasetName', 'listDatasetContents_datasetName' - The name of the dataset whose contents information you want to list.
newListDatasetContents ::
  -- | 'datasetName'
  Prelude.Text ->
  ListDatasetContents
newListDatasetContents pDatasetName_ =
  ListDatasetContents'
    { nextToken = Prelude.Nothing,
      scheduledBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      scheduledOnOrAfter = Prelude.Nothing,
      datasetName = pDatasetName_
    }

-- | The token for the next set of results.
listDatasetContents_nextToken :: Lens.Lens' ListDatasetContents (Prelude.Maybe Prelude.Text)
listDatasetContents_nextToken = Lens.lens (\ListDatasetContents' {nextToken} -> nextToken) (\s@ListDatasetContents' {} a -> s {nextToken = a} :: ListDatasetContents)

-- | A filter to limit results to those dataset contents whose creation is
-- scheduled before the given time. See the field @triggers.schedule@ in
-- the @CreateDataset@ request. (timestamp)
listDatasetContents_scheduledBefore :: Lens.Lens' ListDatasetContents (Prelude.Maybe Prelude.UTCTime)
listDatasetContents_scheduledBefore = Lens.lens (\ListDatasetContents' {scheduledBefore} -> scheduledBefore) (\s@ListDatasetContents' {} a -> s {scheduledBefore = a} :: ListDatasetContents) Prelude.. Lens.mapping Core._Time

-- | The maximum number of results to return in this request.
listDatasetContents_maxResults :: Lens.Lens' ListDatasetContents (Prelude.Maybe Prelude.Natural)
listDatasetContents_maxResults = Lens.lens (\ListDatasetContents' {maxResults} -> maxResults) (\s@ListDatasetContents' {} a -> s {maxResults = a} :: ListDatasetContents)

-- | A filter to limit results to those dataset contents whose creation is
-- scheduled on or after the given time. See the field @triggers.schedule@
-- in the @CreateDataset@ request. (timestamp)
listDatasetContents_scheduledOnOrAfter :: Lens.Lens' ListDatasetContents (Prelude.Maybe Prelude.UTCTime)
listDatasetContents_scheduledOnOrAfter = Lens.lens (\ListDatasetContents' {scheduledOnOrAfter} -> scheduledOnOrAfter) (\s@ListDatasetContents' {} a -> s {scheduledOnOrAfter = a} :: ListDatasetContents) Prelude.. Lens.mapping Core._Time

-- | The name of the dataset whose contents information you want to list.
listDatasetContents_datasetName :: Lens.Lens' ListDatasetContents Prelude.Text
listDatasetContents_datasetName = Lens.lens (\ListDatasetContents' {datasetName} -> datasetName) (\s@ListDatasetContents' {} a -> s {datasetName = a} :: ListDatasetContents)

instance Core.AWSPager ListDatasetContents where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDatasetContentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDatasetContentsResponse_datasetContentSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDatasetContents_nextToken
          Lens..~ rs
          Lens.^? listDatasetContentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDatasetContents where
  type
    AWSResponse ListDatasetContents =
      ListDatasetContentsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDatasetContentsResponse'
            Prelude.<$> ( x Core..?> "datasetContentSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDatasetContents where
  hashWithSalt salt' ListDatasetContents' {..} =
    salt' `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` scheduledOnOrAfter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` scheduledBefore
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDatasetContents where
  rnf ListDatasetContents' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf scheduledOnOrAfter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf scheduledBefore

instance Core.ToHeaders ListDatasetContents where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListDatasetContents where
  toPath ListDatasetContents' {..} =
    Prelude.mconcat
      ["/datasets/", Core.toBS datasetName, "/contents"]

instance Core.ToQuery ListDatasetContents where
  toQuery ListDatasetContents' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "scheduledBefore" Core.=: scheduledBefore,
        "maxResults" Core.=: maxResults,
        "scheduledOnOrAfter" Core.=: scheduledOnOrAfter
      ]

-- | /See:/ 'newListDatasetContentsResponse' smart constructor.
data ListDatasetContentsResponse = ListDatasetContentsResponse'
  { -- | Summary information about dataset contents that have been created.
    datasetContentSummaries :: Prelude.Maybe [DatasetContentSummary],
    -- | The token to retrieve the next set of results, or @null@ if there are no
    -- more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDatasetContentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'datasetContentSummaries', 'listDatasetContentsResponse_datasetContentSummaries' - Summary information about dataset contents that have been created.
--
-- 'nextToken', 'listDatasetContentsResponse_nextToken' - The token to retrieve the next set of results, or @null@ if there are no
-- more results.
--
-- 'httpStatus', 'listDatasetContentsResponse_httpStatus' - The response's http status code.
newListDatasetContentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDatasetContentsResponse
newListDatasetContentsResponse pHttpStatus_ =
  ListDatasetContentsResponse'
    { datasetContentSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Summary information about dataset contents that have been created.
listDatasetContentsResponse_datasetContentSummaries :: Lens.Lens' ListDatasetContentsResponse (Prelude.Maybe [DatasetContentSummary])
listDatasetContentsResponse_datasetContentSummaries = Lens.lens (\ListDatasetContentsResponse' {datasetContentSummaries} -> datasetContentSummaries) (\s@ListDatasetContentsResponse' {} a -> s {datasetContentSummaries = a} :: ListDatasetContentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results, or @null@ if there are no
-- more results.
listDatasetContentsResponse_nextToken :: Lens.Lens' ListDatasetContentsResponse (Prelude.Maybe Prelude.Text)
listDatasetContentsResponse_nextToken = Lens.lens (\ListDatasetContentsResponse' {nextToken} -> nextToken) (\s@ListDatasetContentsResponse' {} a -> s {nextToken = a} :: ListDatasetContentsResponse)

-- | The response's http status code.
listDatasetContentsResponse_httpStatus :: Lens.Lens' ListDatasetContentsResponse Prelude.Int
listDatasetContentsResponse_httpStatus = Lens.lens (\ListDatasetContentsResponse' {httpStatus} -> httpStatus) (\s@ListDatasetContentsResponse' {} a -> s {httpStatus = a} :: ListDatasetContentsResponse)

instance Prelude.NFData ListDatasetContentsResponse where
  rnf ListDatasetContentsResponse' {..} =
    Prelude.rnf datasetContentSummaries
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
