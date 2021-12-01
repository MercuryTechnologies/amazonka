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
-- Module      : Amazonka.SageMaker.ListActions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the actions in your account and their properties.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListActions
  ( -- * Creating a Request
    ListActions (..),
    newListActions,

    -- * Request Lenses
    listActions_createdAfter,
    listActions_nextToken,
    listActions_sortOrder,
    listActions_sourceUri,
    listActions_actionType,
    listActions_maxResults,
    listActions_createdBefore,
    listActions_sortBy,

    -- * Destructuring the Response
    ListActionsResponse (..),
    newListActionsResponse,

    -- * Response Lenses
    listActionsResponse_actionSummaries,
    listActionsResponse_nextToken,
    listActionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListActions' smart constructor.
data ListActions = ListActions'
  { -- | A filter that returns only actions created on or after the specified
    -- time.
    createdAfter :: Prelude.Maybe Core.POSIX,
    -- | If the previous call to @ListActions@ didn\'t return the full set of
    -- actions, the call returns a token for getting the next set of actions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only actions with the specified source URI.
    sourceUri :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only actions of the specified type.
    actionType :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of actions to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only actions created on or before the specified
    -- time.
    createdBefore :: Prelude.Maybe Core.POSIX,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortActionsBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'listActions_createdAfter' - A filter that returns only actions created on or after the specified
-- time.
--
-- 'nextToken', 'listActions_nextToken' - If the previous call to @ListActions@ didn\'t return the full set of
-- actions, the call returns a token for getting the next set of actions.
--
-- 'sortOrder', 'listActions_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'sourceUri', 'listActions_sourceUri' - A filter that returns only actions with the specified source URI.
--
-- 'actionType', 'listActions_actionType' - A filter that returns only actions of the specified type.
--
-- 'maxResults', 'listActions_maxResults' - The maximum number of actions to return in the response. The default
-- value is 10.
--
-- 'createdBefore', 'listActions_createdBefore' - A filter that returns only actions created on or before the specified
-- time.
--
-- 'sortBy', 'listActions_sortBy' - The property used to sort results. The default value is @CreationTime@.
newListActions ::
  ListActions
newListActions =
  ListActions'
    { createdAfter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      sourceUri = Prelude.Nothing,
      actionType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A filter that returns only actions created on or after the specified
-- time.
listActions_createdAfter :: Lens.Lens' ListActions (Prelude.Maybe Prelude.UTCTime)
listActions_createdAfter = Lens.lens (\ListActions' {createdAfter} -> createdAfter) (\s@ListActions' {} a -> s {createdAfter = a} :: ListActions) Prelude.. Lens.mapping Core._Time

-- | If the previous call to @ListActions@ didn\'t return the full set of
-- actions, the call returns a token for getting the next set of actions.
listActions_nextToken :: Lens.Lens' ListActions (Prelude.Maybe Prelude.Text)
listActions_nextToken = Lens.lens (\ListActions' {nextToken} -> nextToken) (\s@ListActions' {} a -> s {nextToken = a} :: ListActions)

-- | The sort order. The default value is @Descending@.
listActions_sortOrder :: Lens.Lens' ListActions (Prelude.Maybe SortOrder)
listActions_sortOrder = Lens.lens (\ListActions' {sortOrder} -> sortOrder) (\s@ListActions' {} a -> s {sortOrder = a} :: ListActions)

-- | A filter that returns only actions with the specified source URI.
listActions_sourceUri :: Lens.Lens' ListActions (Prelude.Maybe Prelude.Text)
listActions_sourceUri = Lens.lens (\ListActions' {sourceUri} -> sourceUri) (\s@ListActions' {} a -> s {sourceUri = a} :: ListActions)

-- | A filter that returns only actions of the specified type.
listActions_actionType :: Lens.Lens' ListActions (Prelude.Maybe Prelude.Text)
listActions_actionType = Lens.lens (\ListActions' {actionType} -> actionType) (\s@ListActions' {} a -> s {actionType = a} :: ListActions)

-- | The maximum number of actions to return in the response. The default
-- value is 10.
listActions_maxResults :: Lens.Lens' ListActions (Prelude.Maybe Prelude.Natural)
listActions_maxResults = Lens.lens (\ListActions' {maxResults} -> maxResults) (\s@ListActions' {} a -> s {maxResults = a} :: ListActions)

-- | A filter that returns only actions created on or before the specified
-- time.
listActions_createdBefore :: Lens.Lens' ListActions (Prelude.Maybe Prelude.UTCTime)
listActions_createdBefore = Lens.lens (\ListActions' {createdBefore} -> createdBefore) (\s@ListActions' {} a -> s {createdBefore = a} :: ListActions) Prelude.. Lens.mapping Core._Time

-- | The property used to sort results. The default value is @CreationTime@.
listActions_sortBy :: Lens.Lens' ListActions (Prelude.Maybe SortActionsBy)
listActions_sortBy = Lens.lens (\ListActions' {sortBy} -> sortBy) (\s@ListActions' {} a -> s {sortBy = a} :: ListActions)

instance Core.AWSPager ListActions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listActionsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listActionsResponse_actionSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listActions_nextToken
          Lens..~ rs
          Lens.^? listActionsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListActions where
  type AWSResponse ListActions = ListActionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListActionsResponse'
            Prelude.<$> ( x Core..?> "ActionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListActions where
  hashWithSalt salt' ListActions' {..} =
    salt' `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` actionType
      `Prelude.hashWithSalt` sourceUri
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` createdAfter

instance Prelude.NFData ListActions where
  rnf ListActions' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf actionType
      `Prelude.seq` Prelude.rnf sourceUri
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders ListActions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListActions" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListActions where
  toJSON ListActions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CreatedAfter" Core..=) Prelude.<$> createdAfter,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("SourceUri" Core..=) Prelude.<$> sourceUri,
            ("ActionType" Core..=) Prelude.<$> actionType,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreatedBefore" Core..=) Prelude.<$> createdBefore,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListActions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListActions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListActionsResponse' smart constructor.
data ListActionsResponse = ListActionsResponse'
  { -- | A list of actions and their properties.
    actionSummaries :: Prelude.Maybe [ActionSummary],
    -- | A token for getting the next set of actions, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListActionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionSummaries', 'listActionsResponse_actionSummaries' - A list of actions and their properties.
--
-- 'nextToken', 'listActionsResponse_nextToken' - A token for getting the next set of actions, if there are any.
--
-- 'httpStatus', 'listActionsResponse_httpStatus' - The response's http status code.
newListActionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListActionsResponse
newListActionsResponse pHttpStatus_ =
  ListActionsResponse'
    { actionSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of actions and their properties.
listActionsResponse_actionSummaries :: Lens.Lens' ListActionsResponse (Prelude.Maybe [ActionSummary])
listActionsResponse_actionSummaries = Lens.lens (\ListActionsResponse' {actionSummaries} -> actionSummaries) (\s@ListActionsResponse' {} a -> s {actionSummaries = a} :: ListActionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token for getting the next set of actions, if there are any.
listActionsResponse_nextToken :: Lens.Lens' ListActionsResponse (Prelude.Maybe Prelude.Text)
listActionsResponse_nextToken = Lens.lens (\ListActionsResponse' {nextToken} -> nextToken) (\s@ListActionsResponse' {} a -> s {nextToken = a} :: ListActionsResponse)

-- | The response's http status code.
listActionsResponse_httpStatus :: Lens.Lens' ListActionsResponse Prelude.Int
listActionsResponse_httpStatus = Lens.lens (\ListActionsResponse' {httpStatus} -> httpStatus) (\s@ListActionsResponse' {} a -> s {httpStatus = a} :: ListActionsResponse)

instance Prelude.NFData ListActionsResponse where
  rnf ListActionsResponse' {..} =
    Prelude.rnf actionSummaries
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
