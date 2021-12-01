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
-- Module      : Amazonka.QuickSight.ListFolders
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all folders in an account.
module Amazonka.QuickSight.ListFolders
  ( -- * Creating a Request
    ListFolders (..),
    newListFolders,

    -- * Request Lenses
    listFolders_nextToken,
    listFolders_maxResults,
    listFolders_awsAccountId,

    -- * Destructuring the Response
    ListFoldersResponse (..),
    newListFoldersResponse,

    -- * Response Lenses
    listFoldersResponse_requestId,
    listFoldersResponse_nextToken,
    listFoldersResponse_folderSummaryList,
    listFoldersResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFolders' smart constructor.
data ListFolders = ListFolders'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The AWS account ID.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFolders' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFolders_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'maxResults', 'listFolders_maxResults' - The maximum number of results to be returned per request.
--
-- 'awsAccountId', 'listFolders_awsAccountId' - The AWS account ID.
newListFolders ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListFolders
newListFolders pAwsAccountId_ =
  ListFolders'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listFolders_nextToken :: Lens.Lens' ListFolders (Prelude.Maybe Prelude.Text)
listFolders_nextToken = Lens.lens (\ListFolders' {nextToken} -> nextToken) (\s@ListFolders' {} a -> s {nextToken = a} :: ListFolders)

-- | The maximum number of results to be returned per request.
listFolders_maxResults :: Lens.Lens' ListFolders (Prelude.Maybe Prelude.Natural)
listFolders_maxResults = Lens.lens (\ListFolders' {maxResults} -> maxResults) (\s@ListFolders' {} a -> s {maxResults = a} :: ListFolders)

-- | The AWS account ID.
listFolders_awsAccountId :: Lens.Lens' ListFolders Prelude.Text
listFolders_awsAccountId = Lens.lens (\ListFolders' {awsAccountId} -> awsAccountId) (\s@ListFolders' {} a -> s {awsAccountId = a} :: ListFolders)

instance Core.AWSRequest ListFolders where
  type AWSResponse ListFolders = ListFoldersResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFoldersResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "FolderSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFolders where
  hashWithSalt salt' ListFolders' {..} =
    salt' `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListFolders where
  rnf ListFolders' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListFolders where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListFolders where
  toPath ListFolders' {..} =
    Prelude.mconcat
      ["/accounts/", Core.toBS awsAccountId, "/folders"]

instance Core.ToQuery ListFolders where
  toQuery ListFolders' {..} =
    Prelude.mconcat
      [ "next-token" Core.=: nextToken,
        "max-results" Core.=: maxResults
      ]

-- | /See:/ 'newListFoldersResponse' smart constructor.
data ListFoldersResponse = ListFoldersResponse'
  { -- | The request ID.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains all of the folders in your AWS account. This
    -- structure provides basic information about the folders.
    folderSummaryList :: Prelude.Maybe [FolderSummary],
    -- | The status. If succeeded, the status is @SC_OK@
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFoldersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'listFoldersResponse_requestId' - The request ID.
--
-- 'nextToken', 'listFoldersResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'folderSummaryList', 'listFoldersResponse_folderSummaryList' - A structure that contains all of the folders in your AWS account. This
-- structure provides basic information about the folders.
--
-- 'status', 'listFoldersResponse_status' - The status. If succeeded, the status is @SC_OK@
newListFoldersResponse ::
  -- | 'status'
  Prelude.Int ->
  ListFoldersResponse
newListFoldersResponse pStatus_ =
  ListFoldersResponse'
    { requestId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      folderSummaryList = Prelude.Nothing,
      status = pStatus_
    }

-- | The request ID.
listFoldersResponse_requestId :: Lens.Lens' ListFoldersResponse (Prelude.Maybe Prelude.Text)
listFoldersResponse_requestId = Lens.lens (\ListFoldersResponse' {requestId} -> requestId) (\s@ListFoldersResponse' {} a -> s {requestId = a} :: ListFoldersResponse)

-- | The token for the next set of results, or null if there are no more
-- results.
listFoldersResponse_nextToken :: Lens.Lens' ListFoldersResponse (Prelude.Maybe Prelude.Text)
listFoldersResponse_nextToken = Lens.lens (\ListFoldersResponse' {nextToken} -> nextToken) (\s@ListFoldersResponse' {} a -> s {nextToken = a} :: ListFoldersResponse)

-- | A structure that contains all of the folders in your AWS account. This
-- structure provides basic information about the folders.
listFoldersResponse_folderSummaryList :: Lens.Lens' ListFoldersResponse (Prelude.Maybe [FolderSummary])
listFoldersResponse_folderSummaryList = Lens.lens (\ListFoldersResponse' {folderSummaryList} -> folderSummaryList) (\s@ListFoldersResponse' {} a -> s {folderSummaryList = a} :: ListFoldersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The status. If succeeded, the status is @SC_OK@
listFoldersResponse_status :: Lens.Lens' ListFoldersResponse Prelude.Int
listFoldersResponse_status = Lens.lens (\ListFoldersResponse' {status} -> status) (\s@ListFoldersResponse' {} a -> s {status = a} :: ListFoldersResponse)

instance Prelude.NFData ListFoldersResponse where
  rnf ListFoldersResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf folderSummaryList
      `Prelude.seq` Prelude.rnf nextToken
