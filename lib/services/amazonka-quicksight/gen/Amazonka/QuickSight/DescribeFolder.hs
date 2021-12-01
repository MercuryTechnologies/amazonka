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
-- Module      : Amazonka.QuickSight.DescribeFolder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a folder.
module Amazonka.QuickSight.DescribeFolder
  ( -- * Creating a Request
    DescribeFolder (..),
    newDescribeFolder,

    -- * Request Lenses
    describeFolder_awsAccountId,
    describeFolder_folderId,

    -- * Destructuring the Response
    DescribeFolderResponse (..),
    newDescribeFolderResponse,

    -- * Response Lenses
    describeFolderResponse_requestId,
    describeFolderResponse_folder,
    describeFolderResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFolder' smart constructor.
data DescribeFolder = DescribeFolder'
  { -- | The AWS account ID.
    awsAccountId :: Prelude.Text,
    -- | The folder ID.
    folderId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFolder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsAccountId', 'describeFolder_awsAccountId' - The AWS account ID.
--
-- 'folderId', 'describeFolder_folderId' - The folder ID.
newDescribeFolder ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'folderId'
  Prelude.Text ->
  DescribeFolder
newDescribeFolder pAwsAccountId_ pFolderId_ =
  DescribeFolder'
    { awsAccountId = pAwsAccountId_,
      folderId = pFolderId_
    }

-- | The AWS account ID.
describeFolder_awsAccountId :: Lens.Lens' DescribeFolder Prelude.Text
describeFolder_awsAccountId = Lens.lens (\DescribeFolder' {awsAccountId} -> awsAccountId) (\s@DescribeFolder' {} a -> s {awsAccountId = a} :: DescribeFolder)

-- | The folder ID.
describeFolder_folderId :: Lens.Lens' DescribeFolder Prelude.Text
describeFolder_folderId = Lens.lens (\DescribeFolder' {folderId} -> folderId) (\s@DescribeFolder' {} a -> s {folderId = a} :: DescribeFolder)

instance Core.AWSRequest DescribeFolder where
  type
    AWSResponse DescribeFolder =
      DescribeFolderResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeFolderResponse'
            Prelude.<$> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Folder")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeFolder where
  hashWithSalt salt' DescribeFolder' {..} =
    salt' `Prelude.hashWithSalt` folderId
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData DescribeFolder where
  rnf DescribeFolder' {..} =
    Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf folderId

instance Core.ToHeaders DescribeFolder where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeFolder where
  toPath DescribeFolder' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/folders/",
        Core.toBS folderId
      ]

instance Core.ToQuery DescribeFolder where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeFolderResponse' smart constructor.
data DescribeFolderResponse = DescribeFolderResponse'
  { -- | The request ID.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | Information about the folder.
    folder :: Prelude.Maybe Folder,
    -- | The status. If succeeded, the status is @SC_OK (200)@.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeFolderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestId', 'describeFolderResponse_requestId' - The request ID.
--
-- 'folder', 'describeFolderResponse_folder' - Information about the folder.
--
-- 'status', 'describeFolderResponse_status' - The status. If succeeded, the status is @SC_OK (200)@.
newDescribeFolderResponse ::
  -- | 'status'
  Prelude.Int ->
  DescribeFolderResponse
newDescribeFolderResponse pStatus_ =
  DescribeFolderResponse'
    { requestId =
        Prelude.Nothing,
      folder = Prelude.Nothing,
      status = pStatus_
    }

-- | The request ID.
describeFolderResponse_requestId :: Lens.Lens' DescribeFolderResponse (Prelude.Maybe Prelude.Text)
describeFolderResponse_requestId = Lens.lens (\DescribeFolderResponse' {requestId} -> requestId) (\s@DescribeFolderResponse' {} a -> s {requestId = a} :: DescribeFolderResponse)

-- | Information about the folder.
describeFolderResponse_folder :: Lens.Lens' DescribeFolderResponse (Prelude.Maybe Folder)
describeFolderResponse_folder = Lens.lens (\DescribeFolderResponse' {folder} -> folder) (\s@DescribeFolderResponse' {} a -> s {folder = a} :: DescribeFolderResponse)

-- | The status. If succeeded, the status is @SC_OK (200)@.
describeFolderResponse_status :: Lens.Lens' DescribeFolderResponse Prelude.Int
describeFolderResponse_status = Lens.lens (\DescribeFolderResponse' {status} -> status) (\s@DescribeFolderResponse' {} a -> s {status = a} :: DescribeFolderResponse)

instance Prelude.NFData DescribeFolderResponse where
  rnf DescribeFolderResponse' {..} =
    Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf folder
