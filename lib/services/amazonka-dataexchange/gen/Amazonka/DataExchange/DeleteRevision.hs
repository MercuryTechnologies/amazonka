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
-- Module      : Amazonka.DataExchange.DeleteRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes a revision.
module Amazonka.DataExchange.DeleteRevision
  ( -- * Creating a Request
    DeleteRevision (..),
    newDeleteRevision,

    -- * Request Lenses
    deleteRevision_revisionId,
    deleteRevision_dataSetId,

    -- * Destructuring the Response
    DeleteRevisionResponse (..),
    newDeleteRevisionResponse,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteRevision' smart constructor.
data DeleteRevision = DeleteRevision'
  { -- | The unique identifier for a revision.
    revisionId :: Prelude.Text,
    -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'deleteRevision_revisionId' - The unique identifier for a revision.
--
-- 'dataSetId', 'deleteRevision_dataSetId' - The unique identifier for a data set.
newDeleteRevision ::
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  DeleteRevision
newDeleteRevision pRevisionId_ pDataSetId_ =
  DeleteRevision'
    { revisionId = pRevisionId_,
      dataSetId = pDataSetId_
    }

-- | The unique identifier for a revision.
deleteRevision_revisionId :: Lens.Lens' DeleteRevision Prelude.Text
deleteRevision_revisionId = Lens.lens (\DeleteRevision' {revisionId} -> revisionId) (\s@DeleteRevision' {} a -> s {revisionId = a} :: DeleteRevision)

-- | The unique identifier for a data set.
deleteRevision_dataSetId :: Lens.Lens' DeleteRevision Prelude.Text
deleteRevision_dataSetId = Lens.lens (\DeleteRevision' {dataSetId} -> dataSetId) (\s@DeleteRevision' {} a -> s {dataSetId = a} :: DeleteRevision)

instance Core.AWSRequest DeleteRevision where
  type
    AWSResponse DeleteRevision =
      DeleteRevisionResponse
  request = Request.delete defaultService
  response =
    Response.receiveNull DeleteRevisionResponse'

instance Prelude.Hashable DeleteRevision where
  hashWithSalt salt' DeleteRevision' {..} =
    salt' `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` revisionId

instance Prelude.NFData DeleteRevision where
  rnf DeleteRevision' {..} =
    Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf dataSetId

instance Core.ToHeaders DeleteRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteRevision where
  toPath DeleteRevision' {..} =
    Prelude.mconcat
      [ "/v1/data-sets/",
        Core.toBS dataSetId,
        "/revisions/",
        Core.toBS revisionId
      ]

instance Core.ToQuery DeleteRevision where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteRevisionResponse' smart constructor.
data DeleteRevisionResponse = DeleteRevisionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteRevisionResponse ::
  DeleteRevisionResponse
newDeleteRevisionResponse = DeleteRevisionResponse'

instance Prelude.NFData DeleteRevisionResponse where
  rnf _ = ()
