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
-- Module      : Amazonka.Glue.UpdateTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a metadata table in the Data Catalog.
module Amazonka.Glue.UpdateTable
  ( -- * Creating a Request
    UpdateTable (..),
    newUpdateTable,

    -- * Request Lenses
    updateTable_skipArchive,
    updateTable_catalogId,
    updateTable_databaseName,
    updateTable_tableInput,

    -- * Destructuring the Response
    UpdateTableResponse (..),
    newUpdateTableResponse,

    -- * Response Lenses
    updateTableResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateTable' smart constructor.
data UpdateTable = UpdateTable'
  { -- | By default, @UpdateTable@ always creates an archived version of the
    -- table before updating it. However, if @skipArchive@ is set to true,
    -- @UpdateTable@ does not create the archived version.
    skipArchive :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the Data Catalog where the table resides. If none is provided,
    -- the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the catalog database in which the table resides. For Hive
    -- compatibility, this name is entirely lowercase.
    databaseName :: Prelude.Text,
    -- | An updated @TableInput@ object to define the metadata table in the
    -- catalog.
    tableInput :: TableInput
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'skipArchive', 'updateTable_skipArchive' - By default, @UpdateTable@ always creates an archived version of the
-- table before updating it. However, if @skipArchive@ is set to true,
-- @UpdateTable@ does not create the archived version.
--
-- 'catalogId', 'updateTable_catalogId' - The ID of the Data Catalog where the table resides. If none is provided,
-- the Amazon Web Services account ID is used by default.
--
-- 'databaseName', 'updateTable_databaseName' - The name of the catalog database in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
--
-- 'tableInput', 'updateTable_tableInput' - An updated @TableInput@ object to define the metadata table in the
-- catalog.
newUpdateTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableInput'
  TableInput ->
  UpdateTable
newUpdateTable pDatabaseName_ pTableInput_ =
  UpdateTable'
    { skipArchive = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableInput = pTableInput_
    }

-- | By default, @UpdateTable@ always creates an archived version of the
-- table before updating it. However, if @skipArchive@ is set to true,
-- @UpdateTable@ does not create the archived version.
updateTable_skipArchive :: Lens.Lens' UpdateTable (Prelude.Maybe Prelude.Bool)
updateTable_skipArchive = Lens.lens (\UpdateTable' {skipArchive} -> skipArchive) (\s@UpdateTable' {} a -> s {skipArchive = a} :: UpdateTable)

-- | The ID of the Data Catalog where the table resides. If none is provided,
-- the Amazon Web Services account ID is used by default.
updateTable_catalogId :: Lens.Lens' UpdateTable (Prelude.Maybe Prelude.Text)
updateTable_catalogId = Lens.lens (\UpdateTable' {catalogId} -> catalogId) (\s@UpdateTable' {} a -> s {catalogId = a} :: UpdateTable)

-- | The name of the catalog database in which the table resides. For Hive
-- compatibility, this name is entirely lowercase.
updateTable_databaseName :: Lens.Lens' UpdateTable Prelude.Text
updateTable_databaseName = Lens.lens (\UpdateTable' {databaseName} -> databaseName) (\s@UpdateTable' {} a -> s {databaseName = a} :: UpdateTable)

-- | An updated @TableInput@ object to define the metadata table in the
-- catalog.
updateTable_tableInput :: Lens.Lens' UpdateTable TableInput
updateTable_tableInput = Lens.lens (\UpdateTable' {tableInput} -> tableInput) (\s@UpdateTable' {} a -> s {tableInput = a} :: UpdateTable)

instance Core.AWSRequest UpdateTable where
  type AWSResponse UpdateTable = UpdateTableResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateTable where
  hashWithSalt salt' UpdateTable' {..} =
    salt' `Prelude.hashWithSalt` tableInput
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` skipArchive

instance Prelude.NFData UpdateTable where
  rnf UpdateTable' {..} =
    Prelude.rnf skipArchive
      `Prelude.seq` Prelude.rnf tableInput
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf catalogId

instance Core.ToHeaders UpdateTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.UpdateTable" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateTable where
  toJSON UpdateTable' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SkipArchive" Core..=) Prelude.<$> skipArchive,
            ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableInput" Core..= tableInput)
          ]
      )

instance Core.ToPath UpdateTable where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateTableResponse' smart constructor.
data UpdateTableResponse = UpdateTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateTableResponse_httpStatus' - The response's http status code.
newUpdateTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateTableResponse
newUpdateTableResponse pHttpStatus_ =
  UpdateTableResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateTableResponse_httpStatus :: Lens.Lens' UpdateTableResponse Prelude.Int
updateTableResponse_httpStatus = Lens.lens (\UpdateTableResponse' {httpStatus} -> httpStatus) (\s@UpdateTableResponse' {} a -> s {httpStatus = a} :: UpdateTableResponse)

instance Prelude.NFData UpdateTableResponse where
  rnf UpdateTableResponse' {..} = Prelude.rnf httpStatus
