{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Glue.Types.GlueTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.GlueTable where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The database and table in the Glue Data Catalog that is used for input
-- or output data.
--
-- /See:/ 'newGlueTable' smart constructor.
data GlueTable = GlueTable'
  { -- | A unique identifier for the Glue Data Catalog.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of the connection to the Glue Data Catalog.
    connectionName :: Prelude.Maybe Prelude.Text,
    -- | A database name in the Glue Data Catalog.
    databaseName :: Prelude.Text,
    -- | A table name in the Glue Data Catalog.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GlueTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'glueTable_catalogId' - A unique identifier for the Glue Data Catalog.
--
-- 'connectionName', 'glueTable_connectionName' - The name of the connection to the Glue Data Catalog.
--
-- 'databaseName', 'glueTable_databaseName' - A database name in the Glue Data Catalog.
--
-- 'tableName', 'glueTable_tableName' - A table name in the Glue Data Catalog.
newGlueTable ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  GlueTable
newGlueTable pDatabaseName_ pTableName_ =
  GlueTable'
    { catalogId = Prelude.Nothing,
      connectionName = Prelude.Nothing,
      databaseName = pDatabaseName_,
      tableName = pTableName_
    }

-- | A unique identifier for the Glue Data Catalog.
glueTable_catalogId :: Lens.Lens' GlueTable (Prelude.Maybe Prelude.Text)
glueTable_catalogId = Lens.lens (\GlueTable' {catalogId} -> catalogId) (\s@GlueTable' {} a -> s {catalogId = a} :: GlueTable)

-- | The name of the connection to the Glue Data Catalog.
glueTable_connectionName :: Lens.Lens' GlueTable (Prelude.Maybe Prelude.Text)
glueTable_connectionName = Lens.lens (\GlueTable' {connectionName} -> connectionName) (\s@GlueTable' {} a -> s {connectionName = a} :: GlueTable)

-- | A database name in the Glue Data Catalog.
glueTable_databaseName :: Lens.Lens' GlueTable Prelude.Text
glueTable_databaseName = Lens.lens (\GlueTable' {databaseName} -> databaseName) (\s@GlueTable' {} a -> s {databaseName = a} :: GlueTable)

-- | A table name in the Glue Data Catalog.
glueTable_tableName :: Lens.Lens' GlueTable Prelude.Text
glueTable_tableName = Lens.lens (\GlueTable' {tableName} -> tableName) (\s@GlueTable' {} a -> s {tableName = a} :: GlueTable)

instance Core.FromJSON GlueTable where
  parseJSON =
    Core.withObject
      "GlueTable"
      ( \x ->
          GlueTable'
            Prelude.<$> (x Core..:? "CatalogId")
            Prelude.<*> (x Core..:? "ConnectionName")
            Prelude.<*> (x Core..: "DatabaseName")
            Prelude.<*> (x Core..: "TableName")
      )

instance Prelude.Hashable GlueTable where
  hashWithSalt salt' GlueTable' {..} =
    salt' `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` connectionName
      `Prelude.hashWithSalt` catalogId

instance Prelude.NFData GlueTable where
  rnf GlueTable' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf connectionName

instance Core.ToJSON GlueTable where
  toJSON GlueTable' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CatalogId" Core..=) Prelude.<$> catalogId,
            ("ConnectionName" Core..=)
              Prelude.<$> connectionName,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableName" Core..= tableName)
          ]
      )
