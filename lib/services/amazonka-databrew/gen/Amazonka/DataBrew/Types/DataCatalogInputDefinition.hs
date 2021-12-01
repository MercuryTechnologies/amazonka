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
-- Module      : Amazonka.DataBrew.Types.DataCatalogInputDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.DataCatalogInputDefinition where

import qualified Amazonka.Core as Core
import Amazonka.DataBrew.Types.S3Location
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents how metadata stored in the Glue Data Catalog is defined in a
-- DataBrew dataset.
--
-- /See:/ 'newDataCatalogInputDefinition' smart constructor.
data DataCatalogInputDefinition = DataCatalogInputDefinition'
  { -- | Represents an Amazon location where DataBrew can store intermediate
    -- results.
    tempDirectory :: Prelude.Maybe S3Location,
    -- | The unique identifier of the Amazon Web Services account that holds the
    -- Data Catalog that stores the data.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The name of a database in the Data Catalog.
    databaseName :: Prelude.Text,
    -- | The name of a database table in the Data Catalog. This table corresponds
    -- to a DataBrew dataset.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataCatalogInputDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tempDirectory', 'dataCatalogInputDefinition_tempDirectory' - Represents an Amazon location where DataBrew can store intermediate
-- results.
--
-- 'catalogId', 'dataCatalogInputDefinition_catalogId' - The unique identifier of the Amazon Web Services account that holds the
-- Data Catalog that stores the data.
--
-- 'databaseName', 'dataCatalogInputDefinition_databaseName' - The name of a database in the Data Catalog.
--
-- 'tableName', 'dataCatalogInputDefinition_tableName' - The name of a database table in the Data Catalog. This table corresponds
-- to a DataBrew dataset.
newDataCatalogInputDefinition ::
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  DataCatalogInputDefinition
newDataCatalogInputDefinition
  pDatabaseName_
  pTableName_ =
    DataCatalogInputDefinition'
      { tempDirectory =
          Prelude.Nothing,
        catalogId = Prelude.Nothing,
        databaseName = pDatabaseName_,
        tableName = pTableName_
      }

-- | Represents an Amazon location where DataBrew can store intermediate
-- results.
dataCatalogInputDefinition_tempDirectory :: Lens.Lens' DataCatalogInputDefinition (Prelude.Maybe S3Location)
dataCatalogInputDefinition_tempDirectory = Lens.lens (\DataCatalogInputDefinition' {tempDirectory} -> tempDirectory) (\s@DataCatalogInputDefinition' {} a -> s {tempDirectory = a} :: DataCatalogInputDefinition)

-- | The unique identifier of the Amazon Web Services account that holds the
-- Data Catalog that stores the data.
dataCatalogInputDefinition_catalogId :: Lens.Lens' DataCatalogInputDefinition (Prelude.Maybe Prelude.Text)
dataCatalogInputDefinition_catalogId = Lens.lens (\DataCatalogInputDefinition' {catalogId} -> catalogId) (\s@DataCatalogInputDefinition' {} a -> s {catalogId = a} :: DataCatalogInputDefinition)

-- | The name of a database in the Data Catalog.
dataCatalogInputDefinition_databaseName :: Lens.Lens' DataCatalogInputDefinition Prelude.Text
dataCatalogInputDefinition_databaseName = Lens.lens (\DataCatalogInputDefinition' {databaseName} -> databaseName) (\s@DataCatalogInputDefinition' {} a -> s {databaseName = a} :: DataCatalogInputDefinition)

-- | The name of a database table in the Data Catalog. This table corresponds
-- to a DataBrew dataset.
dataCatalogInputDefinition_tableName :: Lens.Lens' DataCatalogInputDefinition Prelude.Text
dataCatalogInputDefinition_tableName = Lens.lens (\DataCatalogInputDefinition' {tableName} -> tableName) (\s@DataCatalogInputDefinition' {} a -> s {tableName = a} :: DataCatalogInputDefinition)

instance Core.FromJSON DataCatalogInputDefinition where
  parseJSON =
    Core.withObject
      "DataCatalogInputDefinition"
      ( \x ->
          DataCatalogInputDefinition'
            Prelude.<$> (x Core..:? "TempDirectory")
            Prelude.<*> (x Core..:? "CatalogId")
            Prelude.<*> (x Core..: "DatabaseName")
            Prelude.<*> (x Core..: "TableName")
      )

instance Prelude.Hashable DataCatalogInputDefinition where
  hashWithSalt salt' DataCatalogInputDefinition' {..} =
    salt' `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` tempDirectory

instance Prelude.NFData DataCatalogInputDefinition where
  rnf DataCatalogInputDefinition' {..} =
    Prelude.rnf tempDirectory
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf catalogId

instance Core.ToJSON DataCatalogInputDefinition where
  toJSON DataCatalogInputDefinition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TempDirectory" Core..=) Prelude.<$> tempDirectory,
            ("CatalogId" Core..=) Prelude.<$> catalogId,
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableName" Core..= tableName)
          ]
      )
