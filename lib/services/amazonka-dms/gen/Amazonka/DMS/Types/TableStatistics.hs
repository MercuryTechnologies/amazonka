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
-- Module      : Amazonka.DMS.Types.TableStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.TableStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a collection of table statistics in response to a request by
-- the @DescribeTableStatistics@ operation.
--
-- /See:/ 'newTableStatistics' smart constructor.
data TableStatistics = TableStatistics'
  { -- | The validation state of the table.
    --
    -- This parameter can have the following values:
    --
    -- -   Not enabled – Validation isn\'t enabled for the table in the
    --     migration task.
    --
    -- -   Pending records – Some records in the table are waiting for
    --     validation.
    --
    -- -   Mismatched records – Some records in the table don\'t match between
    --     the source and target.
    --
    -- -   Suspended records – Some records in the table couldn\'t be
    --     validated.
    --
    -- -   No primary key –The table couldn\'t be validated because it has no
    --     primary key.
    --
    -- -   Table error – The table wasn\'t validated because it\'s in an error
    --     state and some data wasn\'t migrated.
    --
    -- -   Validated – All rows in the table are validated. If the table is
    --     updated, the status can change from Validated.
    --
    -- -   Error – The table couldn\'t be validated because of an unexpected
    --     error.
    --
    -- -   Pending validation – The table is waiting validation.
    --
    -- -   Preparing table – Preparing the table enabled in the migration task
    --     for validation.
    --
    -- -   Pending revalidation – All rows in the table are pending validation
    --     after the table was updated.
    validationState :: Prelude.Maybe Prelude.Text,
    -- | The number of rows added during the full load operation.
    fullLoadRows :: Prelude.Maybe Prelude.Integer,
    -- | The number of insert actions performed on a table.
    inserts :: Prelude.Maybe Prelude.Integer,
    -- | The time when the full load operation completed.
    fullLoadEndTime :: Prelude.Maybe Core.POSIX,
    -- | The number of rows that failed conditional checks during the full load
    -- operation (valid only for migrations where DynamoDB is the target).
    fullLoadCondtnlChkFailedRows :: Prelude.Maybe Prelude.Integer,
    -- | A value that indicates if the table was reloaded (@true@) or loaded as
    -- part of a new full load operation (@false@).
    fullLoadReloaded :: Prelude.Maybe Prelude.Bool,
    -- | The number of records that failed validation.
    validationFailedRecords :: Prelude.Maybe Prelude.Integer,
    -- | The number of records that couldn\'t be validated.
    validationSuspendedRecords :: Prelude.Maybe Prelude.Integer,
    -- | The schema name.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | Additional details about the state of validation.
    validationStateDetails :: Prelude.Maybe Prelude.Text,
    -- | The state of the tables described.
    --
    -- Valid states: Table does not exist | Before load | Full load | Table
    -- completed | Table cancelled | Table error | Table all | Table updates |
    -- Table is being reloaded
    tableState :: Prelude.Maybe Prelude.Text,
    -- | The number of rows that failed to load during the full load operation
    -- (valid only for migrations where DynamoDB is the target).
    fullLoadErrorRows :: Prelude.Maybe Prelude.Integer,
    -- | The data definition language (DDL) used to build and modify the
    -- structure of your tables.
    ddls :: Prelude.Maybe Prelude.Integer,
    -- | The number of delete actions performed on a table.
    deletes :: Prelude.Maybe Prelude.Integer,
    -- | The number of update actions performed on a table.
    updates :: Prelude.Maybe Prelude.Integer,
    -- | The number of records that have yet to be validated.
    validationPendingRecords :: Prelude.Maybe Prelude.Integer,
    -- | The time when the full load operation started.
    fullLoadStartTime :: Prelude.Maybe Core.POSIX,
    -- | The last time a table was updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the table.
    tableName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'validationState', 'tableStatistics_validationState' - The validation state of the table.
--
-- This parameter can have the following values:
--
-- -   Not enabled – Validation isn\'t enabled for the table in the
--     migration task.
--
-- -   Pending records – Some records in the table are waiting for
--     validation.
--
-- -   Mismatched records – Some records in the table don\'t match between
--     the source and target.
--
-- -   Suspended records – Some records in the table couldn\'t be
--     validated.
--
-- -   No primary key –The table couldn\'t be validated because it has no
--     primary key.
--
-- -   Table error – The table wasn\'t validated because it\'s in an error
--     state and some data wasn\'t migrated.
--
-- -   Validated – All rows in the table are validated. If the table is
--     updated, the status can change from Validated.
--
-- -   Error – The table couldn\'t be validated because of an unexpected
--     error.
--
-- -   Pending validation – The table is waiting validation.
--
-- -   Preparing table – Preparing the table enabled in the migration task
--     for validation.
--
-- -   Pending revalidation – All rows in the table are pending validation
--     after the table was updated.
--
-- 'fullLoadRows', 'tableStatistics_fullLoadRows' - The number of rows added during the full load operation.
--
-- 'inserts', 'tableStatistics_inserts' - The number of insert actions performed on a table.
--
-- 'fullLoadEndTime', 'tableStatistics_fullLoadEndTime' - The time when the full load operation completed.
--
-- 'fullLoadCondtnlChkFailedRows', 'tableStatistics_fullLoadCondtnlChkFailedRows' - The number of rows that failed conditional checks during the full load
-- operation (valid only for migrations where DynamoDB is the target).
--
-- 'fullLoadReloaded', 'tableStatistics_fullLoadReloaded' - A value that indicates if the table was reloaded (@true@) or loaded as
-- part of a new full load operation (@false@).
--
-- 'validationFailedRecords', 'tableStatistics_validationFailedRecords' - The number of records that failed validation.
--
-- 'validationSuspendedRecords', 'tableStatistics_validationSuspendedRecords' - The number of records that couldn\'t be validated.
--
-- 'schemaName', 'tableStatistics_schemaName' - The schema name.
--
-- 'validationStateDetails', 'tableStatistics_validationStateDetails' - Additional details about the state of validation.
--
-- 'tableState', 'tableStatistics_tableState' - The state of the tables described.
--
-- Valid states: Table does not exist | Before load | Full load | Table
-- completed | Table cancelled | Table error | Table all | Table updates |
-- Table is being reloaded
--
-- 'fullLoadErrorRows', 'tableStatistics_fullLoadErrorRows' - The number of rows that failed to load during the full load operation
-- (valid only for migrations where DynamoDB is the target).
--
-- 'ddls', 'tableStatistics_ddls' - The data definition language (DDL) used to build and modify the
-- structure of your tables.
--
-- 'deletes', 'tableStatistics_deletes' - The number of delete actions performed on a table.
--
-- 'updates', 'tableStatistics_updates' - The number of update actions performed on a table.
--
-- 'validationPendingRecords', 'tableStatistics_validationPendingRecords' - The number of records that have yet to be validated.
--
-- 'fullLoadStartTime', 'tableStatistics_fullLoadStartTime' - The time when the full load operation started.
--
-- 'lastUpdateTime', 'tableStatistics_lastUpdateTime' - The last time a table was updated.
--
-- 'tableName', 'tableStatistics_tableName' - The name of the table.
newTableStatistics ::
  TableStatistics
newTableStatistics =
  TableStatistics'
    { validationState = Prelude.Nothing,
      fullLoadRows = Prelude.Nothing,
      inserts = Prelude.Nothing,
      fullLoadEndTime = Prelude.Nothing,
      fullLoadCondtnlChkFailedRows = Prelude.Nothing,
      fullLoadReloaded = Prelude.Nothing,
      validationFailedRecords = Prelude.Nothing,
      validationSuspendedRecords = Prelude.Nothing,
      schemaName = Prelude.Nothing,
      validationStateDetails = Prelude.Nothing,
      tableState = Prelude.Nothing,
      fullLoadErrorRows = Prelude.Nothing,
      ddls = Prelude.Nothing,
      deletes = Prelude.Nothing,
      updates = Prelude.Nothing,
      validationPendingRecords = Prelude.Nothing,
      fullLoadStartTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      tableName = Prelude.Nothing
    }

-- | The validation state of the table.
--
-- This parameter can have the following values:
--
-- -   Not enabled – Validation isn\'t enabled for the table in the
--     migration task.
--
-- -   Pending records – Some records in the table are waiting for
--     validation.
--
-- -   Mismatched records – Some records in the table don\'t match between
--     the source and target.
--
-- -   Suspended records – Some records in the table couldn\'t be
--     validated.
--
-- -   No primary key –The table couldn\'t be validated because it has no
--     primary key.
--
-- -   Table error – The table wasn\'t validated because it\'s in an error
--     state and some data wasn\'t migrated.
--
-- -   Validated – All rows in the table are validated. If the table is
--     updated, the status can change from Validated.
--
-- -   Error – The table couldn\'t be validated because of an unexpected
--     error.
--
-- -   Pending validation – The table is waiting validation.
--
-- -   Preparing table – Preparing the table enabled in the migration task
--     for validation.
--
-- -   Pending revalidation – All rows in the table are pending validation
--     after the table was updated.
tableStatistics_validationState :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Text)
tableStatistics_validationState = Lens.lens (\TableStatistics' {validationState} -> validationState) (\s@TableStatistics' {} a -> s {validationState = a} :: TableStatistics)

-- | The number of rows added during the full load operation.
tableStatistics_fullLoadRows :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Integer)
tableStatistics_fullLoadRows = Lens.lens (\TableStatistics' {fullLoadRows} -> fullLoadRows) (\s@TableStatistics' {} a -> s {fullLoadRows = a} :: TableStatistics)

-- | The number of insert actions performed on a table.
tableStatistics_inserts :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Integer)
tableStatistics_inserts = Lens.lens (\TableStatistics' {inserts} -> inserts) (\s@TableStatistics' {} a -> s {inserts = a} :: TableStatistics)

-- | The time when the full load operation completed.
tableStatistics_fullLoadEndTime :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.UTCTime)
tableStatistics_fullLoadEndTime = Lens.lens (\TableStatistics' {fullLoadEndTime} -> fullLoadEndTime) (\s@TableStatistics' {} a -> s {fullLoadEndTime = a} :: TableStatistics) Prelude.. Lens.mapping Core._Time

-- | The number of rows that failed conditional checks during the full load
-- operation (valid only for migrations where DynamoDB is the target).
tableStatistics_fullLoadCondtnlChkFailedRows :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Integer)
tableStatistics_fullLoadCondtnlChkFailedRows = Lens.lens (\TableStatistics' {fullLoadCondtnlChkFailedRows} -> fullLoadCondtnlChkFailedRows) (\s@TableStatistics' {} a -> s {fullLoadCondtnlChkFailedRows = a} :: TableStatistics)

-- | A value that indicates if the table was reloaded (@true@) or loaded as
-- part of a new full load operation (@false@).
tableStatistics_fullLoadReloaded :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Bool)
tableStatistics_fullLoadReloaded = Lens.lens (\TableStatistics' {fullLoadReloaded} -> fullLoadReloaded) (\s@TableStatistics' {} a -> s {fullLoadReloaded = a} :: TableStatistics)

-- | The number of records that failed validation.
tableStatistics_validationFailedRecords :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Integer)
tableStatistics_validationFailedRecords = Lens.lens (\TableStatistics' {validationFailedRecords} -> validationFailedRecords) (\s@TableStatistics' {} a -> s {validationFailedRecords = a} :: TableStatistics)

-- | The number of records that couldn\'t be validated.
tableStatistics_validationSuspendedRecords :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Integer)
tableStatistics_validationSuspendedRecords = Lens.lens (\TableStatistics' {validationSuspendedRecords} -> validationSuspendedRecords) (\s@TableStatistics' {} a -> s {validationSuspendedRecords = a} :: TableStatistics)

-- | The schema name.
tableStatistics_schemaName :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Text)
tableStatistics_schemaName = Lens.lens (\TableStatistics' {schemaName} -> schemaName) (\s@TableStatistics' {} a -> s {schemaName = a} :: TableStatistics)

-- | Additional details about the state of validation.
tableStatistics_validationStateDetails :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Text)
tableStatistics_validationStateDetails = Lens.lens (\TableStatistics' {validationStateDetails} -> validationStateDetails) (\s@TableStatistics' {} a -> s {validationStateDetails = a} :: TableStatistics)

-- | The state of the tables described.
--
-- Valid states: Table does not exist | Before load | Full load | Table
-- completed | Table cancelled | Table error | Table all | Table updates |
-- Table is being reloaded
tableStatistics_tableState :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Text)
tableStatistics_tableState = Lens.lens (\TableStatistics' {tableState} -> tableState) (\s@TableStatistics' {} a -> s {tableState = a} :: TableStatistics)

-- | The number of rows that failed to load during the full load operation
-- (valid only for migrations where DynamoDB is the target).
tableStatistics_fullLoadErrorRows :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Integer)
tableStatistics_fullLoadErrorRows = Lens.lens (\TableStatistics' {fullLoadErrorRows} -> fullLoadErrorRows) (\s@TableStatistics' {} a -> s {fullLoadErrorRows = a} :: TableStatistics)

-- | The data definition language (DDL) used to build and modify the
-- structure of your tables.
tableStatistics_ddls :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Integer)
tableStatistics_ddls = Lens.lens (\TableStatistics' {ddls} -> ddls) (\s@TableStatistics' {} a -> s {ddls = a} :: TableStatistics)

-- | The number of delete actions performed on a table.
tableStatistics_deletes :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Integer)
tableStatistics_deletes = Lens.lens (\TableStatistics' {deletes} -> deletes) (\s@TableStatistics' {} a -> s {deletes = a} :: TableStatistics)

-- | The number of update actions performed on a table.
tableStatistics_updates :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Integer)
tableStatistics_updates = Lens.lens (\TableStatistics' {updates} -> updates) (\s@TableStatistics' {} a -> s {updates = a} :: TableStatistics)

-- | The number of records that have yet to be validated.
tableStatistics_validationPendingRecords :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Integer)
tableStatistics_validationPendingRecords = Lens.lens (\TableStatistics' {validationPendingRecords} -> validationPendingRecords) (\s@TableStatistics' {} a -> s {validationPendingRecords = a} :: TableStatistics)

-- | The time when the full load operation started.
tableStatistics_fullLoadStartTime :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.UTCTime)
tableStatistics_fullLoadStartTime = Lens.lens (\TableStatistics' {fullLoadStartTime} -> fullLoadStartTime) (\s@TableStatistics' {} a -> s {fullLoadStartTime = a} :: TableStatistics) Prelude.. Lens.mapping Core._Time

-- | The last time a table was updated.
tableStatistics_lastUpdateTime :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.UTCTime)
tableStatistics_lastUpdateTime = Lens.lens (\TableStatistics' {lastUpdateTime} -> lastUpdateTime) (\s@TableStatistics' {} a -> s {lastUpdateTime = a} :: TableStatistics) Prelude.. Lens.mapping Core._Time

-- | The name of the table.
tableStatistics_tableName :: Lens.Lens' TableStatistics (Prelude.Maybe Prelude.Text)
tableStatistics_tableName = Lens.lens (\TableStatistics' {tableName} -> tableName) (\s@TableStatistics' {} a -> s {tableName = a} :: TableStatistics)

instance Core.FromJSON TableStatistics where
  parseJSON =
    Core.withObject
      "TableStatistics"
      ( \x ->
          TableStatistics'
            Prelude.<$> (x Core..:? "ValidationState")
            Prelude.<*> (x Core..:? "FullLoadRows")
            Prelude.<*> (x Core..:? "Inserts")
            Prelude.<*> (x Core..:? "FullLoadEndTime")
            Prelude.<*> (x Core..:? "FullLoadCondtnlChkFailedRows")
            Prelude.<*> (x Core..:? "FullLoadReloaded")
            Prelude.<*> (x Core..:? "ValidationFailedRecords")
            Prelude.<*> (x Core..:? "ValidationSuspendedRecords")
            Prelude.<*> (x Core..:? "SchemaName")
            Prelude.<*> (x Core..:? "ValidationStateDetails")
            Prelude.<*> (x Core..:? "TableState")
            Prelude.<*> (x Core..:? "FullLoadErrorRows")
            Prelude.<*> (x Core..:? "Ddls")
            Prelude.<*> (x Core..:? "Deletes")
            Prelude.<*> (x Core..:? "Updates")
            Prelude.<*> (x Core..:? "ValidationPendingRecords")
            Prelude.<*> (x Core..:? "FullLoadStartTime")
            Prelude.<*> (x Core..:? "LastUpdateTime")
            Prelude.<*> (x Core..:? "TableName")
      )

instance Prelude.Hashable TableStatistics where
  hashWithSalt salt' TableStatistics' {..} =
    salt' `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` fullLoadStartTime
      `Prelude.hashWithSalt` validationPendingRecords
      `Prelude.hashWithSalt` updates
      `Prelude.hashWithSalt` deletes
      `Prelude.hashWithSalt` ddls
      `Prelude.hashWithSalt` fullLoadErrorRows
      `Prelude.hashWithSalt` tableState
      `Prelude.hashWithSalt` validationStateDetails
      `Prelude.hashWithSalt` schemaName
      `Prelude.hashWithSalt` validationSuspendedRecords
      `Prelude.hashWithSalt` validationFailedRecords
      `Prelude.hashWithSalt` fullLoadReloaded
      `Prelude.hashWithSalt` fullLoadCondtnlChkFailedRows
      `Prelude.hashWithSalt` fullLoadEndTime
      `Prelude.hashWithSalt` inserts
      `Prelude.hashWithSalt` fullLoadRows
      `Prelude.hashWithSalt` validationState

instance Prelude.NFData TableStatistics where
  rnf TableStatistics' {..} =
    Prelude.rnf validationState
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf fullLoadStartTime
      `Prelude.seq` Prelude.rnf validationPendingRecords
      `Prelude.seq` Prelude.rnf updates
      `Prelude.seq` Prelude.rnf deletes
      `Prelude.seq` Prelude.rnf ddls
      `Prelude.seq` Prelude.rnf fullLoadErrorRows
      `Prelude.seq` Prelude.rnf tableState
      `Prelude.seq` Prelude.rnf validationStateDetails
      `Prelude.seq` Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf validationSuspendedRecords
      `Prelude.seq` Prelude.rnf validationFailedRecords
      `Prelude.seq` Prelude.rnf fullLoadReloaded
      `Prelude.seq` Prelude.rnf fullLoadCondtnlChkFailedRows
      `Prelude.seq` Prelude.rnf fullLoadEndTime
      `Prelude.seq` Prelude.rnf inserts
      `Prelude.seq` Prelude.rnf fullLoadRows
