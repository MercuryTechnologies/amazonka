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
-- Module      : Amazonka.SecurityHub.Types.Cell
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Cell where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An occurrence of sensitive data detected in a Microsoft Excel workbook,
-- comma-separated value (CSV) file, or tab-separated value (TSV) file.
--
-- /See:/ 'newCell' smart constructor.
data Cell = Cell'
  { -- | The row number of the row that contains the data.
    row :: Prelude.Maybe Prelude.Integer,
    -- | For a Microsoft Excel workbook, provides the location of the cell, as an
    -- absolute cell reference, that contains the data. For example, Sheet2!C5
    -- for cell C5 on Sheet2.
    cellReference :: Prelude.Maybe Prelude.Text,
    -- | The column number of the column that contains the data. For a Microsoft
    -- Excel workbook, the column number corresponds to the alphabetical column
    -- identifiers. For example, a value of 1 for Column corresponds to the A
    -- column in the workbook.
    column :: Prelude.Maybe Prelude.Integer,
    -- | The name of the column that contains the data.
    columnName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cell' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'row', 'cell_row' - The row number of the row that contains the data.
--
-- 'cellReference', 'cell_cellReference' - For a Microsoft Excel workbook, provides the location of the cell, as an
-- absolute cell reference, that contains the data. For example, Sheet2!C5
-- for cell C5 on Sheet2.
--
-- 'column', 'cell_column' - The column number of the column that contains the data. For a Microsoft
-- Excel workbook, the column number corresponds to the alphabetical column
-- identifiers. For example, a value of 1 for Column corresponds to the A
-- column in the workbook.
--
-- 'columnName', 'cell_columnName' - The name of the column that contains the data.
newCell ::
  Cell
newCell =
  Cell'
    { row = Prelude.Nothing,
      cellReference = Prelude.Nothing,
      column = Prelude.Nothing,
      columnName = Prelude.Nothing
    }

-- | The row number of the row that contains the data.
cell_row :: Lens.Lens' Cell (Prelude.Maybe Prelude.Integer)
cell_row = Lens.lens (\Cell' {row} -> row) (\s@Cell' {} a -> s {row = a} :: Cell)

-- | For a Microsoft Excel workbook, provides the location of the cell, as an
-- absolute cell reference, that contains the data. For example, Sheet2!C5
-- for cell C5 on Sheet2.
cell_cellReference :: Lens.Lens' Cell (Prelude.Maybe Prelude.Text)
cell_cellReference = Lens.lens (\Cell' {cellReference} -> cellReference) (\s@Cell' {} a -> s {cellReference = a} :: Cell)

-- | The column number of the column that contains the data. For a Microsoft
-- Excel workbook, the column number corresponds to the alphabetical column
-- identifiers. For example, a value of 1 for Column corresponds to the A
-- column in the workbook.
cell_column :: Lens.Lens' Cell (Prelude.Maybe Prelude.Integer)
cell_column = Lens.lens (\Cell' {column} -> column) (\s@Cell' {} a -> s {column = a} :: Cell)

-- | The name of the column that contains the data.
cell_columnName :: Lens.Lens' Cell (Prelude.Maybe Prelude.Text)
cell_columnName = Lens.lens (\Cell' {columnName} -> columnName) (\s@Cell' {} a -> s {columnName = a} :: Cell)

instance Core.FromJSON Cell where
  parseJSON =
    Core.withObject
      "Cell"
      ( \x ->
          Cell'
            Prelude.<$> (x Core..:? "Row")
            Prelude.<*> (x Core..:? "CellReference")
            Prelude.<*> (x Core..:? "Column")
            Prelude.<*> (x Core..:? "ColumnName")
      )

instance Prelude.Hashable Cell where
  hashWithSalt salt' Cell' {..} =
    salt' `Prelude.hashWithSalt` columnName
      `Prelude.hashWithSalt` column
      `Prelude.hashWithSalt` cellReference
      `Prelude.hashWithSalt` row

instance Prelude.NFData Cell where
  rnf Cell' {..} =
    Prelude.rnf row
      `Prelude.seq` Prelude.rnf columnName
      `Prelude.seq` Prelude.rnf column
      `Prelude.seq` Prelude.rnf cellReference

instance Core.ToJSON Cell where
  toJSON Cell' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Row" Core..=) Prelude.<$> row,
            ("CellReference" Core..=) Prelude.<$> cellReference,
            ("Column" Core..=) Prelude.<$> column,
            ("ColumnName" Core..=) Prelude.<$> columnName
          ]
      )
