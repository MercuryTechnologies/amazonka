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
-- Module      : Amazonka.QuickSight.Types.RenameColumnOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.RenameColumnOperation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A transform operation that renames a column.
--
-- /See:/ 'newRenameColumnOperation' smart constructor.
data RenameColumnOperation = RenameColumnOperation'
  { -- | The name of the column to be renamed.
    columnName :: Prelude.Text,
    -- | The new name for the column.
    newColumnName' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RenameColumnOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnName', 'renameColumnOperation_columnName' - The name of the column to be renamed.
--
-- 'newColumnName'', 'renameColumnOperation_newColumnName' - The new name for the column.
newRenameColumnOperation ::
  -- | 'columnName'
  Prelude.Text ->
  -- | 'newColumnName''
  Prelude.Text ->
  RenameColumnOperation
newRenameColumnOperation pColumnName_ pNewColumnName_ =
  RenameColumnOperation'
    { columnName = pColumnName_,
      newColumnName' = pNewColumnName_
    }

-- | The name of the column to be renamed.
renameColumnOperation_columnName :: Lens.Lens' RenameColumnOperation Prelude.Text
renameColumnOperation_columnName = Lens.lens (\RenameColumnOperation' {columnName} -> columnName) (\s@RenameColumnOperation' {} a -> s {columnName = a} :: RenameColumnOperation)

-- | The new name for the column.
renameColumnOperation_newColumnName :: Lens.Lens' RenameColumnOperation Prelude.Text
renameColumnOperation_newColumnName = Lens.lens (\RenameColumnOperation' {newColumnName'} -> newColumnName') (\s@RenameColumnOperation' {} a -> s {newColumnName' = a} :: RenameColumnOperation)

instance Core.FromJSON RenameColumnOperation where
  parseJSON =
    Core.withObject
      "RenameColumnOperation"
      ( \x ->
          RenameColumnOperation'
            Prelude.<$> (x Core..: "ColumnName")
            Prelude.<*> (x Core..: "NewColumnName")
      )

instance Prelude.Hashable RenameColumnOperation where
  hashWithSalt salt' RenameColumnOperation' {..} =
    salt' `Prelude.hashWithSalt` newColumnName'
      `Prelude.hashWithSalt` columnName

instance Prelude.NFData RenameColumnOperation where
  rnf RenameColumnOperation' {..} =
    Prelude.rnf columnName
      `Prelude.seq` Prelude.rnf newColumnName'

instance Core.ToJSON RenameColumnOperation where
  toJSON RenameColumnOperation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ColumnName" Core..= columnName),
            Prelude.Just
              ("NewColumnName" Core..= newColumnName')
          ]
      )
