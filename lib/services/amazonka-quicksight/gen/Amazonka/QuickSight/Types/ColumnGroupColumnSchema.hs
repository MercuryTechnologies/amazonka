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
-- Module      : Amazonka.QuickSight.Types.ColumnGroupColumnSchema
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ColumnGroupColumnSchema where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure describing the name, data type, and geographic role of the
-- columns.
--
-- /See:/ 'newColumnGroupColumnSchema' smart constructor.
data ColumnGroupColumnSchema = ColumnGroupColumnSchema'
  { -- | The name of the column group\'s column schema.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnGroupColumnSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'columnGroupColumnSchema_name' - The name of the column group\'s column schema.
newColumnGroupColumnSchema ::
  ColumnGroupColumnSchema
newColumnGroupColumnSchema =
  ColumnGroupColumnSchema' {name = Prelude.Nothing}

-- | The name of the column group\'s column schema.
columnGroupColumnSchema_name :: Lens.Lens' ColumnGroupColumnSchema (Prelude.Maybe Prelude.Text)
columnGroupColumnSchema_name = Lens.lens (\ColumnGroupColumnSchema' {name} -> name) (\s@ColumnGroupColumnSchema' {} a -> s {name = a} :: ColumnGroupColumnSchema)

instance Core.FromJSON ColumnGroupColumnSchema where
  parseJSON =
    Core.withObject
      "ColumnGroupColumnSchema"
      ( \x ->
          ColumnGroupColumnSchema'
            Prelude.<$> (x Core..:? "Name")
      )

instance Prelude.Hashable ColumnGroupColumnSchema where
  hashWithSalt salt' ColumnGroupColumnSchema' {..} =
    salt' `Prelude.hashWithSalt` name

instance Prelude.NFData ColumnGroupColumnSchema where
  rnf ColumnGroupColumnSchema' {..} = Prelude.rnf name
