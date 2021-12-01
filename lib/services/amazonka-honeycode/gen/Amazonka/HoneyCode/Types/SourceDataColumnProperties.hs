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
-- Module      : Amazonka.HoneyCode.Types.SourceDataColumnProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HoneyCode.Types.SourceDataColumnProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the properties for importing data to a specific
-- column in a table.
--
-- /See:/ 'newSourceDataColumnProperties' smart constructor.
data SourceDataColumnProperties = SourceDataColumnProperties'
  { -- | The index of the column in the input file.
    columnIndex :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceDataColumnProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnIndex', 'sourceDataColumnProperties_columnIndex' - The index of the column in the input file.
newSourceDataColumnProperties ::
  SourceDataColumnProperties
newSourceDataColumnProperties =
  SourceDataColumnProperties'
    { columnIndex =
        Prelude.Nothing
    }

-- | The index of the column in the input file.
sourceDataColumnProperties_columnIndex :: Lens.Lens' SourceDataColumnProperties (Prelude.Maybe Prelude.Natural)
sourceDataColumnProperties_columnIndex = Lens.lens (\SourceDataColumnProperties' {columnIndex} -> columnIndex) (\s@SourceDataColumnProperties' {} a -> s {columnIndex = a} :: SourceDataColumnProperties)

instance Core.FromJSON SourceDataColumnProperties where
  parseJSON =
    Core.withObject
      "SourceDataColumnProperties"
      ( \x ->
          SourceDataColumnProperties'
            Prelude.<$> (x Core..:? "columnIndex")
      )

instance Prelude.Hashable SourceDataColumnProperties where
  hashWithSalt salt' SourceDataColumnProperties' {..} =
    salt' `Prelude.hashWithSalt` columnIndex

instance Prelude.NFData SourceDataColumnProperties where
  rnf SourceDataColumnProperties' {..} =
    Prelude.rnf columnIndex

instance Core.ToJSON SourceDataColumnProperties where
  toJSON SourceDataColumnProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [("columnIndex" Core..=) Prelude.<$> columnIndex]
      )
