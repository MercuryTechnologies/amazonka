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
-- Module      : Amazonka.Glue.Types.ColumnStatisticsError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ColumnStatisticsError where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.ColumnStatistics
import Amazonka.Glue.Types.ErrorDetail
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Encapsulates a @ColumnStatistics@ object that failed and the reason for
-- failure.
--
-- /See:/ 'newColumnStatisticsError' smart constructor.
data ColumnStatisticsError = ColumnStatisticsError'
  { -- | An error message with the reason for the failure of an operation.
    error :: Prelude.Maybe ErrorDetail,
    -- | The @ColumnStatistics@ of the column.
    columnStatistics :: Prelude.Maybe ColumnStatistics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ColumnStatisticsError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'columnStatisticsError_error' - An error message with the reason for the failure of an operation.
--
-- 'columnStatistics', 'columnStatisticsError_columnStatistics' - The @ColumnStatistics@ of the column.
newColumnStatisticsError ::
  ColumnStatisticsError
newColumnStatisticsError =
  ColumnStatisticsError'
    { error = Prelude.Nothing,
      columnStatistics = Prelude.Nothing
    }

-- | An error message with the reason for the failure of an operation.
columnStatisticsError_error :: Lens.Lens' ColumnStatisticsError (Prelude.Maybe ErrorDetail)
columnStatisticsError_error = Lens.lens (\ColumnStatisticsError' {error} -> error) (\s@ColumnStatisticsError' {} a -> s {error = a} :: ColumnStatisticsError)

-- | The @ColumnStatistics@ of the column.
columnStatisticsError_columnStatistics :: Lens.Lens' ColumnStatisticsError (Prelude.Maybe ColumnStatistics)
columnStatisticsError_columnStatistics = Lens.lens (\ColumnStatisticsError' {columnStatistics} -> columnStatistics) (\s@ColumnStatisticsError' {} a -> s {columnStatistics = a} :: ColumnStatisticsError)

instance Core.FromJSON ColumnStatisticsError where
  parseJSON =
    Core.withObject
      "ColumnStatisticsError"
      ( \x ->
          ColumnStatisticsError'
            Prelude.<$> (x Core..:? "Error")
            Prelude.<*> (x Core..:? "ColumnStatistics")
      )

instance Prelude.Hashable ColumnStatisticsError where
  hashWithSalt salt' ColumnStatisticsError' {..} =
    salt' `Prelude.hashWithSalt` columnStatistics
      `Prelude.hashWithSalt` error

instance Prelude.NFData ColumnStatisticsError where
  rnf ColumnStatisticsError' {..} =
    Prelude.rnf error
      `Prelude.seq` Prelude.rnf columnStatistics
