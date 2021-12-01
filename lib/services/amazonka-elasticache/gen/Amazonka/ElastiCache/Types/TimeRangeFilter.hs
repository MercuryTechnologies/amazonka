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
-- Module      : Amazonka.ElastiCache.Types.TimeRangeFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.TimeRangeFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Filters update actions from the service updates that are in available
-- status during the time range.
--
-- /See:/ 'newTimeRangeFilter' smart constructor.
data TimeRangeFilter = TimeRangeFilter'
  { -- | The start time of the time range filter
    startTime :: Prelude.Maybe Core.ISO8601,
    -- | The end time of the time range filter
    endTime :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimeRangeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startTime', 'timeRangeFilter_startTime' - The start time of the time range filter
--
-- 'endTime', 'timeRangeFilter_endTime' - The end time of the time range filter
newTimeRangeFilter ::
  TimeRangeFilter
newTimeRangeFilter =
  TimeRangeFilter'
    { startTime = Prelude.Nothing,
      endTime = Prelude.Nothing
    }

-- | The start time of the time range filter
timeRangeFilter_startTime :: Lens.Lens' TimeRangeFilter (Prelude.Maybe Prelude.UTCTime)
timeRangeFilter_startTime = Lens.lens (\TimeRangeFilter' {startTime} -> startTime) (\s@TimeRangeFilter' {} a -> s {startTime = a} :: TimeRangeFilter) Prelude.. Lens.mapping Core._Time

-- | The end time of the time range filter
timeRangeFilter_endTime :: Lens.Lens' TimeRangeFilter (Prelude.Maybe Prelude.UTCTime)
timeRangeFilter_endTime = Lens.lens (\TimeRangeFilter' {endTime} -> endTime) (\s@TimeRangeFilter' {} a -> s {endTime = a} :: TimeRangeFilter) Prelude.. Lens.mapping Core._Time

instance Prelude.Hashable TimeRangeFilter where
  hashWithSalt salt' TimeRangeFilter' {..} =
    salt' `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData TimeRangeFilter where
  rnf TimeRangeFilter' {..} =
    Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Core.ToQuery TimeRangeFilter where
  toQuery TimeRangeFilter' {..} =
    Prelude.mconcat
      [ "StartTime" Core.=: startTime,
        "EndTime" Core.=: endTime
      ]
