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
-- Module      : Amazonka.DevOpsGuru.Types.PredictionTimeRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.PredictionTimeRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The time range during which anomalous behavior in a proactive anomaly or
-- an insight is expected to occur.
--
-- /See:/ 'newPredictionTimeRange' smart constructor.
data PredictionTimeRange = PredictionTimeRange'
  { -- | The time when the behavior in a proactive insight is expected to end.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The time range during which a metric limit is expected to be exceeded.
    -- This applies to proactive insights only.
    startTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PredictionTimeRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'predictionTimeRange_endTime' - The time when the behavior in a proactive insight is expected to end.
--
-- 'startTime', 'predictionTimeRange_startTime' - The time range during which a metric limit is expected to be exceeded.
-- This applies to proactive insights only.
newPredictionTimeRange ::
  -- | 'startTime'
  Prelude.UTCTime ->
  PredictionTimeRange
newPredictionTimeRange pStartTime_ =
  PredictionTimeRange'
    { endTime = Prelude.Nothing,
      startTime = Core._Time Lens.# pStartTime_
    }

-- | The time when the behavior in a proactive insight is expected to end.
predictionTimeRange_endTime :: Lens.Lens' PredictionTimeRange (Prelude.Maybe Prelude.UTCTime)
predictionTimeRange_endTime = Lens.lens (\PredictionTimeRange' {endTime} -> endTime) (\s@PredictionTimeRange' {} a -> s {endTime = a} :: PredictionTimeRange) Prelude.. Lens.mapping Core._Time

-- | The time range during which a metric limit is expected to be exceeded.
-- This applies to proactive insights only.
predictionTimeRange_startTime :: Lens.Lens' PredictionTimeRange Prelude.UTCTime
predictionTimeRange_startTime = Lens.lens (\PredictionTimeRange' {startTime} -> startTime) (\s@PredictionTimeRange' {} a -> s {startTime = a} :: PredictionTimeRange) Prelude.. Core._Time

instance Core.FromJSON PredictionTimeRange where
  parseJSON =
    Core.withObject
      "PredictionTimeRange"
      ( \x ->
          PredictionTimeRange'
            Prelude.<$> (x Core..:? "EndTime")
            Prelude.<*> (x Core..: "StartTime")
      )

instance Prelude.Hashable PredictionTimeRange where
  hashWithSalt salt' PredictionTimeRange' {..} =
    salt' `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData PredictionTimeRange where
  rnf PredictionTimeRange' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf startTime
