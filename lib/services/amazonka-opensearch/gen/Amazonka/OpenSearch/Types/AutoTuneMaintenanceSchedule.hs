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
-- Module      : Amazonka.OpenSearch.Types.AutoTuneMaintenanceSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AutoTuneMaintenanceSchedule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.Duration
import qualified Amazonka.Prelude as Prelude

-- | Specifies the Auto-Tune maintenance schedule. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
--
-- /See:/ 'newAutoTuneMaintenanceSchedule' smart constructor.
data AutoTuneMaintenanceSchedule = AutoTuneMaintenanceSchedule'
  { -- | The timestamp at which the Auto-Tune maintenance schedule starts.
    startAt :: Prelude.Maybe Core.POSIX,
    -- | A cron expression for a recurring maintenance schedule. See
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
    -- for more information.
    cronExpressionForRecurrence :: Prelude.Maybe Prelude.Text,
    -- | Specifies maintenance schedule duration: duration value and duration
    -- unit. See
    -- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
    -- for more information.
    duration :: Prelude.Maybe Duration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneMaintenanceSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startAt', 'autoTuneMaintenanceSchedule_startAt' - The timestamp at which the Auto-Tune maintenance schedule starts.
--
-- 'cronExpressionForRecurrence', 'autoTuneMaintenanceSchedule_cronExpressionForRecurrence' - A cron expression for a recurring maintenance schedule. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
--
-- 'duration', 'autoTuneMaintenanceSchedule_duration' - Specifies maintenance schedule duration: duration value and duration
-- unit. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
newAutoTuneMaintenanceSchedule ::
  AutoTuneMaintenanceSchedule
newAutoTuneMaintenanceSchedule =
  AutoTuneMaintenanceSchedule'
    { startAt =
        Prelude.Nothing,
      cronExpressionForRecurrence = Prelude.Nothing,
      duration = Prelude.Nothing
    }

-- | The timestamp at which the Auto-Tune maintenance schedule starts.
autoTuneMaintenanceSchedule_startAt :: Lens.Lens' AutoTuneMaintenanceSchedule (Prelude.Maybe Prelude.UTCTime)
autoTuneMaintenanceSchedule_startAt = Lens.lens (\AutoTuneMaintenanceSchedule' {startAt} -> startAt) (\s@AutoTuneMaintenanceSchedule' {} a -> s {startAt = a} :: AutoTuneMaintenanceSchedule) Prelude.. Lens.mapping Core._Time

-- | A cron expression for a recurring maintenance schedule. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
autoTuneMaintenanceSchedule_cronExpressionForRecurrence :: Lens.Lens' AutoTuneMaintenanceSchedule (Prelude.Maybe Prelude.Text)
autoTuneMaintenanceSchedule_cronExpressionForRecurrence = Lens.lens (\AutoTuneMaintenanceSchedule' {cronExpressionForRecurrence} -> cronExpressionForRecurrence) (\s@AutoTuneMaintenanceSchedule' {} a -> s {cronExpressionForRecurrence = a} :: AutoTuneMaintenanceSchedule)

-- | Specifies maintenance schedule duration: duration value and duration
-- unit. See
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>
-- for more information.
autoTuneMaintenanceSchedule_duration :: Lens.Lens' AutoTuneMaintenanceSchedule (Prelude.Maybe Duration)
autoTuneMaintenanceSchedule_duration = Lens.lens (\AutoTuneMaintenanceSchedule' {duration} -> duration) (\s@AutoTuneMaintenanceSchedule' {} a -> s {duration = a} :: AutoTuneMaintenanceSchedule)

instance Core.FromJSON AutoTuneMaintenanceSchedule where
  parseJSON =
    Core.withObject
      "AutoTuneMaintenanceSchedule"
      ( \x ->
          AutoTuneMaintenanceSchedule'
            Prelude.<$> (x Core..:? "StartAt")
            Prelude.<*> (x Core..:? "CronExpressionForRecurrence")
            Prelude.<*> (x Core..:? "Duration")
      )

instance Prelude.Hashable AutoTuneMaintenanceSchedule where
  hashWithSalt salt' AutoTuneMaintenanceSchedule' {..} =
    salt' `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` cronExpressionForRecurrence
      `Prelude.hashWithSalt` startAt

instance Prelude.NFData AutoTuneMaintenanceSchedule where
  rnf AutoTuneMaintenanceSchedule' {..} =
    Prelude.rnf startAt
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf cronExpressionForRecurrence

instance Core.ToJSON AutoTuneMaintenanceSchedule where
  toJSON AutoTuneMaintenanceSchedule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StartAt" Core..=) Prelude.<$> startAt,
            ("CronExpressionForRecurrence" Core..=)
              Prelude.<$> cronExpressionForRecurrence,
            ("Duration" Core..=) Prelude.<$> duration
          ]
      )
