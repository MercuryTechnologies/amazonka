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
-- Module      : Amazonka.SageMaker.Types.TrialComponentMetricSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TrialComponentMetricSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of the metrics of a trial component.
--
-- /See:/ 'newTrialComponentMetricSummary' smart constructor.
data TrialComponentMetricSummary = TrialComponentMetricSummary'
  { -- | The maximum value of the metric.
    max :: Prelude.Maybe Prelude.Double,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The average value of the metric.
    avg :: Prelude.Maybe Prelude.Double,
    -- | The number of samples used to generate the metric.
    count :: Prelude.Maybe Prelude.Int,
    -- | The name of the metric.
    metricName :: Prelude.Maybe Prelude.Text,
    -- | The standard deviation of the metric.
    stdDev :: Prelude.Maybe Prelude.Double,
    -- | The minimum value of the metric.
    min :: Prelude.Maybe Prelude.Double,
    -- | The most recent value of the metric.
    last :: Prelude.Maybe Prelude.Double,
    -- | When the metric was last updated.
    timeStamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrialComponentMetricSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'trialComponentMetricSummary_max' - The maximum value of the metric.
--
-- 'sourceArn', 'trialComponentMetricSummary_sourceArn' - The Amazon Resource Name (ARN) of the source.
--
-- 'avg', 'trialComponentMetricSummary_avg' - The average value of the metric.
--
-- 'count', 'trialComponentMetricSummary_count' - The number of samples used to generate the metric.
--
-- 'metricName', 'trialComponentMetricSummary_metricName' - The name of the metric.
--
-- 'stdDev', 'trialComponentMetricSummary_stdDev' - The standard deviation of the metric.
--
-- 'min', 'trialComponentMetricSummary_min' - The minimum value of the metric.
--
-- 'last', 'trialComponentMetricSummary_last' - The most recent value of the metric.
--
-- 'timeStamp', 'trialComponentMetricSummary_timeStamp' - When the metric was last updated.
newTrialComponentMetricSummary ::
  TrialComponentMetricSummary
newTrialComponentMetricSummary =
  TrialComponentMetricSummary'
    { max = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      avg = Prelude.Nothing,
      count = Prelude.Nothing,
      metricName = Prelude.Nothing,
      stdDev = Prelude.Nothing,
      min = Prelude.Nothing,
      last = Prelude.Nothing,
      timeStamp = Prelude.Nothing
    }

-- | The maximum value of the metric.
trialComponentMetricSummary_max :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Double)
trialComponentMetricSummary_max = Lens.lens (\TrialComponentMetricSummary' {max} -> max) (\s@TrialComponentMetricSummary' {} a -> s {max = a} :: TrialComponentMetricSummary)

-- | The Amazon Resource Name (ARN) of the source.
trialComponentMetricSummary_sourceArn :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Text)
trialComponentMetricSummary_sourceArn = Lens.lens (\TrialComponentMetricSummary' {sourceArn} -> sourceArn) (\s@TrialComponentMetricSummary' {} a -> s {sourceArn = a} :: TrialComponentMetricSummary)

-- | The average value of the metric.
trialComponentMetricSummary_avg :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Double)
trialComponentMetricSummary_avg = Lens.lens (\TrialComponentMetricSummary' {avg} -> avg) (\s@TrialComponentMetricSummary' {} a -> s {avg = a} :: TrialComponentMetricSummary)

-- | The number of samples used to generate the metric.
trialComponentMetricSummary_count :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Int)
trialComponentMetricSummary_count = Lens.lens (\TrialComponentMetricSummary' {count} -> count) (\s@TrialComponentMetricSummary' {} a -> s {count = a} :: TrialComponentMetricSummary)

-- | The name of the metric.
trialComponentMetricSummary_metricName :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Text)
trialComponentMetricSummary_metricName = Lens.lens (\TrialComponentMetricSummary' {metricName} -> metricName) (\s@TrialComponentMetricSummary' {} a -> s {metricName = a} :: TrialComponentMetricSummary)

-- | The standard deviation of the metric.
trialComponentMetricSummary_stdDev :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Double)
trialComponentMetricSummary_stdDev = Lens.lens (\TrialComponentMetricSummary' {stdDev} -> stdDev) (\s@TrialComponentMetricSummary' {} a -> s {stdDev = a} :: TrialComponentMetricSummary)

-- | The minimum value of the metric.
trialComponentMetricSummary_min :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Double)
trialComponentMetricSummary_min = Lens.lens (\TrialComponentMetricSummary' {min} -> min) (\s@TrialComponentMetricSummary' {} a -> s {min = a} :: TrialComponentMetricSummary)

-- | The most recent value of the metric.
trialComponentMetricSummary_last :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.Double)
trialComponentMetricSummary_last = Lens.lens (\TrialComponentMetricSummary' {last} -> last) (\s@TrialComponentMetricSummary' {} a -> s {last = a} :: TrialComponentMetricSummary)

-- | When the metric was last updated.
trialComponentMetricSummary_timeStamp :: Lens.Lens' TrialComponentMetricSummary (Prelude.Maybe Prelude.UTCTime)
trialComponentMetricSummary_timeStamp = Lens.lens (\TrialComponentMetricSummary' {timeStamp} -> timeStamp) (\s@TrialComponentMetricSummary' {} a -> s {timeStamp = a} :: TrialComponentMetricSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON TrialComponentMetricSummary where
  parseJSON =
    Core.withObject
      "TrialComponentMetricSummary"
      ( \x ->
          TrialComponentMetricSummary'
            Prelude.<$> (x Core..:? "Max")
            Prelude.<*> (x Core..:? "SourceArn")
            Prelude.<*> (x Core..:? "Avg")
            Prelude.<*> (x Core..:? "Count")
            Prelude.<*> (x Core..:? "MetricName")
            Prelude.<*> (x Core..:? "StdDev")
            Prelude.<*> (x Core..:? "Min")
            Prelude.<*> (x Core..:? "Last")
            Prelude.<*> (x Core..:? "TimeStamp")
      )

instance Prelude.Hashable TrialComponentMetricSummary where
  hashWithSalt salt' TrialComponentMetricSummary' {..} =
    salt' `Prelude.hashWithSalt` timeStamp
      `Prelude.hashWithSalt` last
      `Prelude.hashWithSalt` min
      `Prelude.hashWithSalt` stdDev
      `Prelude.hashWithSalt` metricName
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` avg
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` max

instance Prelude.NFData TrialComponentMetricSummary where
  rnf TrialComponentMetricSummary' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf timeStamp
      `Prelude.seq` Prelude.rnf last
      `Prelude.seq` Prelude.rnf min
      `Prelude.seq` Prelude.rnf stdDev
      `Prelude.seq` Prelude.rnf metricName
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf avg
      `Prelude.seq` Prelude.rnf sourceArn
