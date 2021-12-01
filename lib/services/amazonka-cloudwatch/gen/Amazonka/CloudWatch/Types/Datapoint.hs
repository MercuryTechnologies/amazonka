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
-- Module      : Amazonka.CloudWatch.Types.Datapoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatch.Types.Datapoint where

import Amazonka.CloudWatch.Types.StandardUnit
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Encapsulates the statistical data that CloudWatch computes from metric
-- data.
--
-- /See:/ 'newDatapoint' smart constructor.
data Datapoint = Datapoint'
  { -- | The number of metric values that contributed to the aggregate value of
    -- this data point.
    sampleCount :: Prelude.Maybe Prelude.Double,
    -- | The maximum metric value for the data point.
    maximum :: Prelude.Maybe Prelude.Double,
    -- | The average of the metric values that correspond to the data point.
    average :: Prelude.Maybe Prelude.Double,
    -- | The minimum metric value for the data point.
    minimum :: Prelude.Maybe Prelude.Double,
    -- | The percentile statistic for the data point.
    extendedStatistics :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double),
    -- | The sum of the metric values for the data point.
    sum :: Prelude.Maybe Prelude.Double,
    -- | The standard unit for the data point.
    unit :: Prelude.Maybe StandardUnit,
    -- | The time stamp used for the data point.
    timestamp :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Datapoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sampleCount', 'datapoint_sampleCount' - The number of metric values that contributed to the aggregate value of
-- this data point.
--
-- 'maximum', 'datapoint_maximum' - The maximum metric value for the data point.
--
-- 'average', 'datapoint_average' - The average of the metric values that correspond to the data point.
--
-- 'minimum', 'datapoint_minimum' - The minimum metric value for the data point.
--
-- 'extendedStatistics', 'datapoint_extendedStatistics' - The percentile statistic for the data point.
--
-- 'sum', 'datapoint_sum' - The sum of the metric values for the data point.
--
-- 'unit', 'datapoint_unit' - The standard unit for the data point.
--
-- 'timestamp', 'datapoint_timestamp' - The time stamp used for the data point.
newDatapoint ::
  Datapoint
newDatapoint =
  Datapoint'
    { sampleCount = Prelude.Nothing,
      maximum = Prelude.Nothing,
      average = Prelude.Nothing,
      minimum = Prelude.Nothing,
      extendedStatistics = Prelude.Nothing,
      sum = Prelude.Nothing,
      unit = Prelude.Nothing,
      timestamp = Prelude.Nothing
    }

-- | The number of metric values that contributed to the aggregate value of
-- this data point.
datapoint_sampleCount :: Lens.Lens' Datapoint (Prelude.Maybe Prelude.Double)
datapoint_sampleCount = Lens.lens (\Datapoint' {sampleCount} -> sampleCount) (\s@Datapoint' {} a -> s {sampleCount = a} :: Datapoint)

-- | The maximum metric value for the data point.
datapoint_maximum :: Lens.Lens' Datapoint (Prelude.Maybe Prelude.Double)
datapoint_maximum = Lens.lens (\Datapoint' {maximum} -> maximum) (\s@Datapoint' {} a -> s {maximum = a} :: Datapoint)

-- | The average of the metric values that correspond to the data point.
datapoint_average :: Lens.Lens' Datapoint (Prelude.Maybe Prelude.Double)
datapoint_average = Lens.lens (\Datapoint' {average} -> average) (\s@Datapoint' {} a -> s {average = a} :: Datapoint)

-- | The minimum metric value for the data point.
datapoint_minimum :: Lens.Lens' Datapoint (Prelude.Maybe Prelude.Double)
datapoint_minimum = Lens.lens (\Datapoint' {minimum} -> minimum) (\s@Datapoint' {} a -> s {minimum = a} :: Datapoint)

-- | The percentile statistic for the data point.
datapoint_extendedStatistics :: Lens.Lens' Datapoint (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Double))
datapoint_extendedStatistics = Lens.lens (\Datapoint' {extendedStatistics} -> extendedStatistics) (\s@Datapoint' {} a -> s {extendedStatistics = a} :: Datapoint) Prelude.. Lens.mapping Lens.coerced

-- | The sum of the metric values for the data point.
datapoint_sum :: Lens.Lens' Datapoint (Prelude.Maybe Prelude.Double)
datapoint_sum = Lens.lens (\Datapoint' {sum} -> sum) (\s@Datapoint' {} a -> s {sum = a} :: Datapoint)

-- | The standard unit for the data point.
datapoint_unit :: Lens.Lens' Datapoint (Prelude.Maybe StandardUnit)
datapoint_unit = Lens.lens (\Datapoint' {unit} -> unit) (\s@Datapoint' {} a -> s {unit = a} :: Datapoint)

-- | The time stamp used for the data point.
datapoint_timestamp :: Lens.Lens' Datapoint (Prelude.Maybe Prelude.UTCTime)
datapoint_timestamp = Lens.lens (\Datapoint' {timestamp} -> timestamp) (\s@Datapoint' {} a -> s {timestamp = a} :: Datapoint) Prelude.. Lens.mapping Core._Time

instance Core.FromXML Datapoint where
  parseXML x =
    Datapoint'
      Prelude.<$> (x Core..@? "SampleCount")
      Prelude.<*> (x Core..@? "Maximum")
      Prelude.<*> (x Core..@? "Average")
      Prelude.<*> (x Core..@? "Minimum")
      Prelude.<*> ( x Core..@? "ExtendedStatistics"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
                  )
      Prelude.<*> (x Core..@? "Sum")
      Prelude.<*> (x Core..@? "Unit")
      Prelude.<*> (x Core..@? "Timestamp")

instance Prelude.Hashable Datapoint where
  hashWithSalt salt' Datapoint' {..} =
    salt' `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` sum
      `Prelude.hashWithSalt` extendedStatistics
      `Prelude.hashWithSalt` minimum
      `Prelude.hashWithSalt` average
      `Prelude.hashWithSalt` maximum
      `Prelude.hashWithSalt` sampleCount

instance Prelude.NFData Datapoint where
  rnf Datapoint' {..} =
    Prelude.rnf sampleCount
      `Prelude.seq` Prelude.rnf timestamp
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf sum
      `Prelude.seq` Prelude.rnf extendedStatistics
      `Prelude.seq` Prelude.rnf minimum
      `Prelude.seq` Prelude.rnf average
      `Prelude.seq` Prelude.rnf maximum
