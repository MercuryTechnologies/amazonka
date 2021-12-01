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
-- Module      : Amazonka.LookoutMetrics.Types.AnomalyGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutMetrics.Types.MetricLevelImpact
import qualified Amazonka.Prelude as Prelude

-- | A group of anomalous metrics
--
-- /See:/ 'newAnomalyGroup' smart constructor.
data AnomalyGroup = AnomalyGroup'
  { -- | A list of measures affected by the anomaly.
    metricLevelImpactList :: Prelude.Maybe [MetricLevelImpact],
    -- | The start time for the group.
    startTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the anomaly group.
    anomalyGroupId :: Prelude.Maybe Prelude.Text,
    -- | The severity score of the group.
    anomalyGroupScore :: Prelude.Maybe Prelude.Double,
    -- | The name of the primary affected measure for the group.
    primaryMetricName :: Prelude.Maybe Prelude.Text,
    -- | The end time for the group.
    endTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metricLevelImpactList', 'anomalyGroup_metricLevelImpactList' - A list of measures affected by the anomaly.
--
-- 'startTime', 'anomalyGroup_startTime' - The start time for the group.
--
-- 'anomalyGroupId', 'anomalyGroup_anomalyGroupId' - The ID of the anomaly group.
--
-- 'anomalyGroupScore', 'anomalyGroup_anomalyGroupScore' - The severity score of the group.
--
-- 'primaryMetricName', 'anomalyGroup_primaryMetricName' - The name of the primary affected measure for the group.
--
-- 'endTime', 'anomalyGroup_endTime' - The end time for the group.
newAnomalyGroup ::
  AnomalyGroup
newAnomalyGroup =
  AnomalyGroup'
    { metricLevelImpactList =
        Prelude.Nothing,
      startTime = Prelude.Nothing,
      anomalyGroupId = Prelude.Nothing,
      anomalyGroupScore = Prelude.Nothing,
      primaryMetricName = Prelude.Nothing,
      endTime = Prelude.Nothing
    }

-- | A list of measures affected by the anomaly.
anomalyGroup_metricLevelImpactList :: Lens.Lens' AnomalyGroup (Prelude.Maybe [MetricLevelImpact])
anomalyGroup_metricLevelImpactList = Lens.lens (\AnomalyGroup' {metricLevelImpactList} -> metricLevelImpactList) (\s@AnomalyGroup' {} a -> s {metricLevelImpactList = a} :: AnomalyGroup) Prelude.. Lens.mapping Lens.coerced

-- | The start time for the group.
anomalyGroup_startTime :: Lens.Lens' AnomalyGroup (Prelude.Maybe Prelude.Text)
anomalyGroup_startTime = Lens.lens (\AnomalyGroup' {startTime} -> startTime) (\s@AnomalyGroup' {} a -> s {startTime = a} :: AnomalyGroup)

-- | The ID of the anomaly group.
anomalyGroup_anomalyGroupId :: Lens.Lens' AnomalyGroup (Prelude.Maybe Prelude.Text)
anomalyGroup_anomalyGroupId = Lens.lens (\AnomalyGroup' {anomalyGroupId} -> anomalyGroupId) (\s@AnomalyGroup' {} a -> s {anomalyGroupId = a} :: AnomalyGroup)

-- | The severity score of the group.
anomalyGroup_anomalyGroupScore :: Lens.Lens' AnomalyGroup (Prelude.Maybe Prelude.Double)
anomalyGroup_anomalyGroupScore = Lens.lens (\AnomalyGroup' {anomalyGroupScore} -> anomalyGroupScore) (\s@AnomalyGroup' {} a -> s {anomalyGroupScore = a} :: AnomalyGroup)

-- | The name of the primary affected measure for the group.
anomalyGroup_primaryMetricName :: Lens.Lens' AnomalyGroup (Prelude.Maybe Prelude.Text)
anomalyGroup_primaryMetricName = Lens.lens (\AnomalyGroup' {primaryMetricName} -> primaryMetricName) (\s@AnomalyGroup' {} a -> s {primaryMetricName = a} :: AnomalyGroup)

-- | The end time for the group.
anomalyGroup_endTime :: Lens.Lens' AnomalyGroup (Prelude.Maybe Prelude.Text)
anomalyGroup_endTime = Lens.lens (\AnomalyGroup' {endTime} -> endTime) (\s@AnomalyGroup' {} a -> s {endTime = a} :: AnomalyGroup)

instance Core.FromJSON AnomalyGroup where
  parseJSON =
    Core.withObject
      "AnomalyGroup"
      ( \x ->
          AnomalyGroup'
            Prelude.<$> ( x Core..:? "MetricLevelImpactList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "AnomalyGroupId")
            Prelude.<*> (x Core..:? "AnomalyGroupScore")
            Prelude.<*> (x Core..:? "PrimaryMetricName")
            Prelude.<*> (x Core..:? "EndTime")
      )

instance Prelude.Hashable AnomalyGroup where
  hashWithSalt salt' AnomalyGroup' {..} =
    salt' `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` primaryMetricName
      `Prelude.hashWithSalt` anomalyGroupScore
      `Prelude.hashWithSalt` anomalyGroupId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` metricLevelImpactList

instance Prelude.NFData AnomalyGroup where
  rnf AnomalyGroup' {..} =
    Prelude.rnf metricLevelImpactList
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf primaryMetricName
      `Prelude.seq` Prelude.rnf anomalyGroupScore
      `Prelude.seq` Prelude.rnf anomalyGroupId
      `Prelude.seq` Prelude.rnf startTime
