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
-- Module      : Amazonka.LookoutMetrics.Types.AnomalyGroupStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.AnomalyGroupStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutMetrics.Types.ItemizedMetricStats
import qualified Amazonka.Prelude as Prelude

-- | Aggregated statistics for a group of anomalous metrics.
--
-- /See:/ 'newAnomalyGroupStatistics' smart constructor.
data AnomalyGroupStatistics = AnomalyGroupStatistics'
  { -- | Statistics for individual metrics within the group.
    itemizedMetricStatsList :: Prelude.Maybe [ItemizedMetricStats],
    -- | The start of the time range that was searched.
    evaluationStartDate :: Prelude.Maybe Prelude.Text,
    -- | The number of groups found.
    totalCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnomalyGroupStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'itemizedMetricStatsList', 'anomalyGroupStatistics_itemizedMetricStatsList' - Statistics for individual metrics within the group.
--
-- 'evaluationStartDate', 'anomalyGroupStatistics_evaluationStartDate' - The start of the time range that was searched.
--
-- 'totalCount', 'anomalyGroupStatistics_totalCount' - The number of groups found.
newAnomalyGroupStatistics ::
  AnomalyGroupStatistics
newAnomalyGroupStatistics =
  AnomalyGroupStatistics'
    { itemizedMetricStatsList =
        Prelude.Nothing,
      evaluationStartDate = Prelude.Nothing,
      totalCount = Prelude.Nothing
    }

-- | Statistics for individual metrics within the group.
anomalyGroupStatistics_itemizedMetricStatsList :: Lens.Lens' AnomalyGroupStatistics (Prelude.Maybe [ItemizedMetricStats])
anomalyGroupStatistics_itemizedMetricStatsList = Lens.lens (\AnomalyGroupStatistics' {itemizedMetricStatsList} -> itemizedMetricStatsList) (\s@AnomalyGroupStatistics' {} a -> s {itemizedMetricStatsList = a} :: AnomalyGroupStatistics) Prelude.. Lens.mapping Lens.coerced

-- | The start of the time range that was searched.
anomalyGroupStatistics_evaluationStartDate :: Lens.Lens' AnomalyGroupStatistics (Prelude.Maybe Prelude.Text)
anomalyGroupStatistics_evaluationStartDate = Lens.lens (\AnomalyGroupStatistics' {evaluationStartDate} -> evaluationStartDate) (\s@AnomalyGroupStatistics' {} a -> s {evaluationStartDate = a} :: AnomalyGroupStatistics)

-- | The number of groups found.
anomalyGroupStatistics_totalCount :: Lens.Lens' AnomalyGroupStatistics (Prelude.Maybe Prelude.Int)
anomalyGroupStatistics_totalCount = Lens.lens (\AnomalyGroupStatistics' {totalCount} -> totalCount) (\s@AnomalyGroupStatistics' {} a -> s {totalCount = a} :: AnomalyGroupStatistics)

instance Core.FromJSON AnomalyGroupStatistics where
  parseJSON =
    Core.withObject
      "AnomalyGroupStatistics"
      ( \x ->
          AnomalyGroupStatistics'
            Prelude.<$> ( x Core..:? "ItemizedMetricStatsList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EvaluationStartDate")
            Prelude.<*> (x Core..:? "TotalCount")
      )

instance Prelude.Hashable AnomalyGroupStatistics where
  hashWithSalt salt' AnomalyGroupStatistics' {..} =
    salt' `Prelude.hashWithSalt` totalCount
      `Prelude.hashWithSalt` evaluationStartDate
      `Prelude.hashWithSalt` itemizedMetricStatsList

instance Prelude.NFData AnomalyGroupStatistics where
  rnf AnomalyGroupStatistics' {..} =
    Prelude.rnf itemizedMetricStatsList
      `Prelude.seq` Prelude.rnf totalCount
      `Prelude.seq` Prelude.rnf evaluationStartDate
