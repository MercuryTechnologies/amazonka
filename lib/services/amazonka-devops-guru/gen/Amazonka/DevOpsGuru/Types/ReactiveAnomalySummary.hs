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
-- Module      : Amazonka.DevOpsGuru.Types.ReactiveAnomalySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ReactiveAnomalySummary where

import qualified Amazonka.Core as Core
import Amazonka.DevOpsGuru.Types.AnomalyReportedTimeRange
import Amazonka.DevOpsGuru.Types.AnomalySeverity
import Amazonka.DevOpsGuru.Types.AnomalySourceDetails
import Amazonka.DevOpsGuru.Types.AnomalyStatus
import Amazonka.DevOpsGuru.Types.AnomalyTimeRange
import Amazonka.DevOpsGuru.Types.ResourceCollection
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a reactive anomaly. This object is returned by
-- @DescribeAnomaly.@
--
-- /See:/ 'newReactiveAnomalySummary' smart constructor.
data ReactiveAnomalySummary = ReactiveAnomalySummary'
  { -- | A @AnomalyReportedTimeRange@ object that specifies the time range
    -- between when the anomaly is opened and the time when it is closed.
    anomalyReportedTimeRange :: Prelude.Maybe AnomalyReportedTimeRange,
    -- | The status of the reactive anomaly.
    status :: Prelude.Maybe AnomalyStatus,
    resourceCollection :: Prelude.Maybe ResourceCollection,
    -- | The severity of the reactive anomaly.
    severity :: Prelude.Maybe AnomalySeverity,
    -- | Details about the source of the analyzed operational data that triggered
    -- the anomaly. The one supported source is Amazon CloudWatch metrics.
    sourceDetails :: Prelude.Maybe AnomalySourceDetails,
    -- | The ID of the reactive anomaly.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the insight that contains this anomaly. An insight is composed
    -- of related anomalies.
    associatedInsightId :: Prelude.Maybe Prelude.Text,
    anomalyTimeRange :: Prelude.Maybe AnomalyTimeRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReactiveAnomalySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'anomalyReportedTimeRange', 'reactiveAnomalySummary_anomalyReportedTimeRange' - A @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
--
-- 'status', 'reactiveAnomalySummary_status' - The status of the reactive anomaly.
--
-- 'resourceCollection', 'reactiveAnomalySummary_resourceCollection' - Undocumented member.
--
-- 'severity', 'reactiveAnomalySummary_severity' - The severity of the reactive anomaly.
--
-- 'sourceDetails', 'reactiveAnomalySummary_sourceDetails' - Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
--
-- 'id', 'reactiveAnomalySummary_id' - The ID of the reactive anomaly.
--
-- 'associatedInsightId', 'reactiveAnomalySummary_associatedInsightId' - The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
--
-- 'anomalyTimeRange', 'reactiveAnomalySummary_anomalyTimeRange' - Undocumented member.
newReactiveAnomalySummary ::
  ReactiveAnomalySummary
newReactiveAnomalySummary =
  ReactiveAnomalySummary'
    { anomalyReportedTimeRange =
        Prelude.Nothing,
      status = Prelude.Nothing,
      resourceCollection = Prelude.Nothing,
      severity = Prelude.Nothing,
      sourceDetails = Prelude.Nothing,
      id = Prelude.Nothing,
      associatedInsightId = Prelude.Nothing,
      anomalyTimeRange = Prelude.Nothing
    }

-- | A @AnomalyReportedTimeRange@ object that specifies the time range
-- between when the anomaly is opened and the time when it is closed.
reactiveAnomalySummary_anomalyReportedTimeRange :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe AnomalyReportedTimeRange)
reactiveAnomalySummary_anomalyReportedTimeRange = Lens.lens (\ReactiveAnomalySummary' {anomalyReportedTimeRange} -> anomalyReportedTimeRange) (\s@ReactiveAnomalySummary' {} a -> s {anomalyReportedTimeRange = a} :: ReactiveAnomalySummary)

-- | The status of the reactive anomaly.
reactiveAnomalySummary_status :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe AnomalyStatus)
reactiveAnomalySummary_status = Lens.lens (\ReactiveAnomalySummary' {status} -> status) (\s@ReactiveAnomalySummary' {} a -> s {status = a} :: ReactiveAnomalySummary)

-- | Undocumented member.
reactiveAnomalySummary_resourceCollection :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe ResourceCollection)
reactiveAnomalySummary_resourceCollection = Lens.lens (\ReactiveAnomalySummary' {resourceCollection} -> resourceCollection) (\s@ReactiveAnomalySummary' {} a -> s {resourceCollection = a} :: ReactiveAnomalySummary)

-- | The severity of the reactive anomaly.
reactiveAnomalySummary_severity :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe AnomalySeverity)
reactiveAnomalySummary_severity = Lens.lens (\ReactiveAnomalySummary' {severity} -> severity) (\s@ReactiveAnomalySummary' {} a -> s {severity = a} :: ReactiveAnomalySummary)

-- | Details about the source of the analyzed operational data that triggered
-- the anomaly. The one supported source is Amazon CloudWatch metrics.
reactiveAnomalySummary_sourceDetails :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe AnomalySourceDetails)
reactiveAnomalySummary_sourceDetails = Lens.lens (\ReactiveAnomalySummary' {sourceDetails} -> sourceDetails) (\s@ReactiveAnomalySummary' {} a -> s {sourceDetails = a} :: ReactiveAnomalySummary)

-- | The ID of the reactive anomaly.
reactiveAnomalySummary_id :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe Prelude.Text)
reactiveAnomalySummary_id = Lens.lens (\ReactiveAnomalySummary' {id} -> id) (\s@ReactiveAnomalySummary' {} a -> s {id = a} :: ReactiveAnomalySummary)

-- | The ID of the insight that contains this anomaly. An insight is composed
-- of related anomalies.
reactiveAnomalySummary_associatedInsightId :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe Prelude.Text)
reactiveAnomalySummary_associatedInsightId = Lens.lens (\ReactiveAnomalySummary' {associatedInsightId} -> associatedInsightId) (\s@ReactiveAnomalySummary' {} a -> s {associatedInsightId = a} :: ReactiveAnomalySummary)

-- | Undocumented member.
reactiveAnomalySummary_anomalyTimeRange :: Lens.Lens' ReactiveAnomalySummary (Prelude.Maybe AnomalyTimeRange)
reactiveAnomalySummary_anomalyTimeRange = Lens.lens (\ReactiveAnomalySummary' {anomalyTimeRange} -> anomalyTimeRange) (\s@ReactiveAnomalySummary' {} a -> s {anomalyTimeRange = a} :: ReactiveAnomalySummary)

instance Core.FromJSON ReactiveAnomalySummary where
  parseJSON =
    Core.withObject
      "ReactiveAnomalySummary"
      ( \x ->
          ReactiveAnomalySummary'
            Prelude.<$> (x Core..:? "AnomalyReportedTimeRange")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ResourceCollection")
            Prelude.<*> (x Core..:? "Severity")
            Prelude.<*> (x Core..:? "SourceDetails")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "AssociatedInsightId")
            Prelude.<*> (x Core..:? "AnomalyTimeRange")
      )

instance Prelude.Hashable ReactiveAnomalySummary where
  hashWithSalt salt' ReactiveAnomalySummary' {..} =
    salt' `Prelude.hashWithSalt` anomalyTimeRange
      `Prelude.hashWithSalt` associatedInsightId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` sourceDetails
      `Prelude.hashWithSalt` severity
      `Prelude.hashWithSalt` resourceCollection
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` anomalyReportedTimeRange

instance Prelude.NFData ReactiveAnomalySummary where
  rnf ReactiveAnomalySummary' {..} =
    Prelude.rnf anomalyReportedTimeRange
      `Prelude.seq` Prelude.rnf anomalyTimeRange
      `Prelude.seq` Prelude.rnf associatedInsightId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf sourceDetails
      `Prelude.seq` Prelude.rnf severity
      `Prelude.seq` Prelude.rnf resourceCollection
      `Prelude.seq` Prelude.rnf status
