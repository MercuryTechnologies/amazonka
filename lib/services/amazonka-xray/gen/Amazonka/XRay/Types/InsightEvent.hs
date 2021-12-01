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
-- Module      : Amazonka.XRay.Types.InsightEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.InsightEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.AnomalousService
import Amazonka.XRay.Types.RequestImpactStatistics

-- | X-Ray reevaluates insights periodically until they are resolved, and
-- records each intermediate state in an event. You can review incident
-- events in the Impact Timeline on the Inspect page in the X-Ray console.
--
-- /See:/ 'newInsightEvent' smart constructor.
data InsightEvent = InsightEvent'
  { -- | A brief description of the event.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The time, in Unix seconds, at which the event was recorded.
    eventTime :: Prelude.Maybe Core.POSIX,
    -- | The impact statistics of the root cause service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    rootCauseServiceRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics,
    -- | The service during the event that is most impacted by the incident.
    topAnomalousServices :: Prelude.Maybe [AnomalousService],
    -- | The impact statistics of the client side service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    clientRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'insightEvent_summary' - A brief description of the event.
--
-- 'eventTime', 'insightEvent_eventTime' - The time, in Unix seconds, at which the event was recorded.
--
-- 'rootCauseServiceRequestImpactStatistics', 'insightEvent_rootCauseServiceRequestImpactStatistics' - The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'topAnomalousServices', 'insightEvent_topAnomalousServices' - The service during the event that is most impacted by the incident.
--
-- 'clientRequestImpactStatistics', 'insightEvent_clientRequestImpactStatistics' - The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
newInsightEvent ::
  InsightEvent
newInsightEvent =
  InsightEvent'
    { summary = Prelude.Nothing,
      eventTime = Prelude.Nothing,
      rootCauseServiceRequestImpactStatistics =
        Prelude.Nothing,
      topAnomalousServices = Prelude.Nothing,
      clientRequestImpactStatistics = Prelude.Nothing
    }

-- | A brief description of the event.
insightEvent_summary :: Lens.Lens' InsightEvent (Prelude.Maybe Prelude.Text)
insightEvent_summary = Lens.lens (\InsightEvent' {summary} -> summary) (\s@InsightEvent' {} a -> s {summary = a} :: InsightEvent)

-- | The time, in Unix seconds, at which the event was recorded.
insightEvent_eventTime :: Lens.Lens' InsightEvent (Prelude.Maybe Prelude.UTCTime)
insightEvent_eventTime = Lens.lens (\InsightEvent' {eventTime} -> eventTime) (\s@InsightEvent' {} a -> s {eventTime = a} :: InsightEvent) Prelude.. Lens.mapping Core._Time

-- | The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insightEvent_rootCauseServiceRequestImpactStatistics :: Lens.Lens' InsightEvent (Prelude.Maybe RequestImpactStatistics)
insightEvent_rootCauseServiceRequestImpactStatistics = Lens.lens (\InsightEvent' {rootCauseServiceRequestImpactStatistics} -> rootCauseServiceRequestImpactStatistics) (\s@InsightEvent' {} a -> s {rootCauseServiceRequestImpactStatistics = a} :: InsightEvent)

-- | The service during the event that is most impacted by the incident.
insightEvent_topAnomalousServices :: Lens.Lens' InsightEvent (Prelude.Maybe [AnomalousService])
insightEvent_topAnomalousServices = Lens.lens (\InsightEvent' {topAnomalousServices} -> topAnomalousServices) (\s@InsightEvent' {} a -> s {topAnomalousServices = a} :: InsightEvent) Prelude.. Lens.mapping Lens.coerced

-- | The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insightEvent_clientRequestImpactStatistics :: Lens.Lens' InsightEvent (Prelude.Maybe RequestImpactStatistics)
insightEvent_clientRequestImpactStatistics = Lens.lens (\InsightEvent' {clientRequestImpactStatistics} -> clientRequestImpactStatistics) (\s@InsightEvent' {} a -> s {clientRequestImpactStatistics = a} :: InsightEvent)

instance Core.FromJSON InsightEvent where
  parseJSON =
    Core.withObject
      "InsightEvent"
      ( \x ->
          InsightEvent'
            Prelude.<$> (x Core..:? "Summary")
            Prelude.<*> (x Core..:? "EventTime")
            Prelude.<*> ( x
                            Core..:? "RootCauseServiceRequestImpactStatistics"
                        )
            Prelude.<*> ( x Core..:? "TopAnomalousServices"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ClientRequestImpactStatistics")
      )

instance Prelude.Hashable InsightEvent where
  hashWithSalt salt' InsightEvent' {..} =
    salt'
      `Prelude.hashWithSalt` clientRequestImpactStatistics
      `Prelude.hashWithSalt` topAnomalousServices
      `Prelude.hashWithSalt` rootCauseServiceRequestImpactStatistics
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` summary

instance Prelude.NFData InsightEvent where
  rnf InsightEvent' {..} =
    Prelude.rnf summary
      `Prelude.seq` Prelude.rnf clientRequestImpactStatistics
      `Prelude.seq` Prelude.rnf topAnomalousServices
      `Prelude.seq` Prelude.rnf rootCauseServiceRequestImpactStatistics
      `Prelude.seq` Prelude.rnf eventTime
