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
-- Module      : Amazonka.XRay.Types.InsightSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.InsightSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.AnomalousService
import Amazonka.XRay.Types.InsightCategory
import Amazonka.XRay.Types.InsightState
import Amazonka.XRay.Types.RequestImpactStatistics
import Amazonka.XRay.Types.ServiceId

-- | Information that describes an insight.
--
-- /See:/ 'newInsightSummary' smart constructor.
data InsightSummary = InsightSummary'
  { -- | A brief description of the insight.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The current state of the insight.
    state :: Prelude.Maybe InsightState,
    -- | The time, in Unix seconds, at which the insight began.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The insights unique identifier.
    insightId :: Prelude.Maybe Prelude.Text,
    -- | Categories The categories that label and describe the type of insight.
    categories :: Prelude.Maybe [InsightCategory],
    -- | The impact statistics of the root cause service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    rootCauseServiceRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics,
    -- | The service within the insight that is most impacted by the incident.
    topAnomalousServices :: Prelude.Maybe [AnomalousService],
    rootCauseServiceId :: Prelude.Maybe ServiceId,
    -- | The impact statistics of the client side service. This includes the
    -- number of requests to the client service and whether the requests were
    -- faults or okay.
    clientRequestImpactStatistics :: Prelude.Maybe RequestImpactStatistics,
    -- | The time, in Unix seconds, at which the insight ended.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
    groupARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the group that the insight belongs to.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The time, in Unix seconds, that the insight was last updated.
    lastUpdateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InsightSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'insightSummary_summary' - A brief description of the insight.
--
-- 'state', 'insightSummary_state' - The current state of the insight.
--
-- 'startTime', 'insightSummary_startTime' - The time, in Unix seconds, at which the insight began.
--
-- 'insightId', 'insightSummary_insightId' - The insights unique identifier.
--
-- 'categories', 'insightSummary_categories' - Categories The categories that label and describe the type of insight.
--
-- 'rootCauseServiceRequestImpactStatistics', 'insightSummary_rootCauseServiceRequestImpactStatistics' - The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'topAnomalousServices', 'insightSummary_topAnomalousServices' - The service within the insight that is most impacted by the incident.
--
-- 'rootCauseServiceId', 'insightSummary_rootCauseServiceId' - Undocumented member.
--
-- 'clientRequestImpactStatistics', 'insightSummary_clientRequestImpactStatistics' - The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
--
-- 'endTime', 'insightSummary_endTime' - The time, in Unix seconds, at which the insight ended.
--
-- 'groupARN', 'insightSummary_groupARN' - The Amazon Resource Name (ARN) of the group that the insight belongs to.
--
-- 'groupName', 'insightSummary_groupName' - The name of the group that the insight belongs to.
--
-- 'lastUpdateTime', 'insightSummary_lastUpdateTime' - The time, in Unix seconds, that the insight was last updated.
newInsightSummary ::
  InsightSummary
newInsightSummary =
  InsightSummary'
    { summary = Prelude.Nothing,
      state = Prelude.Nothing,
      startTime = Prelude.Nothing,
      insightId = Prelude.Nothing,
      categories = Prelude.Nothing,
      rootCauseServiceRequestImpactStatistics =
        Prelude.Nothing,
      topAnomalousServices = Prelude.Nothing,
      rootCauseServiceId = Prelude.Nothing,
      clientRequestImpactStatistics = Prelude.Nothing,
      endTime = Prelude.Nothing,
      groupARN = Prelude.Nothing,
      groupName = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing
    }

-- | A brief description of the insight.
insightSummary_summary :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.Text)
insightSummary_summary = Lens.lens (\InsightSummary' {summary} -> summary) (\s@InsightSummary' {} a -> s {summary = a} :: InsightSummary)

-- | The current state of the insight.
insightSummary_state :: Lens.Lens' InsightSummary (Prelude.Maybe InsightState)
insightSummary_state = Lens.lens (\InsightSummary' {state} -> state) (\s@InsightSummary' {} a -> s {state = a} :: InsightSummary)

-- | The time, in Unix seconds, at which the insight began.
insightSummary_startTime :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.UTCTime)
insightSummary_startTime = Lens.lens (\InsightSummary' {startTime} -> startTime) (\s@InsightSummary' {} a -> s {startTime = a} :: InsightSummary) Prelude.. Lens.mapping Core._Time

-- | The insights unique identifier.
insightSummary_insightId :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.Text)
insightSummary_insightId = Lens.lens (\InsightSummary' {insightId} -> insightId) (\s@InsightSummary' {} a -> s {insightId = a} :: InsightSummary)

-- | Categories The categories that label and describe the type of insight.
insightSummary_categories :: Lens.Lens' InsightSummary (Prelude.Maybe [InsightCategory])
insightSummary_categories = Lens.lens (\InsightSummary' {categories} -> categories) (\s@InsightSummary' {} a -> s {categories = a} :: InsightSummary) Prelude.. Lens.mapping Lens.coerced

-- | The impact statistics of the root cause service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insightSummary_rootCauseServiceRequestImpactStatistics :: Lens.Lens' InsightSummary (Prelude.Maybe RequestImpactStatistics)
insightSummary_rootCauseServiceRequestImpactStatistics = Lens.lens (\InsightSummary' {rootCauseServiceRequestImpactStatistics} -> rootCauseServiceRequestImpactStatistics) (\s@InsightSummary' {} a -> s {rootCauseServiceRequestImpactStatistics = a} :: InsightSummary)

-- | The service within the insight that is most impacted by the incident.
insightSummary_topAnomalousServices :: Lens.Lens' InsightSummary (Prelude.Maybe [AnomalousService])
insightSummary_topAnomalousServices = Lens.lens (\InsightSummary' {topAnomalousServices} -> topAnomalousServices) (\s@InsightSummary' {} a -> s {topAnomalousServices = a} :: InsightSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
insightSummary_rootCauseServiceId :: Lens.Lens' InsightSummary (Prelude.Maybe ServiceId)
insightSummary_rootCauseServiceId = Lens.lens (\InsightSummary' {rootCauseServiceId} -> rootCauseServiceId) (\s@InsightSummary' {} a -> s {rootCauseServiceId = a} :: InsightSummary)

-- | The impact statistics of the client side service. This includes the
-- number of requests to the client service and whether the requests were
-- faults or okay.
insightSummary_clientRequestImpactStatistics :: Lens.Lens' InsightSummary (Prelude.Maybe RequestImpactStatistics)
insightSummary_clientRequestImpactStatistics = Lens.lens (\InsightSummary' {clientRequestImpactStatistics} -> clientRequestImpactStatistics) (\s@InsightSummary' {} a -> s {clientRequestImpactStatistics = a} :: InsightSummary)

-- | The time, in Unix seconds, at which the insight ended.
insightSummary_endTime :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.UTCTime)
insightSummary_endTime = Lens.lens (\InsightSummary' {endTime} -> endTime) (\s@InsightSummary' {} a -> s {endTime = a} :: InsightSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the group that the insight belongs to.
insightSummary_groupARN :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.Text)
insightSummary_groupARN = Lens.lens (\InsightSummary' {groupARN} -> groupARN) (\s@InsightSummary' {} a -> s {groupARN = a} :: InsightSummary)

-- | The name of the group that the insight belongs to.
insightSummary_groupName :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.Text)
insightSummary_groupName = Lens.lens (\InsightSummary' {groupName} -> groupName) (\s@InsightSummary' {} a -> s {groupName = a} :: InsightSummary)

-- | The time, in Unix seconds, that the insight was last updated.
insightSummary_lastUpdateTime :: Lens.Lens' InsightSummary (Prelude.Maybe Prelude.UTCTime)
insightSummary_lastUpdateTime = Lens.lens (\InsightSummary' {lastUpdateTime} -> lastUpdateTime) (\s@InsightSummary' {} a -> s {lastUpdateTime = a} :: InsightSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON InsightSummary where
  parseJSON =
    Core.withObject
      "InsightSummary"
      ( \x ->
          InsightSummary'
            Prelude.<$> (x Core..:? "Summary")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "InsightId")
            Prelude.<*> (x Core..:? "Categories" Core..!= Prelude.mempty)
            Prelude.<*> ( x
                            Core..:? "RootCauseServiceRequestImpactStatistics"
                        )
            Prelude.<*> ( x Core..:? "TopAnomalousServices"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "RootCauseServiceId")
            Prelude.<*> (x Core..:? "ClientRequestImpactStatistics")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "GroupARN")
            Prelude.<*> (x Core..:? "GroupName")
            Prelude.<*> (x Core..:? "LastUpdateTime")
      )

instance Prelude.Hashable InsightSummary where
  hashWithSalt salt' InsightSummary' {..} =
    salt' `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` groupARN
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` clientRequestImpactStatistics
      `Prelude.hashWithSalt` rootCauseServiceId
      `Prelude.hashWithSalt` topAnomalousServices
      `Prelude.hashWithSalt` rootCauseServiceRequestImpactStatistics
      `Prelude.hashWithSalt` categories
      `Prelude.hashWithSalt` insightId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` summary

instance Prelude.NFData InsightSummary where
  rnf InsightSummary' {..} =
    Prelude.rnf summary
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf groupARN
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf clientRequestImpactStatistics
      `Prelude.seq` Prelude.rnf rootCauseServiceId
      `Prelude.seq` Prelude.rnf topAnomalousServices
      `Prelude.seq` Prelude.rnf rootCauseServiceRequestImpactStatistics
      `Prelude.seq` Prelude.rnf categories
      `Prelude.seq` Prelude.rnf insightId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf state
