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
-- Module      : Amazonka.AppConfig.Types.DeploymentSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppConfig.Types.DeploymentSummary where

import Amazonka.AppConfig.Types.DeploymentState
import Amazonka.AppConfig.Types.GrowthType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the deployment.
--
-- /See:/ 'newDeploymentSummary' smart constructor.
data DeploymentSummary = DeploymentSummary'
  { -- | The percentage of targets to receive a deployed configuration during
    -- each interval.
    growthFactor :: Prelude.Maybe Prelude.Double,
    -- | The name of the configuration.
    configurationName :: Prelude.Maybe Prelude.Text,
    -- | The state of the deployment.
    state :: Prelude.Maybe DeploymentState,
    -- | The sequence number of the deployment.
    deploymentNumber :: Prelude.Maybe Prelude.Int,
    -- | The version of the configuration.
    configurationVersion :: Prelude.Maybe Prelude.Text,
    -- | The percentage of targets for which the deployment is available.
    percentageComplete :: Prelude.Maybe Prelude.Double,
    -- | Time the deployment started.
    startedAt :: Prelude.Maybe Core.POSIX,
    -- | Total amount of time the deployment lasted.
    deploymentDurationInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | Time the deployment completed.
    completedAt :: Prelude.Maybe Core.POSIX,
    -- | The amount of time AppConfig monitors for alarms before considering the
    -- deployment to be complete and no longer eligible for automatic roll
    -- back.
    finalBakeTimeInMinutes :: Prelude.Maybe Prelude.Natural,
    -- | The algorithm used to define how percentage grows over time.
    growthType :: Prelude.Maybe GrowthType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'growthFactor', 'deploymentSummary_growthFactor' - The percentage of targets to receive a deployed configuration during
-- each interval.
--
-- 'configurationName', 'deploymentSummary_configurationName' - The name of the configuration.
--
-- 'state', 'deploymentSummary_state' - The state of the deployment.
--
-- 'deploymentNumber', 'deploymentSummary_deploymentNumber' - The sequence number of the deployment.
--
-- 'configurationVersion', 'deploymentSummary_configurationVersion' - The version of the configuration.
--
-- 'percentageComplete', 'deploymentSummary_percentageComplete' - The percentage of targets for which the deployment is available.
--
-- 'startedAt', 'deploymentSummary_startedAt' - Time the deployment started.
--
-- 'deploymentDurationInMinutes', 'deploymentSummary_deploymentDurationInMinutes' - Total amount of time the deployment lasted.
--
-- 'completedAt', 'deploymentSummary_completedAt' - Time the deployment completed.
--
-- 'finalBakeTimeInMinutes', 'deploymentSummary_finalBakeTimeInMinutes' - The amount of time AppConfig monitors for alarms before considering the
-- deployment to be complete and no longer eligible for automatic roll
-- back.
--
-- 'growthType', 'deploymentSummary_growthType' - The algorithm used to define how percentage grows over time.
newDeploymentSummary ::
  DeploymentSummary
newDeploymentSummary =
  DeploymentSummary'
    { growthFactor = Prelude.Nothing,
      configurationName = Prelude.Nothing,
      state = Prelude.Nothing,
      deploymentNumber = Prelude.Nothing,
      configurationVersion = Prelude.Nothing,
      percentageComplete = Prelude.Nothing,
      startedAt = Prelude.Nothing,
      deploymentDurationInMinutes = Prelude.Nothing,
      completedAt = Prelude.Nothing,
      finalBakeTimeInMinutes = Prelude.Nothing,
      growthType = Prelude.Nothing
    }

-- | The percentage of targets to receive a deployed configuration during
-- each interval.
deploymentSummary_growthFactor :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Double)
deploymentSummary_growthFactor = Lens.lens (\DeploymentSummary' {growthFactor} -> growthFactor) (\s@DeploymentSummary' {} a -> s {growthFactor = a} :: DeploymentSummary)

-- | The name of the configuration.
deploymentSummary_configurationName :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Text)
deploymentSummary_configurationName = Lens.lens (\DeploymentSummary' {configurationName} -> configurationName) (\s@DeploymentSummary' {} a -> s {configurationName = a} :: DeploymentSummary)

-- | The state of the deployment.
deploymentSummary_state :: Lens.Lens' DeploymentSummary (Prelude.Maybe DeploymentState)
deploymentSummary_state = Lens.lens (\DeploymentSummary' {state} -> state) (\s@DeploymentSummary' {} a -> s {state = a} :: DeploymentSummary)

-- | The sequence number of the deployment.
deploymentSummary_deploymentNumber :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Int)
deploymentSummary_deploymentNumber = Lens.lens (\DeploymentSummary' {deploymentNumber} -> deploymentNumber) (\s@DeploymentSummary' {} a -> s {deploymentNumber = a} :: DeploymentSummary)

-- | The version of the configuration.
deploymentSummary_configurationVersion :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Text)
deploymentSummary_configurationVersion = Lens.lens (\DeploymentSummary' {configurationVersion} -> configurationVersion) (\s@DeploymentSummary' {} a -> s {configurationVersion = a} :: DeploymentSummary)

-- | The percentage of targets for which the deployment is available.
deploymentSummary_percentageComplete :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Double)
deploymentSummary_percentageComplete = Lens.lens (\DeploymentSummary' {percentageComplete} -> percentageComplete) (\s@DeploymentSummary' {} a -> s {percentageComplete = a} :: DeploymentSummary)

-- | Time the deployment started.
deploymentSummary_startedAt :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.UTCTime)
deploymentSummary_startedAt = Lens.lens (\DeploymentSummary' {startedAt} -> startedAt) (\s@DeploymentSummary' {} a -> s {startedAt = a} :: DeploymentSummary) Prelude.. Lens.mapping Core._Time

-- | Total amount of time the deployment lasted.
deploymentSummary_deploymentDurationInMinutes :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Natural)
deploymentSummary_deploymentDurationInMinutes = Lens.lens (\DeploymentSummary' {deploymentDurationInMinutes} -> deploymentDurationInMinutes) (\s@DeploymentSummary' {} a -> s {deploymentDurationInMinutes = a} :: DeploymentSummary)

-- | Time the deployment completed.
deploymentSummary_completedAt :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.UTCTime)
deploymentSummary_completedAt = Lens.lens (\DeploymentSummary' {completedAt} -> completedAt) (\s@DeploymentSummary' {} a -> s {completedAt = a} :: DeploymentSummary) Prelude.. Lens.mapping Core._Time

-- | The amount of time AppConfig monitors for alarms before considering the
-- deployment to be complete and no longer eligible for automatic roll
-- back.
deploymentSummary_finalBakeTimeInMinutes :: Lens.Lens' DeploymentSummary (Prelude.Maybe Prelude.Natural)
deploymentSummary_finalBakeTimeInMinutes = Lens.lens (\DeploymentSummary' {finalBakeTimeInMinutes} -> finalBakeTimeInMinutes) (\s@DeploymentSummary' {} a -> s {finalBakeTimeInMinutes = a} :: DeploymentSummary)

-- | The algorithm used to define how percentage grows over time.
deploymentSummary_growthType :: Lens.Lens' DeploymentSummary (Prelude.Maybe GrowthType)
deploymentSummary_growthType = Lens.lens (\DeploymentSummary' {growthType} -> growthType) (\s@DeploymentSummary' {} a -> s {growthType = a} :: DeploymentSummary)

instance Core.FromJSON DeploymentSummary where
  parseJSON =
    Core.withObject
      "DeploymentSummary"
      ( \x ->
          DeploymentSummary'
            Prelude.<$> (x Core..:? "GrowthFactor")
            Prelude.<*> (x Core..:? "ConfigurationName")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "DeploymentNumber")
            Prelude.<*> (x Core..:? "ConfigurationVersion")
            Prelude.<*> (x Core..:? "PercentageComplete")
            Prelude.<*> (x Core..:? "StartedAt")
            Prelude.<*> (x Core..:? "DeploymentDurationInMinutes")
            Prelude.<*> (x Core..:? "CompletedAt")
            Prelude.<*> (x Core..:? "FinalBakeTimeInMinutes")
            Prelude.<*> (x Core..:? "GrowthType")
      )

instance Prelude.Hashable DeploymentSummary where
  hashWithSalt salt' DeploymentSummary' {..} =
    salt' `Prelude.hashWithSalt` growthType
      `Prelude.hashWithSalt` finalBakeTimeInMinutes
      `Prelude.hashWithSalt` completedAt
      `Prelude.hashWithSalt` deploymentDurationInMinutes
      `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` percentageComplete
      `Prelude.hashWithSalt` configurationVersion
      `Prelude.hashWithSalt` deploymentNumber
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` configurationName
      `Prelude.hashWithSalt` growthFactor

instance Prelude.NFData DeploymentSummary where
  rnf DeploymentSummary' {..} =
    Prelude.rnf growthFactor
      `Prelude.seq` Prelude.rnf growthType
      `Prelude.seq` Prelude.rnf finalBakeTimeInMinutes
      `Prelude.seq` Prelude.rnf completedAt
      `Prelude.seq` Prelude.rnf deploymentDurationInMinutes
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf percentageComplete
      `Prelude.seq` Prelude.rnf configurationVersion
      `Prelude.seq` Prelude.rnf deploymentNumber
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf configurationName
