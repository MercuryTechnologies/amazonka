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
-- Module      : Amazonka.ComputeOptimizer.Types.VolumeRecommendation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.VolumeRecommendation where

import Amazonka.ComputeOptimizer.Types.EBSFinding
import Amazonka.ComputeOptimizer.Types.EBSUtilizationMetric
import Amazonka.ComputeOptimizer.Types.VolumeConfiguration
import Amazonka.ComputeOptimizer.Types.VolumeRecommendationOption
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Elastic Block Store (Amazon EBS) volume
-- recommendation.
--
-- /See:/ 'newVolumeRecommendation' smart constructor.
data VolumeRecommendation = VolumeRecommendation'
  { -- | The finding classification of the volume.
    --
    -- Findings for volumes include:
    --
    -- -   __@NotOptimized@__ —A volume is considered not optimized when
    --     Compute Optimizer identifies a recommendation that can provide
    --     better performance for your workload.
    --
    -- -   __@Optimized@__ —An volume is considered optimized when Compute
    --     Optimizer determines that the volume is correctly provisioned to run
    --     your workload based on the chosen volume type. For optimized
    --     resources, Compute Optimizer might recommend a new generation volume
    --     type.
    finding :: Prelude.Maybe EBSFinding,
    -- | The Amazon Resource Name (ARN) of the current volume.
    volumeArn :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of when the volume recommendation was last refreshed.
    lastRefreshTimestamp :: Prelude.Maybe Core.POSIX,
    -- | An array of objects that describe the current configuration of the
    -- volume.
    currentConfiguration :: Prelude.Maybe VolumeConfiguration,
    -- | The Amazon Web Services account ID of the volume.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the utilization metrics of the volume.
    utilizationMetrics :: Prelude.Maybe [EBSUtilizationMetric],
    -- | An array of objects that describe the recommendation options for the
    -- volume.
    volumeRecommendationOptions :: Prelude.Maybe [VolumeRecommendationOption],
    -- | The number of days for which utilization metrics were analyzed for the
    -- volume.
    lookBackPeriodInDays :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VolumeRecommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finding', 'volumeRecommendation_finding' - The finding classification of the volume.
--
-- Findings for volumes include:
--
-- -   __@NotOptimized@__ —A volume is considered not optimized when
--     Compute Optimizer identifies a recommendation that can provide
--     better performance for your workload.
--
-- -   __@Optimized@__ —An volume is considered optimized when Compute
--     Optimizer determines that the volume is correctly provisioned to run
--     your workload based on the chosen volume type. For optimized
--     resources, Compute Optimizer might recommend a new generation volume
--     type.
--
-- 'volumeArn', 'volumeRecommendation_volumeArn' - The Amazon Resource Name (ARN) of the current volume.
--
-- 'lastRefreshTimestamp', 'volumeRecommendation_lastRefreshTimestamp' - The timestamp of when the volume recommendation was last refreshed.
--
-- 'currentConfiguration', 'volumeRecommendation_currentConfiguration' - An array of objects that describe the current configuration of the
-- volume.
--
-- 'accountId', 'volumeRecommendation_accountId' - The Amazon Web Services account ID of the volume.
--
-- 'utilizationMetrics', 'volumeRecommendation_utilizationMetrics' - An array of objects that describe the utilization metrics of the volume.
--
-- 'volumeRecommendationOptions', 'volumeRecommendation_volumeRecommendationOptions' - An array of objects that describe the recommendation options for the
-- volume.
--
-- 'lookBackPeriodInDays', 'volumeRecommendation_lookBackPeriodInDays' - The number of days for which utilization metrics were analyzed for the
-- volume.
newVolumeRecommendation ::
  VolumeRecommendation
newVolumeRecommendation =
  VolumeRecommendation'
    { finding = Prelude.Nothing,
      volumeArn = Prelude.Nothing,
      lastRefreshTimestamp = Prelude.Nothing,
      currentConfiguration = Prelude.Nothing,
      accountId = Prelude.Nothing,
      utilizationMetrics = Prelude.Nothing,
      volumeRecommendationOptions = Prelude.Nothing,
      lookBackPeriodInDays = Prelude.Nothing
    }

-- | The finding classification of the volume.
--
-- Findings for volumes include:
--
-- -   __@NotOptimized@__ —A volume is considered not optimized when
--     Compute Optimizer identifies a recommendation that can provide
--     better performance for your workload.
--
-- -   __@Optimized@__ —An volume is considered optimized when Compute
--     Optimizer determines that the volume is correctly provisioned to run
--     your workload based on the chosen volume type. For optimized
--     resources, Compute Optimizer might recommend a new generation volume
--     type.
volumeRecommendation_finding :: Lens.Lens' VolumeRecommendation (Prelude.Maybe EBSFinding)
volumeRecommendation_finding = Lens.lens (\VolumeRecommendation' {finding} -> finding) (\s@VolumeRecommendation' {} a -> s {finding = a} :: VolumeRecommendation)

-- | The Amazon Resource Name (ARN) of the current volume.
volumeRecommendation_volumeArn :: Lens.Lens' VolumeRecommendation (Prelude.Maybe Prelude.Text)
volumeRecommendation_volumeArn = Lens.lens (\VolumeRecommendation' {volumeArn} -> volumeArn) (\s@VolumeRecommendation' {} a -> s {volumeArn = a} :: VolumeRecommendation)

-- | The timestamp of when the volume recommendation was last refreshed.
volumeRecommendation_lastRefreshTimestamp :: Lens.Lens' VolumeRecommendation (Prelude.Maybe Prelude.UTCTime)
volumeRecommendation_lastRefreshTimestamp = Lens.lens (\VolumeRecommendation' {lastRefreshTimestamp} -> lastRefreshTimestamp) (\s@VolumeRecommendation' {} a -> s {lastRefreshTimestamp = a} :: VolumeRecommendation) Prelude.. Lens.mapping Core._Time

-- | An array of objects that describe the current configuration of the
-- volume.
volumeRecommendation_currentConfiguration :: Lens.Lens' VolumeRecommendation (Prelude.Maybe VolumeConfiguration)
volumeRecommendation_currentConfiguration = Lens.lens (\VolumeRecommendation' {currentConfiguration} -> currentConfiguration) (\s@VolumeRecommendation' {} a -> s {currentConfiguration = a} :: VolumeRecommendation)

-- | The Amazon Web Services account ID of the volume.
volumeRecommendation_accountId :: Lens.Lens' VolumeRecommendation (Prelude.Maybe Prelude.Text)
volumeRecommendation_accountId = Lens.lens (\VolumeRecommendation' {accountId} -> accountId) (\s@VolumeRecommendation' {} a -> s {accountId = a} :: VolumeRecommendation)

-- | An array of objects that describe the utilization metrics of the volume.
volumeRecommendation_utilizationMetrics :: Lens.Lens' VolumeRecommendation (Prelude.Maybe [EBSUtilizationMetric])
volumeRecommendation_utilizationMetrics = Lens.lens (\VolumeRecommendation' {utilizationMetrics} -> utilizationMetrics) (\s@VolumeRecommendation' {} a -> s {utilizationMetrics = a} :: VolumeRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects that describe the recommendation options for the
-- volume.
volumeRecommendation_volumeRecommendationOptions :: Lens.Lens' VolumeRecommendation (Prelude.Maybe [VolumeRecommendationOption])
volumeRecommendation_volumeRecommendationOptions = Lens.lens (\VolumeRecommendation' {volumeRecommendationOptions} -> volumeRecommendationOptions) (\s@VolumeRecommendation' {} a -> s {volumeRecommendationOptions = a} :: VolumeRecommendation) Prelude.. Lens.mapping Lens.coerced

-- | The number of days for which utilization metrics were analyzed for the
-- volume.
volumeRecommendation_lookBackPeriodInDays :: Lens.Lens' VolumeRecommendation (Prelude.Maybe Prelude.Double)
volumeRecommendation_lookBackPeriodInDays = Lens.lens (\VolumeRecommendation' {lookBackPeriodInDays} -> lookBackPeriodInDays) (\s@VolumeRecommendation' {} a -> s {lookBackPeriodInDays = a} :: VolumeRecommendation)

instance Core.FromJSON VolumeRecommendation where
  parseJSON =
    Core.withObject
      "VolumeRecommendation"
      ( \x ->
          VolumeRecommendation'
            Prelude.<$> (x Core..:? "finding")
            Prelude.<*> (x Core..:? "volumeArn")
            Prelude.<*> (x Core..:? "lastRefreshTimestamp")
            Prelude.<*> (x Core..:? "currentConfiguration")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> ( x Core..:? "utilizationMetrics"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "volumeRecommendationOptions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "lookBackPeriodInDays")
      )

instance Prelude.Hashable VolumeRecommendation where
  hashWithSalt salt' VolumeRecommendation' {..} =
    salt' `Prelude.hashWithSalt` lookBackPeriodInDays
      `Prelude.hashWithSalt` volumeRecommendationOptions
      `Prelude.hashWithSalt` utilizationMetrics
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` currentConfiguration
      `Prelude.hashWithSalt` lastRefreshTimestamp
      `Prelude.hashWithSalt` volumeArn
      `Prelude.hashWithSalt` finding

instance Prelude.NFData VolumeRecommendation where
  rnf VolumeRecommendation' {..} =
    Prelude.rnf finding
      `Prelude.seq` Prelude.rnf lookBackPeriodInDays
      `Prelude.seq` Prelude.rnf volumeRecommendationOptions
      `Prelude.seq` Prelude.rnf utilizationMetrics
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf currentConfiguration
      `Prelude.seq` Prelude.rnf lastRefreshTimestamp
      `Prelude.seq` Prelude.rnf volumeArn
