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
-- Module      : Amazonka.EMRContainers.Types.MonitoringConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.MonitoringConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.EMRContainers.Types.CloudWatchMonitoringConfiguration
import Amazonka.EMRContainers.Types.PersistentAppUI
import Amazonka.EMRContainers.Types.S3MonitoringConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration setting for monitoring.
--
-- /See:/ 'newMonitoringConfiguration' smart constructor.
data MonitoringConfiguration = MonitoringConfiguration'
  { -- | Monitoring configurations for the persistent application UI.
    persistentAppUI :: Prelude.Maybe PersistentAppUI,
    -- | Amazon S3 configuration for monitoring log publishing.
    s3MonitoringConfiguration :: Prelude.Maybe S3MonitoringConfiguration,
    -- | Monitoring configurations for CloudWatch.
    cloudWatchMonitoringConfiguration :: Prelude.Maybe CloudWatchMonitoringConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MonitoringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'persistentAppUI', 'monitoringConfiguration_persistentAppUI' - Monitoring configurations for the persistent application UI.
--
-- 's3MonitoringConfiguration', 'monitoringConfiguration_s3MonitoringConfiguration' - Amazon S3 configuration for monitoring log publishing.
--
-- 'cloudWatchMonitoringConfiguration', 'monitoringConfiguration_cloudWatchMonitoringConfiguration' - Monitoring configurations for CloudWatch.
newMonitoringConfiguration ::
  MonitoringConfiguration
newMonitoringConfiguration =
  MonitoringConfiguration'
    { persistentAppUI =
        Prelude.Nothing,
      s3MonitoringConfiguration = Prelude.Nothing,
      cloudWatchMonitoringConfiguration =
        Prelude.Nothing
    }

-- | Monitoring configurations for the persistent application UI.
monitoringConfiguration_persistentAppUI :: Lens.Lens' MonitoringConfiguration (Prelude.Maybe PersistentAppUI)
monitoringConfiguration_persistentAppUI = Lens.lens (\MonitoringConfiguration' {persistentAppUI} -> persistentAppUI) (\s@MonitoringConfiguration' {} a -> s {persistentAppUI = a} :: MonitoringConfiguration)

-- | Amazon S3 configuration for monitoring log publishing.
monitoringConfiguration_s3MonitoringConfiguration :: Lens.Lens' MonitoringConfiguration (Prelude.Maybe S3MonitoringConfiguration)
monitoringConfiguration_s3MonitoringConfiguration = Lens.lens (\MonitoringConfiguration' {s3MonitoringConfiguration} -> s3MonitoringConfiguration) (\s@MonitoringConfiguration' {} a -> s {s3MonitoringConfiguration = a} :: MonitoringConfiguration)

-- | Monitoring configurations for CloudWatch.
monitoringConfiguration_cloudWatchMonitoringConfiguration :: Lens.Lens' MonitoringConfiguration (Prelude.Maybe CloudWatchMonitoringConfiguration)
monitoringConfiguration_cloudWatchMonitoringConfiguration = Lens.lens (\MonitoringConfiguration' {cloudWatchMonitoringConfiguration} -> cloudWatchMonitoringConfiguration) (\s@MonitoringConfiguration' {} a -> s {cloudWatchMonitoringConfiguration = a} :: MonitoringConfiguration)

instance Core.FromJSON MonitoringConfiguration where
  parseJSON =
    Core.withObject
      "MonitoringConfiguration"
      ( \x ->
          MonitoringConfiguration'
            Prelude.<$> (x Core..:? "persistentAppUI")
            Prelude.<*> (x Core..:? "s3MonitoringConfiguration")
            Prelude.<*> (x Core..:? "cloudWatchMonitoringConfiguration")
      )

instance Prelude.Hashable MonitoringConfiguration where
  hashWithSalt salt' MonitoringConfiguration' {..} =
    salt'
      `Prelude.hashWithSalt` cloudWatchMonitoringConfiguration
      `Prelude.hashWithSalt` s3MonitoringConfiguration
      `Prelude.hashWithSalt` persistentAppUI

instance Prelude.NFData MonitoringConfiguration where
  rnf MonitoringConfiguration' {..} =
    Prelude.rnf persistentAppUI
      `Prelude.seq` Prelude.rnf cloudWatchMonitoringConfiguration
      `Prelude.seq` Prelude.rnf s3MonitoringConfiguration

instance Core.ToJSON MonitoringConfiguration where
  toJSON MonitoringConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("persistentAppUI" Core..=)
              Prelude.<$> persistentAppUI,
            ("s3MonitoringConfiguration" Core..=)
              Prelude.<$> s3MonitoringConfiguration,
            ("cloudWatchMonitoringConfiguration" Core..=)
              Prelude.<$> cloudWatchMonitoringConfiguration
          ]
      )
