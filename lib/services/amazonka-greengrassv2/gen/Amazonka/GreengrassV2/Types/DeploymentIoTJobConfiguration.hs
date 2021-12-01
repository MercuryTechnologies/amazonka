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
-- Module      : Amazonka.GreengrassV2.Types.DeploymentIoTJobConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GreengrassV2.Types.DeploymentIoTJobConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.GreengrassV2.Types.IoTJobAbortConfig
import Amazonka.GreengrassV2.Types.IoTJobExecutionsRolloutConfig
import Amazonka.GreengrassV2.Types.IoTJobTimeoutConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an IoT job configuration.
--
-- /See:/ 'newDeploymentIoTJobConfiguration' smart constructor.
data DeploymentIoTJobConfiguration = DeploymentIoTJobConfiguration'
  { -- | The rollout configuration for the job. This configuration defines the
    -- rate at which the job rolls out to the fleet of target devices.
    jobExecutionsRolloutConfig :: Prelude.Maybe IoTJobExecutionsRolloutConfig,
    -- | The stop configuration for the job. This configuration defines when and
    -- how to stop a job rollout.
    abortConfig :: Prelude.Maybe IoTJobAbortConfig,
    -- | The timeout configuration for the job. This configuration defines the
    -- amount of time each device has to complete the job.
    timeoutConfig :: Prelude.Maybe IoTJobTimeoutConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentIoTJobConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobExecutionsRolloutConfig', 'deploymentIoTJobConfiguration_jobExecutionsRolloutConfig' - The rollout configuration for the job. This configuration defines the
-- rate at which the job rolls out to the fleet of target devices.
--
-- 'abortConfig', 'deploymentIoTJobConfiguration_abortConfig' - The stop configuration for the job. This configuration defines when and
-- how to stop a job rollout.
--
-- 'timeoutConfig', 'deploymentIoTJobConfiguration_timeoutConfig' - The timeout configuration for the job. This configuration defines the
-- amount of time each device has to complete the job.
newDeploymentIoTJobConfiguration ::
  DeploymentIoTJobConfiguration
newDeploymentIoTJobConfiguration =
  DeploymentIoTJobConfiguration'
    { jobExecutionsRolloutConfig =
        Prelude.Nothing,
      abortConfig = Prelude.Nothing,
      timeoutConfig = Prelude.Nothing
    }

-- | The rollout configuration for the job. This configuration defines the
-- rate at which the job rolls out to the fleet of target devices.
deploymentIoTJobConfiguration_jobExecutionsRolloutConfig :: Lens.Lens' DeploymentIoTJobConfiguration (Prelude.Maybe IoTJobExecutionsRolloutConfig)
deploymentIoTJobConfiguration_jobExecutionsRolloutConfig = Lens.lens (\DeploymentIoTJobConfiguration' {jobExecutionsRolloutConfig} -> jobExecutionsRolloutConfig) (\s@DeploymentIoTJobConfiguration' {} a -> s {jobExecutionsRolloutConfig = a} :: DeploymentIoTJobConfiguration)

-- | The stop configuration for the job. This configuration defines when and
-- how to stop a job rollout.
deploymentIoTJobConfiguration_abortConfig :: Lens.Lens' DeploymentIoTJobConfiguration (Prelude.Maybe IoTJobAbortConfig)
deploymentIoTJobConfiguration_abortConfig = Lens.lens (\DeploymentIoTJobConfiguration' {abortConfig} -> abortConfig) (\s@DeploymentIoTJobConfiguration' {} a -> s {abortConfig = a} :: DeploymentIoTJobConfiguration)

-- | The timeout configuration for the job. This configuration defines the
-- amount of time each device has to complete the job.
deploymentIoTJobConfiguration_timeoutConfig :: Lens.Lens' DeploymentIoTJobConfiguration (Prelude.Maybe IoTJobTimeoutConfig)
deploymentIoTJobConfiguration_timeoutConfig = Lens.lens (\DeploymentIoTJobConfiguration' {timeoutConfig} -> timeoutConfig) (\s@DeploymentIoTJobConfiguration' {} a -> s {timeoutConfig = a} :: DeploymentIoTJobConfiguration)

instance Core.FromJSON DeploymentIoTJobConfiguration where
  parseJSON =
    Core.withObject
      "DeploymentIoTJobConfiguration"
      ( \x ->
          DeploymentIoTJobConfiguration'
            Prelude.<$> (x Core..:? "jobExecutionsRolloutConfig")
            Prelude.<*> (x Core..:? "abortConfig")
            Prelude.<*> (x Core..:? "timeoutConfig")
      )

instance
  Prelude.Hashable
    DeploymentIoTJobConfiguration
  where
  hashWithSalt salt' DeploymentIoTJobConfiguration' {..} =
    salt' `Prelude.hashWithSalt` timeoutConfig
      `Prelude.hashWithSalt` abortConfig
      `Prelude.hashWithSalt` jobExecutionsRolloutConfig

instance Prelude.NFData DeploymentIoTJobConfiguration where
  rnf DeploymentIoTJobConfiguration' {..} =
    Prelude.rnf jobExecutionsRolloutConfig
      `Prelude.seq` Prelude.rnf timeoutConfig
      `Prelude.seq` Prelude.rnf abortConfig

instance Core.ToJSON DeploymentIoTJobConfiguration where
  toJSON DeploymentIoTJobConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("jobExecutionsRolloutConfig" Core..=)
              Prelude.<$> jobExecutionsRolloutConfig,
            ("abortConfig" Core..=) Prelude.<$> abortConfig,
            ("timeoutConfig" Core..=) Prelude.<$> timeoutConfig
          ]
      )
