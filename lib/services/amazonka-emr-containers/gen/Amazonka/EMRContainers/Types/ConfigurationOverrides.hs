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
-- Module      : Amazonka.EMRContainers.Types.ConfigurationOverrides
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.ConfigurationOverrides where

import qualified Amazonka.Core as Core
import Amazonka.EMRContainers.Types.Configuration
import Amazonka.EMRContainers.Types.MonitoringConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A configuration specification to be used to override existing
-- configurations.
--
-- /See:/ 'newConfigurationOverrides' smart constructor.
data ConfigurationOverrides = ConfigurationOverrides'
  { -- | The configurations for monitoring.
    monitoringConfiguration :: Prelude.Maybe MonitoringConfiguration,
    -- | The configurations for the application running by the job run.
    applicationConfiguration :: Prelude.Maybe [Configuration]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigurationOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'monitoringConfiguration', 'configurationOverrides_monitoringConfiguration' - The configurations for monitoring.
--
-- 'applicationConfiguration', 'configurationOverrides_applicationConfiguration' - The configurations for the application running by the job run.
newConfigurationOverrides ::
  ConfigurationOverrides
newConfigurationOverrides =
  ConfigurationOverrides'
    { monitoringConfiguration =
        Prelude.Nothing,
      applicationConfiguration = Prelude.Nothing
    }

-- | The configurations for monitoring.
configurationOverrides_monitoringConfiguration :: Lens.Lens' ConfigurationOverrides (Prelude.Maybe MonitoringConfiguration)
configurationOverrides_monitoringConfiguration = Lens.lens (\ConfigurationOverrides' {monitoringConfiguration} -> monitoringConfiguration) (\s@ConfigurationOverrides' {} a -> s {monitoringConfiguration = a} :: ConfigurationOverrides)

-- | The configurations for the application running by the job run.
configurationOverrides_applicationConfiguration :: Lens.Lens' ConfigurationOverrides (Prelude.Maybe [Configuration])
configurationOverrides_applicationConfiguration = Lens.lens (\ConfigurationOverrides' {applicationConfiguration} -> applicationConfiguration) (\s@ConfigurationOverrides' {} a -> s {applicationConfiguration = a} :: ConfigurationOverrides) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ConfigurationOverrides where
  parseJSON =
    Core.withObject
      "ConfigurationOverrides"
      ( \x ->
          ConfigurationOverrides'
            Prelude.<$> (x Core..:? "monitoringConfiguration")
            Prelude.<*> ( x Core..:? "applicationConfiguration"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ConfigurationOverrides where
  hashWithSalt salt' ConfigurationOverrides' {..} =
    salt'
      `Prelude.hashWithSalt` applicationConfiguration
      `Prelude.hashWithSalt` monitoringConfiguration

instance Prelude.NFData ConfigurationOverrides where
  rnf ConfigurationOverrides' {..} =
    Prelude.rnf monitoringConfiguration
      `Prelude.seq` Prelude.rnf applicationConfiguration

instance Core.ToJSON ConfigurationOverrides where
  toJSON ConfigurationOverrides' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("monitoringConfiguration" Core..=)
              Prelude.<$> monitoringConfiguration,
            ("applicationConfiguration" Core..=)
              Prelude.<$> applicationConfiguration
          ]
      )
