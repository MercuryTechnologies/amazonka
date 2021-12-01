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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfigurationUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfigurationUpdate where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalyticsV2.Types.CatalogConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.CustomArtifactConfiguration
import Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfigurationUpdate
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfigurationUpdate
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Updates to the configuration of Kinesis Data Analytics Studio notebook.
--
-- /See:/ 'newZeppelinApplicationConfigurationUpdate' smart constructor.
data ZeppelinApplicationConfigurationUpdate = ZeppelinApplicationConfigurationUpdate'
  { -- | Updates to the customer artifacts. Custom artifacts are dependency JAR
    -- files and user-defined functions (UDF).
    customArtifactsConfigurationUpdate :: Prelude.Maybe [CustomArtifactConfiguration],
    -- | Updates to the configuration of the Amazon Glue Data Catalog that is
    -- associated with the Kinesis Data Analytics Studio notebook.
    catalogConfigurationUpdate :: Prelude.Maybe CatalogConfigurationUpdate,
    deployAsApplicationConfigurationUpdate :: Prelude.Maybe DeployAsApplicationConfigurationUpdate,
    -- | Updates to the monitoring configuration of a Kinesis Data Analytics
    -- Studio notebook.
    monitoringConfigurationUpdate :: Prelude.Maybe ZeppelinMonitoringConfigurationUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZeppelinApplicationConfigurationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customArtifactsConfigurationUpdate', 'zeppelinApplicationConfigurationUpdate_customArtifactsConfigurationUpdate' - Updates to the customer artifacts. Custom artifacts are dependency JAR
-- files and user-defined functions (UDF).
--
-- 'catalogConfigurationUpdate', 'zeppelinApplicationConfigurationUpdate_catalogConfigurationUpdate' - Updates to the configuration of the Amazon Glue Data Catalog that is
-- associated with the Kinesis Data Analytics Studio notebook.
--
-- 'deployAsApplicationConfigurationUpdate', 'zeppelinApplicationConfigurationUpdate_deployAsApplicationConfigurationUpdate' - Undocumented member.
--
-- 'monitoringConfigurationUpdate', 'zeppelinApplicationConfigurationUpdate_monitoringConfigurationUpdate' - Updates to the monitoring configuration of a Kinesis Data Analytics
-- Studio notebook.
newZeppelinApplicationConfigurationUpdate ::
  ZeppelinApplicationConfigurationUpdate
newZeppelinApplicationConfigurationUpdate =
  ZeppelinApplicationConfigurationUpdate'
    { customArtifactsConfigurationUpdate =
        Prelude.Nothing,
      catalogConfigurationUpdate =
        Prelude.Nothing,
      deployAsApplicationConfigurationUpdate =
        Prelude.Nothing,
      monitoringConfigurationUpdate =
        Prelude.Nothing
    }

-- | Updates to the customer artifacts. Custom artifacts are dependency JAR
-- files and user-defined functions (UDF).
zeppelinApplicationConfigurationUpdate_customArtifactsConfigurationUpdate :: Lens.Lens' ZeppelinApplicationConfigurationUpdate (Prelude.Maybe [CustomArtifactConfiguration])
zeppelinApplicationConfigurationUpdate_customArtifactsConfigurationUpdate = Lens.lens (\ZeppelinApplicationConfigurationUpdate' {customArtifactsConfigurationUpdate} -> customArtifactsConfigurationUpdate) (\s@ZeppelinApplicationConfigurationUpdate' {} a -> s {customArtifactsConfigurationUpdate = a} :: ZeppelinApplicationConfigurationUpdate) Prelude.. Lens.mapping Lens.coerced

-- | Updates to the configuration of the Amazon Glue Data Catalog that is
-- associated with the Kinesis Data Analytics Studio notebook.
zeppelinApplicationConfigurationUpdate_catalogConfigurationUpdate :: Lens.Lens' ZeppelinApplicationConfigurationUpdate (Prelude.Maybe CatalogConfigurationUpdate)
zeppelinApplicationConfigurationUpdate_catalogConfigurationUpdate = Lens.lens (\ZeppelinApplicationConfigurationUpdate' {catalogConfigurationUpdate} -> catalogConfigurationUpdate) (\s@ZeppelinApplicationConfigurationUpdate' {} a -> s {catalogConfigurationUpdate = a} :: ZeppelinApplicationConfigurationUpdate)

-- | Undocumented member.
zeppelinApplicationConfigurationUpdate_deployAsApplicationConfigurationUpdate :: Lens.Lens' ZeppelinApplicationConfigurationUpdate (Prelude.Maybe DeployAsApplicationConfigurationUpdate)
zeppelinApplicationConfigurationUpdate_deployAsApplicationConfigurationUpdate = Lens.lens (\ZeppelinApplicationConfigurationUpdate' {deployAsApplicationConfigurationUpdate} -> deployAsApplicationConfigurationUpdate) (\s@ZeppelinApplicationConfigurationUpdate' {} a -> s {deployAsApplicationConfigurationUpdate = a} :: ZeppelinApplicationConfigurationUpdate)

-- | Updates to the monitoring configuration of a Kinesis Data Analytics
-- Studio notebook.
zeppelinApplicationConfigurationUpdate_monitoringConfigurationUpdate :: Lens.Lens' ZeppelinApplicationConfigurationUpdate (Prelude.Maybe ZeppelinMonitoringConfigurationUpdate)
zeppelinApplicationConfigurationUpdate_monitoringConfigurationUpdate = Lens.lens (\ZeppelinApplicationConfigurationUpdate' {monitoringConfigurationUpdate} -> monitoringConfigurationUpdate) (\s@ZeppelinApplicationConfigurationUpdate' {} a -> s {monitoringConfigurationUpdate = a} :: ZeppelinApplicationConfigurationUpdate)

instance
  Prelude.Hashable
    ZeppelinApplicationConfigurationUpdate
  where
  hashWithSalt
    salt'
    ZeppelinApplicationConfigurationUpdate' {..} =
      salt'
        `Prelude.hashWithSalt` monitoringConfigurationUpdate
        `Prelude.hashWithSalt` deployAsApplicationConfigurationUpdate
        `Prelude.hashWithSalt` catalogConfigurationUpdate
        `Prelude.hashWithSalt` customArtifactsConfigurationUpdate

instance
  Prelude.NFData
    ZeppelinApplicationConfigurationUpdate
  where
  rnf ZeppelinApplicationConfigurationUpdate' {..} =
    Prelude.rnf customArtifactsConfigurationUpdate
      `Prelude.seq` Prelude.rnf monitoringConfigurationUpdate
      `Prelude.seq` Prelude.rnf deployAsApplicationConfigurationUpdate
      `Prelude.seq` Prelude.rnf catalogConfigurationUpdate

instance
  Core.ToJSON
    ZeppelinApplicationConfigurationUpdate
  where
  toJSON ZeppelinApplicationConfigurationUpdate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CustomArtifactsConfigurationUpdate" Core..=)
              Prelude.<$> customArtifactsConfigurationUpdate,
            ("CatalogConfigurationUpdate" Core..=)
              Prelude.<$> catalogConfigurationUpdate,
            ("DeployAsApplicationConfigurationUpdate" Core..=)
              Prelude.<$> deployAsApplicationConfigurationUpdate,
            ("MonitoringConfigurationUpdate" Core..=)
              Prelude.<$> monitoringConfigurationUpdate
          ]
      )
