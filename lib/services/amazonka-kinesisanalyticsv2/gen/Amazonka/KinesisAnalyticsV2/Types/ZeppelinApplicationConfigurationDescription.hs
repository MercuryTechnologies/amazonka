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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfigurationDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.ZeppelinApplicationConfigurationDescription where

import qualified Amazonka.Core as Core
import Amazonka.KinesisAnalyticsV2.Types.CatalogConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.CustomArtifactConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.DeployAsApplicationConfigurationDescription
import Amazonka.KinesisAnalyticsV2.Types.ZeppelinMonitoringConfigurationDescription
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration of a Kinesis Data Analytics Studio notebook.
--
-- /See:/ 'newZeppelinApplicationConfigurationDescription' smart constructor.
data ZeppelinApplicationConfigurationDescription = ZeppelinApplicationConfigurationDescription'
  { -- | Custom artifacts are dependency JARs and user-defined functions (UDF).
    customArtifactsConfigurationDescription :: Prelude.Maybe [CustomArtifactConfigurationDescription],
    -- | The parameters required to deploy a Kinesis Data Analytics Studio
    -- notebook as an application with durable state.
    deployAsApplicationConfigurationDescription :: Prelude.Maybe DeployAsApplicationConfigurationDescription,
    -- | The Amazon Glue Data Catalog that is associated with the Kinesis Data
    -- Analytics Studio notebook.
    catalogConfigurationDescription :: Prelude.Maybe CatalogConfigurationDescription,
    -- | The monitoring configuration of a Kinesis Data Analytics Studio
    -- notebook.
    monitoringConfigurationDescription :: ZeppelinMonitoringConfigurationDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ZeppelinApplicationConfigurationDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customArtifactsConfigurationDescription', 'zeppelinApplicationConfigurationDescription_customArtifactsConfigurationDescription' - Custom artifacts are dependency JARs and user-defined functions (UDF).
--
-- 'deployAsApplicationConfigurationDescription', 'zeppelinApplicationConfigurationDescription_deployAsApplicationConfigurationDescription' - The parameters required to deploy a Kinesis Data Analytics Studio
-- notebook as an application with durable state.
--
-- 'catalogConfigurationDescription', 'zeppelinApplicationConfigurationDescription_catalogConfigurationDescription' - The Amazon Glue Data Catalog that is associated with the Kinesis Data
-- Analytics Studio notebook.
--
-- 'monitoringConfigurationDescription', 'zeppelinApplicationConfigurationDescription_monitoringConfigurationDescription' - The monitoring configuration of a Kinesis Data Analytics Studio
-- notebook.
newZeppelinApplicationConfigurationDescription ::
  -- | 'monitoringConfigurationDescription'
  ZeppelinMonitoringConfigurationDescription ->
  ZeppelinApplicationConfigurationDescription
newZeppelinApplicationConfigurationDescription
  pMonitoringConfigurationDescription_ =
    ZeppelinApplicationConfigurationDescription'
      { customArtifactsConfigurationDescription =
          Prelude.Nothing,
        deployAsApplicationConfigurationDescription =
          Prelude.Nothing,
        catalogConfigurationDescription =
          Prelude.Nothing,
        monitoringConfigurationDescription =
          pMonitoringConfigurationDescription_
      }

-- | Custom artifacts are dependency JARs and user-defined functions (UDF).
zeppelinApplicationConfigurationDescription_customArtifactsConfigurationDescription :: Lens.Lens' ZeppelinApplicationConfigurationDescription (Prelude.Maybe [CustomArtifactConfigurationDescription])
zeppelinApplicationConfigurationDescription_customArtifactsConfigurationDescription = Lens.lens (\ZeppelinApplicationConfigurationDescription' {customArtifactsConfigurationDescription} -> customArtifactsConfigurationDescription) (\s@ZeppelinApplicationConfigurationDescription' {} a -> s {customArtifactsConfigurationDescription = a} :: ZeppelinApplicationConfigurationDescription) Prelude.. Lens.mapping Lens.coerced

-- | The parameters required to deploy a Kinesis Data Analytics Studio
-- notebook as an application with durable state.
zeppelinApplicationConfigurationDescription_deployAsApplicationConfigurationDescription :: Lens.Lens' ZeppelinApplicationConfigurationDescription (Prelude.Maybe DeployAsApplicationConfigurationDescription)
zeppelinApplicationConfigurationDescription_deployAsApplicationConfigurationDescription = Lens.lens (\ZeppelinApplicationConfigurationDescription' {deployAsApplicationConfigurationDescription} -> deployAsApplicationConfigurationDescription) (\s@ZeppelinApplicationConfigurationDescription' {} a -> s {deployAsApplicationConfigurationDescription = a} :: ZeppelinApplicationConfigurationDescription)

-- | The Amazon Glue Data Catalog that is associated with the Kinesis Data
-- Analytics Studio notebook.
zeppelinApplicationConfigurationDescription_catalogConfigurationDescription :: Lens.Lens' ZeppelinApplicationConfigurationDescription (Prelude.Maybe CatalogConfigurationDescription)
zeppelinApplicationConfigurationDescription_catalogConfigurationDescription = Lens.lens (\ZeppelinApplicationConfigurationDescription' {catalogConfigurationDescription} -> catalogConfigurationDescription) (\s@ZeppelinApplicationConfigurationDescription' {} a -> s {catalogConfigurationDescription = a} :: ZeppelinApplicationConfigurationDescription)

-- | The monitoring configuration of a Kinesis Data Analytics Studio
-- notebook.
zeppelinApplicationConfigurationDescription_monitoringConfigurationDescription :: Lens.Lens' ZeppelinApplicationConfigurationDescription ZeppelinMonitoringConfigurationDescription
zeppelinApplicationConfigurationDescription_monitoringConfigurationDescription = Lens.lens (\ZeppelinApplicationConfigurationDescription' {monitoringConfigurationDescription} -> monitoringConfigurationDescription) (\s@ZeppelinApplicationConfigurationDescription' {} a -> s {monitoringConfigurationDescription = a} :: ZeppelinApplicationConfigurationDescription)

instance
  Core.FromJSON
    ZeppelinApplicationConfigurationDescription
  where
  parseJSON =
    Core.withObject
      "ZeppelinApplicationConfigurationDescription"
      ( \x ->
          ZeppelinApplicationConfigurationDescription'
            Prelude.<$> ( x Core..:? "CustomArtifactsConfigurationDescription"
                            Core..!= Prelude.mempty
                        )
              Prelude.<*> ( x
                              Core..:? "DeployAsApplicationConfigurationDescription"
                          )
              Prelude.<*> (x Core..:? "CatalogConfigurationDescription")
              Prelude.<*> (x Core..: "MonitoringConfigurationDescription")
      )

instance
  Prelude.Hashable
    ZeppelinApplicationConfigurationDescription
  where
  hashWithSalt
    salt'
    ZeppelinApplicationConfigurationDescription' {..} =
      salt'
        `Prelude.hashWithSalt` monitoringConfigurationDescription
        `Prelude.hashWithSalt` catalogConfigurationDescription
        `Prelude.hashWithSalt` deployAsApplicationConfigurationDescription
        `Prelude.hashWithSalt` customArtifactsConfigurationDescription

instance
  Prelude.NFData
    ZeppelinApplicationConfigurationDescription
  where
  rnf ZeppelinApplicationConfigurationDescription' {..} =
    Prelude.rnf customArtifactsConfigurationDescription
      `Prelude.seq` Prelude.rnf monitoringConfigurationDescription
      `Prelude.seq` Prelude.rnf catalogConfigurationDescription
      `Prelude.seq` Prelude.rnf
        deployAsApplicationConfigurationDescription
