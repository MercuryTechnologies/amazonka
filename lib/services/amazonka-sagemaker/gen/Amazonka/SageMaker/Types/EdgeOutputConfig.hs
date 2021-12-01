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
-- Module      : Amazonka.SageMaker.Types.EdgeOutputConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EdgeOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.EdgePresetDeploymentType

-- | The output configuration.
--
-- /See:/ 'newEdgeOutputConfig' smart constructor.
data EdgeOutputConfig = EdgeOutputConfig'
  { -- | The deployment type SageMaker Edge Manager will create. Currently only
    -- supports Amazon Web Services IoT Greengrass Version 2 components.
    presetDeploymentType :: Prelude.Maybe EdgePresetDeploymentType,
    -- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
    -- key that Amazon SageMaker uses to encrypt data on the storage volume
    -- after compilation job. If you don\'t provide a KMS key ID, Amazon
    -- SageMaker uses the default KMS key for Amazon S3 for your role\'s
    -- account.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The configuration used to create deployment artifacts. Specify
    -- configuration options with a JSON string. The available configuration
    -- options for each type are:
    --
    -- -   @ComponentName@ (optional) - Name of the GreenGrass V2 component. If
    --     not specified, the default name generated consists of
    --     \"SagemakerEdgeManager\" and the name of your SageMaker Edge Manager
    --     packaging job.
    --
    -- -   @ComponentDescription@ (optional) - Description of the component.
    --
    -- -   @ComponentVersion@ (optional) - The version of the component.
    --
    --     Amazon Web Services IoT Greengrass uses semantic versions for
    --     components. Semantic versions follow a /major.minor.patch/ number
    --     system. For example, version 1.0.0 represents the first major
    --     release for a component. For more information, see the
    --     <https://semver.org/ semantic version specification>.
    --
    -- -   @PlatformOS@ (optional) - The name of the operating system for the
    --     platform. Supported platforms include Windows and Linux.
    --
    -- -   @PlatformArchitecture@ (optional) - The processor architecture for
    --     the platform.
    --
    --     Supported architectures Windows include: Windows32_x86,
    --     Windows64_x64.
    --
    --     Supported architectures for Linux include: Linux x86_64, Linux
    --     ARMV8.
    presetDeploymentConfig :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Simple Storage (S3) bucker URI.
    s3OutputLocation :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgeOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'presetDeploymentType', 'edgeOutputConfig_presetDeploymentType' - The deployment type SageMaker Edge Manager will create. Currently only
-- supports Amazon Web Services IoT Greengrass Version 2 components.
--
-- 'kmsKeyId', 'edgeOutputConfig_kmsKeyId' - The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data on the storage volume
-- after compilation job. If you don\'t provide a KMS key ID, Amazon
-- SageMaker uses the default KMS key for Amazon S3 for your role\'s
-- account.
--
-- 'presetDeploymentConfig', 'edgeOutputConfig_presetDeploymentConfig' - The configuration used to create deployment artifacts. Specify
-- configuration options with a JSON string. The available configuration
-- options for each type are:
--
-- -   @ComponentName@ (optional) - Name of the GreenGrass V2 component. If
--     not specified, the default name generated consists of
--     \"SagemakerEdgeManager\" and the name of your SageMaker Edge Manager
--     packaging job.
--
-- -   @ComponentDescription@ (optional) - Description of the component.
--
-- -   @ComponentVersion@ (optional) - The version of the component.
--
--     Amazon Web Services IoT Greengrass uses semantic versions for
--     components. Semantic versions follow a /major.minor.patch/ number
--     system. For example, version 1.0.0 represents the first major
--     release for a component. For more information, see the
--     <https://semver.org/ semantic version specification>.
--
-- -   @PlatformOS@ (optional) - The name of the operating system for the
--     platform. Supported platforms include Windows and Linux.
--
-- -   @PlatformArchitecture@ (optional) - The processor architecture for
--     the platform.
--
--     Supported architectures Windows include: Windows32_x86,
--     Windows64_x64.
--
--     Supported architectures for Linux include: Linux x86_64, Linux
--     ARMV8.
--
-- 's3OutputLocation', 'edgeOutputConfig_s3OutputLocation' - The Amazon Simple Storage (S3) bucker URI.
newEdgeOutputConfig ::
  -- | 's3OutputLocation'
  Prelude.Text ->
  EdgeOutputConfig
newEdgeOutputConfig pS3OutputLocation_ =
  EdgeOutputConfig'
    { presetDeploymentType =
        Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      presetDeploymentConfig = Prelude.Nothing,
      s3OutputLocation = pS3OutputLocation_
    }

-- | The deployment type SageMaker Edge Manager will create. Currently only
-- supports Amazon Web Services IoT Greengrass Version 2 components.
edgeOutputConfig_presetDeploymentType :: Lens.Lens' EdgeOutputConfig (Prelude.Maybe EdgePresetDeploymentType)
edgeOutputConfig_presetDeploymentType = Lens.lens (\EdgeOutputConfig' {presetDeploymentType} -> presetDeploymentType) (\s@EdgeOutputConfig' {} a -> s {presetDeploymentType = a} :: EdgeOutputConfig)

-- | The Amazon Web Services Key Management Service (Amazon Web Services KMS)
-- key that Amazon SageMaker uses to encrypt data on the storage volume
-- after compilation job. If you don\'t provide a KMS key ID, Amazon
-- SageMaker uses the default KMS key for Amazon S3 for your role\'s
-- account.
edgeOutputConfig_kmsKeyId :: Lens.Lens' EdgeOutputConfig (Prelude.Maybe Prelude.Text)
edgeOutputConfig_kmsKeyId = Lens.lens (\EdgeOutputConfig' {kmsKeyId} -> kmsKeyId) (\s@EdgeOutputConfig' {} a -> s {kmsKeyId = a} :: EdgeOutputConfig)

-- | The configuration used to create deployment artifacts. Specify
-- configuration options with a JSON string. The available configuration
-- options for each type are:
--
-- -   @ComponentName@ (optional) - Name of the GreenGrass V2 component. If
--     not specified, the default name generated consists of
--     \"SagemakerEdgeManager\" and the name of your SageMaker Edge Manager
--     packaging job.
--
-- -   @ComponentDescription@ (optional) - Description of the component.
--
-- -   @ComponentVersion@ (optional) - The version of the component.
--
--     Amazon Web Services IoT Greengrass uses semantic versions for
--     components. Semantic versions follow a /major.minor.patch/ number
--     system. For example, version 1.0.0 represents the first major
--     release for a component. For more information, see the
--     <https://semver.org/ semantic version specification>.
--
-- -   @PlatformOS@ (optional) - The name of the operating system for the
--     platform. Supported platforms include Windows and Linux.
--
-- -   @PlatformArchitecture@ (optional) - The processor architecture for
--     the platform.
--
--     Supported architectures Windows include: Windows32_x86,
--     Windows64_x64.
--
--     Supported architectures for Linux include: Linux x86_64, Linux
--     ARMV8.
edgeOutputConfig_presetDeploymentConfig :: Lens.Lens' EdgeOutputConfig (Prelude.Maybe Prelude.Text)
edgeOutputConfig_presetDeploymentConfig = Lens.lens (\EdgeOutputConfig' {presetDeploymentConfig} -> presetDeploymentConfig) (\s@EdgeOutputConfig' {} a -> s {presetDeploymentConfig = a} :: EdgeOutputConfig)

-- | The Amazon Simple Storage (S3) bucker URI.
edgeOutputConfig_s3OutputLocation :: Lens.Lens' EdgeOutputConfig Prelude.Text
edgeOutputConfig_s3OutputLocation = Lens.lens (\EdgeOutputConfig' {s3OutputLocation} -> s3OutputLocation) (\s@EdgeOutputConfig' {} a -> s {s3OutputLocation = a} :: EdgeOutputConfig)

instance Core.FromJSON EdgeOutputConfig where
  parseJSON =
    Core.withObject
      "EdgeOutputConfig"
      ( \x ->
          EdgeOutputConfig'
            Prelude.<$> (x Core..:? "PresetDeploymentType")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "PresetDeploymentConfig")
            Prelude.<*> (x Core..: "S3OutputLocation")
      )

instance Prelude.Hashable EdgeOutputConfig where
  hashWithSalt salt' EdgeOutputConfig' {..} =
    salt' `Prelude.hashWithSalt` s3OutputLocation
      `Prelude.hashWithSalt` presetDeploymentConfig
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` presetDeploymentType

instance Prelude.NFData EdgeOutputConfig where
  rnf EdgeOutputConfig' {..} =
    Prelude.rnf presetDeploymentType
      `Prelude.seq` Prelude.rnf s3OutputLocation
      `Prelude.seq` Prelude.rnf presetDeploymentConfig
      `Prelude.seq` Prelude.rnf kmsKeyId

instance Core.ToJSON EdgeOutputConfig where
  toJSON EdgeOutputConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PresetDeploymentType" Core..=)
              Prelude.<$> presetDeploymentType,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("PresetDeploymentConfig" Core..=)
              Prelude.<$> presetDeploymentConfig,
            Prelude.Just
              ("S3OutputLocation" Core..= s3OutputLocation)
          ]
      )
