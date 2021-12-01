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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A host device to expose to the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails = AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails'
  { -- | The path inside the container at which to expose the host device.
    containerPath :: Prelude.Maybe Prelude.Text,
    -- | The path for the device on the host container instance.
    hostPath :: Prelude.Maybe Prelude.Text,
    -- | The explicit permissions to provide to the container for the device. By
    -- default, the container has permissions for read, write, and @mknod@ for
    -- the device.
    permissions :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'containerPath', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_containerPath' - The path inside the container at which to expose the host device.
--
-- 'hostPath', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_hostPath' - The path for the device on the host container instance.
--
-- 'permissions', 'awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_permissions' - The explicit permissions to provide to the container for the device. By
-- default, the container has permissions for read, write, and @mknod@ for
-- the device.
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
newAwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails =
  AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails'
    { containerPath =
        Prelude.Nothing,
      hostPath =
        Prelude.Nothing,
      permissions =
        Prelude.Nothing
    }

-- | The path inside the container at which to expose the host device.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_containerPath :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_containerPath = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails' {containerPath} -> containerPath) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails' {} a -> s {containerPath = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails)

-- | The path for the device on the host container instance.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_hostPath :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_hostPath = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails' {hostPath} -> hostPath) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails' {} a -> s {hostPath = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails)

-- | The explicit permissions to provide to the container for the device. By
-- default, the container has permissions for read, write, and @mknod@ for
-- the device.
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_permissions :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails_permissions = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails' {permissions} -> permissions) (\s@AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails' {} a -> s {permissions = a} :: AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails'
            Prelude.<$> (x Core..:? "ContainerPath")
              Prelude.<*> (x Core..:? "HostPath")
              Prelude.<*> (x Core..:? "Permissions" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
  where
  hashWithSalt
    salt'
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails' {..} =
      salt' `Prelude.hashWithSalt` permissions
        `Prelude.hashWithSalt` hostPath
        `Prelude.hashWithSalt` containerPath

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails' {..} =
      Prelude.rnf containerPath
        `Prelude.seq` Prelude.rnf permissions
        `Prelude.seq` Prelude.rnf hostPath

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDevicesDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ContainerPath" Core..=) Prelude.<$> containerPath,
              ("HostPath" Core..=) Prelude.<$> hostPath,
              ("Permissions" Core..=) Prelude.<$> permissions
            ]
        )
