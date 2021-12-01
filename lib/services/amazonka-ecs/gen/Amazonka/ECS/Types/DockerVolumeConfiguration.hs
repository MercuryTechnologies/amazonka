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
-- Module      : Amazonka.ECS.Types.DockerVolumeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.DockerVolumeConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types.Scope
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | This parameter is specified when you are using Docker volumes. Docker
-- volumes are only supported when you are using the EC2 launch type.
-- Windows containers only support the use of the @local@ driver. To use
-- bind mounts, specify a @host@ instead.
--
-- /See:/ 'newDockerVolumeConfiguration' smart constructor.
data DockerVolumeConfiguration = DockerVolumeConfiguration'
  { -- | A map of Docker driver-specific options passed through. This parameter
    -- maps to @DriverOpts@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @xxopt@ option to
    -- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
    driverOpts :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Docker volume driver to use. The driver value must match the driver
    -- name provided by Docker because it is used for task placement. If the
    -- driver was installed using the Docker plugin CLI, use @docker plugin ls@
    -- to retrieve the driver name from your container instance. If the driver
    -- was installed using another method, use Docker plugin discovery to
    -- retrieve the driver name. For more information, see
    -- <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery>.
    -- This parameter maps to @Driver@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @xxdriver@ option to
    -- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
    driver :: Prelude.Maybe Prelude.Text,
    -- | The scope for the Docker volume that determines its lifecycle. Docker
    -- volumes that are scoped to a @task@ are automatically provisioned when
    -- the task starts and destroyed when the task stops. Docker volumes that
    -- are scoped as @shared@ persist after the task stops.
    scope :: Prelude.Maybe Scope,
    -- | Custom metadata to add to your Docker volume. This parameter maps to
    -- @Labels@ in the
    -- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
    -- section of the
    -- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
    -- @xxlabel@ option to
    -- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
    labels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | If this value is @true@, the Docker volume is created if it does not
    -- already exist.
    --
    -- This field is only used if the @scope@ is @shared@.
    autoprovision :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DockerVolumeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'driverOpts', 'dockerVolumeConfiguration_driverOpts' - A map of Docker driver-specific options passed through. This parameter
-- maps to @DriverOpts@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxopt@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
--
-- 'driver', 'dockerVolumeConfiguration_driver' - The Docker volume driver to use. The driver value must match the driver
-- name provided by Docker because it is used for task placement. If the
-- driver was installed using the Docker plugin CLI, use @docker plugin ls@
-- to retrieve the driver name from your container instance. If the driver
-- was installed using another method, use Docker plugin discovery to
-- retrieve the driver name. For more information, see
-- <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery>.
-- This parameter maps to @Driver@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxdriver@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
--
-- 'scope', 'dockerVolumeConfiguration_scope' - The scope for the Docker volume that determines its lifecycle. Docker
-- volumes that are scoped to a @task@ are automatically provisioned when
-- the task starts and destroyed when the task stops. Docker volumes that
-- are scoped as @shared@ persist after the task stops.
--
-- 'labels', 'dockerVolumeConfiguration_labels' - Custom metadata to add to your Docker volume. This parameter maps to
-- @Labels@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxlabel@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
--
-- 'autoprovision', 'dockerVolumeConfiguration_autoprovision' - If this value is @true@, the Docker volume is created if it does not
-- already exist.
--
-- This field is only used if the @scope@ is @shared@.
newDockerVolumeConfiguration ::
  DockerVolumeConfiguration
newDockerVolumeConfiguration =
  DockerVolumeConfiguration'
    { driverOpts =
        Prelude.Nothing,
      driver = Prelude.Nothing,
      scope = Prelude.Nothing,
      labels = Prelude.Nothing,
      autoprovision = Prelude.Nothing
    }

-- | A map of Docker driver-specific options passed through. This parameter
-- maps to @DriverOpts@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxopt@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
dockerVolumeConfiguration_driverOpts :: Lens.Lens' DockerVolumeConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dockerVolumeConfiguration_driverOpts = Lens.lens (\DockerVolumeConfiguration' {driverOpts} -> driverOpts) (\s@DockerVolumeConfiguration' {} a -> s {driverOpts = a} :: DockerVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Docker volume driver to use. The driver value must match the driver
-- name provided by Docker because it is used for task placement. If the
-- driver was installed using the Docker plugin CLI, use @docker plugin ls@
-- to retrieve the driver name from your container instance. If the driver
-- was installed using another method, use Docker plugin discovery to
-- retrieve the driver name. For more information, see
-- <https://docs.docker.com/engine/extend/plugin_api/#plugin-discovery Docker plugin discovery>.
-- This parameter maps to @Driver@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxdriver@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
dockerVolumeConfiguration_driver :: Lens.Lens' DockerVolumeConfiguration (Prelude.Maybe Prelude.Text)
dockerVolumeConfiguration_driver = Lens.lens (\DockerVolumeConfiguration' {driver} -> driver) (\s@DockerVolumeConfiguration' {} a -> s {driver = a} :: DockerVolumeConfiguration)

-- | The scope for the Docker volume that determines its lifecycle. Docker
-- volumes that are scoped to a @task@ are automatically provisioned when
-- the task starts and destroyed when the task stops. Docker volumes that
-- are scoped as @shared@ persist after the task stops.
dockerVolumeConfiguration_scope :: Lens.Lens' DockerVolumeConfiguration (Prelude.Maybe Scope)
dockerVolumeConfiguration_scope = Lens.lens (\DockerVolumeConfiguration' {scope} -> scope) (\s@DockerVolumeConfiguration' {} a -> s {scope = a} :: DockerVolumeConfiguration)

-- | Custom metadata to add to your Docker volume. This parameter maps to
-- @Labels@ in the
-- <https://docs.docker.com/engine/api/v1.35/#operation/VolumeCreate Create a volume>
-- section of the
-- <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the
-- @xxlabel@ option to
-- <https://docs.docker.com/engine/reference/commandline/volume_create/ docker volume create>.
dockerVolumeConfiguration_labels :: Lens.Lens' DockerVolumeConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
dockerVolumeConfiguration_labels = Lens.lens (\DockerVolumeConfiguration' {labels} -> labels) (\s@DockerVolumeConfiguration' {} a -> s {labels = a} :: DockerVolumeConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | If this value is @true@, the Docker volume is created if it does not
-- already exist.
--
-- This field is only used if the @scope@ is @shared@.
dockerVolumeConfiguration_autoprovision :: Lens.Lens' DockerVolumeConfiguration (Prelude.Maybe Prelude.Bool)
dockerVolumeConfiguration_autoprovision = Lens.lens (\DockerVolumeConfiguration' {autoprovision} -> autoprovision) (\s@DockerVolumeConfiguration' {} a -> s {autoprovision = a} :: DockerVolumeConfiguration)

instance Core.FromJSON DockerVolumeConfiguration where
  parseJSON =
    Core.withObject
      "DockerVolumeConfiguration"
      ( \x ->
          DockerVolumeConfiguration'
            Prelude.<$> (x Core..:? "driverOpts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "driver")
            Prelude.<*> (x Core..:? "scope")
            Prelude.<*> (x Core..:? "labels" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "autoprovision")
      )

instance Prelude.Hashable DockerVolumeConfiguration where
  hashWithSalt salt' DockerVolumeConfiguration' {..} =
    salt' `Prelude.hashWithSalt` autoprovision
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` scope
      `Prelude.hashWithSalt` driver
      `Prelude.hashWithSalt` driverOpts

instance Prelude.NFData DockerVolumeConfiguration where
  rnf DockerVolumeConfiguration' {..} =
    Prelude.rnf driverOpts
      `Prelude.seq` Prelude.rnf autoprovision
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf scope
      `Prelude.seq` Prelude.rnf driver

instance Core.ToJSON DockerVolumeConfiguration where
  toJSON DockerVolumeConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("driverOpts" Core..=) Prelude.<$> driverOpts,
            ("driver" Core..=) Prelude.<$> driver,
            ("scope" Core..=) Prelude.<$> scope,
            ("labels" Core..=) Prelude.<$> labels,
            ("autoprovision" Core..=) Prelude.<$> autoprovision
          ]
      )
