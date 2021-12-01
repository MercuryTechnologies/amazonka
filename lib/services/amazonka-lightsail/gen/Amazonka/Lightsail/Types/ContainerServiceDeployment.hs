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
-- Module      : Amazonka.Lightsail.Types.ContainerServiceDeployment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ContainerServiceDeployment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.Container
import Amazonka.Lightsail.Types.ContainerServiceDeploymentState
import Amazonka.Lightsail.Types.ContainerServiceEndpoint
import qualified Amazonka.Prelude as Prelude

-- | Describes a container deployment configuration of an Amazon Lightsail
-- container service.
--
-- A deployment specifies the settings, such as the ports and launch
-- command, of containers that are deployed to your container service.
--
-- /See:/ 'newContainerServiceDeployment' smart constructor.
data ContainerServiceDeployment = ContainerServiceDeployment'
  { -- | The state of the deployment.
    --
    -- A deployment can be in one of the following states:
    --
    -- -   @Activating@ - The deployment is being created.
    --
    -- -   @Active@ - The deployment was successfully created, and it\'s
    --     currently running on the container service. The container service
    --     can have only one deployment in an active state at a time.
    --
    -- -   @Inactive@ - The deployment was previously successfully created, but
    --     it is not currently running on the container service.
    --
    -- -   @Failed@ - The deployment failed. Use the @GetContainerLog@ action
    --     to view the log events for the containers in the deployment to try
    --     to determine the reason for the failure.
    state :: Prelude.Maybe ContainerServiceDeploymentState,
    -- | An object that describes the endpoint of the deployment.
    publicEndpoint :: Prelude.Maybe ContainerServiceEndpoint,
    -- | The timestamp when the deployment was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | An object that describes the configuration for the containers of the
    -- deployment.
    containers :: Prelude.Maybe (Prelude.HashMap Prelude.Text Container),
    -- | The version number of the deployment.
    version :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerServiceDeployment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'containerServiceDeployment_state' - The state of the deployment.
--
-- A deployment can be in one of the following states:
--
-- -   @Activating@ - The deployment is being created.
--
-- -   @Active@ - The deployment was successfully created, and it\'s
--     currently running on the container service. The container service
--     can have only one deployment in an active state at a time.
--
-- -   @Inactive@ - The deployment was previously successfully created, but
--     it is not currently running on the container service.
--
-- -   @Failed@ - The deployment failed. Use the @GetContainerLog@ action
--     to view the log events for the containers in the deployment to try
--     to determine the reason for the failure.
--
-- 'publicEndpoint', 'containerServiceDeployment_publicEndpoint' - An object that describes the endpoint of the deployment.
--
-- 'createdAt', 'containerServiceDeployment_createdAt' - The timestamp when the deployment was created.
--
-- 'containers', 'containerServiceDeployment_containers' - An object that describes the configuration for the containers of the
-- deployment.
--
-- 'version', 'containerServiceDeployment_version' - The version number of the deployment.
newContainerServiceDeployment ::
  ContainerServiceDeployment
newContainerServiceDeployment =
  ContainerServiceDeployment'
    { state =
        Prelude.Nothing,
      publicEndpoint = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      containers = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The state of the deployment.
--
-- A deployment can be in one of the following states:
--
-- -   @Activating@ - The deployment is being created.
--
-- -   @Active@ - The deployment was successfully created, and it\'s
--     currently running on the container service. The container service
--     can have only one deployment in an active state at a time.
--
-- -   @Inactive@ - The deployment was previously successfully created, but
--     it is not currently running on the container service.
--
-- -   @Failed@ - The deployment failed. Use the @GetContainerLog@ action
--     to view the log events for the containers in the deployment to try
--     to determine the reason for the failure.
containerServiceDeployment_state :: Lens.Lens' ContainerServiceDeployment (Prelude.Maybe ContainerServiceDeploymentState)
containerServiceDeployment_state = Lens.lens (\ContainerServiceDeployment' {state} -> state) (\s@ContainerServiceDeployment' {} a -> s {state = a} :: ContainerServiceDeployment)

-- | An object that describes the endpoint of the deployment.
containerServiceDeployment_publicEndpoint :: Lens.Lens' ContainerServiceDeployment (Prelude.Maybe ContainerServiceEndpoint)
containerServiceDeployment_publicEndpoint = Lens.lens (\ContainerServiceDeployment' {publicEndpoint} -> publicEndpoint) (\s@ContainerServiceDeployment' {} a -> s {publicEndpoint = a} :: ContainerServiceDeployment)

-- | The timestamp when the deployment was created.
containerServiceDeployment_createdAt :: Lens.Lens' ContainerServiceDeployment (Prelude.Maybe Prelude.UTCTime)
containerServiceDeployment_createdAt = Lens.lens (\ContainerServiceDeployment' {createdAt} -> createdAt) (\s@ContainerServiceDeployment' {} a -> s {createdAt = a} :: ContainerServiceDeployment) Prelude.. Lens.mapping Core._Time

-- | An object that describes the configuration for the containers of the
-- deployment.
containerServiceDeployment_containers :: Lens.Lens' ContainerServiceDeployment (Prelude.Maybe (Prelude.HashMap Prelude.Text Container))
containerServiceDeployment_containers = Lens.lens (\ContainerServiceDeployment' {containers} -> containers) (\s@ContainerServiceDeployment' {} a -> s {containers = a} :: ContainerServiceDeployment) Prelude.. Lens.mapping Lens.coerced

-- | The version number of the deployment.
containerServiceDeployment_version :: Lens.Lens' ContainerServiceDeployment (Prelude.Maybe Prelude.Int)
containerServiceDeployment_version = Lens.lens (\ContainerServiceDeployment' {version} -> version) (\s@ContainerServiceDeployment' {} a -> s {version = a} :: ContainerServiceDeployment)

instance Core.FromJSON ContainerServiceDeployment where
  parseJSON =
    Core.withObject
      "ContainerServiceDeployment"
      ( \x ->
          ContainerServiceDeployment'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "publicEndpoint")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "containers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "version")
      )

instance Prelude.Hashable ContainerServiceDeployment where
  hashWithSalt salt' ContainerServiceDeployment' {..} =
    salt' `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` containers
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` publicEndpoint
      `Prelude.hashWithSalt` state

instance Prelude.NFData ContainerServiceDeployment where
  rnf ContainerServiceDeployment' {..} =
    Prelude.rnf state `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf containers
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf publicEndpoint
