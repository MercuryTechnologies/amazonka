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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsSecretsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails
import Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails

-- | A container definition that describes a container in the task.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsDetails = AwsEcsTaskDefinitionContainerDefinitionsDetails'
  { -- | The command that is passed to the container.
    command :: Prelude.Maybe [Prelude.Text],
    -- | The hostname to use for the container.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The image used to start the container.
    image :: Prelude.Maybe Prelude.Text,
    -- | The private repository authentication credentials to use.
    repositoryCredentials :: Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails,
    -- | A list of strings to provide custom labels for SELinux and AppArmor
    -- multi-level security systems.
    dockerSecurityOptions :: Prelude.Maybe [Prelude.Text],
    -- | The container health check command and associated configuration
    -- parameters for the container.
    healthCheck :: Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails,
    -- | Whether to disable networking within the container.
    disableNetworking :: Prelude.Maybe Prelude.Bool,
    -- | Data volumes to mount from another container.
    volumesFrom :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails],
    -- | The environment variables to pass to a container.
    environment :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails],
    -- | The secrets to pass to the container.
    secrets :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsSecretsDetails],
    -- | The entry point that is passed to the container.
    entryPoint :: Prelude.Maybe [Prelude.Text],
    -- | The working directory in which to run commands inside the container.
    workingDirectory :: Prelude.Maybe Prelude.Text,
    -- | A list of files containing the environment variables to pass to a
    -- container.
    environmentFiles :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails],
    -- | The number of seconds to wait before the container is stopped if it
    -- doesn\'t shut down normally on its own.
    stopTimeout :: Prelude.Maybe Prelude.Int,
    -- | Whether the container is given elevated privileges on the host container
    -- instance. The elevated privileges are similar to the root user.
    privileged :: Prelude.Maybe Prelude.Bool,
    -- | A list of ulimits to set in the container.
    ulimits :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails],
    -- | The list of port mappings for the container.
    portMappings :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails],
    -- | The type and amount of a resource to assign to a container. The only
    -- supported resource is a GPU.
    resourceRequirements :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails],
    -- | The amount (in MiB) of memory to present to the container. If the
    -- container attempts to exceed the memory specified here, the container is
    -- shut down. The total amount of memory reserved for all containers within
    -- a task must be lower than the task memory value, if one is specified.
    memory :: Prelude.Maybe Prelude.Int,
    -- | A list of hostnames and IP address mappings to append to the
    -- __\/etc\/hosts__ file on the container.
    extraHosts :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails],
    -- | A key-value map of labels to add to the container.
    dockerLabels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The user to use inside the container.
    --
    -- The value can use one of the following formats.
    --
    -- -   @ user @
    --
    -- -   @ user @:@ group @
    --
    -- -   @ uid @
    --
    -- -   @ uid @:@ gid @
    --
    -- -   @ user @:@ gid @
    --
    -- -   @ uid @:@ group @
    user :: Prelude.Maybe Prelude.Text,
    -- | A list of namespaced kernel parameters to set in the container.
    systemControls :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails],
    -- | Linux-specific modifications that are applied to the container, such as
    -- Linux kernel capabilities.
    linuxParameters :: Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails,
    -- | The log configuration specification for the container.
    logConfiguration :: Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails,
    -- | The FireLens configuration for the container. Specifies and configures a
    -- log router for container logs.
    firelensConfiguration :: Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails,
    -- | A list of DNS search domains that are presented to the container.
    dnsSearchDomains :: Prelude.Maybe [Prelude.Text],
    -- | Whether to allocate a TTY to the container.
    pseudoTerminal :: Prelude.Maybe Prelude.Bool,
    -- | The dependencies that are defined for container startup and shutdown.
    dependsOn :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails],
    -- | A list of DNS servers that are presented to the container.
    dnsServers :: Prelude.Maybe [Prelude.Text],
    -- | The name of the container.
    name :: Prelude.Maybe Prelude.Text,
    -- | The mount points for the data volumes in the container.
    mountPoints :: Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails],
    -- | If set to true, then containerized applications can be deployed that
    -- require @stdin@ or a @tty@ to be allocated.
    interactive :: Prelude.Maybe Prelude.Bool,
    -- | The number of seconds to wait before giving up on resolving dependencies
    -- for a container.
    startTimeout :: Prelude.Maybe Prelude.Int,
    -- | Whether the container is given read-only access to its root file system.
    readonlyRootFilesystem :: Prelude.Maybe Prelude.Bool,
    -- | A list of links for the container in the form @ container_name:alias @.
    -- Allows containers to communicate with each other without the need for
    -- port mappings.
    links :: Prelude.Maybe [Prelude.Text],
    -- | The number of CPU units reserved for the container.
    cpu :: Prelude.Maybe Prelude.Int,
    -- | Whether the container is essential. All tasks must have at least one
    -- essential container.
    essential :: Prelude.Maybe Prelude.Bool,
    -- | The soft limit (in MiB) of memory to reserve for the container.
    memoryReservation :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'command', 'awsEcsTaskDefinitionContainerDefinitionsDetails_command' - The command that is passed to the container.
--
-- 'hostname', 'awsEcsTaskDefinitionContainerDefinitionsDetails_hostname' - The hostname to use for the container.
--
-- 'image', 'awsEcsTaskDefinitionContainerDefinitionsDetails_image' - The image used to start the container.
--
-- 'repositoryCredentials', 'awsEcsTaskDefinitionContainerDefinitionsDetails_repositoryCredentials' - The private repository authentication credentials to use.
--
-- 'dockerSecurityOptions', 'awsEcsTaskDefinitionContainerDefinitionsDetails_dockerSecurityOptions' - A list of strings to provide custom labels for SELinux and AppArmor
-- multi-level security systems.
--
-- 'healthCheck', 'awsEcsTaskDefinitionContainerDefinitionsDetails_healthCheck' - The container health check command and associated configuration
-- parameters for the container.
--
-- 'disableNetworking', 'awsEcsTaskDefinitionContainerDefinitionsDetails_disableNetworking' - Whether to disable networking within the container.
--
-- 'volumesFrom', 'awsEcsTaskDefinitionContainerDefinitionsDetails_volumesFrom' - Data volumes to mount from another container.
--
-- 'environment', 'awsEcsTaskDefinitionContainerDefinitionsDetails_environment' - The environment variables to pass to a container.
--
-- 'secrets', 'awsEcsTaskDefinitionContainerDefinitionsDetails_secrets' - The secrets to pass to the container.
--
-- 'entryPoint', 'awsEcsTaskDefinitionContainerDefinitionsDetails_entryPoint' - The entry point that is passed to the container.
--
-- 'workingDirectory', 'awsEcsTaskDefinitionContainerDefinitionsDetails_workingDirectory' - The working directory in which to run commands inside the container.
--
-- 'environmentFiles', 'awsEcsTaskDefinitionContainerDefinitionsDetails_environmentFiles' - A list of files containing the environment variables to pass to a
-- container.
--
-- 'stopTimeout', 'awsEcsTaskDefinitionContainerDefinitionsDetails_stopTimeout' - The number of seconds to wait before the container is stopped if it
-- doesn\'t shut down normally on its own.
--
-- 'privileged', 'awsEcsTaskDefinitionContainerDefinitionsDetails_privileged' - Whether the container is given elevated privileges on the host container
-- instance. The elevated privileges are similar to the root user.
--
-- 'ulimits', 'awsEcsTaskDefinitionContainerDefinitionsDetails_ulimits' - A list of ulimits to set in the container.
--
-- 'portMappings', 'awsEcsTaskDefinitionContainerDefinitionsDetails_portMappings' - The list of port mappings for the container.
--
-- 'resourceRequirements', 'awsEcsTaskDefinitionContainerDefinitionsDetails_resourceRequirements' - The type and amount of a resource to assign to a container. The only
-- supported resource is a GPU.
--
-- 'memory', 'awsEcsTaskDefinitionContainerDefinitionsDetails_memory' - The amount (in MiB) of memory to present to the container. If the
-- container attempts to exceed the memory specified here, the container is
-- shut down. The total amount of memory reserved for all containers within
-- a task must be lower than the task memory value, if one is specified.
--
-- 'extraHosts', 'awsEcsTaskDefinitionContainerDefinitionsDetails_extraHosts' - A list of hostnames and IP address mappings to append to the
-- __\/etc\/hosts__ file on the container.
--
-- 'dockerLabels', 'awsEcsTaskDefinitionContainerDefinitionsDetails_dockerLabels' - A key-value map of labels to add to the container.
--
-- 'user', 'awsEcsTaskDefinitionContainerDefinitionsDetails_user' - The user to use inside the container.
--
-- The value can use one of the following formats.
--
-- -   @ user @
--
-- -   @ user @:@ group @
--
-- -   @ uid @
--
-- -   @ uid @:@ gid @
--
-- -   @ user @:@ gid @
--
-- -   @ uid @:@ group @
--
-- 'systemControls', 'awsEcsTaskDefinitionContainerDefinitionsDetails_systemControls' - A list of namespaced kernel parameters to set in the container.
--
-- 'linuxParameters', 'awsEcsTaskDefinitionContainerDefinitionsDetails_linuxParameters' - Linux-specific modifications that are applied to the container, such as
-- Linux kernel capabilities.
--
-- 'logConfiguration', 'awsEcsTaskDefinitionContainerDefinitionsDetails_logConfiguration' - The log configuration specification for the container.
--
-- 'firelensConfiguration', 'awsEcsTaskDefinitionContainerDefinitionsDetails_firelensConfiguration' - The FireLens configuration for the container. Specifies and configures a
-- log router for container logs.
--
-- 'dnsSearchDomains', 'awsEcsTaskDefinitionContainerDefinitionsDetails_dnsSearchDomains' - A list of DNS search domains that are presented to the container.
--
-- 'pseudoTerminal', 'awsEcsTaskDefinitionContainerDefinitionsDetails_pseudoTerminal' - Whether to allocate a TTY to the container.
--
-- 'dependsOn', 'awsEcsTaskDefinitionContainerDefinitionsDetails_dependsOn' - The dependencies that are defined for container startup and shutdown.
--
-- 'dnsServers', 'awsEcsTaskDefinitionContainerDefinitionsDetails_dnsServers' - A list of DNS servers that are presented to the container.
--
-- 'name', 'awsEcsTaskDefinitionContainerDefinitionsDetails_name' - The name of the container.
--
-- 'mountPoints', 'awsEcsTaskDefinitionContainerDefinitionsDetails_mountPoints' - The mount points for the data volumes in the container.
--
-- 'interactive', 'awsEcsTaskDefinitionContainerDefinitionsDetails_interactive' - If set to true, then containerized applications can be deployed that
-- require @stdin@ or a @tty@ to be allocated.
--
-- 'startTimeout', 'awsEcsTaskDefinitionContainerDefinitionsDetails_startTimeout' - The number of seconds to wait before giving up on resolving dependencies
-- for a container.
--
-- 'readonlyRootFilesystem', 'awsEcsTaskDefinitionContainerDefinitionsDetails_readonlyRootFilesystem' - Whether the container is given read-only access to its root file system.
--
-- 'links', 'awsEcsTaskDefinitionContainerDefinitionsDetails_links' - A list of links for the container in the form @ container_name:alias @.
-- Allows containers to communicate with each other without the need for
-- port mappings.
--
-- 'cpu', 'awsEcsTaskDefinitionContainerDefinitionsDetails_cpu' - The number of CPU units reserved for the container.
--
-- 'essential', 'awsEcsTaskDefinitionContainerDefinitionsDetails_essential' - Whether the container is essential. All tasks must have at least one
-- essential container.
--
-- 'memoryReservation', 'awsEcsTaskDefinitionContainerDefinitionsDetails_memoryReservation' - The soft limit (in MiB) of memory to reserve for the container.
newAwsEcsTaskDefinitionContainerDefinitionsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsDetails
newAwsEcsTaskDefinitionContainerDefinitionsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsDetails'
    { command =
        Prelude.Nothing,
      hostname = Prelude.Nothing,
      image = Prelude.Nothing,
      repositoryCredentials =
        Prelude.Nothing,
      dockerSecurityOptions =
        Prelude.Nothing,
      healthCheck =
        Prelude.Nothing,
      disableNetworking =
        Prelude.Nothing,
      volumesFrom =
        Prelude.Nothing,
      environment =
        Prelude.Nothing,
      secrets = Prelude.Nothing,
      entryPoint =
        Prelude.Nothing,
      workingDirectory =
        Prelude.Nothing,
      environmentFiles =
        Prelude.Nothing,
      stopTimeout =
        Prelude.Nothing,
      privileged =
        Prelude.Nothing,
      ulimits = Prelude.Nothing,
      portMappings =
        Prelude.Nothing,
      resourceRequirements =
        Prelude.Nothing,
      memory = Prelude.Nothing,
      extraHosts =
        Prelude.Nothing,
      dockerLabels =
        Prelude.Nothing,
      user = Prelude.Nothing,
      systemControls =
        Prelude.Nothing,
      linuxParameters =
        Prelude.Nothing,
      logConfiguration =
        Prelude.Nothing,
      firelensConfiguration =
        Prelude.Nothing,
      dnsSearchDomains =
        Prelude.Nothing,
      pseudoTerminal =
        Prelude.Nothing,
      dependsOn =
        Prelude.Nothing,
      dnsServers =
        Prelude.Nothing,
      name = Prelude.Nothing,
      mountPoints =
        Prelude.Nothing,
      interactive =
        Prelude.Nothing,
      startTimeout =
        Prelude.Nothing,
      readonlyRootFilesystem =
        Prelude.Nothing,
      links = Prelude.Nothing,
      cpu = Prelude.Nothing,
      essential =
        Prelude.Nothing,
      memoryReservation =
        Prelude.Nothing
    }

-- | The command that is passed to the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_command :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsDetails_command = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {command} -> command) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {command = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The hostname to use for the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_hostname :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsDetails_hostname = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {hostname} -> hostname) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {hostname = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | The image used to start the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_image :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsDetails_image = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {image} -> image) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {image = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | The private repository authentication credentials to use.
awsEcsTaskDefinitionContainerDefinitionsDetails_repositoryCredentials :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsRepositoryCredentialsDetails)
awsEcsTaskDefinitionContainerDefinitionsDetails_repositoryCredentials = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {repositoryCredentials} -> repositoryCredentials) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {repositoryCredentials = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | A list of strings to provide custom labels for SELinux and AppArmor
-- multi-level security systems.
awsEcsTaskDefinitionContainerDefinitionsDetails_dockerSecurityOptions :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsDetails_dockerSecurityOptions = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {dockerSecurityOptions} -> dockerSecurityOptions) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {dockerSecurityOptions = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The container health check command and associated configuration
-- parameters for the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_healthCheck :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsHealthCheckDetails)
awsEcsTaskDefinitionContainerDefinitionsDetails_healthCheck = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {healthCheck} -> healthCheck) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {healthCheck = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | Whether to disable networking within the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_disableNetworking :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionContainerDefinitionsDetails_disableNetworking = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {disableNetworking} -> disableNetworking) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {disableNetworking = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | Data volumes to mount from another container.
awsEcsTaskDefinitionContainerDefinitionsDetails_volumesFrom :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsVolumesFromDetails])
awsEcsTaskDefinitionContainerDefinitionsDetails_volumesFrom = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {volumesFrom} -> volumesFrom) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {volumesFrom = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The environment variables to pass to a container.
awsEcsTaskDefinitionContainerDefinitionsDetails_environment :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsEnvironmentDetails])
awsEcsTaskDefinitionContainerDefinitionsDetails_environment = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {environment} -> environment) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {environment = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The secrets to pass to the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_secrets :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsSecretsDetails])
awsEcsTaskDefinitionContainerDefinitionsDetails_secrets = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {secrets} -> secrets) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {secrets = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The entry point that is passed to the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_entryPoint :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsDetails_entryPoint = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {entryPoint} -> entryPoint) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {entryPoint = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The working directory in which to run commands inside the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_workingDirectory :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsDetails_workingDirectory = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {workingDirectory} -> workingDirectory) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {workingDirectory = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | A list of files containing the environment variables to pass to a
-- container.
awsEcsTaskDefinitionContainerDefinitionsDetails_environmentFiles :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsEnvironmentFilesDetails])
awsEcsTaskDefinitionContainerDefinitionsDetails_environmentFiles = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {environmentFiles} -> environmentFiles) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {environmentFiles = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of seconds to wait before the container is stopped if it
-- doesn\'t shut down normally on its own.
awsEcsTaskDefinitionContainerDefinitionsDetails_stopTimeout :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsDetails_stopTimeout = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {stopTimeout} -> stopTimeout) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {stopTimeout = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | Whether the container is given elevated privileges on the host container
-- instance. The elevated privileges are similar to the root user.
awsEcsTaskDefinitionContainerDefinitionsDetails_privileged :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionContainerDefinitionsDetails_privileged = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {privileged} -> privileged) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {privileged = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | A list of ulimits to set in the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_ulimits :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsUlimitsDetails])
awsEcsTaskDefinitionContainerDefinitionsDetails_ulimits = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {ulimits} -> ulimits) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {ulimits = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The list of port mappings for the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_portMappings :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsPortMappingsDetails])
awsEcsTaskDefinitionContainerDefinitionsDetails_portMappings = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {portMappings} -> portMappings) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {portMappings = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The type and amount of a resource to assign to a container. The only
-- supported resource is a GPU.
awsEcsTaskDefinitionContainerDefinitionsDetails_resourceRequirements :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsResourceRequirementsDetails])
awsEcsTaskDefinitionContainerDefinitionsDetails_resourceRequirements = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {resourceRequirements} -> resourceRequirements) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {resourceRequirements = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The amount (in MiB) of memory to present to the container. If the
-- container attempts to exceed the memory specified here, the container is
-- shut down. The total amount of memory reserved for all containers within
-- a task must be lower than the task memory value, if one is specified.
awsEcsTaskDefinitionContainerDefinitionsDetails_memory :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsDetails_memory = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {memory} -> memory) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {memory = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | A list of hostnames and IP address mappings to append to the
-- __\/etc\/hosts__ file on the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_extraHosts :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails])
awsEcsTaskDefinitionContainerDefinitionsDetails_extraHosts = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {extraHosts} -> extraHosts) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {extraHosts = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | A key-value map of labels to add to the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_dockerLabels :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
awsEcsTaskDefinitionContainerDefinitionsDetails_dockerLabels = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {dockerLabels} -> dockerLabels) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {dockerLabels = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The user to use inside the container.
--
-- The value can use one of the following formats.
--
-- -   @ user @
--
-- -   @ user @:@ group @
--
-- -   @ uid @
--
-- -   @ uid @:@ gid @
--
-- -   @ user @:@ gid @
--
-- -   @ uid @:@ group @
awsEcsTaskDefinitionContainerDefinitionsDetails_user :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsDetails_user = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {user} -> user) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {user = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | A list of namespaced kernel parameters to set in the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_systemControls :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsSystemControlsDetails])
awsEcsTaskDefinitionContainerDefinitionsDetails_systemControls = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {systemControls} -> systemControls) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {systemControls = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | Linux-specific modifications that are applied to the container, such as
-- Linux kernel capabilities.
awsEcsTaskDefinitionContainerDefinitionsDetails_linuxParameters :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsLinuxParametersDetails)
awsEcsTaskDefinitionContainerDefinitionsDetails_linuxParameters = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {linuxParameters} -> linuxParameters) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {linuxParameters = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | The log configuration specification for the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_logConfiguration :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsLogConfigurationDetails)
awsEcsTaskDefinitionContainerDefinitionsDetails_logConfiguration = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {logConfiguration} -> logConfiguration) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {logConfiguration = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | The FireLens configuration for the container. Specifies and configures a
-- log router for container logs.
awsEcsTaskDefinitionContainerDefinitionsDetails_firelensConfiguration :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe AwsEcsTaskDefinitionContainerDefinitionsFirelensConfigurationDetails)
awsEcsTaskDefinitionContainerDefinitionsDetails_firelensConfiguration = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {firelensConfiguration} -> firelensConfiguration) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {firelensConfiguration = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | A list of DNS search domains that are presented to the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_dnsSearchDomains :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsDetails_dnsSearchDomains = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {dnsSearchDomains} -> dnsSearchDomains) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {dnsSearchDomains = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | Whether to allocate a TTY to the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_pseudoTerminal :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionContainerDefinitionsDetails_pseudoTerminal = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {pseudoTerminal} -> pseudoTerminal) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {pseudoTerminal = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | The dependencies that are defined for container startup and shutdown.
awsEcsTaskDefinitionContainerDefinitionsDetails_dependsOn :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsDependsOnDetails])
awsEcsTaskDefinitionContainerDefinitionsDetails_dependsOn = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {dependsOn} -> dependsOn) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {dependsOn = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of DNS servers that are presented to the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_dnsServers :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsDetails_dnsServers = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {dnsServers} -> dnsServers) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {dnsServers = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_name :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsDetails_name = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {name} -> name) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {name = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | The mount points for the data volumes in the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_mountPoints :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [AwsEcsTaskDefinitionContainerDefinitionsMountPointsDetails])
awsEcsTaskDefinitionContainerDefinitionsDetails_mountPoints = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {mountPoints} -> mountPoints) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {mountPoints = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | If set to true, then containerized applications can be deployed that
-- require @stdin@ or a @tty@ to be allocated.
awsEcsTaskDefinitionContainerDefinitionsDetails_interactive :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionContainerDefinitionsDetails_interactive = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {interactive} -> interactive) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {interactive = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | The number of seconds to wait before giving up on resolving dependencies
-- for a container.
awsEcsTaskDefinitionContainerDefinitionsDetails_startTimeout :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsDetails_startTimeout = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {startTimeout} -> startTimeout) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {startTimeout = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | Whether the container is given read-only access to its root file system.
awsEcsTaskDefinitionContainerDefinitionsDetails_readonlyRootFilesystem :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionContainerDefinitionsDetails_readonlyRootFilesystem = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {readonlyRootFilesystem} -> readonlyRootFilesystem) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {readonlyRootFilesystem = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | A list of links for the container in the form @ container_name:alias @.
-- Allows containers to communicate with each other without the need for
-- port mappings.
awsEcsTaskDefinitionContainerDefinitionsDetails_links :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe [Prelude.Text])
awsEcsTaskDefinitionContainerDefinitionsDetails_links = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {links} -> links) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {links = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of CPU units reserved for the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_cpu :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsDetails_cpu = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {cpu} -> cpu) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {cpu = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | Whether the container is essential. All tasks must have at least one
-- essential container.
awsEcsTaskDefinitionContainerDefinitionsDetails_essential :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Bool)
awsEcsTaskDefinitionContainerDefinitionsDetails_essential = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {essential} -> essential) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {essential = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

-- | The soft limit (in MiB) of memory to reserve for the container.
awsEcsTaskDefinitionContainerDefinitionsDetails_memoryReservation :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsDetails (Prelude.Maybe Prelude.Int)
awsEcsTaskDefinitionContainerDefinitionsDetails_memoryReservation = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsDetails' {memoryReservation} -> memoryReservation) (\s@AwsEcsTaskDefinitionContainerDefinitionsDetails' {} a -> s {memoryReservation = a} :: AwsEcsTaskDefinitionContainerDefinitionsDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsDetails'
            Prelude.<$> (x Core..:? "Command" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "Hostname")
              Prelude.<*> (x Core..:? "Image")
              Prelude.<*> (x Core..:? "RepositoryCredentials")
              Prelude.<*> ( x Core..:? "DockerSecurityOptions"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "HealthCheck")
              Prelude.<*> (x Core..:? "DisableNetworking")
              Prelude.<*> (x Core..:? "VolumesFrom" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "Environment" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "Secrets" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "EntryPoint" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "WorkingDirectory")
              Prelude.<*> ( x Core..:? "EnvironmentFiles"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "StopTimeout")
              Prelude.<*> (x Core..:? "Privileged")
              Prelude.<*> (x Core..:? "Ulimits" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "PortMappings" Core..!= Prelude.mempty)
              Prelude.<*> ( x Core..:? "ResourceRequirements"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "Memory")
              Prelude.<*> (x Core..:? "ExtraHosts" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "DockerLabels" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "User")
              Prelude.<*> (x Core..:? "SystemControls" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "LinuxParameters")
              Prelude.<*> (x Core..:? "LogConfiguration")
              Prelude.<*> (x Core..:? "FirelensConfiguration")
              Prelude.<*> ( x Core..:? "DnsSearchDomains"
                              Core..!= Prelude.mempty
                          )
              Prelude.<*> (x Core..:? "PseudoTerminal")
              Prelude.<*> (x Core..:? "DependsOn" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "DnsServers" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "Name")
              Prelude.<*> (x Core..:? "MountPoints" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "Interactive")
              Prelude.<*> (x Core..:? "StartTimeout")
              Prelude.<*> (x Core..:? "ReadonlyRootFilesystem")
              Prelude.<*> (x Core..:? "Links" Core..!= Prelude.mempty)
              Prelude.<*> (x Core..:? "Cpu")
              Prelude.<*> (x Core..:? "Essential")
              Prelude.<*> (x Core..:? "MemoryReservation")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsDetails
  where
  hashWithSalt
    salt'
    AwsEcsTaskDefinitionContainerDefinitionsDetails' {..} =
      salt' `Prelude.hashWithSalt` memoryReservation
        `Prelude.hashWithSalt` essential
        `Prelude.hashWithSalt` cpu
        `Prelude.hashWithSalt` links
        `Prelude.hashWithSalt` readonlyRootFilesystem
        `Prelude.hashWithSalt` startTimeout
        `Prelude.hashWithSalt` interactive
        `Prelude.hashWithSalt` mountPoints
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` dnsServers
        `Prelude.hashWithSalt` dependsOn
        `Prelude.hashWithSalt` pseudoTerminal
        `Prelude.hashWithSalt` dnsSearchDomains
        `Prelude.hashWithSalt` firelensConfiguration
        `Prelude.hashWithSalt` logConfiguration
        `Prelude.hashWithSalt` linuxParameters
        `Prelude.hashWithSalt` systemControls
        `Prelude.hashWithSalt` user
        `Prelude.hashWithSalt` dockerLabels
        `Prelude.hashWithSalt` extraHosts
        `Prelude.hashWithSalt` memory
        `Prelude.hashWithSalt` resourceRequirements
        `Prelude.hashWithSalt` portMappings
        `Prelude.hashWithSalt` ulimits
        `Prelude.hashWithSalt` privileged
        `Prelude.hashWithSalt` stopTimeout
        `Prelude.hashWithSalt` environmentFiles
        `Prelude.hashWithSalt` workingDirectory
        `Prelude.hashWithSalt` entryPoint
        `Prelude.hashWithSalt` secrets
        `Prelude.hashWithSalt` environment
        `Prelude.hashWithSalt` volumesFrom
        `Prelude.hashWithSalt` disableNetworking
        `Prelude.hashWithSalt` healthCheck
        `Prelude.hashWithSalt` dockerSecurityOptions
        `Prelude.hashWithSalt` repositoryCredentials
        `Prelude.hashWithSalt` image
        `Prelude.hashWithSalt` hostname
        `Prelude.hashWithSalt` command

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsDetails' {..} =
      Prelude.rnf command
        `Prelude.seq` Prelude.rnf memoryReservation
        `Prelude.seq` Prelude.rnf essential
        `Prelude.seq` Prelude.rnf cpu
        `Prelude.seq` Prelude.rnf links
        `Prelude.seq` Prelude.rnf readonlyRootFilesystem
        `Prelude.seq` Prelude.rnf startTimeout
        `Prelude.seq` Prelude.rnf interactive
        `Prelude.seq` Prelude.rnf mountPoints
        `Prelude.seq` Prelude.rnf name
        `Prelude.seq` Prelude.rnf dnsServers
        `Prelude.seq` Prelude.rnf dependsOn
        `Prelude.seq` Prelude.rnf pseudoTerminal
        `Prelude.seq` Prelude.rnf dnsSearchDomains
        `Prelude.seq` Prelude.rnf firelensConfiguration
        `Prelude.seq` Prelude.rnf logConfiguration
        `Prelude.seq` Prelude.rnf linuxParameters
        `Prelude.seq` Prelude.rnf systemControls
        `Prelude.seq` Prelude.rnf user
        `Prelude.seq` Prelude.rnf dockerLabels
        `Prelude.seq` Prelude.rnf extraHosts
        `Prelude.seq` Prelude.rnf memory
        `Prelude.seq` Prelude.rnf resourceRequirements
        `Prelude.seq` Prelude.rnf portMappings
        `Prelude.seq` Prelude.rnf ulimits
        `Prelude.seq` Prelude.rnf privileged
        `Prelude.seq` Prelude.rnf stopTimeout
        `Prelude.seq` Prelude.rnf environmentFiles
        `Prelude.seq` Prelude.rnf workingDirectory
        `Prelude.seq` Prelude.rnf entryPoint
        `Prelude.seq` Prelude.rnf secrets
        `Prelude.seq` Prelude.rnf environment
        `Prelude.seq` Prelude.rnf volumesFrom
        `Prelude.seq` Prelude.rnf disableNetworking
        `Prelude.seq` Prelude.rnf healthCheck
        `Prelude.seq` Prelude.rnf dockerSecurityOptions
        `Prelude.seq` Prelude.rnf repositoryCredentials
        `Prelude.seq` Prelude.rnf image
        `Prelude.seq` Prelude.rnf hostname

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Command" Core..=) Prelude.<$> command,
              ("Hostname" Core..=) Prelude.<$> hostname,
              ("Image" Core..=) Prelude.<$> image,
              ("RepositoryCredentials" Core..=)
                Prelude.<$> repositoryCredentials,
              ("DockerSecurityOptions" Core..=)
                Prelude.<$> dockerSecurityOptions,
              ("HealthCheck" Core..=) Prelude.<$> healthCheck,
              ("DisableNetworking" Core..=)
                Prelude.<$> disableNetworking,
              ("VolumesFrom" Core..=) Prelude.<$> volumesFrom,
              ("Environment" Core..=) Prelude.<$> environment,
              ("Secrets" Core..=) Prelude.<$> secrets,
              ("EntryPoint" Core..=) Prelude.<$> entryPoint,
              ("WorkingDirectory" Core..=)
                Prelude.<$> workingDirectory,
              ("EnvironmentFiles" Core..=)
                Prelude.<$> environmentFiles,
              ("StopTimeout" Core..=) Prelude.<$> stopTimeout,
              ("Privileged" Core..=) Prelude.<$> privileged,
              ("Ulimits" Core..=) Prelude.<$> ulimits,
              ("PortMappings" Core..=) Prelude.<$> portMappings,
              ("ResourceRequirements" Core..=)
                Prelude.<$> resourceRequirements,
              ("Memory" Core..=) Prelude.<$> memory,
              ("ExtraHosts" Core..=) Prelude.<$> extraHosts,
              ("DockerLabels" Core..=) Prelude.<$> dockerLabels,
              ("User" Core..=) Prelude.<$> user,
              ("SystemControls" Core..=)
                Prelude.<$> systemControls,
              ("LinuxParameters" Core..=)
                Prelude.<$> linuxParameters,
              ("LogConfiguration" Core..=)
                Prelude.<$> logConfiguration,
              ("FirelensConfiguration" Core..=)
                Prelude.<$> firelensConfiguration,
              ("DnsSearchDomains" Core..=)
                Prelude.<$> dnsSearchDomains,
              ("PseudoTerminal" Core..=)
                Prelude.<$> pseudoTerminal,
              ("DependsOn" Core..=) Prelude.<$> dependsOn,
              ("DnsServers" Core..=) Prelude.<$> dnsServers,
              ("Name" Core..=) Prelude.<$> name,
              ("MountPoints" Core..=) Prelude.<$> mountPoints,
              ("Interactive" Core..=) Prelude.<$> interactive,
              ("StartTimeout" Core..=) Prelude.<$> startTimeout,
              ("ReadonlyRootFilesystem" Core..=)
                Prelude.<$> readonlyRootFilesystem,
              ("Links" Core..=) Prelude.<$> links,
              ("Cpu" Core..=) Prelude.<$> cpu,
              ("Essential" Core..=) Prelude.<$> essential,
              ("MemoryReservation" Core..=)
                Prelude.<$> memoryReservation
            ]
        )
