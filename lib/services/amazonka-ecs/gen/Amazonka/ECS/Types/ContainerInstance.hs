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
-- Module      : Amazonka.ECS.Types.ContainerInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ContainerInstance where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types.AgentUpdateStatus
import Amazonka.ECS.Types.Attachment
import Amazonka.ECS.Types.Attribute
import Amazonka.ECS.Types.Resource
import Amazonka.ECS.Types.Tag
import Amazonka.ECS.Types.VersionInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An EC2 instance that is running the Amazon ECS agent and has been
-- registered with a cluster.
--
-- /See:/ 'newContainerInstance' smart constructor.
data ContainerInstance = ContainerInstance'
  { -- | The status of the container instance. The valid values are
    -- @REGISTERING@, @REGISTRATION_FAILED@, @ACTIVE@, @INACTIVE@,
    -- @DEREGISTERING@, or @DRAINING@.
    --
    -- If your account has opted in to the @awsvpcTrunking@ account setting,
    -- then any newly registered container instance will transition to a
    -- @REGISTERING@ status while the trunk elastic network interface is
    -- provisioned for the instance. If the registration fails, the instance
    -- will transition to a @REGISTRATION_FAILED@ status. You can describe the
    -- container instance and see the reason for failure in the @statusReason@
    -- parameter. Once the container instance is terminated, the instance
    -- transitions to a @DEREGISTERING@ status while the trunk elastic network
    -- interface is deprovisioned. The instance then transitions to an
    -- @INACTIVE@ status.
    --
    -- The @ACTIVE@ status indicates that the container instance can accept
    -- tasks. The @DRAINING@ indicates that new tasks are not placed on the
    -- container instance and any service tasks running on the container
    -- instance are removed if possible. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    status :: Prelude.Maybe Prelude.Text,
    -- | The resources attached to a container instance, such as elastic network
    -- interfaces.
    attachments :: Prelude.Maybe [Attachment],
    -- | The number of tasks on the container instance that are in the @RUNNING@
    -- status.
    runningTasksCount :: Prelude.Maybe Prelude.Int,
    -- | For CPU and memory resource types, this parameter describes the
    -- remaining CPU and memory that has not already been allocated to tasks
    -- and is therefore available for new tasks. For port resource types, this
    -- parameter describes the ports that were reserved by the Amazon ECS
    -- container agent (at instance registration time) and any task containers
    -- that have reserved port mappings on the host (with the @host@ or
    -- @bridge@ network mode). Any port that is not specified here is available
    -- for new tasks.
    remainingResources :: Prelude.Maybe [Resource],
    -- | The ID of the container instance. For Amazon EC2 instances, this value
    -- is the Amazon EC2 instance ID. For external instances, this value is the
    -- Amazon Web Services Systems Manager managed instance ID.
    ec2InstanceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the container instance. The ARN
    -- contains the @arn:aws:ecs@ namespace, followed by the Region of the
    -- container instance, the Amazon Web Services account ID of the container
    -- instance owner, the @container-instance@ namespace, and then the
    -- container instance ID. For example,
    -- @arn:aws:ecs:region:aws_account_id:container-instance\/container_instance_ID@.
    containerInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | This parameter returns @true@ if the agent is connected to Amazon ECS.
    -- Registered instances with an agent that may be unhealthy or stopped
    -- return @false@. Only instances connected to an agent can accept
    -- placement requests.
    agentConnected :: Prelude.Maybe Prelude.Bool,
    -- | The version information for the Amazon ECS container agent and Docker
    -- daemon running on the container instance.
    versionInfo :: Prelude.Maybe VersionInfo,
    -- | The status of the most recent agent update. If an update has never been
    -- requested, this value is @NULL@.
    agentUpdateStatus :: Prelude.Maybe AgentUpdateStatus,
    -- | The attributes set for the container instance, either by the Amazon ECS
    -- container agent at instance registration or manually with the
    -- PutAttributes operation.
    attributes :: Prelude.Maybe [Attribute],
    -- | The version counter for the container instance. Every time a container
    -- instance experiences a change that triggers a CloudWatch event, the
    -- version counter is incremented. If you are replicating your Amazon ECS
    -- container instance state with CloudWatch Events, you can compare the
    -- version of a container instance reported by the Amazon ECS APIs with the
    -- version reported in CloudWatch Events for the container instance (inside
    -- the @detail@ object) to verify that the version in your event stream is
    -- current.
    version :: Prelude.Maybe Prelude.Integer,
    -- | The number of tasks on the container instance that are in the @PENDING@
    -- status.
    pendingTasksCount :: Prelude.Maybe Prelude.Int,
    -- | The capacity provider associated with the container instance.
    capacityProviderName :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp for when the container instance was registered.
    registeredAt :: Prelude.Maybe Core.POSIX,
    -- | The reason that the container instance reached its current status.
    statusReason :: Prelude.Maybe Prelude.Text,
    -- | The metadata that you apply to the container instance to help you
    -- categorize and organize them. Each tag consists of a key and an optional
    -- value, both of which you define.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case-sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for either keys or values as it is reserved for
    --     Amazon Web Services use. You cannot edit or delete tag keys or
    --     values with this prefix. Tags with this prefix do not count against
    --     your tags per resource limit.
    tags :: Prelude.Maybe [Tag],
    -- | For CPU and memory resource types, this parameter describes the amount
    -- of each resource that was available on the container instance when the
    -- container agent registered it with Amazon ECS. This value represents the
    -- total amount of CPU and memory that can be allocated on this container
    -- instance to tasks. For port resource types, this parameter describes the
    -- ports that were reserved by the Amazon ECS container agent when it
    -- registered the container instance with Amazon ECS.
    registeredResources :: Prelude.Maybe [Resource]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'containerInstance_status' - The status of the container instance. The valid values are
-- @REGISTERING@, @REGISTRATION_FAILED@, @ACTIVE@, @INACTIVE@,
-- @DEREGISTERING@, or @DRAINING@.
--
-- If your account has opted in to the @awsvpcTrunking@ account setting,
-- then any newly registered container instance will transition to a
-- @REGISTERING@ status while the trunk elastic network interface is
-- provisioned for the instance. If the registration fails, the instance
-- will transition to a @REGISTRATION_FAILED@ status. You can describe the
-- container instance and see the reason for failure in the @statusReason@
-- parameter. Once the container instance is terminated, the instance
-- transitions to a @DEREGISTERING@ status while the trunk elastic network
-- interface is deprovisioned. The instance then transitions to an
-- @INACTIVE@ status.
--
-- The @ACTIVE@ status indicates that the container instance can accept
-- tasks. The @DRAINING@ indicates that new tasks are not placed on the
-- container instance and any service tasks running on the container
-- instance are removed if possible. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'attachments', 'containerInstance_attachments' - The resources attached to a container instance, such as elastic network
-- interfaces.
--
-- 'runningTasksCount', 'containerInstance_runningTasksCount' - The number of tasks on the container instance that are in the @RUNNING@
-- status.
--
-- 'remainingResources', 'containerInstance_remainingResources' - For CPU and memory resource types, this parameter describes the
-- remaining CPU and memory that has not already been allocated to tasks
-- and is therefore available for new tasks. For port resource types, this
-- parameter describes the ports that were reserved by the Amazon ECS
-- container agent (at instance registration time) and any task containers
-- that have reserved port mappings on the host (with the @host@ or
-- @bridge@ network mode). Any port that is not specified here is available
-- for new tasks.
--
-- 'ec2InstanceId', 'containerInstance_ec2InstanceId' - The ID of the container instance. For Amazon EC2 instances, this value
-- is the Amazon EC2 instance ID. For external instances, this value is the
-- Amazon Web Services Systems Manager managed instance ID.
--
-- 'containerInstanceArn', 'containerInstance_containerInstanceArn' - The Amazon Resource Name (ARN) of the container instance. The ARN
-- contains the @arn:aws:ecs@ namespace, followed by the Region of the
-- container instance, the Amazon Web Services account ID of the container
-- instance owner, the @container-instance@ namespace, and then the
-- container instance ID. For example,
-- @arn:aws:ecs:region:aws_account_id:container-instance\/container_instance_ID@.
--
-- 'agentConnected', 'containerInstance_agentConnected' - This parameter returns @true@ if the agent is connected to Amazon ECS.
-- Registered instances with an agent that may be unhealthy or stopped
-- return @false@. Only instances connected to an agent can accept
-- placement requests.
--
-- 'versionInfo', 'containerInstance_versionInfo' - The version information for the Amazon ECS container agent and Docker
-- daemon running on the container instance.
--
-- 'agentUpdateStatus', 'containerInstance_agentUpdateStatus' - The status of the most recent agent update. If an update has never been
-- requested, this value is @NULL@.
--
-- 'attributes', 'containerInstance_attributes' - The attributes set for the container instance, either by the Amazon ECS
-- container agent at instance registration or manually with the
-- PutAttributes operation.
--
-- 'version', 'containerInstance_version' - The version counter for the container instance. Every time a container
-- instance experiences a change that triggers a CloudWatch event, the
-- version counter is incremented. If you are replicating your Amazon ECS
-- container instance state with CloudWatch Events, you can compare the
-- version of a container instance reported by the Amazon ECS APIs with the
-- version reported in CloudWatch Events for the container instance (inside
-- the @detail@ object) to verify that the version in your event stream is
-- current.
--
-- 'pendingTasksCount', 'containerInstance_pendingTasksCount' - The number of tasks on the container instance that are in the @PENDING@
-- status.
--
-- 'capacityProviderName', 'containerInstance_capacityProviderName' - The capacity provider associated with the container instance.
--
-- 'registeredAt', 'containerInstance_registeredAt' - The Unix timestamp for when the container instance was registered.
--
-- 'statusReason', 'containerInstance_statusReason' - The reason that the container instance reached its current status.
--
-- 'tags', 'containerInstance_tags' - The metadata that you apply to the container instance to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
--
-- 'registeredResources', 'containerInstance_registeredResources' - For CPU and memory resource types, this parameter describes the amount
-- of each resource that was available on the container instance when the
-- container agent registered it with Amazon ECS. This value represents the
-- total amount of CPU and memory that can be allocated on this container
-- instance to tasks. For port resource types, this parameter describes the
-- ports that were reserved by the Amazon ECS container agent when it
-- registered the container instance with Amazon ECS.
newContainerInstance ::
  ContainerInstance
newContainerInstance =
  ContainerInstance'
    { status = Prelude.Nothing,
      attachments = Prelude.Nothing,
      runningTasksCount = Prelude.Nothing,
      remainingResources = Prelude.Nothing,
      ec2InstanceId = Prelude.Nothing,
      containerInstanceArn = Prelude.Nothing,
      agentConnected = Prelude.Nothing,
      versionInfo = Prelude.Nothing,
      agentUpdateStatus = Prelude.Nothing,
      attributes = Prelude.Nothing,
      version = Prelude.Nothing,
      pendingTasksCount = Prelude.Nothing,
      capacityProviderName = Prelude.Nothing,
      registeredAt = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      tags = Prelude.Nothing,
      registeredResources = Prelude.Nothing
    }

-- | The status of the container instance. The valid values are
-- @REGISTERING@, @REGISTRATION_FAILED@, @ACTIVE@, @INACTIVE@,
-- @DEREGISTERING@, or @DRAINING@.
--
-- If your account has opted in to the @awsvpcTrunking@ account setting,
-- then any newly registered container instance will transition to a
-- @REGISTERING@ status while the trunk elastic network interface is
-- provisioned for the instance. If the registration fails, the instance
-- will transition to a @REGISTRATION_FAILED@ status. You can describe the
-- container instance and see the reason for failure in the @statusReason@
-- parameter. Once the container instance is terminated, the instance
-- transitions to a @DEREGISTERING@ status while the trunk elastic network
-- interface is deprovisioned. The instance then transitions to an
-- @INACTIVE@ status.
--
-- The @ACTIVE@ status indicates that the container instance can accept
-- tasks. The @DRAINING@ indicates that new tasks are not placed on the
-- container instance and any service tasks running on the container
-- instance are removed if possible. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/container-instance-draining.html Container Instance Draining>
-- in the /Amazon Elastic Container Service Developer Guide/.
containerInstance_status :: Lens.Lens' ContainerInstance (Prelude.Maybe Prelude.Text)
containerInstance_status = Lens.lens (\ContainerInstance' {status} -> status) (\s@ContainerInstance' {} a -> s {status = a} :: ContainerInstance)

-- | The resources attached to a container instance, such as elastic network
-- interfaces.
containerInstance_attachments :: Lens.Lens' ContainerInstance (Prelude.Maybe [Attachment])
containerInstance_attachments = Lens.lens (\ContainerInstance' {attachments} -> attachments) (\s@ContainerInstance' {} a -> s {attachments = a} :: ContainerInstance) Prelude.. Lens.mapping Lens.coerced

-- | The number of tasks on the container instance that are in the @RUNNING@
-- status.
containerInstance_runningTasksCount :: Lens.Lens' ContainerInstance (Prelude.Maybe Prelude.Int)
containerInstance_runningTasksCount = Lens.lens (\ContainerInstance' {runningTasksCount} -> runningTasksCount) (\s@ContainerInstance' {} a -> s {runningTasksCount = a} :: ContainerInstance)

-- | For CPU and memory resource types, this parameter describes the
-- remaining CPU and memory that has not already been allocated to tasks
-- and is therefore available for new tasks. For port resource types, this
-- parameter describes the ports that were reserved by the Amazon ECS
-- container agent (at instance registration time) and any task containers
-- that have reserved port mappings on the host (with the @host@ or
-- @bridge@ network mode). Any port that is not specified here is available
-- for new tasks.
containerInstance_remainingResources :: Lens.Lens' ContainerInstance (Prelude.Maybe [Resource])
containerInstance_remainingResources = Lens.lens (\ContainerInstance' {remainingResources} -> remainingResources) (\s@ContainerInstance' {} a -> s {remainingResources = a} :: ContainerInstance) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the container instance. For Amazon EC2 instances, this value
-- is the Amazon EC2 instance ID. For external instances, this value is the
-- Amazon Web Services Systems Manager managed instance ID.
containerInstance_ec2InstanceId :: Lens.Lens' ContainerInstance (Prelude.Maybe Prelude.Text)
containerInstance_ec2InstanceId = Lens.lens (\ContainerInstance' {ec2InstanceId} -> ec2InstanceId) (\s@ContainerInstance' {} a -> s {ec2InstanceId = a} :: ContainerInstance)

-- | The Amazon Resource Name (ARN) of the container instance. The ARN
-- contains the @arn:aws:ecs@ namespace, followed by the Region of the
-- container instance, the Amazon Web Services account ID of the container
-- instance owner, the @container-instance@ namespace, and then the
-- container instance ID. For example,
-- @arn:aws:ecs:region:aws_account_id:container-instance\/container_instance_ID@.
containerInstance_containerInstanceArn :: Lens.Lens' ContainerInstance (Prelude.Maybe Prelude.Text)
containerInstance_containerInstanceArn = Lens.lens (\ContainerInstance' {containerInstanceArn} -> containerInstanceArn) (\s@ContainerInstance' {} a -> s {containerInstanceArn = a} :: ContainerInstance)

-- | This parameter returns @true@ if the agent is connected to Amazon ECS.
-- Registered instances with an agent that may be unhealthy or stopped
-- return @false@. Only instances connected to an agent can accept
-- placement requests.
containerInstance_agentConnected :: Lens.Lens' ContainerInstance (Prelude.Maybe Prelude.Bool)
containerInstance_agentConnected = Lens.lens (\ContainerInstance' {agentConnected} -> agentConnected) (\s@ContainerInstance' {} a -> s {agentConnected = a} :: ContainerInstance)

-- | The version information for the Amazon ECS container agent and Docker
-- daemon running on the container instance.
containerInstance_versionInfo :: Lens.Lens' ContainerInstance (Prelude.Maybe VersionInfo)
containerInstance_versionInfo = Lens.lens (\ContainerInstance' {versionInfo} -> versionInfo) (\s@ContainerInstance' {} a -> s {versionInfo = a} :: ContainerInstance)

-- | The status of the most recent agent update. If an update has never been
-- requested, this value is @NULL@.
containerInstance_agentUpdateStatus :: Lens.Lens' ContainerInstance (Prelude.Maybe AgentUpdateStatus)
containerInstance_agentUpdateStatus = Lens.lens (\ContainerInstance' {agentUpdateStatus} -> agentUpdateStatus) (\s@ContainerInstance' {} a -> s {agentUpdateStatus = a} :: ContainerInstance)

-- | The attributes set for the container instance, either by the Amazon ECS
-- container agent at instance registration or manually with the
-- PutAttributes operation.
containerInstance_attributes :: Lens.Lens' ContainerInstance (Prelude.Maybe [Attribute])
containerInstance_attributes = Lens.lens (\ContainerInstance' {attributes} -> attributes) (\s@ContainerInstance' {} a -> s {attributes = a} :: ContainerInstance) Prelude.. Lens.mapping Lens.coerced

-- | The version counter for the container instance. Every time a container
-- instance experiences a change that triggers a CloudWatch event, the
-- version counter is incremented. If you are replicating your Amazon ECS
-- container instance state with CloudWatch Events, you can compare the
-- version of a container instance reported by the Amazon ECS APIs with the
-- version reported in CloudWatch Events for the container instance (inside
-- the @detail@ object) to verify that the version in your event stream is
-- current.
containerInstance_version :: Lens.Lens' ContainerInstance (Prelude.Maybe Prelude.Integer)
containerInstance_version = Lens.lens (\ContainerInstance' {version} -> version) (\s@ContainerInstance' {} a -> s {version = a} :: ContainerInstance)

-- | The number of tasks on the container instance that are in the @PENDING@
-- status.
containerInstance_pendingTasksCount :: Lens.Lens' ContainerInstance (Prelude.Maybe Prelude.Int)
containerInstance_pendingTasksCount = Lens.lens (\ContainerInstance' {pendingTasksCount} -> pendingTasksCount) (\s@ContainerInstance' {} a -> s {pendingTasksCount = a} :: ContainerInstance)

-- | The capacity provider associated with the container instance.
containerInstance_capacityProviderName :: Lens.Lens' ContainerInstance (Prelude.Maybe Prelude.Text)
containerInstance_capacityProviderName = Lens.lens (\ContainerInstance' {capacityProviderName} -> capacityProviderName) (\s@ContainerInstance' {} a -> s {capacityProviderName = a} :: ContainerInstance)

-- | The Unix timestamp for when the container instance was registered.
containerInstance_registeredAt :: Lens.Lens' ContainerInstance (Prelude.Maybe Prelude.UTCTime)
containerInstance_registeredAt = Lens.lens (\ContainerInstance' {registeredAt} -> registeredAt) (\s@ContainerInstance' {} a -> s {registeredAt = a} :: ContainerInstance) Prelude.. Lens.mapping Core._Time

-- | The reason that the container instance reached its current status.
containerInstance_statusReason :: Lens.Lens' ContainerInstance (Prelude.Maybe Prelude.Text)
containerInstance_statusReason = Lens.lens (\ContainerInstance' {statusReason} -> statusReason) (\s@ContainerInstance' {} a -> s {statusReason = a} :: ContainerInstance)

-- | The metadata that you apply to the container instance to help you
-- categorize and organize them. Each tag consists of a key and an optional
-- value, both of which you define.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
containerInstance_tags :: Lens.Lens' ContainerInstance (Prelude.Maybe [Tag])
containerInstance_tags = Lens.lens (\ContainerInstance' {tags} -> tags) (\s@ContainerInstance' {} a -> s {tags = a} :: ContainerInstance) Prelude.. Lens.mapping Lens.coerced

-- | For CPU and memory resource types, this parameter describes the amount
-- of each resource that was available on the container instance when the
-- container agent registered it with Amazon ECS. This value represents the
-- total amount of CPU and memory that can be allocated on this container
-- instance to tasks. For port resource types, this parameter describes the
-- ports that were reserved by the Amazon ECS container agent when it
-- registered the container instance with Amazon ECS.
containerInstance_registeredResources :: Lens.Lens' ContainerInstance (Prelude.Maybe [Resource])
containerInstance_registeredResources = Lens.lens (\ContainerInstance' {registeredResources} -> registeredResources) (\s@ContainerInstance' {} a -> s {registeredResources = a} :: ContainerInstance) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ContainerInstance where
  parseJSON =
    Core.withObject
      "ContainerInstance"
      ( \x ->
          ContainerInstance'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "attachments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "runningTasksCount")
            Prelude.<*> ( x Core..:? "remainingResources"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ec2InstanceId")
            Prelude.<*> (x Core..:? "containerInstanceArn")
            Prelude.<*> (x Core..:? "agentConnected")
            Prelude.<*> (x Core..:? "versionInfo")
            Prelude.<*> (x Core..:? "agentUpdateStatus")
            Prelude.<*> (x Core..:? "attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "pendingTasksCount")
            Prelude.<*> (x Core..:? "capacityProviderName")
            Prelude.<*> (x Core..:? "registeredAt")
            Prelude.<*> (x Core..:? "statusReason")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "registeredResources"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ContainerInstance where
  hashWithSalt salt' ContainerInstance' {..} =
    salt' `Prelude.hashWithSalt` registeredResources
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` registeredAt
      `Prelude.hashWithSalt` capacityProviderName
      `Prelude.hashWithSalt` pendingTasksCount
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` attributes
      `Prelude.hashWithSalt` agentUpdateStatus
      `Prelude.hashWithSalt` versionInfo
      `Prelude.hashWithSalt` agentConnected
      `Prelude.hashWithSalt` containerInstanceArn
      `Prelude.hashWithSalt` ec2InstanceId
      `Prelude.hashWithSalt` remainingResources
      `Prelude.hashWithSalt` runningTasksCount
      `Prelude.hashWithSalt` attachments
      `Prelude.hashWithSalt` status

instance Prelude.NFData ContainerInstance where
  rnf ContainerInstance' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf registeredResources
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf registeredAt
      `Prelude.seq` Prelude.rnf capacityProviderName
      `Prelude.seq` Prelude.rnf pendingTasksCount
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf attributes
      `Prelude.seq` Prelude.rnf agentUpdateStatus
      `Prelude.seq` Prelude.rnf versionInfo
      `Prelude.seq` Prelude.rnf agentConnected
      `Prelude.seq` Prelude.rnf containerInstanceArn
      `Prelude.seq` Prelude.rnf ec2InstanceId
      `Prelude.seq` Prelude.rnf remainingResources
      `Prelude.seq` Prelude.rnf runningTasksCount
      `Prelude.seq` Prelude.rnf attachments
