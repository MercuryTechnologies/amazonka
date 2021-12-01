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
-- Module      : Amazonka.ECS.Types.TaskSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.TaskSet where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types.CapacityProviderStrategyItem
import Amazonka.ECS.Types.LaunchType
import Amazonka.ECS.Types.LoadBalancer
import Amazonka.ECS.Types.NetworkConfiguration
import Amazonka.ECS.Types.Scale
import Amazonka.ECS.Types.ServiceRegistry
import Amazonka.ECS.Types.StabilityStatus
import Amazonka.ECS.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a set of Amazon ECS tasks in either an CodeDeploy or
-- an @EXTERNAL@ deployment. An Amazon ECS task set includes details such
-- as the desired number of tasks, how many tasks are running, and whether
-- the task set serves production traffic.
--
-- /See:/ 'newTaskSet' smart constructor.
data TaskSet = TaskSet'
  { -- | The number of tasks in the task set that are in the @RUNNING@ status
    -- during a deployment. A task in the @RUNNING@ state is running and ready
    -- for use.
    runningCount :: Prelude.Maybe Prelude.Int,
    -- | The status of the task set. The following describes each state:
    --
    -- [PRIMARY]
    --     The task set is serving production traffic.
    --
    -- [ACTIVE]
    --     The task set is not serving production traffic.
    --
    -- [DRAINING]
    --     The tasks in the task set are being stopped and their corresponding
    --     targets are being deregistered from their target group.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster that the service that
    -- hosts the task set exists in.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The computed desired count for the task set. This is calculated by
    -- multiplying the service\'s @desiredCount@ by the task set\'s @scale@
    -- percentage. The result is always rounded up. For example, if the
    -- computed desired count is 1.2, it rounds up to 2 tasks.
    computedDesiredCount :: Prelude.Maybe Prelude.Int,
    -- | The Unix timestamp for when the task set was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Fargate platform version on which the tasks in the task set are
    -- running. A platform version is only specified for tasks run on Fargate.
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    platformVersion :: Prelude.Maybe Prelude.Text,
    -- | A floating-point percentage of the desired number of tasks to place and
    -- keep running in the task set.
    scale :: Prelude.Maybe Scale,
    -- | Details on a load balancer that is used with a task set.
    loadBalancers :: Prelude.Maybe [LoadBalancer],
    -- | The Unix timestamp for when the task set stability status was retrieved.
    stabilityStatusAt :: Prelude.Maybe Core.POSIX,
    -- | The number of tasks in the task set that are in the @PENDING@ status
    -- during a deployment. A task in the @PENDING@ state is preparing to enter
    -- the @RUNNING@ state. A task set enters the @PENDING@ status when it
    -- launches for the first time or when it is restarted after being in the
    -- @STOPPED@ state.
    pendingCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the task set.
    taskSetArn :: Prelude.Maybe Prelude.Text,
    -- | The tag specified when a task set is started. If the task set is created
    -- by an CodeDeploy deployment, the @startedBy@ parameter is @CODE_DEPLOY@.
    -- For a task set created for an external deployment, the startedBy field
    -- isn\'t used.
    startedBy :: Prelude.Maybe Prelude.Text,
    -- | The ID of the task set.
    id :: Prelude.Maybe Prelude.Text,
    -- | The launch type the tasks in the task set are using. For more
    -- information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
    -- in the /Amazon Elastic Container Service Developer Guide/.
    launchType :: Prelude.Maybe LaunchType,
    -- | The Unix timestamp for when the task set was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the service the task set exists in.
    serviceArn :: Prelude.Maybe Prelude.Text,
    -- | The task definition the task set is using.
    taskDefinition :: Prelude.Maybe Prelude.Text,
    -- | The external ID associated with the task set.
    --
    -- If a task set is created by an CodeDeploy deployment, the @externalId@
    -- parameter contains the CodeDeploy deployment ID.
    --
    -- If a task set is created for an external deployment and is associated
    -- with a service discovery registry, the @externalId@ parameter contains
    -- the @ECS_TASK_SET_EXTERNAL_ID@ Cloud Map attribute.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The network configuration for the task set.
    networkConfiguration :: Prelude.Maybe NetworkConfiguration,
    -- | The details of the service discovery registries to assign to this task
    -- set. For more information, see
    -- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
    serviceRegistries :: Prelude.Maybe [ServiceRegistry],
    -- | The capacity provider strategy associated with the task set.
    capacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | The stability status, which indicates whether the task set has reached a
    -- steady state. If the following conditions are met, the task set will be
    -- in @STEADY_STATE@:
    --
    -- -   The task @runningCount@ is equal to the @computedDesiredCount@.
    --
    -- -   The @pendingCount@ is @0@.
    --
    -- -   There are no tasks running on container instances in the @DRAINING@
    --     status.
    --
    -- -   All tasks are reporting a healthy status from the load balancers,
    --     service discovery, and container health checks.
    --
    -- If any of those conditions are not met, the stability status returns
    -- @STABILIZING@.
    stabilityStatus :: Prelude.Maybe StabilityStatus,
    -- | The metadata that you apply to the task set to help you categorize and
    -- organize them. Each tag consists of a key and an optional value, both of
    -- which you define.
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
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'runningCount', 'taskSet_runningCount' - The number of tasks in the task set that are in the @RUNNING@ status
-- during a deployment. A task in the @RUNNING@ state is running and ready
-- for use.
--
-- 'status', 'taskSet_status' - The status of the task set. The following describes each state:
--
-- [PRIMARY]
--     The task set is serving production traffic.
--
-- [ACTIVE]
--     The task set is not serving production traffic.
--
-- [DRAINING]
--     The tasks in the task set are being stopped and their corresponding
--     targets are being deregistered from their target group.
--
-- 'clusterArn', 'taskSet_clusterArn' - The Amazon Resource Name (ARN) of the cluster that the service that
-- hosts the task set exists in.
--
-- 'computedDesiredCount', 'taskSet_computedDesiredCount' - The computed desired count for the task set. This is calculated by
-- multiplying the service\'s @desiredCount@ by the task set\'s @scale@
-- percentage. The result is always rounded up. For example, if the
-- computed desired count is 1.2, it rounds up to 2 tasks.
--
-- 'createdAt', 'taskSet_createdAt' - The Unix timestamp for when the task set was created.
--
-- 'platformVersion', 'taskSet_platformVersion' - The Fargate platform version on which the tasks in the task set are
-- running. A platform version is only specified for tasks run on Fargate.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'scale', 'taskSet_scale' - A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
--
-- 'loadBalancers', 'taskSet_loadBalancers' - Details on a load balancer that is used with a task set.
--
-- 'stabilityStatusAt', 'taskSet_stabilityStatusAt' - The Unix timestamp for when the task set stability status was retrieved.
--
-- 'pendingCount', 'taskSet_pendingCount' - The number of tasks in the task set that are in the @PENDING@ status
-- during a deployment. A task in the @PENDING@ state is preparing to enter
-- the @RUNNING@ state. A task set enters the @PENDING@ status when it
-- launches for the first time or when it is restarted after being in the
-- @STOPPED@ state.
--
-- 'taskSetArn', 'taskSet_taskSetArn' - The Amazon Resource Name (ARN) of the task set.
--
-- 'startedBy', 'taskSet_startedBy' - The tag specified when a task set is started. If the task set is created
-- by an CodeDeploy deployment, the @startedBy@ parameter is @CODE_DEPLOY@.
-- For a task set created for an external deployment, the startedBy field
-- isn\'t used.
--
-- 'id', 'taskSet_id' - The ID of the task set.
--
-- 'launchType', 'taskSet_launchType' - The launch type the tasks in the task set are using. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- 'updatedAt', 'taskSet_updatedAt' - The Unix timestamp for when the task set was last updated.
--
-- 'serviceArn', 'taskSet_serviceArn' - The Amazon Resource Name (ARN) of the service the task set exists in.
--
-- 'taskDefinition', 'taskSet_taskDefinition' - The task definition the task set is using.
--
-- 'externalId', 'taskSet_externalId' - The external ID associated with the task set.
--
-- If a task set is created by an CodeDeploy deployment, the @externalId@
-- parameter contains the CodeDeploy deployment ID.
--
-- If a task set is created for an external deployment and is associated
-- with a service discovery registry, the @externalId@ parameter contains
-- the @ECS_TASK_SET_EXTERNAL_ID@ Cloud Map attribute.
--
-- 'networkConfiguration', 'taskSet_networkConfiguration' - The network configuration for the task set.
--
-- 'serviceRegistries', 'taskSet_serviceRegistries' - The details of the service discovery registries to assign to this task
-- set. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
--
-- 'capacityProviderStrategy', 'taskSet_capacityProviderStrategy' - The capacity provider strategy associated with the task set.
--
-- 'stabilityStatus', 'taskSet_stabilityStatus' - The stability status, which indicates whether the task set has reached a
-- steady state. If the following conditions are met, the task set will be
-- in @STEADY_STATE@:
--
-- -   The task @runningCount@ is equal to the @computedDesiredCount@.
--
-- -   The @pendingCount@ is @0@.
--
-- -   There are no tasks running on container instances in the @DRAINING@
--     status.
--
-- -   All tasks are reporting a healthy status from the load balancers,
--     service discovery, and container health checks.
--
-- If any of those conditions are not met, the stability status returns
-- @STABILIZING@.
--
-- 'tags', 'taskSet_tags' - The metadata that you apply to the task set to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define.
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
newTaskSet ::
  TaskSet
newTaskSet =
  TaskSet'
    { runningCount = Prelude.Nothing,
      status = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      computedDesiredCount = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      platformVersion = Prelude.Nothing,
      scale = Prelude.Nothing,
      loadBalancers = Prelude.Nothing,
      stabilityStatusAt = Prelude.Nothing,
      pendingCount = Prelude.Nothing,
      taskSetArn = Prelude.Nothing,
      startedBy = Prelude.Nothing,
      id = Prelude.Nothing,
      launchType = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      serviceArn = Prelude.Nothing,
      taskDefinition = Prelude.Nothing,
      externalId = Prelude.Nothing,
      networkConfiguration = Prelude.Nothing,
      serviceRegistries = Prelude.Nothing,
      capacityProviderStrategy = Prelude.Nothing,
      stabilityStatus = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The number of tasks in the task set that are in the @RUNNING@ status
-- during a deployment. A task in the @RUNNING@ state is running and ready
-- for use.
taskSet_runningCount :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Int)
taskSet_runningCount = Lens.lens (\TaskSet' {runningCount} -> runningCount) (\s@TaskSet' {} a -> s {runningCount = a} :: TaskSet)

-- | The status of the task set. The following describes each state:
--
-- [PRIMARY]
--     The task set is serving production traffic.
--
-- [ACTIVE]
--     The task set is not serving production traffic.
--
-- [DRAINING]
--     The tasks in the task set are being stopped and their corresponding
--     targets are being deregistered from their target group.
taskSet_status :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_status = Lens.lens (\TaskSet' {status} -> status) (\s@TaskSet' {} a -> s {status = a} :: TaskSet)

-- | The Amazon Resource Name (ARN) of the cluster that the service that
-- hosts the task set exists in.
taskSet_clusterArn :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_clusterArn = Lens.lens (\TaskSet' {clusterArn} -> clusterArn) (\s@TaskSet' {} a -> s {clusterArn = a} :: TaskSet)

-- | The computed desired count for the task set. This is calculated by
-- multiplying the service\'s @desiredCount@ by the task set\'s @scale@
-- percentage. The result is always rounded up. For example, if the
-- computed desired count is 1.2, it rounds up to 2 tasks.
taskSet_computedDesiredCount :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Int)
taskSet_computedDesiredCount = Lens.lens (\TaskSet' {computedDesiredCount} -> computedDesiredCount) (\s@TaskSet' {} a -> s {computedDesiredCount = a} :: TaskSet)

-- | The Unix timestamp for when the task set was created.
taskSet_createdAt :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.UTCTime)
taskSet_createdAt = Lens.lens (\TaskSet' {createdAt} -> createdAt) (\s@TaskSet' {} a -> s {createdAt = a} :: TaskSet) Prelude.. Lens.mapping Core._Time

-- | The Fargate platform version on which the tasks in the task set are
-- running. A platform version is only specified for tasks run on Fargate.
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/platform_versions.html Fargate platform versions>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskSet_platformVersion :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_platformVersion = Lens.lens (\TaskSet' {platformVersion} -> platformVersion) (\s@TaskSet' {} a -> s {platformVersion = a} :: TaskSet)

-- | A floating-point percentage of the desired number of tasks to place and
-- keep running in the task set.
taskSet_scale :: Lens.Lens' TaskSet (Prelude.Maybe Scale)
taskSet_scale = Lens.lens (\TaskSet' {scale} -> scale) (\s@TaskSet' {} a -> s {scale = a} :: TaskSet)

-- | Details on a load balancer that is used with a task set.
taskSet_loadBalancers :: Lens.Lens' TaskSet (Prelude.Maybe [LoadBalancer])
taskSet_loadBalancers = Lens.lens (\TaskSet' {loadBalancers} -> loadBalancers) (\s@TaskSet' {} a -> s {loadBalancers = a} :: TaskSet) Prelude.. Lens.mapping Lens.coerced

-- | The Unix timestamp for when the task set stability status was retrieved.
taskSet_stabilityStatusAt :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.UTCTime)
taskSet_stabilityStatusAt = Lens.lens (\TaskSet' {stabilityStatusAt} -> stabilityStatusAt) (\s@TaskSet' {} a -> s {stabilityStatusAt = a} :: TaskSet) Prelude.. Lens.mapping Core._Time

-- | The number of tasks in the task set that are in the @PENDING@ status
-- during a deployment. A task in the @PENDING@ state is preparing to enter
-- the @RUNNING@ state. A task set enters the @PENDING@ status when it
-- launches for the first time or when it is restarted after being in the
-- @STOPPED@ state.
taskSet_pendingCount :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Int)
taskSet_pendingCount = Lens.lens (\TaskSet' {pendingCount} -> pendingCount) (\s@TaskSet' {} a -> s {pendingCount = a} :: TaskSet)

-- | The Amazon Resource Name (ARN) of the task set.
taskSet_taskSetArn :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_taskSetArn = Lens.lens (\TaskSet' {taskSetArn} -> taskSetArn) (\s@TaskSet' {} a -> s {taskSetArn = a} :: TaskSet)

-- | The tag specified when a task set is started. If the task set is created
-- by an CodeDeploy deployment, the @startedBy@ parameter is @CODE_DEPLOY@.
-- For a task set created for an external deployment, the startedBy field
-- isn\'t used.
taskSet_startedBy :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_startedBy = Lens.lens (\TaskSet' {startedBy} -> startedBy) (\s@TaskSet' {} a -> s {startedBy = a} :: TaskSet)

-- | The ID of the task set.
taskSet_id :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_id = Lens.lens (\TaskSet' {id} -> id) (\s@TaskSet' {} a -> s {id = a} :: TaskSet)

-- | The launch type the tasks in the task set are using. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/launch_types.html Amazon ECS launch types>
-- in the /Amazon Elastic Container Service Developer Guide/.
taskSet_launchType :: Lens.Lens' TaskSet (Prelude.Maybe LaunchType)
taskSet_launchType = Lens.lens (\TaskSet' {launchType} -> launchType) (\s@TaskSet' {} a -> s {launchType = a} :: TaskSet)

-- | The Unix timestamp for when the task set was last updated.
taskSet_updatedAt :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.UTCTime)
taskSet_updatedAt = Lens.lens (\TaskSet' {updatedAt} -> updatedAt) (\s@TaskSet' {} a -> s {updatedAt = a} :: TaskSet) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the service the task set exists in.
taskSet_serviceArn :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_serviceArn = Lens.lens (\TaskSet' {serviceArn} -> serviceArn) (\s@TaskSet' {} a -> s {serviceArn = a} :: TaskSet)

-- | The task definition the task set is using.
taskSet_taskDefinition :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_taskDefinition = Lens.lens (\TaskSet' {taskDefinition} -> taskDefinition) (\s@TaskSet' {} a -> s {taskDefinition = a} :: TaskSet)

-- | The external ID associated with the task set.
--
-- If a task set is created by an CodeDeploy deployment, the @externalId@
-- parameter contains the CodeDeploy deployment ID.
--
-- If a task set is created for an external deployment and is associated
-- with a service discovery registry, the @externalId@ parameter contains
-- the @ECS_TASK_SET_EXTERNAL_ID@ Cloud Map attribute.
taskSet_externalId :: Lens.Lens' TaskSet (Prelude.Maybe Prelude.Text)
taskSet_externalId = Lens.lens (\TaskSet' {externalId} -> externalId) (\s@TaskSet' {} a -> s {externalId = a} :: TaskSet)

-- | The network configuration for the task set.
taskSet_networkConfiguration :: Lens.Lens' TaskSet (Prelude.Maybe NetworkConfiguration)
taskSet_networkConfiguration = Lens.lens (\TaskSet' {networkConfiguration} -> networkConfiguration) (\s@TaskSet' {} a -> s {networkConfiguration = a} :: TaskSet)

-- | The details of the service discovery registries to assign to this task
-- set. For more information, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/service-discovery.html Service discovery>.
taskSet_serviceRegistries :: Lens.Lens' TaskSet (Prelude.Maybe [ServiceRegistry])
taskSet_serviceRegistries = Lens.lens (\TaskSet' {serviceRegistries} -> serviceRegistries) (\s@TaskSet' {} a -> s {serviceRegistries = a} :: TaskSet) Prelude.. Lens.mapping Lens.coerced

-- | The capacity provider strategy associated with the task set.
taskSet_capacityProviderStrategy :: Lens.Lens' TaskSet (Prelude.Maybe [CapacityProviderStrategyItem])
taskSet_capacityProviderStrategy = Lens.lens (\TaskSet' {capacityProviderStrategy} -> capacityProviderStrategy) (\s@TaskSet' {} a -> s {capacityProviderStrategy = a} :: TaskSet) Prelude.. Lens.mapping Lens.coerced

-- | The stability status, which indicates whether the task set has reached a
-- steady state. If the following conditions are met, the task set will be
-- in @STEADY_STATE@:
--
-- -   The task @runningCount@ is equal to the @computedDesiredCount@.
--
-- -   The @pendingCount@ is @0@.
--
-- -   There are no tasks running on container instances in the @DRAINING@
--     status.
--
-- -   All tasks are reporting a healthy status from the load balancers,
--     service discovery, and container health checks.
--
-- If any of those conditions are not met, the stability status returns
-- @STABILIZING@.
taskSet_stabilityStatus :: Lens.Lens' TaskSet (Prelude.Maybe StabilityStatus)
taskSet_stabilityStatus = Lens.lens (\TaskSet' {stabilityStatus} -> stabilityStatus) (\s@TaskSet' {} a -> s {stabilityStatus = a} :: TaskSet)

-- | The metadata that you apply to the task set to help you categorize and
-- organize them. Each tag consists of a key and an optional value, both of
-- which you define.
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
taskSet_tags :: Lens.Lens' TaskSet (Prelude.Maybe [Tag])
taskSet_tags = Lens.lens (\TaskSet' {tags} -> tags) (\s@TaskSet' {} a -> s {tags = a} :: TaskSet) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON TaskSet where
  parseJSON =
    Core.withObject
      "TaskSet"
      ( \x ->
          TaskSet'
            Prelude.<$> (x Core..:? "runningCount")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "clusterArn")
            Prelude.<*> (x Core..:? "computedDesiredCount")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "platformVersion")
            Prelude.<*> (x Core..:? "scale")
            Prelude.<*> (x Core..:? "loadBalancers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "stabilityStatusAt")
            Prelude.<*> (x Core..:? "pendingCount")
            Prelude.<*> (x Core..:? "taskSetArn")
            Prelude.<*> (x Core..:? "startedBy")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "launchType")
            Prelude.<*> (x Core..:? "updatedAt")
            Prelude.<*> (x Core..:? "serviceArn")
            Prelude.<*> (x Core..:? "taskDefinition")
            Prelude.<*> (x Core..:? "externalId")
            Prelude.<*> (x Core..:? "networkConfiguration")
            Prelude.<*> ( x Core..:? "serviceRegistries"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "capacityProviderStrategy"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "stabilityStatus")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable TaskSet where
  hashWithSalt salt' TaskSet' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` stabilityStatus
      `Prelude.hashWithSalt` capacityProviderStrategy
      `Prelude.hashWithSalt` serviceRegistries
      `Prelude.hashWithSalt` networkConfiguration
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` taskDefinition
      `Prelude.hashWithSalt` serviceArn
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` launchType
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` startedBy
      `Prelude.hashWithSalt` taskSetArn
      `Prelude.hashWithSalt` pendingCount
      `Prelude.hashWithSalt` stabilityStatusAt
      `Prelude.hashWithSalt` loadBalancers
      `Prelude.hashWithSalt` scale
      `Prelude.hashWithSalt` platformVersion
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` computedDesiredCount
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` runningCount

instance Prelude.NFData TaskSet where
  rnf TaskSet' {..} =
    Prelude.rnf runningCount
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf stabilityStatus
      `Prelude.seq` Prelude.rnf capacityProviderStrategy
      `Prelude.seq` Prelude.rnf serviceRegistries
      `Prelude.seq` Prelude.rnf networkConfiguration
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf taskDefinition
      `Prelude.seq` Prelude.rnf serviceArn
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf launchType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf startedBy
      `Prelude.seq` Prelude.rnf taskSetArn
      `Prelude.seq` Prelude.rnf pendingCount
      `Prelude.seq` Prelude.rnf stabilityStatusAt
      `Prelude.seq` Prelude.rnf loadBalancers
      `Prelude.seq` Prelude.rnf scale
      `Prelude.seq` Prelude.rnf platformVersion
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf computedDesiredCount
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf status
