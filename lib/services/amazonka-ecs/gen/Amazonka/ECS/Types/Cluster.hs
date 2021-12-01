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
-- Module      : Amazonka.ECS.Types.Cluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.Cluster where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types.Attachment
import Amazonka.ECS.Types.CapacityProviderStrategyItem
import Amazonka.ECS.Types.ClusterConfiguration
import Amazonka.ECS.Types.ClusterSetting
import Amazonka.ECS.Types.KeyValuePair
import Amazonka.ECS.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A regional grouping of one or more container instances on which you can
-- run task requests. Each account receives a default cluster the first
-- time you use the Amazon ECS service, but you may also create other
-- clusters. Clusters may contain more than one instance type
-- simultaneously.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The status of the cluster. The following are the possible states that
    -- will be returned.
    --
    -- [ACTIVE]
    --     The cluster is ready to accept tasks and if applicable you can
    --     register container instances with the cluster.
    --
    -- [PROVISIONING]
    --     The cluster has capacity providers associated with it and the
    --     resources needed for the capacity provider are being created.
    --
    -- [DEPROVISIONING]
    --     The cluster has capacity providers associated with it and the
    --     resources needed for the capacity provider are being deleted.
    --
    -- [FAILED]
    --     The cluster has capacity providers associated with it and the
    --     resources needed for the capacity provider have failed to create.
    --
    -- [INACTIVE]
    --     The cluster has been deleted. Clusters with an @INACTIVE@ status may
    --     remain discoverable in your account for a period of time. However,
    --     this behavior is subject to change in the future, so you should not
    --     rely on @INACTIVE@ clusters persisting.
    status :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the cluster. The ARN
    -- contains the @arn:aws:ecs@ namespace, followed by the Region of the
    -- cluster, the Amazon Web Services account ID of the cluster owner, the
    -- @cluster@ namespace, and then the cluster name. For example,
    -- @arn:aws:ecs:region:012345678910:cluster\/test@.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The resources attached to a cluster. When using a capacity provider with
    -- a cluster, the Auto Scaling plan that is created will be returned as a
    -- cluster attachment.
    attachments :: Prelude.Maybe [Attachment],
    -- | The number of tasks in the cluster that are in the @RUNNING@ state.
    runningTasksCount :: Prelude.Maybe Prelude.Int,
    -- | The default capacity provider strategy for the cluster. When services or
    -- tasks are run in the cluster with no launch type or capacity provider
    -- strategy specified, the default capacity provider strategy is used.
    defaultCapacityProviderStrategy :: Prelude.Maybe [CapacityProviderStrategyItem],
    -- | The settings for the cluster. This parameter indicates whether
    -- CloudWatch Container Insights is enabled or disabled for a cluster.
    settings :: Prelude.Maybe [ClusterSetting],
    -- | The number of container instances registered into the cluster. This
    -- includes container instances in both @ACTIVE@ and @DRAINING@ status.
    registeredContainerInstancesCount :: Prelude.Maybe Prelude.Int,
    -- | The number of tasks in the cluster that are in the @PENDING@ state.
    pendingTasksCount :: Prelude.Maybe Prelude.Int,
    -- | A user-generated string that you use to identify your cluster.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | Additional information about your clusters that are separated by launch
    -- type, including:
    --
    -- -   runningEC2TasksCount
    --
    -- -   RunningFargateTasksCount
    --
    -- -   pendingEC2TasksCount
    --
    -- -   pendingFargateTasksCount
    --
    -- -   activeEC2ServiceCount
    --
    -- -   activeFargateServiceCount
    --
    -- -   drainingEC2ServiceCount
    --
    -- -   drainingFargateServiceCount
    statistics :: Prelude.Maybe [KeyValuePair],
    -- | The execute command configuration for the cluster.
    configuration :: Prelude.Maybe ClusterConfiguration,
    -- | The status of the capacity providers associated with the cluster. The
    -- following are the states that will be returned:
    --
    -- [UPDATE_IN_PROGRESS]
    --     The available capacity providers for the cluster are updating. This
    --     occurs when the Auto Scaling plan is provisioning or deprovisioning.
    --
    -- [UPDATE_COMPLETE]
    --     The capacity providers have successfully updated.
    --
    -- [UPDATE_FAILED]
    --     The capacity provider updates failed.
    attachmentsStatus :: Prelude.Maybe Prelude.Text,
    -- | The capacity providers associated with the cluster.
    capacityProviders :: Prelude.Maybe [Prelude.Text],
    -- | The number of services that are running on the cluster in an @ACTIVE@
    -- state. You can view these services with ListServices.
    activeServicesCount :: Prelude.Maybe Prelude.Int,
    -- | The metadata that you apply to the cluster to help you categorize and
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
-- Create a value of 'Cluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'cluster_status' - The status of the cluster. The following are the possible states that
-- will be returned.
--
-- [ACTIVE]
--     The cluster is ready to accept tasks and if applicable you can
--     register container instances with the cluster.
--
-- [PROVISIONING]
--     The cluster has capacity providers associated with it and the
--     resources needed for the capacity provider are being created.
--
-- [DEPROVISIONING]
--     The cluster has capacity providers associated with it and the
--     resources needed for the capacity provider are being deleted.
--
-- [FAILED]
--     The cluster has capacity providers associated with it and the
--     resources needed for the capacity provider have failed to create.
--
-- [INACTIVE]
--     The cluster has been deleted. Clusters with an @INACTIVE@ status may
--     remain discoverable in your account for a period of time. However,
--     this behavior is subject to change in the future, so you should not
--     rely on @INACTIVE@ clusters persisting.
--
-- 'clusterArn', 'cluster_clusterArn' - The Amazon Resource Name (ARN) that identifies the cluster. The ARN
-- contains the @arn:aws:ecs@ namespace, followed by the Region of the
-- cluster, the Amazon Web Services account ID of the cluster owner, the
-- @cluster@ namespace, and then the cluster name. For example,
-- @arn:aws:ecs:region:012345678910:cluster\/test@.
--
-- 'attachments', 'cluster_attachments' - The resources attached to a cluster. When using a capacity provider with
-- a cluster, the Auto Scaling plan that is created will be returned as a
-- cluster attachment.
--
-- 'runningTasksCount', 'cluster_runningTasksCount' - The number of tasks in the cluster that are in the @RUNNING@ state.
--
-- 'defaultCapacityProviderStrategy', 'cluster_defaultCapacityProviderStrategy' - The default capacity provider strategy for the cluster. When services or
-- tasks are run in the cluster with no launch type or capacity provider
-- strategy specified, the default capacity provider strategy is used.
--
-- 'settings', 'cluster_settings' - The settings for the cluster. This parameter indicates whether
-- CloudWatch Container Insights is enabled or disabled for a cluster.
--
-- 'registeredContainerInstancesCount', 'cluster_registeredContainerInstancesCount' - The number of container instances registered into the cluster. This
-- includes container instances in both @ACTIVE@ and @DRAINING@ status.
--
-- 'pendingTasksCount', 'cluster_pendingTasksCount' - The number of tasks in the cluster that are in the @PENDING@ state.
--
-- 'clusterName', 'cluster_clusterName' - A user-generated string that you use to identify your cluster.
--
-- 'statistics', 'cluster_statistics' - Additional information about your clusters that are separated by launch
-- type, including:
--
-- -   runningEC2TasksCount
--
-- -   RunningFargateTasksCount
--
-- -   pendingEC2TasksCount
--
-- -   pendingFargateTasksCount
--
-- -   activeEC2ServiceCount
--
-- -   activeFargateServiceCount
--
-- -   drainingEC2ServiceCount
--
-- -   drainingFargateServiceCount
--
-- 'configuration', 'cluster_configuration' - The execute command configuration for the cluster.
--
-- 'attachmentsStatus', 'cluster_attachmentsStatus' - The status of the capacity providers associated with the cluster. The
-- following are the states that will be returned:
--
-- [UPDATE_IN_PROGRESS]
--     The available capacity providers for the cluster are updating. This
--     occurs when the Auto Scaling plan is provisioning or deprovisioning.
--
-- [UPDATE_COMPLETE]
--     The capacity providers have successfully updated.
--
-- [UPDATE_FAILED]
--     The capacity provider updates failed.
--
-- 'capacityProviders', 'cluster_capacityProviders' - The capacity providers associated with the cluster.
--
-- 'activeServicesCount', 'cluster_activeServicesCount' - The number of services that are running on the cluster in an @ACTIVE@
-- state. You can view these services with ListServices.
--
-- 'tags', 'cluster_tags' - The metadata that you apply to the cluster to help you categorize and
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
newCluster ::
  Cluster
newCluster =
  Cluster'
    { status = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      attachments = Prelude.Nothing,
      runningTasksCount = Prelude.Nothing,
      defaultCapacityProviderStrategy = Prelude.Nothing,
      settings = Prelude.Nothing,
      registeredContainerInstancesCount = Prelude.Nothing,
      pendingTasksCount = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      statistics = Prelude.Nothing,
      configuration = Prelude.Nothing,
      attachmentsStatus = Prelude.Nothing,
      capacityProviders = Prelude.Nothing,
      activeServicesCount = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The status of the cluster. The following are the possible states that
-- will be returned.
--
-- [ACTIVE]
--     The cluster is ready to accept tasks and if applicable you can
--     register container instances with the cluster.
--
-- [PROVISIONING]
--     The cluster has capacity providers associated with it and the
--     resources needed for the capacity provider are being created.
--
-- [DEPROVISIONING]
--     The cluster has capacity providers associated with it and the
--     resources needed for the capacity provider are being deleted.
--
-- [FAILED]
--     The cluster has capacity providers associated with it and the
--     resources needed for the capacity provider have failed to create.
--
-- [INACTIVE]
--     The cluster has been deleted. Clusters with an @INACTIVE@ status may
--     remain discoverable in your account for a period of time. However,
--     this behavior is subject to change in the future, so you should not
--     rely on @INACTIVE@ clusters persisting.
cluster_status :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_status = Lens.lens (\Cluster' {status} -> status) (\s@Cluster' {} a -> s {status = a} :: Cluster)

-- | The Amazon Resource Name (ARN) that identifies the cluster. The ARN
-- contains the @arn:aws:ecs@ namespace, followed by the Region of the
-- cluster, the Amazon Web Services account ID of the cluster owner, the
-- @cluster@ namespace, and then the cluster name. For example,
-- @arn:aws:ecs:region:012345678910:cluster\/test@.
cluster_clusterArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_clusterArn = Lens.lens (\Cluster' {clusterArn} -> clusterArn) (\s@Cluster' {} a -> s {clusterArn = a} :: Cluster)

-- | The resources attached to a cluster. When using a capacity provider with
-- a cluster, the Auto Scaling plan that is created will be returned as a
-- cluster attachment.
cluster_attachments :: Lens.Lens' Cluster (Prelude.Maybe [Attachment])
cluster_attachments = Lens.lens (\Cluster' {attachments} -> attachments) (\s@Cluster' {} a -> s {attachments = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The number of tasks in the cluster that are in the @RUNNING@ state.
cluster_runningTasksCount :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_runningTasksCount = Lens.lens (\Cluster' {runningTasksCount} -> runningTasksCount) (\s@Cluster' {} a -> s {runningTasksCount = a} :: Cluster)

-- | The default capacity provider strategy for the cluster. When services or
-- tasks are run in the cluster with no launch type or capacity provider
-- strategy specified, the default capacity provider strategy is used.
cluster_defaultCapacityProviderStrategy :: Lens.Lens' Cluster (Prelude.Maybe [CapacityProviderStrategyItem])
cluster_defaultCapacityProviderStrategy = Lens.lens (\Cluster' {defaultCapacityProviderStrategy} -> defaultCapacityProviderStrategy) (\s@Cluster' {} a -> s {defaultCapacityProviderStrategy = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The settings for the cluster. This parameter indicates whether
-- CloudWatch Container Insights is enabled or disabled for a cluster.
cluster_settings :: Lens.Lens' Cluster (Prelude.Maybe [ClusterSetting])
cluster_settings = Lens.lens (\Cluster' {settings} -> settings) (\s@Cluster' {} a -> s {settings = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The number of container instances registered into the cluster. This
-- includes container instances in both @ACTIVE@ and @DRAINING@ status.
cluster_registeredContainerInstancesCount :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_registeredContainerInstancesCount = Lens.lens (\Cluster' {registeredContainerInstancesCount} -> registeredContainerInstancesCount) (\s@Cluster' {} a -> s {registeredContainerInstancesCount = a} :: Cluster)

-- | The number of tasks in the cluster that are in the @PENDING@ state.
cluster_pendingTasksCount :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_pendingTasksCount = Lens.lens (\Cluster' {pendingTasksCount} -> pendingTasksCount) (\s@Cluster' {} a -> s {pendingTasksCount = a} :: Cluster)

-- | A user-generated string that you use to identify your cluster.
cluster_clusterName :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_clusterName = Lens.lens (\Cluster' {clusterName} -> clusterName) (\s@Cluster' {} a -> s {clusterName = a} :: Cluster)

-- | Additional information about your clusters that are separated by launch
-- type, including:
--
-- -   runningEC2TasksCount
--
-- -   RunningFargateTasksCount
--
-- -   pendingEC2TasksCount
--
-- -   pendingFargateTasksCount
--
-- -   activeEC2ServiceCount
--
-- -   activeFargateServiceCount
--
-- -   drainingEC2ServiceCount
--
-- -   drainingFargateServiceCount
cluster_statistics :: Lens.Lens' Cluster (Prelude.Maybe [KeyValuePair])
cluster_statistics = Lens.lens (\Cluster' {statistics} -> statistics) (\s@Cluster' {} a -> s {statistics = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The execute command configuration for the cluster.
cluster_configuration :: Lens.Lens' Cluster (Prelude.Maybe ClusterConfiguration)
cluster_configuration = Lens.lens (\Cluster' {configuration} -> configuration) (\s@Cluster' {} a -> s {configuration = a} :: Cluster)

-- | The status of the capacity providers associated with the cluster. The
-- following are the states that will be returned:
--
-- [UPDATE_IN_PROGRESS]
--     The available capacity providers for the cluster are updating. This
--     occurs when the Auto Scaling plan is provisioning or deprovisioning.
--
-- [UPDATE_COMPLETE]
--     The capacity providers have successfully updated.
--
-- [UPDATE_FAILED]
--     The capacity provider updates failed.
cluster_attachmentsStatus :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_attachmentsStatus = Lens.lens (\Cluster' {attachmentsStatus} -> attachmentsStatus) (\s@Cluster' {} a -> s {attachmentsStatus = a} :: Cluster)

-- | The capacity providers associated with the cluster.
cluster_capacityProviders :: Lens.Lens' Cluster (Prelude.Maybe [Prelude.Text])
cluster_capacityProviders = Lens.lens (\Cluster' {capacityProviders} -> capacityProviders) (\s@Cluster' {} a -> s {capacityProviders = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The number of services that are running on the cluster in an @ACTIVE@
-- state. You can view these services with ListServices.
cluster_activeServicesCount :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_activeServicesCount = Lens.lens (\Cluster' {activeServicesCount} -> activeServicesCount) (\s@Cluster' {} a -> s {activeServicesCount = a} :: Cluster)

-- | The metadata that you apply to the cluster to help you categorize and
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
cluster_tags :: Lens.Lens' Cluster (Prelude.Maybe [Tag])
cluster_tags = Lens.lens (\Cluster' {tags} -> tags) (\s@Cluster' {} a -> s {tags = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "clusterArn")
            Prelude.<*> (x Core..:? "attachments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "runningTasksCount")
            Prelude.<*> ( x Core..:? "defaultCapacityProviderStrategy"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "settings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "registeredContainerInstancesCount")
            Prelude.<*> (x Core..:? "pendingTasksCount")
            Prelude.<*> (x Core..:? "clusterName")
            Prelude.<*> (x Core..:? "statistics" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "configuration")
            Prelude.<*> (x Core..:? "attachmentsStatus")
            Prelude.<*> ( x Core..:? "capacityProviders"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "activeServicesCount")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Cluster where
  hashWithSalt salt' Cluster' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` activeServicesCount
      `Prelude.hashWithSalt` capacityProviders
      `Prelude.hashWithSalt` attachmentsStatus
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` statistics
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` pendingTasksCount
      `Prelude.hashWithSalt` registeredContainerInstancesCount
      `Prelude.hashWithSalt` settings
      `Prelude.hashWithSalt` defaultCapacityProviderStrategy
      `Prelude.hashWithSalt` runningTasksCount
      `Prelude.hashWithSalt` attachments
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData Cluster where
  rnf Cluster' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf activeServicesCount
      `Prelude.seq` Prelude.rnf capacityProviders
      `Prelude.seq` Prelude.rnf attachmentsStatus
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf pendingTasksCount
      `Prelude.seq` Prelude.rnf registeredContainerInstancesCount
      `Prelude.seq` Prelude.rnf settings
      `Prelude.seq` Prelude.rnf defaultCapacityProviderStrategy
      `Prelude.seq` Prelude.rnf runningTasksCount
      `Prelude.seq` Prelude.rnf attachments
      `Prelude.seq` Prelude.rnf clusterArn
