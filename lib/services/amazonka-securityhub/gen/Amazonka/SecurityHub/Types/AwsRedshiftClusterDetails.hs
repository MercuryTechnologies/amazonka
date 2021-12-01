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
-- Module      : Amazonka.SecurityHub.Types.AwsRedshiftClusterDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsRedshiftClusterDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterNode
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterParameterGroup
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSecurityGroup
import Amazonka.SecurityHub.Types.AwsRedshiftClusterClusterSnapshotCopyStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterDeferredMaintenanceWindow
import Amazonka.SecurityHub.Types.AwsRedshiftClusterElasticIpStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterEndpoint
import Amazonka.SecurityHub.Types.AwsRedshiftClusterHsmStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterIamRole
import Amazonka.SecurityHub.Types.AwsRedshiftClusterPendingModifiedValues
import Amazonka.SecurityHub.Types.AwsRedshiftClusterResizeInfo
import Amazonka.SecurityHub.Types.AwsRedshiftClusterRestoreStatus
import Amazonka.SecurityHub.Types.AwsRedshiftClusterVpcSecurityGroup

-- | Details about an Amazon Redshift cluster.
--
-- /See:/ 'newAwsRedshiftClusterDetails' smart constructor.
data AwsRedshiftClusterDetails = AwsRedshiftClusterDetails'
  { -- | Information about the resize operation for the cluster.
    resizeInfo :: Prelude.Maybe AwsRedshiftClusterResizeInfo,
    -- | Information about the status of a cluster restore action. Only applies
    -- to a cluster that was created by restoring a snapshot.
    restoreStatus :: Prelude.Maybe AwsRedshiftClusterRestoreStatus,
    -- | The default number of days to retain a manual snapshot.
    --
    -- If the value is -1, the snapshot is retained indefinitely.
    --
    -- This setting doesn\'t change the retention period of existing snapshots.
    --
    -- Valid values: Either -1 or an integer between 1 and 3,653
    manualSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether to create the cluster with enhanced VPC routing
    -- enabled.
    enhancedVpcRouting :: Prelude.Maybe Prelude.Bool,
    -- | Information about the destination Region and retention period for the
    -- cross-Region snapshot copy.
    clusterSnapshotCopyStatus :: Prelude.Maybe AwsRedshiftClusterClusterSnapshotCopyStatus,
    -- | The availability status of the cluster for queries. Possible values are
    -- the following:
    --
    -- -   @Available@ - The cluster is available for queries.
    --
    -- -   @Unavailable@ - The cluster is not available for queries.
    --
    -- -   @Maintenance@ - The cluster is intermittently available for queries
    --     due to maintenance activities.
    --
    -- -   @Modifying@ -The cluster is intermittently available for queries due
    --     to changes that modify the cluster.
    --
    -- -   @Failed@ - The cluster failed and is not available for queries.
    clusterAvailabilityStatus :: Prelude.Maybe Prelude.Text,
    -- | The specific revision number of the database in the cluster.
    clusterRevisionNumber :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the cluster snapshot schedule.
    snapshotScheduleIdentifier :: Prelude.Maybe Prelude.Text,
    -- | Whether the cluster can be accessed from a public network.
    publiclyAccessible :: Prelude.Maybe Prelude.Bool,
    -- | The master user name for the cluster. This name is used to connect to
    -- the database that is specified in as the value of @DBName@.
    masterUsername :: Prelude.Maybe Prelude.Text,
    -- | The name of the maintenance track for the cluster.
    maintenanceTrackName :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the next snapshot is expected to be taken. The cluster
    -- must have a valid snapshot schedule and have backups enabled.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    expectedNextSnapshotScheduleTime :: Prelude.Maybe Prelude.Text,
    -- | The number of nodes that you can use the elastic resize method to resize
    -- the cluster to.
    elasticResizeNumberOfNodeOptions :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the VPC that the cluster is in, if the cluster is in a
    -- VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | A list of cluster security groups that are associated with the cluster.
    clusterSecurityGroups :: Prelude.Maybe [AwsRedshiftClusterClusterSecurityGroup],
    -- | The number of days that automatic cluster snapshots are retained.
    automatedSnapshotRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | The current state of the cluster snapshot schedule.
    --
    -- Valid values: @MODIFYING@ | @ACTIVE@ | @FAILED@
    snapshotScheduleState :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the data in the cluster is encrypted at rest.
    encrypted :: Prelude.Maybe Prelude.Bool,
    -- | The name of the subnet group that is associated with the cluster. This
    -- parameter is valid only when the cluster is in a VPC.
    clusterSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The status of the next expected snapshot.
    --
    -- Valid values: @OnTrack@ | @Pending@
    expectedNextSnapshotScheduleTimeStatus :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the cluster.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | List of time windows during which maintenance was deferred.
    deferredMaintenanceWindows :: Prelude.Maybe [AwsRedshiftClusterDeferredMaintenanceWindow],
    -- | The number of compute nodes in the cluster.
    numberOfNodes :: Prelude.Maybe Prelude.Int,
    -- | The public key for the cluster.
    clusterPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The weekly time range, in Universal Coordinated Time (UTC), during which
    -- system maintenance can occur.
    --
    -- Format: @ \<day>:HH:MM-\<day>:HH:MM@
    --
    -- For the day values, use @mon@ | @tue@ | @wed@ | @thu@ | @fri@ | @sat@ |
    -- @sun@
    --
    -- For example, @sun:09:32-sun:10:02@
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the KMS encryption key that is used to encrypt data in
    -- the cluster.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The list of cluster parameter groups that are associated with this
    -- cluster.
    clusterParameterGroups :: Prelude.Maybe [AwsRedshiftClusterClusterParameterGroup],
    -- | The name of the Availability Zone in which the cluster is located.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | The list of VPC security groups that the cluster belongs to, if the
    -- cluster is in a VPC.
    vpcSecurityGroups :: Prelude.Maybe [AwsRedshiftClusterVpcSecurityGroup],
    -- | Information about whether the Amazon Redshift cluster finished applying
    -- any changes to hardware security module (HSM) settings that were
    -- specified in a modify cluster command.
    hsmStatus :: Prelude.Maybe AwsRedshiftClusterHsmStatus,
    -- | A list of IAM roles that the cluster can use to access other Amazon Web
    -- Services services.
    iamRoles :: Prelude.Maybe [AwsRedshiftClusterIamRole],
    -- | A list of cluster operations that are waiting to start.
    pendingActions :: Prelude.Maybe [Prelude.Text],
    -- | Information about the status of the Elastic IP (EIP) address.
    elasticIpStatus :: Prelude.Maybe AwsRedshiftClusterElasticIpStatus,
    -- | The version ID of the Amazon Redshift engine that runs on the cluster.
    clusterVersion :: Prelude.Maybe Prelude.Text,
    -- | The node type for the nodes in the cluster.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | Indicates the start of the next maintenance window.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    nextMaintenanceWindowStartTime :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the cluster was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    clusterCreateTime :: Prelude.Maybe Prelude.Text,
    -- | The connection endpoint.
    endpoint :: Prelude.Maybe AwsRedshiftClusterEndpoint,
    -- | Indicates whether major version upgrades are applied automatically to
    -- the cluster during the maintenance window.
    allowVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The current status of the cluster.
    --
    -- Valid values: @available@ | @available, prep-for-resize@ |
    -- @available, resize-cleanup@ |@ cancelling-resize@ | @creating@ |
    -- @deleting@ | @final-snapshot@ | @hardware-failure@ | @incompatible-hsm@
    -- |@ incompatible-network@ | @incompatible-parameters@ |
    -- @incompatible-restore@ | @modifying@ | @paused@ | @rebooting@ |
    -- @renaming@ | @resizing@ | @rotating-keys@ | @storage-full@ |
    -- @updating-hsm@
    clusterStatus :: Prelude.Maybe Prelude.Text,
    -- | A list of changes to the cluster that are currently pending.
    pendingModifiedValues :: Prelude.Maybe AwsRedshiftClusterPendingModifiedValues,
    -- | The nodes in the cluster.
    clusterNodes :: Prelude.Maybe [AwsRedshiftClusterClusterNode],
    -- | The name of the initial database that was created when the cluster was
    -- created.
    --
    -- The same name is returned for the life of the cluster.
    --
    -- If an initial database is not specified, a database named @devdev@ is
    -- created by default.
    dbName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsRedshiftClusterDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resizeInfo', 'awsRedshiftClusterDetails_resizeInfo' - Information about the resize operation for the cluster.
--
-- 'restoreStatus', 'awsRedshiftClusterDetails_restoreStatus' - Information about the status of a cluster restore action. Only applies
-- to a cluster that was created by restoring a snapshot.
--
-- 'manualSnapshotRetentionPeriod', 'awsRedshiftClusterDetails_manualSnapshotRetentionPeriod' - The default number of days to retain a manual snapshot.
--
-- If the value is -1, the snapshot is retained indefinitely.
--
-- This setting doesn\'t change the retention period of existing snapshots.
--
-- Valid values: Either -1 or an integer between 1 and 3,653
--
-- 'enhancedVpcRouting', 'awsRedshiftClusterDetails_enhancedVpcRouting' - Indicates whether to create the cluster with enhanced VPC routing
-- enabled.
--
-- 'clusterSnapshotCopyStatus', 'awsRedshiftClusterDetails_clusterSnapshotCopyStatus' - Information about the destination Region and retention period for the
-- cross-Region snapshot copy.
--
-- 'clusterAvailabilityStatus', 'awsRedshiftClusterDetails_clusterAvailabilityStatus' - The availability status of the cluster for queries. Possible values are
-- the following:
--
-- -   @Available@ - The cluster is available for queries.
--
-- -   @Unavailable@ - The cluster is not available for queries.
--
-- -   @Maintenance@ - The cluster is intermittently available for queries
--     due to maintenance activities.
--
-- -   @Modifying@ -The cluster is intermittently available for queries due
--     to changes that modify the cluster.
--
-- -   @Failed@ - The cluster failed and is not available for queries.
--
-- 'clusterRevisionNumber', 'awsRedshiftClusterDetails_clusterRevisionNumber' - The specific revision number of the database in the cluster.
--
-- 'snapshotScheduleIdentifier', 'awsRedshiftClusterDetails_snapshotScheduleIdentifier' - A unique identifier for the cluster snapshot schedule.
--
-- 'publiclyAccessible', 'awsRedshiftClusterDetails_publiclyAccessible' - Whether the cluster can be accessed from a public network.
--
-- 'masterUsername', 'awsRedshiftClusterDetails_masterUsername' - The master user name for the cluster. This name is used to connect to
-- the database that is specified in as the value of @DBName@.
--
-- 'maintenanceTrackName', 'awsRedshiftClusterDetails_maintenanceTrackName' - The name of the maintenance track for the cluster.
--
-- 'expectedNextSnapshotScheduleTime', 'awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime' - Indicates when the next snapshot is expected to be taken. The cluster
-- must have a valid snapshot schedule and have backups enabled.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'elasticResizeNumberOfNodeOptions', 'awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions' - The number of nodes that you can use the elastic resize method to resize
-- the cluster to.
--
-- 'vpcId', 'awsRedshiftClusterDetails_vpcId' - The identifier of the VPC that the cluster is in, if the cluster is in a
-- VPC.
--
-- 'clusterSecurityGroups', 'awsRedshiftClusterDetails_clusterSecurityGroups' - A list of cluster security groups that are associated with the cluster.
--
-- 'automatedSnapshotRetentionPeriod', 'awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod' - The number of days that automatic cluster snapshots are retained.
--
-- 'snapshotScheduleState', 'awsRedshiftClusterDetails_snapshotScheduleState' - The current state of the cluster snapshot schedule.
--
-- Valid values: @MODIFYING@ | @ACTIVE@ | @FAILED@
--
-- 'encrypted', 'awsRedshiftClusterDetails_encrypted' - Indicates whether the data in the cluster is encrypted at rest.
--
-- 'clusterSubnetGroupName', 'awsRedshiftClusterDetails_clusterSubnetGroupName' - The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
--
-- 'expectedNextSnapshotScheduleTimeStatus', 'awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus' - The status of the next expected snapshot.
--
-- Valid values: @OnTrack@ | @Pending@
--
-- 'clusterIdentifier', 'awsRedshiftClusterDetails_clusterIdentifier' - The unique identifier of the cluster.
--
-- 'deferredMaintenanceWindows', 'awsRedshiftClusterDetails_deferredMaintenanceWindows' - List of time windows during which maintenance was deferred.
--
-- 'numberOfNodes', 'awsRedshiftClusterDetails_numberOfNodes' - The number of compute nodes in the cluster.
--
-- 'clusterPublicKey', 'awsRedshiftClusterDetails_clusterPublicKey' - The public key for the cluster.
--
-- 'preferredMaintenanceWindow', 'awsRedshiftClusterDetails_preferredMaintenanceWindow' - The weekly time range, in Universal Coordinated Time (UTC), during which
-- system maintenance can occur.
--
-- Format: @ \<day>:HH:MM-\<day>:HH:MM@
--
-- For the day values, use @mon@ | @tue@ | @wed@ | @thu@ | @fri@ | @sat@ |
-- @sun@
--
-- For example, @sun:09:32-sun:10:02@
--
-- 'kmsKeyId', 'awsRedshiftClusterDetails_kmsKeyId' - The identifier of the KMS encryption key that is used to encrypt data in
-- the cluster.
--
-- 'clusterParameterGroups', 'awsRedshiftClusterDetails_clusterParameterGroups' - The list of cluster parameter groups that are associated with this
-- cluster.
--
-- 'availabilityZone', 'awsRedshiftClusterDetails_availabilityZone' - The name of the Availability Zone in which the cluster is located.
--
-- 'vpcSecurityGroups', 'awsRedshiftClusterDetails_vpcSecurityGroups' - The list of VPC security groups that the cluster belongs to, if the
-- cluster is in a VPC.
--
-- 'hsmStatus', 'awsRedshiftClusterDetails_hsmStatus' - Information about whether the Amazon Redshift cluster finished applying
-- any changes to hardware security module (HSM) settings that were
-- specified in a modify cluster command.
--
-- 'iamRoles', 'awsRedshiftClusterDetails_iamRoles' - A list of IAM roles that the cluster can use to access other Amazon Web
-- Services services.
--
-- 'pendingActions', 'awsRedshiftClusterDetails_pendingActions' - A list of cluster operations that are waiting to start.
--
-- 'elasticIpStatus', 'awsRedshiftClusterDetails_elasticIpStatus' - Information about the status of the Elastic IP (EIP) address.
--
-- 'clusterVersion', 'awsRedshiftClusterDetails_clusterVersion' - The version ID of the Amazon Redshift engine that runs on the cluster.
--
-- 'nodeType', 'awsRedshiftClusterDetails_nodeType' - The node type for the nodes in the cluster.
--
-- 'nextMaintenanceWindowStartTime', 'awsRedshiftClusterDetails_nextMaintenanceWindowStartTime' - Indicates the start of the next maintenance window.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'clusterCreateTime', 'awsRedshiftClusterDetails_clusterCreateTime' - Indicates when the cluster was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'endpoint', 'awsRedshiftClusterDetails_endpoint' - The connection endpoint.
--
-- 'allowVersionUpgrade', 'awsRedshiftClusterDetails_allowVersionUpgrade' - Indicates whether major version upgrades are applied automatically to
-- the cluster during the maintenance window.
--
-- 'clusterStatus', 'awsRedshiftClusterDetails_clusterStatus' - The current status of the cluster.
--
-- Valid values: @available@ | @available, prep-for-resize@ |
-- @available, resize-cleanup@ |@ cancelling-resize@ | @creating@ |
-- @deleting@ | @final-snapshot@ | @hardware-failure@ | @incompatible-hsm@
-- |@ incompatible-network@ | @incompatible-parameters@ |
-- @incompatible-restore@ | @modifying@ | @paused@ | @rebooting@ |
-- @renaming@ | @resizing@ | @rotating-keys@ | @storage-full@ |
-- @updating-hsm@
--
-- 'pendingModifiedValues', 'awsRedshiftClusterDetails_pendingModifiedValues' - A list of changes to the cluster that are currently pending.
--
-- 'clusterNodes', 'awsRedshiftClusterDetails_clusterNodes' - The nodes in the cluster.
--
-- 'dbName', 'awsRedshiftClusterDetails_dbName' - The name of the initial database that was created when the cluster was
-- created.
--
-- The same name is returned for the life of the cluster.
--
-- If an initial database is not specified, a database named @devdev@ is
-- created by default.
newAwsRedshiftClusterDetails ::
  AwsRedshiftClusterDetails
newAwsRedshiftClusterDetails =
  AwsRedshiftClusterDetails'
    { resizeInfo =
        Prelude.Nothing,
      restoreStatus = Prelude.Nothing,
      manualSnapshotRetentionPeriod = Prelude.Nothing,
      enhancedVpcRouting = Prelude.Nothing,
      clusterSnapshotCopyStatus = Prelude.Nothing,
      clusterAvailabilityStatus = Prelude.Nothing,
      clusterRevisionNumber = Prelude.Nothing,
      snapshotScheduleIdentifier = Prelude.Nothing,
      publiclyAccessible = Prelude.Nothing,
      masterUsername = Prelude.Nothing,
      maintenanceTrackName = Prelude.Nothing,
      expectedNextSnapshotScheduleTime =
        Prelude.Nothing,
      elasticResizeNumberOfNodeOptions =
        Prelude.Nothing,
      vpcId = Prelude.Nothing,
      clusterSecurityGroups = Prelude.Nothing,
      automatedSnapshotRetentionPeriod =
        Prelude.Nothing,
      snapshotScheduleState = Prelude.Nothing,
      encrypted = Prelude.Nothing,
      clusterSubnetGroupName = Prelude.Nothing,
      expectedNextSnapshotScheduleTimeStatus =
        Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      deferredMaintenanceWindows = Prelude.Nothing,
      numberOfNodes = Prelude.Nothing,
      clusterPublicKey = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      clusterParameterGroups = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      vpcSecurityGroups = Prelude.Nothing,
      hsmStatus = Prelude.Nothing,
      iamRoles = Prelude.Nothing,
      pendingActions = Prelude.Nothing,
      elasticIpStatus = Prelude.Nothing,
      clusterVersion = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      nextMaintenanceWindowStartTime = Prelude.Nothing,
      clusterCreateTime = Prelude.Nothing,
      endpoint = Prelude.Nothing,
      allowVersionUpgrade = Prelude.Nothing,
      clusterStatus = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      clusterNodes = Prelude.Nothing,
      dbName = Prelude.Nothing
    }

-- | Information about the resize operation for the cluster.
awsRedshiftClusterDetails_resizeInfo :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterResizeInfo)
awsRedshiftClusterDetails_resizeInfo = Lens.lens (\AwsRedshiftClusterDetails' {resizeInfo} -> resizeInfo) (\s@AwsRedshiftClusterDetails' {} a -> s {resizeInfo = a} :: AwsRedshiftClusterDetails)

-- | Information about the status of a cluster restore action. Only applies
-- to a cluster that was created by restoring a snapshot.
awsRedshiftClusterDetails_restoreStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterRestoreStatus)
awsRedshiftClusterDetails_restoreStatus = Lens.lens (\AwsRedshiftClusterDetails' {restoreStatus} -> restoreStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {restoreStatus = a} :: AwsRedshiftClusterDetails)

-- | The default number of days to retain a manual snapshot.
--
-- If the value is -1, the snapshot is retained indefinitely.
--
-- This setting doesn\'t change the retention period of existing snapshots.
--
-- Valid values: Either -1 or an integer between 1 and 3,653
awsRedshiftClusterDetails_manualSnapshotRetentionPeriod :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Int)
awsRedshiftClusterDetails_manualSnapshotRetentionPeriod = Lens.lens (\AwsRedshiftClusterDetails' {manualSnapshotRetentionPeriod} -> manualSnapshotRetentionPeriod) (\s@AwsRedshiftClusterDetails' {} a -> s {manualSnapshotRetentionPeriod = a} :: AwsRedshiftClusterDetails)

-- | Indicates whether to create the cluster with enhanced VPC routing
-- enabled.
awsRedshiftClusterDetails_enhancedVpcRouting :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_enhancedVpcRouting = Lens.lens (\AwsRedshiftClusterDetails' {enhancedVpcRouting} -> enhancedVpcRouting) (\s@AwsRedshiftClusterDetails' {} a -> s {enhancedVpcRouting = a} :: AwsRedshiftClusterDetails)

-- | Information about the destination Region and retention period for the
-- cross-Region snapshot copy.
awsRedshiftClusterDetails_clusterSnapshotCopyStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterClusterSnapshotCopyStatus)
awsRedshiftClusterDetails_clusterSnapshotCopyStatus = Lens.lens (\AwsRedshiftClusterDetails' {clusterSnapshotCopyStatus} -> clusterSnapshotCopyStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterSnapshotCopyStatus = a} :: AwsRedshiftClusterDetails)

-- | The availability status of the cluster for queries. Possible values are
-- the following:
--
-- -   @Available@ - The cluster is available for queries.
--
-- -   @Unavailable@ - The cluster is not available for queries.
--
-- -   @Maintenance@ - The cluster is intermittently available for queries
--     due to maintenance activities.
--
-- -   @Modifying@ -The cluster is intermittently available for queries due
--     to changes that modify the cluster.
--
-- -   @Failed@ - The cluster failed and is not available for queries.
awsRedshiftClusterDetails_clusterAvailabilityStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterAvailabilityStatus = Lens.lens (\AwsRedshiftClusterDetails' {clusterAvailabilityStatus} -> clusterAvailabilityStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterAvailabilityStatus = a} :: AwsRedshiftClusterDetails)

-- | The specific revision number of the database in the cluster.
awsRedshiftClusterDetails_clusterRevisionNumber :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterRevisionNumber = Lens.lens (\AwsRedshiftClusterDetails' {clusterRevisionNumber} -> clusterRevisionNumber) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterRevisionNumber = a} :: AwsRedshiftClusterDetails)

-- | A unique identifier for the cluster snapshot schedule.
awsRedshiftClusterDetails_snapshotScheduleIdentifier :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_snapshotScheduleIdentifier = Lens.lens (\AwsRedshiftClusterDetails' {snapshotScheduleIdentifier} -> snapshotScheduleIdentifier) (\s@AwsRedshiftClusterDetails' {} a -> s {snapshotScheduleIdentifier = a} :: AwsRedshiftClusterDetails)

-- | Whether the cluster can be accessed from a public network.
awsRedshiftClusterDetails_publiclyAccessible :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_publiclyAccessible = Lens.lens (\AwsRedshiftClusterDetails' {publiclyAccessible} -> publiclyAccessible) (\s@AwsRedshiftClusterDetails' {} a -> s {publiclyAccessible = a} :: AwsRedshiftClusterDetails)

-- | The master user name for the cluster. This name is used to connect to
-- the database that is specified in as the value of @DBName@.
awsRedshiftClusterDetails_masterUsername :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_masterUsername = Lens.lens (\AwsRedshiftClusterDetails' {masterUsername} -> masterUsername) (\s@AwsRedshiftClusterDetails' {} a -> s {masterUsername = a} :: AwsRedshiftClusterDetails)

-- | The name of the maintenance track for the cluster.
awsRedshiftClusterDetails_maintenanceTrackName :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_maintenanceTrackName = Lens.lens (\AwsRedshiftClusterDetails' {maintenanceTrackName} -> maintenanceTrackName) (\s@AwsRedshiftClusterDetails' {} a -> s {maintenanceTrackName = a} :: AwsRedshiftClusterDetails)

-- | Indicates when the next snapshot is expected to be taken. The cluster
-- must have a valid snapshot schedule and have backups enabled.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTime = Lens.lens (\AwsRedshiftClusterDetails' {expectedNextSnapshotScheduleTime} -> expectedNextSnapshotScheduleTime) (\s@AwsRedshiftClusterDetails' {} a -> s {expectedNextSnapshotScheduleTime = a} :: AwsRedshiftClusterDetails)

-- | The number of nodes that you can use the elastic resize method to resize
-- the cluster to.
awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_elasticResizeNumberOfNodeOptions = Lens.lens (\AwsRedshiftClusterDetails' {elasticResizeNumberOfNodeOptions} -> elasticResizeNumberOfNodeOptions) (\s@AwsRedshiftClusterDetails' {} a -> s {elasticResizeNumberOfNodeOptions = a} :: AwsRedshiftClusterDetails)

-- | The identifier of the VPC that the cluster is in, if the cluster is in a
-- VPC.
awsRedshiftClusterDetails_vpcId :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_vpcId = Lens.lens (\AwsRedshiftClusterDetails' {vpcId} -> vpcId) (\s@AwsRedshiftClusterDetails' {} a -> s {vpcId = a} :: AwsRedshiftClusterDetails)

-- | A list of cluster security groups that are associated with the cluster.
awsRedshiftClusterDetails_clusterSecurityGroups :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterClusterSecurityGroup])
awsRedshiftClusterDetails_clusterSecurityGroups = Lens.lens (\AwsRedshiftClusterDetails' {clusterSecurityGroups} -> clusterSecurityGroups) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterSecurityGroups = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of days that automatic cluster snapshots are retained.
awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Int)
awsRedshiftClusterDetails_automatedSnapshotRetentionPeriod = Lens.lens (\AwsRedshiftClusterDetails' {automatedSnapshotRetentionPeriod} -> automatedSnapshotRetentionPeriod) (\s@AwsRedshiftClusterDetails' {} a -> s {automatedSnapshotRetentionPeriod = a} :: AwsRedshiftClusterDetails)

-- | The current state of the cluster snapshot schedule.
--
-- Valid values: @MODIFYING@ | @ACTIVE@ | @FAILED@
awsRedshiftClusterDetails_snapshotScheduleState :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_snapshotScheduleState = Lens.lens (\AwsRedshiftClusterDetails' {snapshotScheduleState} -> snapshotScheduleState) (\s@AwsRedshiftClusterDetails' {} a -> s {snapshotScheduleState = a} :: AwsRedshiftClusterDetails)

-- | Indicates whether the data in the cluster is encrypted at rest.
awsRedshiftClusterDetails_encrypted :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_encrypted = Lens.lens (\AwsRedshiftClusterDetails' {encrypted} -> encrypted) (\s@AwsRedshiftClusterDetails' {} a -> s {encrypted = a} :: AwsRedshiftClusterDetails)

-- | The name of the subnet group that is associated with the cluster. This
-- parameter is valid only when the cluster is in a VPC.
awsRedshiftClusterDetails_clusterSubnetGroupName :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterSubnetGroupName = Lens.lens (\AwsRedshiftClusterDetails' {clusterSubnetGroupName} -> clusterSubnetGroupName) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterSubnetGroupName = a} :: AwsRedshiftClusterDetails)

-- | The status of the next expected snapshot.
--
-- Valid values: @OnTrack@ | @Pending@
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_expectedNextSnapshotScheduleTimeStatus = Lens.lens (\AwsRedshiftClusterDetails' {expectedNextSnapshotScheduleTimeStatus} -> expectedNextSnapshotScheduleTimeStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {expectedNextSnapshotScheduleTimeStatus = a} :: AwsRedshiftClusterDetails)

-- | The unique identifier of the cluster.
awsRedshiftClusterDetails_clusterIdentifier :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterIdentifier = Lens.lens (\AwsRedshiftClusterDetails' {clusterIdentifier} -> clusterIdentifier) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterIdentifier = a} :: AwsRedshiftClusterDetails)

-- | List of time windows during which maintenance was deferred.
awsRedshiftClusterDetails_deferredMaintenanceWindows :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterDeferredMaintenanceWindow])
awsRedshiftClusterDetails_deferredMaintenanceWindows = Lens.lens (\AwsRedshiftClusterDetails' {deferredMaintenanceWindows} -> deferredMaintenanceWindows) (\s@AwsRedshiftClusterDetails' {} a -> s {deferredMaintenanceWindows = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The number of compute nodes in the cluster.
awsRedshiftClusterDetails_numberOfNodes :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Int)
awsRedshiftClusterDetails_numberOfNodes = Lens.lens (\AwsRedshiftClusterDetails' {numberOfNodes} -> numberOfNodes) (\s@AwsRedshiftClusterDetails' {} a -> s {numberOfNodes = a} :: AwsRedshiftClusterDetails)

-- | The public key for the cluster.
awsRedshiftClusterDetails_clusterPublicKey :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterPublicKey = Lens.lens (\AwsRedshiftClusterDetails' {clusterPublicKey} -> clusterPublicKey) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterPublicKey = a} :: AwsRedshiftClusterDetails)

-- | The weekly time range, in Universal Coordinated Time (UTC), during which
-- system maintenance can occur.
--
-- Format: @ \<day>:HH:MM-\<day>:HH:MM@
--
-- For the day values, use @mon@ | @tue@ | @wed@ | @thu@ | @fri@ | @sat@ |
-- @sun@
--
-- For example, @sun:09:32-sun:10:02@
awsRedshiftClusterDetails_preferredMaintenanceWindow :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_preferredMaintenanceWindow = Lens.lens (\AwsRedshiftClusterDetails' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@AwsRedshiftClusterDetails' {} a -> s {preferredMaintenanceWindow = a} :: AwsRedshiftClusterDetails)

-- | The identifier of the KMS encryption key that is used to encrypt data in
-- the cluster.
awsRedshiftClusterDetails_kmsKeyId :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_kmsKeyId = Lens.lens (\AwsRedshiftClusterDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsRedshiftClusterDetails' {} a -> s {kmsKeyId = a} :: AwsRedshiftClusterDetails)

-- | The list of cluster parameter groups that are associated with this
-- cluster.
awsRedshiftClusterDetails_clusterParameterGroups :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterClusterParameterGroup])
awsRedshiftClusterDetails_clusterParameterGroups = Lens.lens (\AwsRedshiftClusterDetails' {clusterParameterGroups} -> clusterParameterGroups) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterParameterGroups = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Availability Zone in which the cluster is located.
awsRedshiftClusterDetails_availabilityZone :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_availabilityZone = Lens.lens (\AwsRedshiftClusterDetails' {availabilityZone} -> availabilityZone) (\s@AwsRedshiftClusterDetails' {} a -> s {availabilityZone = a} :: AwsRedshiftClusterDetails)

-- | The list of VPC security groups that the cluster belongs to, if the
-- cluster is in a VPC.
awsRedshiftClusterDetails_vpcSecurityGroups :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterVpcSecurityGroup])
awsRedshiftClusterDetails_vpcSecurityGroups = Lens.lens (\AwsRedshiftClusterDetails' {vpcSecurityGroups} -> vpcSecurityGroups) (\s@AwsRedshiftClusterDetails' {} a -> s {vpcSecurityGroups = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about whether the Amazon Redshift cluster finished applying
-- any changes to hardware security module (HSM) settings that were
-- specified in a modify cluster command.
awsRedshiftClusterDetails_hsmStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterHsmStatus)
awsRedshiftClusterDetails_hsmStatus = Lens.lens (\AwsRedshiftClusterDetails' {hsmStatus} -> hsmStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {hsmStatus = a} :: AwsRedshiftClusterDetails)

-- | A list of IAM roles that the cluster can use to access other Amazon Web
-- Services services.
awsRedshiftClusterDetails_iamRoles :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterIamRole])
awsRedshiftClusterDetails_iamRoles = Lens.lens (\AwsRedshiftClusterDetails' {iamRoles} -> iamRoles) (\s@AwsRedshiftClusterDetails' {} a -> s {iamRoles = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | A list of cluster operations that are waiting to start.
awsRedshiftClusterDetails_pendingActions :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [Prelude.Text])
awsRedshiftClusterDetails_pendingActions = Lens.lens (\AwsRedshiftClusterDetails' {pendingActions} -> pendingActions) (\s@AwsRedshiftClusterDetails' {} a -> s {pendingActions = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | Information about the status of the Elastic IP (EIP) address.
awsRedshiftClusterDetails_elasticIpStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterElasticIpStatus)
awsRedshiftClusterDetails_elasticIpStatus = Lens.lens (\AwsRedshiftClusterDetails' {elasticIpStatus} -> elasticIpStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {elasticIpStatus = a} :: AwsRedshiftClusterDetails)

-- | The version ID of the Amazon Redshift engine that runs on the cluster.
awsRedshiftClusterDetails_clusterVersion :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterVersion = Lens.lens (\AwsRedshiftClusterDetails' {clusterVersion} -> clusterVersion) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterVersion = a} :: AwsRedshiftClusterDetails)

-- | The node type for the nodes in the cluster.
awsRedshiftClusterDetails_nodeType :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_nodeType = Lens.lens (\AwsRedshiftClusterDetails' {nodeType} -> nodeType) (\s@AwsRedshiftClusterDetails' {} a -> s {nodeType = a} :: AwsRedshiftClusterDetails)

-- | Indicates the start of the next maintenance window.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterDetails_nextMaintenanceWindowStartTime :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_nextMaintenanceWindowStartTime = Lens.lens (\AwsRedshiftClusterDetails' {nextMaintenanceWindowStartTime} -> nextMaintenanceWindowStartTime) (\s@AwsRedshiftClusterDetails' {} a -> s {nextMaintenanceWindowStartTime = a} :: AwsRedshiftClusterDetails)

-- | Indicates when the cluster was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsRedshiftClusterDetails_clusterCreateTime :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterCreateTime = Lens.lens (\AwsRedshiftClusterDetails' {clusterCreateTime} -> clusterCreateTime) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterCreateTime = a} :: AwsRedshiftClusterDetails)

-- | The connection endpoint.
awsRedshiftClusterDetails_endpoint :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterEndpoint)
awsRedshiftClusterDetails_endpoint = Lens.lens (\AwsRedshiftClusterDetails' {endpoint} -> endpoint) (\s@AwsRedshiftClusterDetails' {} a -> s {endpoint = a} :: AwsRedshiftClusterDetails)

-- | Indicates whether major version upgrades are applied automatically to
-- the cluster during the maintenance window.
awsRedshiftClusterDetails_allowVersionUpgrade :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Bool)
awsRedshiftClusterDetails_allowVersionUpgrade = Lens.lens (\AwsRedshiftClusterDetails' {allowVersionUpgrade} -> allowVersionUpgrade) (\s@AwsRedshiftClusterDetails' {} a -> s {allowVersionUpgrade = a} :: AwsRedshiftClusterDetails)

-- | The current status of the cluster.
--
-- Valid values: @available@ | @available, prep-for-resize@ |
-- @available, resize-cleanup@ |@ cancelling-resize@ | @creating@ |
-- @deleting@ | @final-snapshot@ | @hardware-failure@ | @incompatible-hsm@
-- |@ incompatible-network@ | @incompatible-parameters@ |
-- @incompatible-restore@ | @modifying@ | @paused@ | @rebooting@ |
-- @renaming@ | @resizing@ | @rotating-keys@ | @storage-full@ |
-- @updating-hsm@
awsRedshiftClusterDetails_clusterStatus :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_clusterStatus = Lens.lens (\AwsRedshiftClusterDetails' {clusterStatus} -> clusterStatus) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterStatus = a} :: AwsRedshiftClusterDetails)

-- | A list of changes to the cluster that are currently pending.
awsRedshiftClusterDetails_pendingModifiedValues :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe AwsRedshiftClusterPendingModifiedValues)
awsRedshiftClusterDetails_pendingModifiedValues = Lens.lens (\AwsRedshiftClusterDetails' {pendingModifiedValues} -> pendingModifiedValues) (\s@AwsRedshiftClusterDetails' {} a -> s {pendingModifiedValues = a} :: AwsRedshiftClusterDetails)

-- | The nodes in the cluster.
awsRedshiftClusterDetails_clusterNodes :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe [AwsRedshiftClusterClusterNode])
awsRedshiftClusterDetails_clusterNodes = Lens.lens (\AwsRedshiftClusterDetails' {clusterNodes} -> clusterNodes) (\s@AwsRedshiftClusterDetails' {} a -> s {clusterNodes = a} :: AwsRedshiftClusterDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the initial database that was created when the cluster was
-- created.
--
-- The same name is returned for the life of the cluster.
--
-- If an initial database is not specified, a database named @devdev@ is
-- created by default.
awsRedshiftClusterDetails_dbName :: Lens.Lens' AwsRedshiftClusterDetails (Prelude.Maybe Prelude.Text)
awsRedshiftClusterDetails_dbName = Lens.lens (\AwsRedshiftClusterDetails' {dbName} -> dbName) (\s@AwsRedshiftClusterDetails' {} a -> s {dbName = a} :: AwsRedshiftClusterDetails)

instance Core.FromJSON AwsRedshiftClusterDetails where
  parseJSON =
    Core.withObject
      "AwsRedshiftClusterDetails"
      ( \x ->
          AwsRedshiftClusterDetails'
            Prelude.<$> (x Core..:? "ResizeInfo")
            Prelude.<*> (x Core..:? "RestoreStatus")
            Prelude.<*> (x Core..:? "ManualSnapshotRetentionPeriod")
            Prelude.<*> (x Core..:? "EnhancedVpcRouting")
            Prelude.<*> (x Core..:? "ClusterSnapshotCopyStatus")
            Prelude.<*> (x Core..:? "ClusterAvailabilityStatus")
            Prelude.<*> (x Core..:? "ClusterRevisionNumber")
            Prelude.<*> (x Core..:? "SnapshotScheduleIdentifier")
            Prelude.<*> (x Core..:? "PubliclyAccessible")
            Prelude.<*> (x Core..:? "MasterUsername")
            Prelude.<*> (x Core..:? "MaintenanceTrackName")
            Prelude.<*> (x Core..:? "ExpectedNextSnapshotScheduleTime")
            Prelude.<*> (x Core..:? "ElasticResizeNumberOfNodeOptions")
            Prelude.<*> (x Core..:? "VpcId")
            Prelude.<*> ( x Core..:? "ClusterSecurityGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AutomatedSnapshotRetentionPeriod")
            Prelude.<*> (x Core..:? "SnapshotScheduleState")
            Prelude.<*> (x Core..:? "Encrypted")
            Prelude.<*> (x Core..:? "ClusterSubnetGroupName")
            Prelude.<*> (x Core..:? "ExpectedNextSnapshotScheduleTimeStatus")
            Prelude.<*> (x Core..:? "ClusterIdentifier")
            Prelude.<*> ( x Core..:? "DeferredMaintenanceWindows"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "NumberOfNodes")
            Prelude.<*> (x Core..:? "ClusterPublicKey")
            Prelude.<*> (x Core..:? "PreferredMaintenanceWindow")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> ( x Core..:? "ClusterParameterGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "AvailabilityZone")
            Prelude.<*> ( x Core..:? "VpcSecurityGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "HsmStatus")
            Prelude.<*> (x Core..:? "IamRoles" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "PendingActions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ElasticIpStatus")
            Prelude.<*> (x Core..:? "ClusterVersion")
            Prelude.<*> (x Core..:? "NodeType")
            Prelude.<*> (x Core..:? "NextMaintenanceWindowStartTime")
            Prelude.<*> (x Core..:? "ClusterCreateTime")
            Prelude.<*> (x Core..:? "Endpoint")
            Prelude.<*> (x Core..:? "AllowVersionUpgrade")
            Prelude.<*> (x Core..:? "ClusterStatus")
            Prelude.<*> (x Core..:? "PendingModifiedValues")
            Prelude.<*> (x Core..:? "ClusterNodes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DBName")
      )

instance Prelude.Hashable AwsRedshiftClusterDetails where
  hashWithSalt salt' AwsRedshiftClusterDetails' {..} =
    salt' `Prelude.hashWithSalt` dbName
      `Prelude.hashWithSalt` clusterNodes
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` clusterStatus
      `Prelude.hashWithSalt` allowVersionUpgrade
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` clusterCreateTime
      `Prelude.hashWithSalt` nextMaintenanceWindowStartTime
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` clusterVersion
      `Prelude.hashWithSalt` elasticIpStatus
      `Prelude.hashWithSalt` pendingActions
      `Prelude.hashWithSalt` iamRoles
      `Prelude.hashWithSalt` hsmStatus
      `Prelude.hashWithSalt` vpcSecurityGroups
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` clusterParameterGroups
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` clusterPublicKey
      `Prelude.hashWithSalt` numberOfNodes
      `Prelude.hashWithSalt` deferredMaintenanceWindows
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` expectedNextSnapshotScheduleTimeStatus
      `Prelude.hashWithSalt` clusterSubnetGroupName
      `Prelude.hashWithSalt` encrypted
      `Prelude.hashWithSalt` snapshotScheduleState
      `Prelude.hashWithSalt` automatedSnapshotRetentionPeriod
      `Prelude.hashWithSalt` clusterSecurityGroups
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` elasticResizeNumberOfNodeOptions
      `Prelude.hashWithSalt` expectedNextSnapshotScheduleTime
      `Prelude.hashWithSalt` maintenanceTrackName
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` publiclyAccessible
      `Prelude.hashWithSalt` snapshotScheduleIdentifier
      `Prelude.hashWithSalt` clusterRevisionNumber
      `Prelude.hashWithSalt` clusterAvailabilityStatus
      `Prelude.hashWithSalt` clusterSnapshotCopyStatus
      `Prelude.hashWithSalt` enhancedVpcRouting
      `Prelude.hashWithSalt` manualSnapshotRetentionPeriod
      `Prelude.hashWithSalt` restoreStatus
      `Prelude.hashWithSalt` resizeInfo

instance Prelude.NFData AwsRedshiftClusterDetails where
  rnf AwsRedshiftClusterDetails' {..} =
    Prelude.rnf resizeInfo
      `Prelude.seq` Prelude.rnf dbName
      `Prelude.seq` Prelude.rnf clusterNodes
      `Prelude.seq` Prelude.rnf pendingModifiedValues
      `Prelude.seq` Prelude.rnf clusterStatus
      `Prelude.seq` Prelude.rnf allowVersionUpgrade
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf clusterCreateTime
      `Prelude.seq` Prelude.rnf nextMaintenanceWindowStartTime
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf clusterVersion
      `Prelude.seq` Prelude.rnf elasticIpStatus
      `Prelude.seq` Prelude.rnf pendingActions
      `Prelude.seq` Prelude.rnf iamRoles
      `Prelude.seq` Prelude.rnf hsmStatus
      `Prelude.seq` Prelude.rnf vpcSecurityGroups
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf clusterParameterGroups
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf clusterPublicKey
      `Prelude.seq` Prelude.rnf numberOfNodes
      `Prelude.seq` Prelude.rnf deferredMaintenanceWindows
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf expectedNextSnapshotScheduleTimeStatus
      `Prelude.seq` Prelude.rnf clusterSubnetGroupName
      `Prelude.seq` Prelude.rnf encrypted
      `Prelude.seq` Prelude.rnf snapshotScheduleState
      `Prelude.seq` Prelude.rnf automatedSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf clusterSecurityGroups
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf elasticResizeNumberOfNodeOptions
      `Prelude.seq` Prelude.rnf expectedNextSnapshotScheduleTime
      `Prelude.seq` Prelude.rnf maintenanceTrackName
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf publiclyAccessible
      `Prelude.seq` Prelude.rnf snapshotScheduleIdentifier
      `Prelude.seq` Prelude.rnf clusterRevisionNumber
      `Prelude.seq` Prelude.rnf clusterAvailabilityStatus
      `Prelude.seq` Prelude.rnf clusterSnapshotCopyStatus
      `Prelude.seq` Prelude.rnf enhancedVpcRouting
      `Prelude.seq` Prelude.rnf manualSnapshotRetentionPeriod
      `Prelude.seq` Prelude.rnf restoreStatus

instance Core.ToJSON AwsRedshiftClusterDetails where
  toJSON AwsRedshiftClusterDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResizeInfo" Core..=) Prelude.<$> resizeInfo,
            ("RestoreStatus" Core..=) Prelude.<$> restoreStatus,
            ("ManualSnapshotRetentionPeriod" Core..=)
              Prelude.<$> manualSnapshotRetentionPeriod,
            ("EnhancedVpcRouting" Core..=)
              Prelude.<$> enhancedVpcRouting,
            ("ClusterSnapshotCopyStatus" Core..=)
              Prelude.<$> clusterSnapshotCopyStatus,
            ("ClusterAvailabilityStatus" Core..=)
              Prelude.<$> clusterAvailabilityStatus,
            ("ClusterRevisionNumber" Core..=)
              Prelude.<$> clusterRevisionNumber,
            ("SnapshotScheduleIdentifier" Core..=)
              Prelude.<$> snapshotScheduleIdentifier,
            ("PubliclyAccessible" Core..=)
              Prelude.<$> publiclyAccessible,
            ("MasterUsername" Core..=)
              Prelude.<$> masterUsername,
            ("MaintenanceTrackName" Core..=)
              Prelude.<$> maintenanceTrackName,
            ("ExpectedNextSnapshotScheduleTime" Core..=)
              Prelude.<$> expectedNextSnapshotScheduleTime,
            ("ElasticResizeNumberOfNodeOptions" Core..=)
              Prelude.<$> elasticResizeNumberOfNodeOptions,
            ("VpcId" Core..=) Prelude.<$> vpcId,
            ("ClusterSecurityGroups" Core..=)
              Prelude.<$> clusterSecurityGroups,
            ("AutomatedSnapshotRetentionPeriod" Core..=)
              Prelude.<$> automatedSnapshotRetentionPeriod,
            ("SnapshotScheduleState" Core..=)
              Prelude.<$> snapshotScheduleState,
            ("Encrypted" Core..=) Prelude.<$> encrypted,
            ("ClusterSubnetGroupName" Core..=)
              Prelude.<$> clusterSubnetGroupName,
            ("ExpectedNextSnapshotScheduleTimeStatus" Core..=)
              Prelude.<$> expectedNextSnapshotScheduleTimeStatus,
            ("ClusterIdentifier" Core..=)
              Prelude.<$> clusterIdentifier,
            ("DeferredMaintenanceWindows" Core..=)
              Prelude.<$> deferredMaintenanceWindows,
            ("NumberOfNodes" Core..=) Prelude.<$> numberOfNodes,
            ("ClusterPublicKey" Core..=)
              Prelude.<$> clusterPublicKey,
            ("PreferredMaintenanceWindow" Core..=)
              Prelude.<$> preferredMaintenanceWindow,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("ClusterParameterGroups" Core..=)
              Prelude.<$> clusterParameterGroups,
            ("AvailabilityZone" Core..=)
              Prelude.<$> availabilityZone,
            ("VpcSecurityGroups" Core..=)
              Prelude.<$> vpcSecurityGroups,
            ("HsmStatus" Core..=) Prelude.<$> hsmStatus,
            ("IamRoles" Core..=) Prelude.<$> iamRoles,
            ("PendingActions" Core..=)
              Prelude.<$> pendingActions,
            ("ElasticIpStatus" Core..=)
              Prelude.<$> elasticIpStatus,
            ("ClusterVersion" Core..=)
              Prelude.<$> clusterVersion,
            ("NodeType" Core..=) Prelude.<$> nodeType,
            ("NextMaintenanceWindowStartTime" Core..=)
              Prelude.<$> nextMaintenanceWindowStartTime,
            ("ClusterCreateTime" Core..=)
              Prelude.<$> clusterCreateTime,
            ("Endpoint" Core..=) Prelude.<$> endpoint,
            ("AllowVersionUpgrade" Core..=)
              Prelude.<$> allowVersionUpgrade,
            ("ClusterStatus" Core..=) Prelude.<$> clusterStatus,
            ("PendingModifiedValues" Core..=)
              Prelude.<$> pendingModifiedValues,
            ("ClusterNodes" Core..=) Prelude.<$> clusterNodes,
            ("DBName" Core..=) Prelude.<$> dbName
          ]
      )
