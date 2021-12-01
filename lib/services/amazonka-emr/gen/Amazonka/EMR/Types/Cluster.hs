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
-- Module      : Amazonka.EMR.Types.Cluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.Cluster where

import qualified Amazonka.Core as Core
import Amazonka.EMR.Types.Application
import Amazonka.EMR.Types.ClusterStatus
import Amazonka.EMR.Types.Configuration
import Amazonka.EMR.Types.Ec2InstanceAttributes
import Amazonka.EMR.Types.InstanceCollectionType
import Amazonka.EMR.Types.KerberosAttributes
import Amazonka.EMR.Types.PlacementGroupConfig
import Amazonka.EMR.Types.RepoUpgradeOnBoot
import Amazonka.EMR.Types.ScaleDownBehavior
import Amazonka.EMR.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The detailed description of the cluster.
--
-- /See:/ 'newCluster' smart constructor.
data Cluster = Cluster'
  { -- | The KMS key used for encrypting log files. This attribute is only
    -- available with EMR version 5.30.0 and later, excluding EMR 6.0.0.
    logEncryptionKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The AMI version requested for this cluster.
    requestedAmiVersion :: Prelude.Maybe Prelude.Text,
    -- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
    -- that is used for each EC2 instance. Available in Amazon EMR version 4.x
    -- and later.
    ebsRootVolumeSize :: Prelude.Maybe Prelude.Int,
    -- | Provides information about the EC2 instances in a cluster grouped by
    -- category. For example, key name, subnet ID, IAM instance profile, and so
    -- on.
    ec2InstanceAttributes :: Prelude.Maybe Ec2InstanceAttributes,
    -- | The Amazon Resource Name (ARN) of the Outpost where the cluster is
    -- launched.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | An approximation of the cost of the cluster, represented in
    -- m1.small\/hours. This value is incremented one time for every hour an
    -- m1.small instance runs. Larger instances are weighted more, so an EC2
    -- instance that is roughly four times more expensive would result in the
    -- normalized instance hours being incremented by four. This result is only
    -- an approximation and does not reflect the actual billing rate.
    normalizedInstanceHours :: Prelude.Maybe Prelude.Int,
    -- | Applies only to Amazon EMR releases 4.x and later. The list of
    -- Configurations supplied to the EMR cluster.
    configurations :: Prelude.Maybe [Configuration],
    -- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom
    -- Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
    customAmiId :: Prelude.Maybe Prelude.Text,
    -- | An IAM role for automatic scaling policies. The default role is
    -- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
    -- the automatic scaling feature requires to launch and terminate EC2
    -- instances in an instance group.
    autoScalingRole :: Prelude.Maybe Prelude.Text,
    -- | The name of the security configuration applied to the cluster.
    securityConfiguration :: Prelude.Maybe Prelude.Text,
    -- | The way that individual Amazon EC2 instances terminate when an automatic
    -- scale-in activity occurs or an instance group is resized.
    -- @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes
    -- at the instance-hour boundary, regardless of when the request to
    -- terminate the instance was submitted. This option is only available with
    -- Amazon EMR 5.1.0 and later and is the default for clusters created using
    -- that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR
    -- adds nodes to a deny list and drains tasks from nodes before terminating
    -- the Amazon EC2 instances, regardless of the instance-hour boundary. With
    -- either behavior, Amazon EMR removes the least active nodes first and
    -- blocks instance termination if it could lead to HDFS corruption.
    -- @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version
    -- 4.1.0 and later, and is the default for versions of Amazon EMR earlier
    -- than 5.1.0.
    scaleDownBehavior :: Prelude.Maybe ScaleDownBehavior,
    -- | The instance fleet configuration is available only in Amazon EMR
    -- versions 4.8.0 and later, excluding 5.0.x versions.
    --
    -- The instance group configuration of the cluster. A value of
    -- @INSTANCE_GROUP@ indicates a uniform instance group configuration. A
    -- value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
    instanceCollectionType :: Prelude.Maybe InstanceCollectionType,
    -- | The Amazon EMR release label, which determines the version of
    -- open-source application packages installed on the cluster. Release
    -- labels are in the form @emr-x.x.x@, where x.x.x is an Amazon EMR release
    -- version such as @emr-5.14.0@. For more information about Amazon EMR
    -- release versions and included application versions and features, see
    -- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/>. The release
    -- label applies only to Amazon EMR releases version 4.0 and later. Earlier
    -- versions use @AmiVersion@.
    releaseLabel :: Prelude.Maybe Prelude.Text,
    -- | Applies only when @CustomAmiID@ is used. Specifies the type of updates
    -- that are applied from the Amazon Linux AMI package repositories when an
    -- instance boots using the AMI.
    repoUpgradeOnBoot :: Prelude.Maybe RepoUpgradeOnBoot,
    -- | The path to the Amazon S3 location where logs for this cluster are
    -- stored.
    logUri :: Prelude.Maybe Prelude.Text,
    -- | Attributes for Kerberos configuration when Kerberos authentication is
    -- enabled using a security configuration. For more information see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
    -- in the /Amazon EMR Management Guide/.
    kerberosAttributes :: Prelude.Maybe KerberosAttributes,
    -- | Placement group configured for an Amazon EMR cluster.
    placementGroups :: Prelude.Maybe [PlacementGroupConfig],
    -- | The AMI version running on this cluster.
    runningAmiVersion :: Prelude.Maybe Prelude.Text,
    -- | The DNS name of the master node. If the cluster is on a private subnet,
    -- this is the private DNS name. On a public subnet, this is the public DNS
    -- name.
    masterPublicDnsName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2
    -- instances from being terminated by an API call or user intervention, or
    -- in the event of a cluster error.
    terminationProtected :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the cluster is visible to IAM principals in the Amazon
    -- Web Services account associated with the cluster. When @true@, IAM
    -- principals in the Amazon Web Services account can perform EMR cluster
    -- actions on the cluster that their IAM policies allow. When @false@, only
    -- the IAM principal that created the cluster and the Amazon Web Services
    -- account root user can perform EMR actions, regardless of IAM permissions
    -- policies attached to other IAM principals.
    --
    -- The default value is @true@ if a value is not provided when creating a
    -- cluster using the EMR API RunJobFlow command, the CLI
    -- <https://docs.aws.amazon.com/cli/latest/reference/emr/create-cluster.html create-cluster>
    -- command, or the Amazon Web Services Management Console. IAM principals
    -- that are allowed to perform actions on the cluster can use the
    -- SetVisibleToAllUsers action to change the value on a running cluster.
    -- For more information, see
    -- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/security_iam_emr-with-iam.html#security_set_visible_to_all_users Understanding the EMR Cluster VisibleToAllUsers Setting>
    -- in the /Amazon EMRManagement Guide/.
    visibleToAllUsers :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the cluster should terminate after completing all
    -- steps.
    autoTerminate :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the number of steps that can be executed concurrently.
    stepConcurrencyLevel :: Prelude.Maybe Prelude.Int,
    -- | The applications installed on this cluster.
    applications :: Prelude.Maybe [Application],
    -- | A list of tags associated with a cluster.
    tags :: Prelude.Maybe [Tag],
    -- | The IAM role that Amazon EMR assumes in order to access Amazon Web
    -- Services resources on your behalf.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the cluster.
    id :: Prelude.Text,
    -- | The name of the cluster.
    name :: Prelude.Text,
    -- | The current status details about the cluster.
    status :: ClusterStatus
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
-- 'logEncryptionKmsKeyId', 'cluster_logEncryptionKmsKeyId' - The KMS key used for encrypting log files. This attribute is only
-- available with EMR version 5.30.0 and later, excluding EMR 6.0.0.
--
-- 'clusterArn', 'cluster_clusterArn' - The Amazon Resource Name of the cluster.
--
-- 'requestedAmiVersion', 'cluster_requestedAmiVersion' - The AMI version requested for this cluster.
--
-- 'ebsRootVolumeSize', 'cluster_ebsRootVolumeSize' - The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
-- that is used for each EC2 instance. Available in Amazon EMR version 4.x
-- and later.
--
-- 'ec2InstanceAttributes', 'cluster_ec2InstanceAttributes' - Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
--
-- 'outpostArn', 'cluster_outpostArn' - The Amazon Resource Name (ARN) of the Outpost where the cluster is
-- launched.
--
-- 'normalizedInstanceHours', 'cluster_normalizedInstanceHours' - An approximation of the cost of the cluster, represented in
-- m1.small\/hours. This value is incremented one time for every hour an
-- m1.small instance runs. Larger instances are weighted more, so an EC2
-- instance that is roughly four times more expensive would result in the
-- normalized instance hours being incremented by four. This result is only
-- an approximation and does not reflect the actual billing rate.
--
-- 'configurations', 'cluster_configurations' - Applies only to Amazon EMR releases 4.x and later. The list of
-- Configurations supplied to the EMR cluster.
--
-- 'customAmiId', 'cluster_customAmiId' - Available only in Amazon EMR version 5.7.0 and later. The ID of a custom
-- Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
--
-- 'autoScalingRole', 'cluster_autoScalingRole' - An IAM role for automatic scaling policies. The default role is
-- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
-- the automatic scaling feature requires to launch and terminate EC2
-- instances in an instance group.
--
-- 'securityConfiguration', 'cluster_securityConfiguration' - The name of the security configuration applied to the cluster.
--
-- 'scaleDownBehavior', 'cluster_scaleDownBehavior' - The way that individual Amazon EC2 instances terminate when an automatic
-- scale-in activity occurs or an instance group is resized.
-- @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes
-- at the instance-hour boundary, regardless of when the request to
-- terminate the instance was submitted. This option is only available with
-- Amazon EMR 5.1.0 and later and is the default for clusters created using
-- that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR
-- adds nodes to a deny list and drains tasks from nodes before terminating
-- the Amazon EC2 instances, regardless of the instance-hour boundary. With
-- either behavior, Amazon EMR removes the least active nodes first and
-- blocks instance termination if it could lead to HDFS corruption.
-- @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version
-- 4.1.0 and later, and is the default for versions of Amazon EMR earlier
-- than 5.1.0.
--
-- 'instanceCollectionType', 'cluster_instanceCollectionType' - The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- The instance group configuration of the cluster. A value of
-- @INSTANCE_GROUP@ indicates a uniform instance group configuration. A
-- value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
--
-- 'releaseLabel', 'cluster_releaseLabel' - The Amazon EMR release label, which determines the version of
-- open-source application packages installed on the cluster. Release
-- labels are in the form @emr-x.x.x@, where x.x.x is an Amazon EMR release
-- version such as @emr-5.14.0@. For more information about Amazon EMR
-- release versions and included application versions and features, see
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/>. The release
-- label applies only to Amazon EMR releases version 4.0 and later. Earlier
-- versions use @AmiVersion@.
--
-- 'repoUpgradeOnBoot', 'cluster_repoUpgradeOnBoot' - Applies only when @CustomAmiID@ is used. Specifies the type of updates
-- that are applied from the Amazon Linux AMI package repositories when an
-- instance boots using the AMI.
--
-- 'logUri', 'cluster_logUri' - The path to the Amazon S3 location where logs for this cluster are
-- stored.
--
-- 'kerberosAttributes', 'cluster_kerberosAttributes' - Attributes for Kerberos configuration when Kerberos authentication is
-- enabled using a security configuration. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
-- in the /Amazon EMR Management Guide/.
--
-- 'placementGroups', 'cluster_placementGroups' - Placement group configured for an Amazon EMR cluster.
--
-- 'runningAmiVersion', 'cluster_runningAmiVersion' - The AMI version running on this cluster.
--
-- 'masterPublicDnsName', 'cluster_masterPublicDnsName' - The DNS name of the master node. If the cluster is on a private subnet,
-- this is the private DNS name. On a public subnet, this is the public DNS
-- name.
--
-- 'terminationProtected', 'cluster_terminationProtected' - Indicates whether Amazon EMR will lock the cluster to prevent the EC2
-- instances from being terminated by an API call or user intervention, or
-- in the event of a cluster error.
--
-- 'visibleToAllUsers', 'cluster_visibleToAllUsers' - Indicates whether the cluster is visible to IAM principals in the Amazon
-- Web Services account associated with the cluster. When @true@, IAM
-- principals in the Amazon Web Services account can perform EMR cluster
-- actions on the cluster that their IAM policies allow. When @false@, only
-- the IAM principal that created the cluster and the Amazon Web Services
-- account root user can perform EMR actions, regardless of IAM permissions
-- policies attached to other IAM principals.
--
-- The default value is @true@ if a value is not provided when creating a
-- cluster using the EMR API RunJobFlow command, the CLI
-- <https://docs.aws.amazon.com/cli/latest/reference/emr/create-cluster.html create-cluster>
-- command, or the Amazon Web Services Management Console. IAM principals
-- that are allowed to perform actions on the cluster can use the
-- SetVisibleToAllUsers action to change the value on a running cluster.
-- For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/security_iam_emr-with-iam.html#security_set_visible_to_all_users Understanding the EMR Cluster VisibleToAllUsers Setting>
-- in the /Amazon EMRManagement Guide/.
--
-- 'autoTerminate', 'cluster_autoTerminate' - Specifies whether the cluster should terminate after completing all
-- steps.
--
-- 'stepConcurrencyLevel', 'cluster_stepConcurrencyLevel' - Specifies the number of steps that can be executed concurrently.
--
-- 'applications', 'cluster_applications' - The applications installed on this cluster.
--
-- 'tags', 'cluster_tags' - A list of tags associated with a cluster.
--
-- 'serviceRole', 'cluster_serviceRole' - The IAM role that Amazon EMR assumes in order to access Amazon Web
-- Services resources on your behalf.
--
-- 'id', 'cluster_id' - The unique identifier for the cluster.
--
-- 'name', 'cluster_name' - The name of the cluster.
--
-- 'status', 'cluster_status' - The current status details about the cluster.
newCluster ::
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  ClusterStatus ->
  Cluster
newCluster pId_ pName_ pStatus_ =
  Cluster'
    { logEncryptionKmsKeyId = Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      requestedAmiVersion = Prelude.Nothing,
      ebsRootVolumeSize = Prelude.Nothing,
      ec2InstanceAttributes = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      normalizedInstanceHours = Prelude.Nothing,
      configurations = Prelude.Nothing,
      customAmiId = Prelude.Nothing,
      autoScalingRole = Prelude.Nothing,
      securityConfiguration = Prelude.Nothing,
      scaleDownBehavior = Prelude.Nothing,
      instanceCollectionType = Prelude.Nothing,
      releaseLabel = Prelude.Nothing,
      repoUpgradeOnBoot = Prelude.Nothing,
      logUri = Prelude.Nothing,
      kerberosAttributes = Prelude.Nothing,
      placementGroups = Prelude.Nothing,
      runningAmiVersion = Prelude.Nothing,
      masterPublicDnsName = Prelude.Nothing,
      terminationProtected = Prelude.Nothing,
      visibleToAllUsers = Prelude.Nothing,
      autoTerminate = Prelude.Nothing,
      stepConcurrencyLevel = Prelude.Nothing,
      applications = Prelude.Nothing,
      tags = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      id = pId_,
      name = pName_,
      status = pStatus_
    }

-- | The KMS key used for encrypting log files. This attribute is only
-- available with EMR version 5.30.0 and later, excluding EMR 6.0.0.
cluster_logEncryptionKmsKeyId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_logEncryptionKmsKeyId = Lens.lens (\Cluster' {logEncryptionKmsKeyId} -> logEncryptionKmsKeyId) (\s@Cluster' {} a -> s {logEncryptionKmsKeyId = a} :: Cluster)

-- | The Amazon Resource Name of the cluster.
cluster_clusterArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_clusterArn = Lens.lens (\Cluster' {clusterArn} -> clusterArn) (\s@Cluster' {} a -> s {clusterArn = a} :: Cluster)

-- | The AMI version requested for this cluster.
cluster_requestedAmiVersion :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_requestedAmiVersion = Lens.lens (\Cluster' {requestedAmiVersion} -> requestedAmiVersion) (\s@Cluster' {} a -> s {requestedAmiVersion = a} :: Cluster)

-- | The size, in GiB, of the Amazon EBS root device volume of the Linux AMI
-- that is used for each EC2 instance. Available in Amazon EMR version 4.x
-- and later.
cluster_ebsRootVolumeSize :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_ebsRootVolumeSize = Lens.lens (\Cluster' {ebsRootVolumeSize} -> ebsRootVolumeSize) (\s@Cluster' {} a -> s {ebsRootVolumeSize = a} :: Cluster)

-- | Provides information about the EC2 instances in a cluster grouped by
-- category. For example, key name, subnet ID, IAM instance profile, and so
-- on.
cluster_ec2InstanceAttributes :: Lens.Lens' Cluster (Prelude.Maybe Ec2InstanceAttributes)
cluster_ec2InstanceAttributes = Lens.lens (\Cluster' {ec2InstanceAttributes} -> ec2InstanceAttributes) (\s@Cluster' {} a -> s {ec2InstanceAttributes = a} :: Cluster)

-- | The Amazon Resource Name (ARN) of the Outpost where the cluster is
-- launched.
cluster_outpostArn :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_outpostArn = Lens.lens (\Cluster' {outpostArn} -> outpostArn) (\s@Cluster' {} a -> s {outpostArn = a} :: Cluster)

-- | An approximation of the cost of the cluster, represented in
-- m1.small\/hours. This value is incremented one time for every hour an
-- m1.small instance runs. Larger instances are weighted more, so an EC2
-- instance that is roughly four times more expensive would result in the
-- normalized instance hours being incremented by four. This result is only
-- an approximation and does not reflect the actual billing rate.
cluster_normalizedInstanceHours :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_normalizedInstanceHours = Lens.lens (\Cluster' {normalizedInstanceHours} -> normalizedInstanceHours) (\s@Cluster' {} a -> s {normalizedInstanceHours = a} :: Cluster)

-- | Applies only to Amazon EMR releases 4.x and later. The list of
-- Configurations supplied to the EMR cluster.
cluster_configurations :: Lens.Lens' Cluster (Prelude.Maybe [Configuration])
cluster_configurations = Lens.lens (\Cluster' {configurations} -> configurations) (\s@Cluster' {} a -> s {configurations = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | Available only in Amazon EMR version 5.7.0 and later. The ID of a custom
-- Amazon EBS-backed Linux AMI if the cluster uses a custom AMI.
cluster_customAmiId :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_customAmiId = Lens.lens (\Cluster' {customAmiId} -> customAmiId) (\s@Cluster' {} a -> s {customAmiId = a} :: Cluster)

-- | An IAM role for automatic scaling policies. The default role is
-- @EMR_AutoScaling_DefaultRole@. The IAM role provides permissions that
-- the automatic scaling feature requires to launch and terminate EC2
-- instances in an instance group.
cluster_autoScalingRole :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_autoScalingRole = Lens.lens (\Cluster' {autoScalingRole} -> autoScalingRole) (\s@Cluster' {} a -> s {autoScalingRole = a} :: Cluster)

-- | The name of the security configuration applied to the cluster.
cluster_securityConfiguration :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_securityConfiguration = Lens.lens (\Cluster' {securityConfiguration} -> securityConfiguration) (\s@Cluster' {} a -> s {securityConfiguration = a} :: Cluster)

-- | The way that individual Amazon EC2 instances terminate when an automatic
-- scale-in activity occurs or an instance group is resized.
-- @TERMINATE_AT_INSTANCE_HOUR@ indicates that Amazon EMR terminates nodes
-- at the instance-hour boundary, regardless of when the request to
-- terminate the instance was submitted. This option is only available with
-- Amazon EMR 5.1.0 and later and is the default for clusters created using
-- that version. @TERMINATE_AT_TASK_COMPLETION@ indicates that Amazon EMR
-- adds nodes to a deny list and drains tasks from nodes before terminating
-- the Amazon EC2 instances, regardless of the instance-hour boundary. With
-- either behavior, Amazon EMR removes the least active nodes first and
-- blocks instance termination if it could lead to HDFS corruption.
-- @TERMINATE_AT_TASK_COMPLETION@ is available only in Amazon EMR version
-- 4.1.0 and later, and is the default for versions of Amazon EMR earlier
-- than 5.1.0.
cluster_scaleDownBehavior :: Lens.Lens' Cluster (Prelude.Maybe ScaleDownBehavior)
cluster_scaleDownBehavior = Lens.lens (\Cluster' {scaleDownBehavior} -> scaleDownBehavior) (\s@Cluster' {} a -> s {scaleDownBehavior = a} :: Cluster)

-- | The instance fleet configuration is available only in Amazon EMR
-- versions 4.8.0 and later, excluding 5.0.x versions.
--
-- The instance group configuration of the cluster. A value of
-- @INSTANCE_GROUP@ indicates a uniform instance group configuration. A
-- value of @INSTANCE_FLEET@ indicates an instance fleets configuration.
cluster_instanceCollectionType :: Lens.Lens' Cluster (Prelude.Maybe InstanceCollectionType)
cluster_instanceCollectionType = Lens.lens (\Cluster' {instanceCollectionType} -> instanceCollectionType) (\s@Cluster' {} a -> s {instanceCollectionType = a} :: Cluster)

-- | The Amazon EMR release label, which determines the version of
-- open-source application packages installed on the cluster. Release
-- labels are in the form @emr-x.x.x@, where x.x.x is an Amazon EMR release
-- version such as @emr-5.14.0@. For more information about Amazon EMR
-- release versions and included application versions and features, see
-- <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/>. The release
-- label applies only to Amazon EMR releases version 4.0 and later. Earlier
-- versions use @AmiVersion@.
cluster_releaseLabel :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_releaseLabel = Lens.lens (\Cluster' {releaseLabel} -> releaseLabel) (\s@Cluster' {} a -> s {releaseLabel = a} :: Cluster)

-- | Applies only when @CustomAmiID@ is used. Specifies the type of updates
-- that are applied from the Amazon Linux AMI package repositories when an
-- instance boots using the AMI.
cluster_repoUpgradeOnBoot :: Lens.Lens' Cluster (Prelude.Maybe RepoUpgradeOnBoot)
cluster_repoUpgradeOnBoot = Lens.lens (\Cluster' {repoUpgradeOnBoot} -> repoUpgradeOnBoot) (\s@Cluster' {} a -> s {repoUpgradeOnBoot = a} :: Cluster)

-- | The path to the Amazon S3 location where logs for this cluster are
-- stored.
cluster_logUri :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_logUri = Lens.lens (\Cluster' {logUri} -> logUri) (\s@Cluster' {} a -> s {logUri = a} :: Cluster)

-- | Attributes for Kerberos configuration when Kerberos authentication is
-- enabled using a security configuration. For more information see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-kerberos.html Use Kerberos Authentication>
-- in the /Amazon EMR Management Guide/.
cluster_kerberosAttributes :: Lens.Lens' Cluster (Prelude.Maybe KerberosAttributes)
cluster_kerberosAttributes = Lens.lens (\Cluster' {kerberosAttributes} -> kerberosAttributes) (\s@Cluster' {} a -> s {kerberosAttributes = a} :: Cluster)

-- | Placement group configured for an Amazon EMR cluster.
cluster_placementGroups :: Lens.Lens' Cluster (Prelude.Maybe [PlacementGroupConfig])
cluster_placementGroups = Lens.lens (\Cluster' {placementGroups} -> placementGroups) (\s@Cluster' {} a -> s {placementGroups = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The AMI version running on this cluster.
cluster_runningAmiVersion :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_runningAmiVersion = Lens.lens (\Cluster' {runningAmiVersion} -> runningAmiVersion) (\s@Cluster' {} a -> s {runningAmiVersion = a} :: Cluster)

-- | The DNS name of the master node. If the cluster is on a private subnet,
-- this is the private DNS name. On a public subnet, this is the public DNS
-- name.
cluster_masterPublicDnsName :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_masterPublicDnsName = Lens.lens (\Cluster' {masterPublicDnsName} -> masterPublicDnsName) (\s@Cluster' {} a -> s {masterPublicDnsName = a} :: Cluster)

-- | Indicates whether Amazon EMR will lock the cluster to prevent the EC2
-- instances from being terminated by an API call or user intervention, or
-- in the event of a cluster error.
cluster_terminationProtected :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Bool)
cluster_terminationProtected = Lens.lens (\Cluster' {terminationProtected} -> terminationProtected) (\s@Cluster' {} a -> s {terminationProtected = a} :: Cluster)

-- | Indicates whether the cluster is visible to IAM principals in the Amazon
-- Web Services account associated with the cluster. When @true@, IAM
-- principals in the Amazon Web Services account can perform EMR cluster
-- actions on the cluster that their IAM policies allow. When @false@, only
-- the IAM principal that created the cluster and the Amazon Web Services
-- account root user can perform EMR actions, regardless of IAM permissions
-- policies attached to other IAM principals.
--
-- The default value is @true@ if a value is not provided when creating a
-- cluster using the EMR API RunJobFlow command, the CLI
-- <https://docs.aws.amazon.com/cli/latest/reference/emr/create-cluster.html create-cluster>
-- command, or the Amazon Web Services Management Console. IAM principals
-- that are allowed to perform actions on the cluster can use the
-- SetVisibleToAllUsers action to change the value on a running cluster.
-- For more information, see
-- <https://docs.aws.amazon.com/emr/latest/ManagementGuide/security_iam_emr-with-iam.html#security_set_visible_to_all_users Understanding the EMR Cluster VisibleToAllUsers Setting>
-- in the /Amazon EMRManagement Guide/.
cluster_visibleToAllUsers :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Bool)
cluster_visibleToAllUsers = Lens.lens (\Cluster' {visibleToAllUsers} -> visibleToAllUsers) (\s@Cluster' {} a -> s {visibleToAllUsers = a} :: Cluster)

-- | Specifies whether the cluster should terminate after completing all
-- steps.
cluster_autoTerminate :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Bool)
cluster_autoTerminate = Lens.lens (\Cluster' {autoTerminate} -> autoTerminate) (\s@Cluster' {} a -> s {autoTerminate = a} :: Cluster)

-- | Specifies the number of steps that can be executed concurrently.
cluster_stepConcurrencyLevel :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Int)
cluster_stepConcurrencyLevel = Lens.lens (\Cluster' {stepConcurrencyLevel} -> stepConcurrencyLevel) (\s@Cluster' {} a -> s {stepConcurrencyLevel = a} :: Cluster)

-- | The applications installed on this cluster.
cluster_applications :: Lens.Lens' Cluster (Prelude.Maybe [Application])
cluster_applications = Lens.lens (\Cluster' {applications} -> applications) (\s@Cluster' {} a -> s {applications = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags associated with a cluster.
cluster_tags :: Lens.Lens' Cluster (Prelude.Maybe [Tag])
cluster_tags = Lens.lens (\Cluster' {tags} -> tags) (\s@Cluster' {} a -> s {tags = a} :: Cluster) Prelude.. Lens.mapping Lens.coerced

-- | The IAM role that Amazon EMR assumes in order to access Amazon Web
-- Services resources on your behalf.
cluster_serviceRole :: Lens.Lens' Cluster (Prelude.Maybe Prelude.Text)
cluster_serviceRole = Lens.lens (\Cluster' {serviceRole} -> serviceRole) (\s@Cluster' {} a -> s {serviceRole = a} :: Cluster)

-- | The unique identifier for the cluster.
cluster_id :: Lens.Lens' Cluster Prelude.Text
cluster_id = Lens.lens (\Cluster' {id} -> id) (\s@Cluster' {} a -> s {id = a} :: Cluster)

-- | The name of the cluster.
cluster_name :: Lens.Lens' Cluster Prelude.Text
cluster_name = Lens.lens (\Cluster' {name} -> name) (\s@Cluster' {} a -> s {name = a} :: Cluster)

-- | The current status details about the cluster.
cluster_status :: Lens.Lens' Cluster ClusterStatus
cluster_status = Lens.lens (\Cluster' {status} -> status) (\s@Cluster' {} a -> s {status = a} :: Cluster)

instance Core.FromJSON Cluster where
  parseJSON =
    Core.withObject
      "Cluster"
      ( \x ->
          Cluster'
            Prelude.<$> (x Core..:? "LogEncryptionKmsKeyId")
            Prelude.<*> (x Core..:? "ClusterArn")
            Prelude.<*> (x Core..:? "RequestedAmiVersion")
            Prelude.<*> (x Core..:? "EbsRootVolumeSize")
            Prelude.<*> (x Core..:? "Ec2InstanceAttributes")
            Prelude.<*> (x Core..:? "OutpostArn")
            Prelude.<*> (x Core..:? "NormalizedInstanceHours")
            Prelude.<*> (x Core..:? "Configurations" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "CustomAmiId")
            Prelude.<*> (x Core..:? "AutoScalingRole")
            Prelude.<*> (x Core..:? "SecurityConfiguration")
            Prelude.<*> (x Core..:? "ScaleDownBehavior")
            Prelude.<*> (x Core..:? "InstanceCollectionType")
            Prelude.<*> (x Core..:? "ReleaseLabel")
            Prelude.<*> (x Core..:? "RepoUpgradeOnBoot")
            Prelude.<*> (x Core..:? "LogUri")
            Prelude.<*> (x Core..:? "KerberosAttributes")
            Prelude.<*> ( x Core..:? "PlacementGroups"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "RunningAmiVersion")
            Prelude.<*> (x Core..:? "MasterPublicDnsName")
            Prelude.<*> (x Core..:? "TerminationProtected")
            Prelude.<*> (x Core..:? "VisibleToAllUsers")
            Prelude.<*> (x Core..:? "AutoTerminate")
            Prelude.<*> (x Core..:? "StepConcurrencyLevel")
            Prelude.<*> (x Core..:? "Applications" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ServiceRole")
            Prelude.<*> (x Core..: "Id")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Status")
      )

instance Prelude.Hashable Cluster where
  hashWithSalt salt' Cluster' {..} =
    salt' `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` serviceRole
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` applications
      `Prelude.hashWithSalt` stepConcurrencyLevel
      `Prelude.hashWithSalt` autoTerminate
      `Prelude.hashWithSalt` visibleToAllUsers
      `Prelude.hashWithSalt` terminationProtected
      `Prelude.hashWithSalt` masterPublicDnsName
      `Prelude.hashWithSalt` runningAmiVersion
      `Prelude.hashWithSalt` placementGroups
      `Prelude.hashWithSalt` kerberosAttributes
      `Prelude.hashWithSalt` logUri
      `Prelude.hashWithSalt` repoUpgradeOnBoot
      `Prelude.hashWithSalt` releaseLabel
      `Prelude.hashWithSalt` instanceCollectionType
      `Prelude.hashWithSalt` scaleDownBehavior
      `Prelude.hashWithSalt` securityConfiguration
      `Prelude.hashWithSalt` autoScalingRole
      `Prelude.hashWithSalt` customAmiId
      `Prelude.hashWithSalt` configurations
      `Prelude.hashWithSalt` normalizedInstanceHours
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` ec2InstanceAttributes
      `Prelude.hashWithSalt` ebsRootVolumeSize
      `Prelude.hashWithSalt` requestedAmiVersion
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` logEncryptionKmsKeyId

instance Prelude.NFData Cluster where
  rnf Cluster' {..} =
    Prelude.rnf logEncryptionKmsKeyId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf serviceRole
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf applications
      `Prelude.seq` Prelude.rnf stepConcurrencyLevel
      `Prelude.seq` Prelude.rnf autoTerminate
      `Prelude.seq` Prelude.rnf visibleToAllUsers
      `Prelude.seq` Prelude.rnf terminationProtected
      `Prelude.seq` Prelude.rnf masterPublicDnsName
      `Prelude.seq` Prelude.rnf runningAmiVersion
      `Prelude.seq` Prelude.rnf placementGroups
      `Prelude.seq` Prelude.rnf kerberosAttributes
      `Prelude.seq` Prelude.rnf logUri
      `Prelude.seq` Prelude.rnf repoUpgradeOnBoot
      `Prelude.seq` Prelude.rnf releaseLabel
      `Prelude.seq` Prelude.rnf instanceCollectionType
      `Prelude.seq` Prelude.rnf scaleDownBehavior
      `Prelude.seq` Prelude.rnf securityConfiguration
      `Prelude.seq` Prelude.rnf autoScalingRole
      `Prelude.seq` Prelude.rnf customAmiId
      `Prelude.seq` Prelude.rnf configurations
      `Prelude.seq` Prelude.rnf normalizedInstanceHours
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf ec2InstanceAttributes
      `Prelude.seq` Prelude.rnf ebsRootVolumeSize
      `Prelude.seq` Prelude.rnf requestedAmiVersion
      `Prelude.seq` Prelude.rnf clusterArn
