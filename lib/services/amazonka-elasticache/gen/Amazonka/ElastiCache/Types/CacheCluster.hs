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
-- Module      : Amazonka.ElastiCache.Types.CacheCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.CacheCluster where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.CacheNode
import Amazonka.ElastiCache.Types.CacheParameterGroupStatus
import Amazonka.ElastiCache.Types.CacheSecurityGroupMembership
import Amazonka.ElastiCache.Types.Endpoint
import Amazonka.ElastiCache.Types.LogDeliveryConfiguration
import Amazonka.ElastiCache.Types.NotificationConfiguration
import Amazonka.ElastiCache.Types.PendingModifiedValues
import Amazonka.ElastiCache.Types.SecurityGroupMembership
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains all of the attributes of a specific cluster.
--
-- /See:/ 'newCacheCluster' smart constructor.
data CacheCluster = CacheCluster'
  { -- | The date the auth token was last modified
    authTokenLastModifiedDate :: Prelude.Maybe Core.ISO8601,
    -- | The version of the cache engine that is used in this cluster.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the compute and memory capacity node type for the cluster.
    --
    -- The following node types are supported by ElastiCache. Generally
    -- speaking, the current generation types provide more memory and
    -- computational power at lower cost when compared to their equivalent
    -- previous generation counterparts.
    --
    -- -   General purpose:
    --
    --     -   Current generation:
    --
    --         __M6g node types__ (available only for Redis engine version
    --         5.0.6 onward and for Memcached engine version 1.5.16 onward).
    --
    --         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
    --         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
    --         @cache.m6g.16xlarge@
    --
    --         For region availability, see
    --         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
    --
    --         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
    --         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
    --         @cache.m5.24xlarge@
    --
    --         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
    --         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
    --
    --         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
    --         @cache.t3.medium@
    --
    --         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
    --         @cache.t2.medium@
    --
    --     -   Previous generation: (not recommended)
    --
    --         __T1 node types:__ @cache.t1.micro@
    --
    --         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
    --         @cache.m1.large@, @cache.m1.xlarge@
    --
    --         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
    --         @cache.m3.xlarge@, @cache.m3.2xlarge@
    --
    -- -   Compute optimized:
    --
    --     -   Previous generation: (not recommended)
    --
    --         __C1 node types:__ @cache.c1.xlarge@
    --
    -- -   Memory optimized:
    --
    --     -   Current generation:
    --
    --         __R6g node types__ (available only for Redis engine version
    --         5.0.6 onward and for Memcached engine version 1.5.16 onward).
    --
    --         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
    --         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
    --         @cache.r6g.16xlarge@
    --
    --         For region availability, see
    --         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
    --
    --         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
    --         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
    --         @cache.r5.24xlarge@
    --
    --         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
    --         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
    --         @cache.r4.16xlarge@
    --
    --     -   Previous generation: (not recommended)
    --
    --         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
    --         @cache.m2.4xlarge@
    --
    --         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
    --         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
    --
    -- __Additional node type info__
    --
    -- -   All current generation instance types are created in Amazon VPC by
    --     default.
    --
    -- -   Redis append-only files (AOF) are not supported for T1 or T2
    --     instances.
    --
    -- -   Redis Multi-AZ with automatic failover is not supported on T1
    --     instances.
    --
    -- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
    --     supported on Redis version 2.8.22 and later.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | A list of cache nodes that are members of the cluster.
    cacheNodes :: Prelude.Maybe [CacheNode],
    -- | The date and time when the cluster was created.
    cacheClusterCreateTime :: Prelude.Maybe Core.ISO8601,
    -- | A flag that enables encryption at-rest when set to @true@.
    --
    -- You cannot modify the value of @AtRestEncryptionEnabled@ after the
    -- cluster is created. To enable at-rest encryption on a cluster you must
    -- set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    --
    -- Default: @false@
    atRestEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | This parameter is currently disabled.
    autoMinorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | A list of VPC Security Groups associated with the cluster.
    securityGroups :: Prelude.Maybe [SecurityGroupMembership],
    -- | Describes a notification topic and its status. Notification topics are
    -- used for publishing ElastiCache events to subscribers using Amazon
    -- Simple Notification Service (SNS).
    notificationConfiguration :: Prelude.Maybe NotificationConfiguration,
    -- | The ARN (Amazon Resource Name) of the cache cluster.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A flag that enables in-transit encryption when set to @true@.
    --
    -- You cannot modify the value of @TransitEncryptionEnabled@ after the
    -- cluster is created. To enable in-transit encryption on a cluster you
    -- must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
    --
    -- __Required:__ Only available when creating a replication group in an
    -- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
    --
    -- Default: @false@
    transitEncryptionEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The daily time range (in UTC) during which ElastiCache begins taking a
    -- daily snapshot of your cluster.
    --
    -- Example: @05:00-09:00@
    snapshotWindow :: Prelude.Maybe Prelude.Text,
    -- | The user-supplied identifier of the cluster. This identifier is a unique
    -- key that identifies a cluster.
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | Returns the destination, format and type of the logs.
    logDeliveryConfigurations :: Prelude.Maybe [LogDeliveryConfiguration],
    -- | Represents a Memcached cluster endpoint which can be used by an
    -- application to connect to any node in the cluster. The configuration
    -- endpoint will always have @.cfg@ in it.
    --
    -- Example: @mem-3.9dvc4r.cfg.usw2.cache.amazonaws.com:11211@
    configurationEndpoint :: Prelude.Maybe Endpoint,
    -- | The name of the cache engine (@memcached@ or @redis@) to be used for
    -- this cluster.
    engine :: Prelude.Maybe Prelude.Text,
    -- | A list of cache security group elements, composed of name and status
    -- sub-elements.
    cacheSecurityGroups :: Prelude.Maybe [CacheSecurityGroupMembership],
    -- | A boolean value indicating whether log delivery is enabled for the
    -- replication group.
    replicationGroupLogDeliveryEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A flag that enables using an @AuthToken@ (password) when issuing Redis
    -- commands.
    --
    -- Default: @false@
    authTokenEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The URL of the web page where you can download the latest ElastiCache
    -- client library.
    clientDownloadLandingPage :: Prelude.Maybe Prelude.Text,
    -- | Specifies the weekly time range during which maintenance on the cluster
    -- is performed. It is specified as a range in the format
    -- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
    -- is a 60 minute period.
    --
    -- Valid values for @ddd@ are:
    --
    -- -   @sun@
    --
    -- -   @mon@
    --
    -- -   @tue@
    --
    -- -   @wed@
    --
    -- -   @thu@
    --
    -- -   @fri@
    --
    -- -   @sat@
    --
    -- Example: @sun:23:00-mon:01:30@
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | The name of the cache subnet group associated with the cluster.
    cacheSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Availability Zone in which the cluster is located or
    -- \"Multiple\" if the cache nodes are located in different Availability
    -- Zones.
    preferredAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Status of the cache parameter group.
    cacheParameterGroup :: Prelude.Maybe CacheParameterGroupStatus,
    -- | The current state of this cluster, one of the following values:
    -- @available@, @creating@, @deleted@, @deleting@, @incompatible-network@,
    -- @modifying@, @rebooting cluster nodes@, @restore-failed@, or
    -- @snapshotting@.
    cacheClusterStatus :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which ElastiCache retains automatic cluster
    -- snapshots before deleting them. For example, if you set
    -- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
    -- retained for 5 days before being deleted.
    --
    -- If the value of SnapshotRetentionLimit is set to zero (0), backups are
    -- turned off.
    snapshotRetentionLimit :: Prelude.Maybe Prelude.Int,
    -- | The outpost ARN in which the cache cluster is created.
    preferredOutpostArn :: Prelude.Maybe Prelude.Text,
    -- | The replication group to which this cluster belongs. If this field is
    -- empty, the cluster is not associated with any replication group.
    replicationGroupId :: Prelude.Maybe Prelude.Text,
    pendingModifiedValues :: Prelude.Maybe PendingModifiedValues,
    -- | The number of cache nodes in the cluster.
    --
    -- For clusters running Redis, this value must be 1. For clusters running
    -- Memcached, this value must be between 1 and 40.
    numCacheNodes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CacheCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authTokenLastModifiedDate', 'cacheCluster_authTokenLastModifiedDate' - The date the auth token was last modified
--
-- 'engineVersion', 'cacheCluster_engineVersion' - The version of the cache engine that is used in this cluster.
--
-- 'cacheNodeType', 'cacheCluster_cacheNodeType' - The name of the compute and memory capacity node type for the cluster.
--
-- The following node types are supported by ElastiCache. Generally
-- speaking, the current generation types provide more memory and
-- computational power at lower cost when compared to their equivalent
-- previous generation counterparts.
--
-- -   General purpose:
--
--     -   Current generation:
--
--         __M6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
--         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
--         @cache.m6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
--         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
--         @cache.m5.24xlarge@
--
--         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
--         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended)
--
--         __T1 node types:__ @cache.t1.micro@
--
--         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
--         @cache.m1.large@, @cache.m1.xlarge@
--
--         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
--         @cache.m3.xlarge@, @cache.m3.2xlarge@
--
-- -   Compute optimized:
--
--     -   Previous generation: (not recommended)
--
--         __C1 node types:__ @cache.c1.xlarge@
--
-- -   Memory optimized:
--
--     -   Current generation:
--
--         __R6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
--         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
--         @cache.r6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
--         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
--         @cache.r5.24xlarge@
--
--         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
--         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
--         @cache.r4.16xlarge@
--
--     -   Previous generation: (not recommended)
--
--         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
--         @cache.m2.4xlarge@
--
--         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
--         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
--
-- __Additional node type info__
--
-- -   All current generation instance types are created in Amazon VPC by
--     default.
--
-- -   Redis append-only files (AOF) are not supported for T1 or T2
--     instances.
--
-- -   Redis Multi-AZ with automatic failover is not supported on T1
--     instances.
--
-- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
--     supported on Redis version 2.8.22 and later.
--
-- 'cacheNodes', 'cacheCluster_cacheNodes' - A list of cache nodes that are members of the cluster.
--
-- 'cacheClusterCreateTime', 'cacheCluster_cacheClusterCreateTime' - The date and time when the cluster was created.
--
-- 'atRestEncryptionEnabled', 'cacheCluster_atRestEncryptionEnabled' - A flag that enables encryption at-rest when set to @true@.
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the
-- cluster is created. To enable at-rest encryption on a cluster you must
-- set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
--
-- 'autoMinorVersionUpgrade', 'cacheCluster_autoMinorVersionUpgrade' - This parameter is currently disabled.
--
-- 'securityGroups', 'cacheCluster_securityGroups' - A list of VPC Security Groups associated with the cluster.
--
-- 'notificationConfiguration', 'cacheCluster_notificationConfiguration' - Describes a notification topic and its status. Notification topics are
-- used for publishing ElastiCache events to subscribers using Amazon
-- Simple Notification Service (SNS).
--
-- 'arn', 'cacheCluster_arn' - The ARN (Amazon Resource Name) of the cache cluster.
--
-- 'transitEncryptionEnabled', 'cacheCluster_transitEncryptionEnabled' - A flag that enables in-transit encryption when set to @true@.
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the
-- cluster is created. To enable in-transit encryption on a cluster you
-- must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
--
-- 'snapshotWindow', 'cacheCluster_snapshotWindow' - The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your cluster.
--
-- Example: @05:00-09:00@
--
-- 'cacheClusterId', 'cacheCluster_cacheClusterId' - The user-supplied identifier of the cluster. This identifier is a unique
-- key that identifies a cluster.
--
-- 'logDeliveryConfigurations', 'cacheCluster_logDeliveryConfigurations' - Returns the destination, format and type of the logs.
--
-- 'configurationEndpoint', 'cacheCluster_configurationEndpoint' - Represents a Memcached cluster endpoint which can be used by an
-- application to connect to any node in the cluster. The configuration
-- endpoint will always have @.cfg@ in it.
--
-- Example: @mem-3.9dvc4r.cfg.usw2.cache.amazonaws.com:11211@
--
-- 'engine', 'cacheCluster_engine' - The name of the cache engine (@memcached@ or @redis@) to be used for
-- this cluster.
--
-- 'cacheSecurityGroups', 'cacheCluster_cacheSecurityGroups' - A list of cache security group elements, composed of name and status
-- sub-elements.
--
-- 'replicationGroupLogDeliveryEnabled', 'cacheCluster_replicationGroupLogDeliveryEnabled' - A boolean value indicating whether log delivery is enabled for the
-- replication group.
--
-- 'authTokenEnabled', 'cacheCluster_authTokenEnabled' - A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
--
-- 'clientDownloadLandingPage', 'cacheCluster_clientDownloadLandingPage' - The URL of the web page where you can download the latest ElastiCache
-- client library.
--
-- 'preferredMaintenanceWindow', 'cacheCluster_preferredMaintenanceWindow' - Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
--
-- Valid values for @ddd@ are:
--
-- -   @sun@
--
-- -   @mon@
--
-- -   @tue@
--
-- -   @wed@
--
-- -   @thu@
--
-- -   @fri@
--
-- -   @sat@
--
-- Example: @sun:23:00-mon:01:30@
--
-- 'cacheSubnetGroupName', 'cacheCluster_cacheSubnetGroupName' - The name of the cache subnet group associated with the cluster.
--
-- 'preferredAvailabilityZone', 'cacheCluster_preferredAvailabilityZone' - The name of the Availability Zone in which the cluster is located or
-- \"Multiple\" if the cache nodes are located in different Availability
-- Zones.
--
-- 'cacheParameterGroup', 'cacheCluster_cacheParameterGroup' - Status of the cache parameter group.
--
-- 'cacheClusterStatus', 'cacheCluster_cacheClusterStatus' - The current state of this cluster, one of the following values:
-- @available@, @creating@, @deleted@, @deleting@, @incompatible-network@,
-- @modifying@, @rebooting cluster nodes@, @restore-failed@, or
-- @snapshotting@.
--
-- 'snapshotRetentionLimit', 'cacheCluster_snapshotRetentionLimit' - The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of SnapshotRetentionLimit is set to zero (0), backups are
-- turned off.
--
-- 'preferredOutpostArn', 'cacheCluster_preferredOutpostArn' - The outpost ARN in which the cache cluster is created.
--
-- 'replicationGroupId', 'cacheCluster_replicationGroupId' - The replication group to which this cluster belongs. If this field is
-- empty, the cluster is not associated with any replication group.
--
-- 'pendingModifiedValues', 'cacheCluster_pendingModifiedValues' - Undocumented member.
--
-- 'numCacheNodes', 'cacheCluster_numCacheNodes' - The number of cache nodes in the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 40.
newCacheCluster ::
  CacheCluster
newCacheCluster =
  CacheCluster'
    { authTokenLastModifiedDate =
        Prelude.Nothing,
      engineVersion = Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      cacheNodes = Prelude.Nothing,
      cacheClusterCreateTime = Prelude.Nothing,
      atRestEncryptionEnabled = Prelude.Nothing,
      autoMinorVersionUpgrade = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      notificationConfiguration = Prelude.Nothing,
      arn = Prelude.Nothing,
      transitEncryptionEnabled = Prelude.Nothing,
      snapshotWindow = Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      logDeliveryConfigurations = Prelude.Nothing,
      configurationEndpoint = Prelude.Nothing,
      engine = Prelude.Nothing,
      cacheSecurityGroups = Prelude.Nothing,
      replicationGroupLogDeliveryEnabled = Prelude.Nothing,
      authTokenEnabled = Prelude.Nothing,
      clientDownloadLandingPage = Prelude.Nothing,
      preferredMaintenanceWindow = Prelude.Nothing,
      cacheSubnetGroupName = Prelude.Nothing,
      preferredAvailabilityZone = Prelude.Nothing,
      cacheParameterGroup = Prelude.Nothing,
      cacheClusterStatus = Prelude.Nothing,
      snapshotRetentionLimit = Prelude.Nothing,
      preferredOutpostArn = Prelude.Nothing,
      replicationGroupId = Prelude.Nothing,
      pendingModifiedValues = Prelude.Nothing,
      numCacheNodes = Prelude.Nothing
    }

-- | The date the auth token was last modified
cacheCluster_authTokenLastModifiedDate :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.UTCTime)
cacheCluster_authTokenLastModifiedDate = Lens.lens (\CacheCluster' {authTokenLastModifiedDate} -> authTokenLastModifiedDate) (\s@CacheCluster' {} a -> s {authTokenLastModifiedDate = a} :: CacheCluster) Prelude.. Lens.mapping Core._Time

-- | The version of the cache engine that is used in this cluster.
cacheCluster_engineVersion :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_engineVersion = Lens.lens (\CacheCluster' {engineVersion} -> engineVersion) (\s@CacheCluster' {} a -> s {engineVersion = a} :: CacheCluster)

-- | The name of the compute and memory capacity node type for the cluster.
--
-- The following node types are supported by ElastiCache. Generally
-- speaking, the current generation types provide more memory and
-- computational power at lower cost when compared to their equivalent
-- previous generation counterparts.
--
-- -   General purpose:
--
--     -   Current generation:
--
--         __M6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.m6g.large@, @cache.m6g.xlarge@, @cache.m6g.2xlarge@,
--         @cache.m6g.4xlarge@, @cache.m6g.8xlarge@, @cache.m6g.12xlarge@,
--         @cache.m6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __M5 node types:__ @cache.m5.large@, @cache.m5.xlarge@,
--         @cache.m5.2xlarge@, @cache.m5.4xlarge@, @cache.m5.12xlarge@,
--         @cache.m5.24xlarge@
--
--         __M4 node types:__ @cache.m4.large@, @cache.m4.xlarge@,
--         @cache.m4.2xlarge@, @cache.m4.4xlarge@, @cache.m4.10xlarge@
--
--         __T3 node types:__ @cache.t3.micro@, @cache.t3.small@,
--         @cache.t3.medium@
--
--         __T2 node types:__ @cache.t2.micro@, @cache.t2.small@,
--         @cache.t2.medium@
--
--     -   Previous generation: (not recommended)
--
--         __T1 node types:__ @cache.t1.micro@
--
--         __M1 node types:__ @cache.m1.small@, @cache.m1.medium@,
--         @cache.m1.large@, @cache.m1.xlarge@
--
--         __M3 node types:__ @cache.m3.medium@, @cache.m3.large@,
--         @cache.m3.xlarge@, @cache.m3.2xlarge@
--
-- -   Compute optimized:
--
--     -   Previous generation: (not recommended)
--
--         __C1 node types:__ @cache.c1.xlarge@
--
-- -   Memory optimized:
--
--     -   Current generation:
--
--         __R6g node types__ (available only for Redis engine version
--         5.0.6 onward and for Memcached engine version 1.5.16 onward).
--
--         @cache.r6g.large@, @cache.r6g.xlarge@, @cache.r6g.2xlarge@,
--         @cache.r6g.4xlarge@, @cache.r6g.8xlarge@, @cache.r6g.12xlarge@,
--         @cache.r6g.16xlarge@
--
--         For region availability, see
--         <https://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/CacheNodes.SupportedTypes.html#CacheNodes.SupportedTypesByRegion Supported Node Types>
--
--         __R5 node types:__ @cache.r5.large@, @cache.r5.xlarge@,
--         @cache.r5.2xlarge@, @cache.r5.4xlarge@, @cache.r5.12xlarge@,
--         @cache.r5.24xlarge@
--
--         __R4 node types:__ @cache.r4.large@, @cache.r4.xlarge@,
--         @cache.r4.2xlarge@, @cache.r4.4xlarge@, @cache.r4.8xlarge@,
--         @cache.r4.16xlarge@
--
--     -   Previous generation: (not recommended)
--
--         __M2 node types:__ @cache.m2.xlarge@, @cache.m2.2xlarge@,
--         @cache.m2.4xlarge@
--
--         __R3 node types:__ @cache.r3.large@, @cache.r3.xlarge@,
--         @cache.r3.2xlarge@, @cache.r3.4xlarge@, @cache.r3.8xlarge@
--
-- __Additional node type info__
--
-- -   All current generation instance types are created in Amazon VPC by
--     default.
--
-- -   Redis append-only files (AOF) are not supported for T1 or T2
--     instances.
--
-- -   Redis Multi-AZ with automatic failover is not supported on T1
--     instances.
--
-- -   Redis configuration variables @appendonly@ and @appendfsync@ are not
--     supported on Redis version 2.8.22 and later.
cacheCluster_cacheNodeType :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_cacheNodeType = Lens.lens (\CacheCluster' {cacheNodeType} -> cacheNodeType) (\s@CacheCluster' {} a -> s {cacheNodeType = a} :: CacheCluster)

-- | A list of cache nodes that are members of the cluster.
cacheCluster_cacheNodes :: Lens.Lens' CacheCluster (Prelude.Maybe [CacheNode])
cacheCluster_cacheNodes = Lens.lens (\CacheCluster' {cacheNodes} -> cacheNodes) (\s@CacheCluster' {} a -> s {cacheNodes = a} :: CacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the cluster was created.
cacheCluster_cacheClusterCreateTime :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.UTCTime)
cacheCluster_cacheClusterCreateTime = Lens.lens (\CacheCluster' {cacheClusterCreateTime} -> cacheClusterCreateTime) (\s@CacheCluster' {} a -> s {cacheClusterCreateTime = a} :: CacheCluster) Prelude.. Lens.mapping Core._Time

-- | A flag that enables encryption at-rest when set to @true@.
--
-- You cannot modify the value of @AtRestEncryptionEnabled@ after the
-- cluster is created. To enable at-rest encryption on a cluster you must
-- set @AtRestEncryptionEnabled@ to @true@ when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
cacheCluster_atRestEncryptionEnabled :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Bool)
cacheCluster_atRestEncryptionEnabled = Lens.lens (\CacheCluster' {atRestEncryptionEnabled} -> atRestEncryptionEnabled) (\s@CacheCluster' {} a -> s {atRestEncryptionEnabled = a} :: CacheCluster)

-- | This parameter is currently disabled.
cacheCluster_autoMinorVersionUpgrade :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Bool)
cacheCluster_autoMinorVersionUpgrade = Lens.lens (\CacheCluster' {autoMinorVersionUpgrade} -> autoMinorVersionUpgrade) (\s@CacheCluster' {} a -> s {autoMinorVersionUpgrade = a} :: CacheCluster)

-- | A list of VPC Security Groups associated with the cluster.
cacheCluster_securityGroups :: Lens.Lens' CacheCluster (Prelude.Maybe [SecurityGroupMembership])
cacheCluster_securityGroups = Lens.lens (\CacheCluster' {securityGroups} -> securityGroups) (\s@CacheCluster' {} a -> s {securityGroups = a} :: CacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | Describes a notification topic and its status. Notification topics are
-- used for publishing ElastiCache events to subscribers using Amazon
-- Simple Notification Service (SNS).
cacheCluster_notificationConfiguration :: Lens.Lens' CacheCluster (Prelude.Maybe NotificationConfiguration)
cacheCluster_notificationConfiguration = Lens.lens (\CacheCluster' {notificationConfiguration} -> notificationConfiguration) (\s@CacheCluster' {} a -> s {notificationConfiguration = a} :: CacheCluster)

-- | The ARN (Amazon Resource Name) of the cache cluster.
cacheCluster_arn :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_arn = Lens.lens (\CacheCluster' {arn} -> arn) (\s@CacheCluster' {} a -> s {arn = a} :: CacheCluster)

-- | A flag that enables in-transit encryption when set to @true@.
--
-- You cannot modify the value of @TransitEncryptionEnabled@ after the
-- cluster is created. To enable in-transit encryption on a cluster you
-- must set @TransitEncryptionEnabled@ to @true@ when you create a cluster.
--
-- __Required:__ Only available when creating a replication group in an
-- Amazon VPC using redis version @3.2.6@, @4.x@ or later.
--
-- Default: @false@
cacheCluster_transitEncryptionEnabled :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Bool)
cacheCluster_transitEncryptionEnabled = Lens.lens (\CacheCluster' {transitEncryptionEnabled} -> transitEncryptionEnabled) (\s@CacheCluster' {} a -> s {transitEncryptionEnabled = a} :: CacheCluster)

-- | The daily time range (in UTC) during which ElastiCache begins taking a
-- daily snapshot of your cluster.
--
-- Example: @05:00-09:00@
cacheCluster_snapshotWindow :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_snapshotWindow = Lens.lens (\CacheCluster' {snapshotWindow} -> snapshotWindow) (\s@CacheCluster' {} a -> s {snapshotWindow = a} :: CacheCluster)

-- | The user-supplied identifier of the cluster. This identifier is a unique
-- key that identifies a cluster.
cacheCluster_cacheClusterId :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_cacheClusterId = Lens.lens (\CacheCluster' {cacheClusterId} -> cacheClusterId) (\s@CacheCluster' {} a -> s {cacheClusterId = a} :: CacheCluster)

-- | Returns the destination, format and type of the logs.
cacheCluster_logDeliveryConfigurations :: Lens.Lens' CacheCluster (Prelude.Maybe [LogDeliveryConfiguration])
cacheCluster_logDeliveryConfigurations = Lens.lens (\CacheCluster' {logDeliveryConfigurations} -> logDeliveryConfigurations) (\s@CacheCluster' {} a -> s {logDeliveryConfigurations = a} :: CacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | Represents a Memcached cluster endpoint which can be used by an
-- application to connect to any node in the cluster. The configuration
-- endpoint will always have @.cfg@ in it.
--
-- Example: @mem-3.9dvc4r.cfg.usw2.cache.amazonaws.com:11211@
cacheCluster_configurationEndpoint :: Lens.Lens' CacheCluster (Prelude.Maybe Endpoint)
cacheCluster_configurationEndpoint = Lens.lens (\CacheCluster' {configurationEndpoint} -> configurationEndpoint) (\s@CacheCluster' {} a -> s {configurationEndpoint = a} :: CacheCluster)

-- | The name of the cache engine (@memcached@ or @redis@) to be used for
-- this cluster.
cacheCluster_engine :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_engine = Lens.lens (\CacheCluster' {engine} -> engine) (\s@CacheCluster' {} a -> s {engine = a} :: CacheCluster)

-- | A list of cache security group elements, composed of name and status
-- sub-elements.
cacheCluster_cacheSecurityGroups :: Lens.Lens' CacheCluster (Prelude.Maybe [CacheSecurityGroupMembership])
cacheCluster_cacheSecurityGroups = Lens.lens (\CacheCluster' {cacheSecurityGroups} -> cacheSecurityGroups) (\s@CacheCluster' {} a -> s {cacheSecurityGroups = a} :: CacheCluster) Prelude.. Lens.mapping Lens.coerced

-- | A boolean value indicating whether log delivery is enabled for the
-- replication group.
cacheCluster_replicationGroupLogDeliveryEnabled :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Bool)
cacheCluster_replicationGroupLogDeliveryEnabled = Lens.lens (\CacheCluster' {replicationGroupLogDeliveryEnabled} -> replicationGroupLogDeliveryEnabled) (\s@CacheCluster' {} a -> s {replicationGroupLogDeliveryEnabled = a} :: CacheCluster)

-- | A flag that enables using an @AuthToken@ (password) when issuing Redis
-- commands.
--
-- Default: @false@
cacheCluster_authTokenEnabled :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Bool)
cacheCluster_authTokenEnabled = Lens.lens (\CacheCluster' {authTokenEnabled} -> authTokenEnabled) (\s@CacheCluster' {} a -> s {authTokenEnabled = a} :: CacheCluster)

-- | The URL of the web page where you can download the latest ElastiCache
-- client library.
cacheCluster_clientDownloadLandingPage :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_clientDownloadLandingPage = Lens.lens (\CacheCluster' {clientDownloadLandingPage} -> clientDownloadLandingPage) (\s@CacheCluster' {} a -> s {clientDownloadLandingPage = a} :: CacheCluster)

-- | Specifies the weekly time range during which maintenance on the cluster
-- is performed. It is specified as a range in the format
-- ddd:hh24:mi-ddd:hh24:mi (24H Clock UTC). The minimum maintenance window
-- is a 60 minute period.
--
-- Valid values for @ddd@ are:
--
-- -   @sun@
--
-- -   @mon@
--
-- -   @tue@
--
-- -   @wed@
--
-- -   @thu@
--
-- -   @fri@
--
-- -   @sat@
--
-- Example: @sun:23:00-mon:01:30@
cacheCluster_preferredMaintenanceWindow :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_preferredMaintenanceWindow = Lens.lens (\CacheCluster' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@CacheCluster' {} a -> s {preferredMaintenanceWindow = a} :: CacheCluster)

-- | The name of the cache subnet group associated with the cluster.
cacheCluster_cacheSubnetGroupName :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_cacheSubnetGroupName = Lens.lens (\CacheCluster' {cacheSubnetGroupName} -> cacheSubnetGroupName) (\s@CacheCluster' {} a -> s {cacheSubnetGroupName = a} :: CacheCluster)

-- | The name of the Availability Zone in which the cluster is located or
-- \"Multiple\" if the cache nodes are located in different Availability
-- Zones.
cacheCluster_preferredAvailabilityZone :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_preferredAvailabilityZone = Lens.lens (\CacheCluster' {preferredAvailabilityZone} -> preferredAvailabilityZone) (\s@CacheCluster' {} a -> s {preferredAvailabilityZone = a} :: CacheCluster)

-- | Status of the cache parameter group.
cacheCluster_cacheParameterGroup :: Lens.Lens' CacheCluster (Prelude.Maybe CacheParameterGroupStatus)
cacheCluster_cacheParameterGroup = Lens.lens (\CacheCluster' {cacheParameterGroup} -> cacheParameterGroup) (\s@CacheCluster' {} a -> s {cacheParameterGroup = a} :: CacheCluster)

-- | The current state of this cluster, one of the following values:
-- @available@, @creating@, @deleted@, @deleting@, @incompatible-network@,
-- @modifying@, @rebooting cluster nodes@, @restore-failed@, or
-- @snapshotting@.
cacheCluster_cacheClusterStatus :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_cacheClusterStatus = Lens.lens (\CacheCluster' {cacheClusterStatus} -> cacheClusterStatus) (\s@CacheCluster' {} a -> s {cacheClusterStatus = a} :: CacheCluster)

-- | The number of days for which ElastiCache retains automatic cluster
-- snapshots before deleting them. For example, if you set
-- @SnapshotRetentionLimit@ to 5, a snapshot that was taken today is
-- retained for 5 days before being deleted.
--
-- If the value of SnapshotRetentionLimit is set to zero (0), backups are
-- turned off.
cacheCluster_snapshotRetentionLimit :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Int)
cacheCluster_snapshotRetentionLimit = Lens.lens (\CacheCluster' {snapshotRetentionLimit} -> snapshotRetentionLimit) (\s@CacheCluster' {} a -> s {snapshotRetentionLimit = a} :: CacheCluster)

-- | The outpost ARN in which the cache cluster is created.
cacheCluster_preferredOutpostArn :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_preferredOutpostArn = Lens.lens (\CacheCluster' {preferredOutpostArn} -> preferredOutpostArn) (\s@CacheCluster' {} a -> s {preferredOutpostArn = a} :: CacheCluster)

-- | The replication group to which this cluster belongs. If this field is
-- empty, the cluster is not associated with any replication group.
cacheCluster_replicationGroupId :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Text)
cacheCluster_replicationGroupId = Lens.lens (\CacheCluster' {replicationGroupId} -> replicationGroupId) (\s@CacheCluster' {} a -> s {replicationGroupId = a} :: CacheCluster)

-- | Undocumented member.
cacheCluster_pendingModifiedValues :: Lens.Lens' CacheCluster (Prelude.Maybe PendingModifiedValues)
cacheCluster_pendingModifiedValues = Lens.lens (\CacheCluster' {pendingModifiedValues} -> pendingModifiedValues) (\s@CacheCluster' {} a -> s {pendingModifiedValues = a} :: CacheCluster)

-- | The number of cache nodes in the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 40.
cacheCluster_numCacheNodes :: Lens.Lens' CacheCluster (Prelude.Maybe Prelude.Int)
cacheCluster_numCacheNodes = Lens.lens (\CacheCluster' {numCacheNodes} -> numCacheNodes) (\s@CacheCluster' {} a -> s {numCacheNodes = a} :: CacheCluster)

instance Core.FromXML CacheCluster where
  parseXML x =
    CacheCluster'
      Prelude.<$> (x Core..@? "AuthTokenLastModifiedDate")
      Prelude.<*> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "CacheNodeType")
      Prelude.<*> ( x Core..@? "CacheNodes" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "CacheNode")
                  )
      Prelude.<*> (x Core..@? "CacheClusterCreateTime")
      Prelude.<*> (x Core..@? "AtRestEncryptionEnabled")
      Prelude.<*> (x Core..@? "AutoMinorVersionUpgrade")
      Prelude.<*> ( x Core..@? "SecurityGroups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "NotificationConfiguration")
      Prelude.<*> (x Core..@? "ARN")
      Prelude.<*> (x Core..@? "TransitEncryptionEnabled")
      Prelude.<*> (x Core..@? "SnapshotWindow")
      Prelude.<*> (x Core..@? "CacheClusterId")
      Prelude.<*> ( x Core..@? "LogDeliveryConfigurations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Core.parseXMLList "LogDeliveryConfiguration")
                  )
      Prelude.<*> (x Core..@? "ConfigurationEndpoint")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> ( x Core..@? "CacheSecurityGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "CacheSecurityGroup")
                  )
      Prelude.<*> (x Core..@? "ReplicationGroupLogDeliveryEnabled")
      Prelude.<*> (x Core..@? "AuthTokenEnabled")
      Prelude.<*> (x Core..@? "ClientDownloadLandingPage")
      Prelude.<*> (x Core..@? "PreferredMaintenanceWindow")
      Prelude.<*> (x Core..@? "CacheSubnetGroupName")
      Prelude.<*> (x Core..@? "PreferredAvailabilityZone")
      Prelude.<*> (x Core..@? "CacheParameterGroup")
      Prelude.<*> (x Core..@? "CacheClusterStatus")
      Prelude.<*> (x Core..@? "SnapshotRetentionLimit")
      Prelude.<*> (x Core..@? "PreferredOutpostArn")
      Prelude.<*> (x Core..@? "ReplicationGroupId")
      Prelude.<*> (x Core..@? "PendingModifiedValues")
      Prelude.<*> (x Core..@? "NumCacheNodes")

instance Prelude.Hashable CacheCluster where
  hashWithSalt salt' CacheCluster' {..} =
    salt' `Prelude.hashWithSalt` numCacheNodes
      `Prelude.hashWithSalt` pendingModifiedValues
      `Prelude.hashWithSalt` replicationGroupId
      `Prelude.hashWithSalt` preferredOutpostArn
      `Prelude.hashWithSalt` snapshotRetentionLimit
      `Prelude.hashWithSalt` cacheClusterStatus
      `Prelude.hashWithSalt` cacheParameterGroup
      `Prelude.hashWithSalt` preferredAvailabilityZone
      `Prelude.hashWithSalt` cacheSubnetGroupName
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` clientDownloadLandingPage
      `Prelude.hashWithSalt` authTokenEnabled
      `Prelude.hashWithSalt` replicationGroupLogDeliveryEnabled
      `Prelude.hashWithSalt` cacheSecurityGroups
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` configurationEndpoint
      `Prelude.hashWithSalt` logDeliveryConfigurations
      `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` snapshotWindow
      `Prelude.hashWithSalt` transitEncryptionEnabled
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` notificationConfiguration
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` autoMinorVersionUpgrade
      `Prelude.hashWithSalt` atRestEncryptionEnabled
      `Prelude.hashWithSalt` cacheClusterCreateTime
      `Prelude.hashWithSalt` cacheNodes
      `Prelude.hashWithSalt` cacheNodeType
      `Prelude.hashWithSalt` engineVersion
      `Prelude.hashWithSalt` authTokenLastModifiedDate

instance Prelude.NFData CacheCluster where
  rnf CacheCluster' {..} =
    Prelude.rnf authTokenLastModifiedDate
      `Prelude.seq` Prelude.rnf numCacheNodes
      `Prelude.seq` Prelude.rnf pendingModifiedValues
      `Prelude.seq` Prelude.rnf replicationGroupId
      `Prelude.seq` Prelude.rnf preferredOutpostArn
      `Prelude.seq` Prelude.rnf snapshotRetentionLimit
      `Prelude.seq` Prelude.rnf cacheClusterStatus
      `Prelude.seq` Prelude.rnf cacheParameterGroup
      `Prelude.seq` Prelude.rnf preferredAvailabilityZone
      `Prelude.seq` Prelude.rnf cacheSubnetGroupName
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf clientDownloadLandingPage
      `Prelude.seq` Prelude.rnf authTokenEnabled
      `Prelude.seq` Prelude.rnf replicationGroupLogDeliveryEnabled
      `Prelude.seq` Prelude.rnf cacheSecurityGroups
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf configurationEndpoint
      `Prelude.seq` Prelude.rnf logDeliveryConfigurations
      `Prelude.seq` Prelude.rnf cacheClusterId
      `Prelude.seq` Prelude.rnf snapshotWindow
      `Prelude.seq` Prelude.rnf transitEncryptionEnabled
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf notificationConfiguration
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf autoMinorVersionUpgrade
      `Prelude.seq` Prelude.rnf atRestEncryptionEnabled
      `Prelude.seq` Prelude.rnf cacheClusterCreateTime
      `Prelude.seq` Prelude.rnf cacheNodes
      `Prelude.seq` Prelude.rnf cacheNodeType
      `Prelude.seq` Prelude.rnf engineVersion
