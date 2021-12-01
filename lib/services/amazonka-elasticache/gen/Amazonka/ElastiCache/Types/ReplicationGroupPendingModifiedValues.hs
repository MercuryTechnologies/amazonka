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
-- Module      : Amazonka.ElastiCache.Types.ReplicationGroupPendingModifiedValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.ReplicationGroupPendingModifiedValues where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.AuthTokenUpdateStatus
import Amazonka.ElastiCache.Types.PendingAutomaticFailoverStatus
import Amazonka.ElastiCache.Types.PendingLogDeliveryConfiguration
import Amazonka.ElastiCache.Types.ReshardingStatus
import Amazonka.ElastiCache.Types.UserGroupsUpdateStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The settings to be applied to the Redis replication group, either
-- immediately or during the next maintenance window.
--
-- /See:/ 'newReplicationGroupPendingModifiedValues' smart constructor.
data ReplicationGroupPendingModifiedValues = ReplicationGroupPendingModifiedValues'
  { -- | The auth token status
    authTokenStatus :: Prelude.Maybe AuthTokenUpdateStatus,
    -- | The user group being modified.
    userGroups :: Prelude.Maybe UserGroupsUpdateStatus,
    -- | The log delivery configurations being modified
    logDeliveryConfigurations :: Prelude.Maybe [PendingLogDeliveryConfiguration],
    -- | The status of an online resharding operation.
    resharding :: Prelude.Maybe ReshardingStatus,
    -- | The primary cluster ID that is applied immediately (if
    -- @--apply-immediately@ was specified), or during the next maintenance
    -- window.
    primaryClusterId :: Prelude.Maybe Prelude.Text,
    -- | Indicates the status of automatic failover for this Redis replication
    -- group.
    automaticFailoverStatus :: Prelude.Maybe PendingAutomaticFailoverStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationGroupPendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authTokenStatus', 'replicationGroupPendingModifiedValues_authTokenStatus' - The auth token status
--
-- 'userGroups', 'replicationGroupPendingModifiedValues_userGroups' - The user group being modified.
--
-- 'logDeliveryConfigurations', 'replicationGroupPendingModifiedValues_logDeliveryConfigurations' - The log delivery configurations being modified
--
-- 'resharding', 'replicationGroupPendingModifiedValues_resharding' - The status of an online resharding operation.
--
-- 'primaryClusterId', 'replicationGroupPendingModifiedValues_primaryClusterId' - The primary cluster ID that is applied immediately (if
-- @--apply-immediately@ was specified), or during the next maintenance
-- window.
--
-- 'automaticFailoverStatus', 'replicationGroupPendingModifiedValues_automaticFailoverStatus' - Indicates the status of automatic failover for this Redis replication
-- group.
newReplicationGroupPendingModifiedValues ::
  ReplicationGroupPendingModifiedValues
newReplicationGroupPendingModifiedValues =
  ReplicationGroupPendingModifiedValues'
    { authTokenStatus =
        Prelude.Nothing,
      userGroups = Prelude.Nothing,
      logDeliveryConfigurations =
        Prelude.Nothing,
      resharding = Prelude.Nothing,
      primaryClusterId = Prelude.Nothing,
      automaticFailoverStatus =
        Prelude.Nothing
    }

-- | The auth token status
replicationGroupPendingModifiedValues_authTokenStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe AuthTokenUpdateStatus)
replicationGroupPendingModifiedValues_authTokenStatus = Lens.lens (\ReplicationGroupPendingModifiedValues' {authTokenStatus} -> authTokenStatus) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {authTokenStatus = a} :: ReplicationGroupPendingModifiedValues)

-- | The user group being modified.
replicationGroupPendingModifiedValues_userGroups :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe UserGroupsUpdateStatus)
replicationGroupPendingModifiedValues_userGroups = Lens.lens (\ReplicationGroupPendingModifiedValues' {userGroups} -> userGroups) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {userGroups = a} :: ReplicationGroupPendingModifiedValues)

-- | The log delivery configurations being modified
replicationGroupPendingModifiedValues_logDeliveryConfigurations :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe [PendingLogDeliveryConfiguration])
replicationGroupPendingModifiedValues_logDeliveryConfigurations = Lens.lens (\ReplicationGroupPendingModifiedValues' {logDeliveryConfigurations} -> logDeliveryConfigurations) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {logDeliveryConfigurations = a} :: ReplicationGroupPendingModifiedValues) Prelude.. Lens.mapping Lens.coerced

-- | The status of an online resharding operation.
replicationGroupPendingModifiedValues_resharding :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe ReshardingStatus)
replicationGroupPendingModifiedValues_resharding = Lens.lens (\ReplicationGroupPendingModifiedValues' {resharding} -> resharding) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {resharding = a} :: ReplicationGroupPendingModifiedValues)

-- | The primary cluster ID that is applied immediately (if
-- @--apply-immediately@ was specified), or during the next maintenance
-- window.
replicationGroupPendingModifiedValues_primaryClusterId :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe Prelude.Text)
replicationGroupPendingModifiedValues_primaryClusterId = Lens.lens (\ReplicationGroupPendingModifiedValues' {primaryClusterId} -> primaryClusterId) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {primaryClusterId = a} :: ReplicationGroupPendingModifiedValues)

-- | Indicates the status of automatic failover for this Redis replication
-- group.
replicationGroupPendingModifiedValues_automaticFailoverStatus :: Lens.Lens' ReplicationGroupPendingModifiedValues (Prelude.Maybe PendingAutomaticFailoverStatus)
replicationGroupPendingModifiedValues_automaticFailoverStatus = Lens.lens (\ReplicationGroupPendingModifiedValues' {automaticFailoverStatus} -> automaticFailoverStatus) (\s@ReplicationGroupPendingModifiedValues' {} a -> s {automaticFailoverStatus = a} :: ReplicationGroupPendingModifiedValues)

instance
  Core.FromXML
    ReplicationGroupPendingModifiedValues
  where
  parseXML x =
    ReplicationGroupPendingModifiedValues'
      Prelude.<$> (x Core..@? "AuthTokenStatus")
      Prelude.<*> (x Core..@? "UserGroups")
      Prelude.<*> ( x Core..@? "LogDeliveryConfigurations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Resharding")
      Prelude.<*> (x Core..@? "PrimaryClusterId")
      Prelude.<*> (x Core..@? "AutomaticFailoverStatus")

instance
  Prelude.Hashable
    ReplicationGroupPendingModifiedValues
  where
  hashWithSalt
    salt'
    ReplicationGroupPendingModifiedValues' {..} =
      salt'
        `Prelude.hashWithSalt` automaticFailoverStatus
        `Prelude.hashWithSalt` primaryClusterId
        `Prelude.hashWithSalt` resharding
        `Prelude.hashWithSalt` logDeliveryConfigurations
        `Prelude.hashWithSalt` userGroups
        `Prelude.hashWithSalt` authTokenStatus

instance
  Prelude.NFData
    ReplicationGroupPendingModifiedValues
  where
  rnf ReplicationGroupPendingModifiedValues' {..} =
    Prelude.rnf authTokenStatus
      `Prelude.seq` Prelude.rnf automaticFailoverStatus
      `Prelude.seq` Prelude.rnf primaryClusterId
      `Prelude.seq` Prelude.rnf resharding
      `Prelude.seq` Prelude.rnf logDeliveryConfigurations
      `Prelude.seq` Prelude.rnf userGroups
