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
-- Module      : Amazonka.ElastiCache.Types.PendingModifiedValues
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.PendingModifiedValues where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.AuthTokenUpdateStatus
import Amazonka.ElastiCache.Types.PendingLogDeliveryConfiguration
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A group of settings that are applied to the cluster in the future, or
-- that are currently being applied.
--
-- /See:/ 'newPendingModifiedValues' smart constructor.
data PendingModifiedValues = PendingModifiedValues'
  { -- | The new cache engine version that the cluster runs.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | The cache node type that this cluster or replication group is scaled to.
    cacheNodeType :: Prelude.Maybe Prelude.Text,
    -- | The auth token status
    authTokenStatus :: Prelude.Maybe AuthTokenUpdateStatus,
    -- | The log delivery configurations being modified
    logDeliveryConfigurations :: Prelude.Maybe [PendingLogDeliveryConfiguration],
    -- | A list of cache node IDs that are being removed (or will be removed)
    -- from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002,
    -- etc.).
    cacheNodeIdsToRemove :: Prelude.Maybe [Prelude.Text],
    -- | The new number of cache nodes for the cluster.
    --
    -- For clusters running Redis, this value must be 1. For clusters running
    -- Memcached, this value must be between 1 and 40.
    numCacheNodes :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingModifiedValues' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'pendingModifiedValues_engineVersion' - The new cache engine version that the cluster runs.
--
-- 'cacheNodeType', 'pendingModifiedValues_cacheNodeType' - The cache node type that this cluster or replication group is scaled to.
--
-- 'authTokenStatus', 'pendingModifiedValues_authTokenStatus' - The auth token status
--
-- 'logDeliveryConfigurations', 'pendingModifiedValues_logDeliveryConfigurations' - The log delivery configurations being modified
--
-- 'cacheNodeIdsToRemove', 'pendingModifiedValues_cacheNodeIdsToRemove' - A list of cache node IDs that are being removed (or will be removed)
-- from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002,
-- etc.).
--
-- 'numCacheNodes', 'pendingModifiedValues_numCacheNodes' - The new number of cache nodes for the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 40.
newPendingModifiedValues ::
  PendingModifiedValues
newPendingModifiedValues =
  PendingModifiedValues'
    { engineVersion =
        Prelude.Nothing,
      cacheNodeType = Prelude.Nothing,
      authTokenStatus = Prelude.Nothing,
      logDeliveryConfigurations = Prelude.Nothing,
      cacheNodeIdsToRemove = Prelude.Nothing,
      numCacheNodes = Prelude.Nothing
    }

-- | The new cache engine version that the cluster runs.
pendingModifiedValues_engineVersion :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_engineVersion = Lens.lens (\PendingModifiedValues' {engineVersion} -> engineVersion) (\s@PendingModifiedValues' {} a -> s {engineVersion = a} :: PendingModifiedValues)

-- | The cache node type that this cluster or replication group is scaled to.
pendingModifiedValues_cacheNodeType :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Text)
pendingModifiedValues_cacheNodeType = Lens.lens (\PendingModifiedValues' {cacheNodeType} -> cacheNodeType) (\s@PendingModifiedValues' {} a -> s {cacheNodeType = a} :: PendingModifiedValues)

-- | The auth token status
pendingModifiedValues_authTokenStatus :: Lens.Lens' PendingModifiedValues (Prelude.Maybe AuthTokenUpdateStatus)
pendingModifiedValues_authTokenStatus = Lens.lens (\PendingModifiedValues' {authTokenStatus} -> authTokenStatus) (\s@PendingModifiedValues' {} a -> s {authTokenStatus = a} :: PendingModifiedValues)

-- | The log delivery configurations being modified
pendingModifiedValues_logDeliveryConfigurations :: Lens.Lens' PendingModifiedValues (Prelude.Maybe [PendingLogDeliveryConfiguration])
pendingModifiedValues_logDeliveryConfigurations = Lens.lens (\PendingModifiedValues' {logDeliveryConfigurations} -> logDeliveryConfigurations) (\s@PendingModifiedValues' {} a -> s {logDeliveryConfigurations = a} :: PendingModifiedValues) Prelude.. Lens.mapping Lens.coerced

-- | A list of cache node IDs that are being removed (or will be removed)
-- from the cluster. A node ID is a 4-digit numeric identifier (0001, 0002,
-- etc.).
pendingModifiedValues_cacheNodeIdsToRemove :: Lens.Lens' PendingModifiedValues (Prelude.Maybe [Prelude.Text])
pendingModifiedValues_cacheNodeIdsToRemove = Lens.lens (\PendingModifiedValues' {cacheNodeIdsToRemove} -> cacheNodeIdsToRemove) (\s@PendingModifiedValues' {} a -> s {cacheNodeIdsToRemove = a} :: PendingModifiedValues) Prelude.. Lens.mapping Lens.coerced

-- | The new number of cache nodes for the cluster.
--
-- For clusters running Redis, this value must be 1. For clusters running
-- Memcached, this value must be between 1 and 40.
pendingModifiedValues_numCacheNodes :: Lens.Lens' PendingModifiedValues (Prelude.Maybe Prelude.Int)
pendingModifiedValues_numCacheNodes = Lens.lens (\PendingModifiedValues' {numCacheNodes} -> numCacheNodes) (\s@PendingModifiedValues' {} a -> s {numCacheNodes = a} :: PendingModifiedValues)

instance Core.FromXML PendingModifiedValues where
  parseXML x =
    PendingModifiedValues'
      Prelude.<$> (x Core..@? "EngineVersion")
      Prelude.<*> (x Core..@? "CacheNodeType")
      Prelude.<*> (x Core..@? "AuthTokenStatus")
      Prelude.<*> ( x Core..@? "LogDeliveryConfigurations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "CacheNodeIdsToRemove"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "CacheNodeId")
                  )
      Prelude.<*> (x Core..@? "NumCacheNodes")

instance Prelude.Hashable PendingModifiedValues where
  hashWithSalt salt' PendingModifiedValues' {..} =
    salt' `Prelude.hashWithSalt` numCacheNodes
      `Prelude.hashWithSalt` cacheNodeIdsToRemove
      `Prelude.hashWithSalt` logDeliveryConfigurations
      `Prelude.hashWithSalt` authTokenStatus
      `Prelude.hashWithSalt` cacheNodeType
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData PendingModifiedValues where
  rnf PendingModifiedValues' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf numCacheNodes
      `Prelude.seq` Prelude.rnf cacheNodeIdsToRemove
      `Prelude.seq` Prelude.rnf logDeliveryConfigurations
      `Prelude.seq` Prelude.rnf authTokenStatus
      `Prelude.seq` Prelude.rnf cacheNodeType
