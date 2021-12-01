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
-- Module      : Amazonka.ElastiCache.Types.NodeGroupConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.NodeGroupConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Node group (shard) configuration options. Each node group (shard)
-- configuration has the following: @Slots@, @PrimaryAvailabilityZone@,
-- @ReplicaAvailabilityZones@, @ReplicaCount@.
--
-- /See:/ 'newNodeGroupConfiguration' smart constructor.
data NodeGroupConfiguration = NodeGroupConfiguration'
  { -- | A string that specifies the keyspace for a particular node group.
    -- Keyspaces range from 0 to 16,383. The string is in the format
    -- @startkey-endkey@.
    --
    -- Example: @\"0-3999\"@
    slots :: Prelude.Maybe Prelude.Text,
    -- | The outpost ARN of the node replicas.
    replicaOutpostArns :: Prelude.Maybe [Prelude.Text],
    -- | The number of read replica nodes in this node group (shard).
    replicaCount :: Prelude.Maybe Prelude.Int,
    -- | The Availability Zone where the primary node of this node group (shard)
    -- is launched.
    primaryAvailabilityZone :: Prelude.Maybe Prelude.Text,
    -- | A list of Availability Zones to be used for the read replicas. The
    -- number of Availability Zones in this list must match the value of
    -- @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
    replicaAvailabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | The outpost ARN of the primary node.
    primaryOutpostArn :: Prelude.Maybe Prelude.Text,
    -- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied
    -- id for the node group these configuration values apply to.
    nodeGroupId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeGroupConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'slots', 'nodeGroupConfiguration_slots' - A string that specifies the keyspace for a particular node group.
-- Keyspaces range from 0 to 16,383. The string is in the format
-- @startkey-endkey@.
--
-- Example: @\"0-3999\"@
--
-- 'replicaOutpostArns', 'nodeGroupConfiguration_replicaOutpostArns' - The outpost ARN of the node replicas.
--
-- 'replicaCount', 'nodeGroupConfiguration_replicaCount' - The number of read replica nodes in this node group (shard).
--
-- 'primaryAvailabilityZone', 'nodeGroupConfiguration_primaryAvailabilityZone' - The Availability Zone where the primary node of this node group (shard)
-- is launched.
--
-- 'replicaAvailabilityZones', 'nodeGroupConfiguration_replicaAvailabilityZones' - A list of Availability Zones to be used for the read replicas. The
-- number of Availability Zones in this list must match the value of
-- @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
--
-- 'primaryOutpostArn', 'nodeGroupConfiguration_primaryOutpostArn' - The outpost ARN of the primary node.
--
-- 'nodeGroupId', 'nodeGroupConfiguration_nodeGroupId' - Either the ElastiCache for Redis supplied 4-digit id or a user supplied
-- id for the node group these configuration values apply to.
newNodeGroupConfiguration ::
  NodeGroupConfiguration
newNodeGroupConfiguration =
  NodeGroupConfiguration'
    { slots = Prelude.Nothing,
      replicaOutpostArns = Prelude.Nothing,
      replicaCount = Prelude.Nothing,
      primaryAvailabilityZone = Prelude.Nothing,
      replicaAvailabilityZones = Prelude.Nothing,
      primaryOutpostArn = Prelude.Nothing,
      nodeGroupId = Prelude.Nothing
    }

-- | A string that specifies the keyspace for a particular node group.
-- Keyspaces range from 0 to 16,383. The string is in the format
-- @startkey-endkey@.
--
-- Example: @\"0-3999\"@
nodeGroupConfiguration_slots :: Lens.Lens' NodeGroupConfiguration (Prelude.Maybe Prelude.Text)
nodeGroupConfiguration_slots = Lens.lens (\NodeGroupConfiguration' {slots} -> slots) (\s@NodeGroupConfiguration' {} a -> s {slots = a} :: NodeGroupConfiguration)

-- | The outpost ARN of the node replicas.
nodeGroupConfiguration_replicaOutpostArns :: Lens.Lens' NodeGroupConfiguration (Prelude.Maybe [Prelude.Text])
nodeGroupConfiguration_replicaOutpostArns = Lens.lens (\NodeGroupConfiguration' {replicaOutpostArns} -> replicaOutpostArns) (\s@NodeGroupConfiguration' {} a -> s {replicaOutpostArns = a} :: NodeGroupConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The number of read replica nodes in this node group (shard).
nodeGroupConfiguration_replicaCount :: Lens.Lens' NodeGroupConfiguration (Prelude.Maybe Prelude.Int)
nodeGroupConfiguration_replicaCount = Lens.lens (\NodeGroupConfiguration' {replicaCount} -> replicaCount) (\s@NodeGroupConfiguration' {} a -> s {replicaCount = a} :: NodeGroupConfiguration)

-- | The Availability Zone where the primary node of this node group (shard)
-- is launched.
nodeGroupConfiguration_primaryAvailabilityZone :: Lens.Lens' NodeGroupConfiguration (Prelude.Maybe Prelude.Text)
nodeGroupConfiguration_primaryAvailabilityZone = Lens.lens (\NodeGroupConfiguration' {primaryAvailabilityZone} -> primaryAvailabilityZone) (\s@NodeGroupConfiguration' {} a -> s {primaryAvailabilityZone = a} :: NodeGroupConfiguration)

-- | A list of Availability Zones to be used for the read replicas. The
-- number of Availability Zones in this list must match the value of
-- @ReplicaCount@ or @ReplicasPerNodeGroup@ if not specified.
nodeGroupConfiguration_replicaAvailabilityZones :: Lens.Lens' NodeGroupConfiguration (Prelude.Maybe [Prelude.Text])
nodeGroupConfiguration_replicaAvailabilityZones = Lens.lens (\NodeGroupConfiguration' {replicaAvailabilityZones} -> replicaAvailabilityZones) (\s@NodeGroupConfiguration' {} a -> s {replicaAvailabilityZones = a} :: NodeGroupConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The outpost ARN of the primary node.
nodeGroupConfiguration_primaryOutpostArn :: Lens.Lens' NodeGroupConfiguration (Prelude.Maybe Prelude.Text)
nodeGroupConfiguration_primaryOutpostArn = Lens.lens (\NodeGroupConfiguration' {primaryOutpostArn} -> primaryOutpostArn) (\s@NodeGroupConfiguration' {} a -> s {primaryOutpostArn = a} :: NodeGroupConfiguration)

-- | Either the ElastiCache for Redis supplied 4-digit id or a user supplied
-- id for the node group these configuration values apply to.
nodeGroupConfiguration_nodeGroupId :: Lens.Lens' NodeGroupConfiguration (Prelude.Maybe Prelude.Text)
nodeGroupConfiguration_nodeGroupId = Lens.lens (\NodeGroupConfiguration' {nodeGroupId} -> nodeGroupId) (\s@NodeGroupConfiguration' {} a -> s {nodeGroupId = a} :: NodeGroupConfiguration)

instance Core.FromXML NodeGroupConfiguration where
  parseXML x =
    NodeGroupConfiguration'
      Prelude.<$> (x Core..@? "Slots")
      Prelude.<*> ( x Core..@? "ReplicaOutpostArns"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "OutpostArn")
                  )
      Prelude.<*> (x Core..@? "ReplicaCount")
      Prelude.<*> (x Core..@? "PrimaryAvailabilityZone")
      Prelude.<*> ( x Core..@? "ReplicaAvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AvailabilityZone")
                  )
      Prelude.<*> (x Core..@? "PrimaryOutpostArn")
      Prelude.<*> (x Core..@? "NodeGroupId")

instance Prelude.Hashable NodeGroupConfiguration where
  hashWithSalt salt' NodeGroupConfiguration' {..} =
    salt' `Prelude.hashWithSalt` nodeGroupId
      `Prelude.hashWithSalt` primaryOutpostArn
      `Prelude.hashWithSalt` replicaAvailabilityZones
      `Prelude.hashWithSalt` primaryAvailabilityZone
      `Prelude.hashWithSalt` replicaCount
      `Prelude.hashWithSalt` replicaOutpostArns
      `Prelude.hashWithSalt` slots

instance Prelude.NFData NodeGroupConfiguration where
  rnf NodeGroupConfiguration' {..} =
    Prelude.rnf slots
      `Prelude.seq` Prelude.rnf nodeGroupId
      `Prelude.seq` Prelude.rnf primaryOutpostArn
      `Prelude.seq` Prelude.rnf replicaAvailabilityZones
      `Prelude.seq` Prelude.rnf primaryAvailabilityZone
      `Prelude.seq` Prelude.rnf replicaCount
      `Prelude.seq` Prelude.rnf replicaOutpostArns

instance Core.ToQuery NodeGroupConfiguration where
  toQuery NodeGroupConfiguration' {..} =
    Prelude.mconcat
      [ "Slots" Core.=: slots,
        "ReplicaOutpostArns"
          Core.=: Core.toQuery
            ( Core.toQueryList "OutpostArn"
                Prelude.<$> replicaOutpostArns
            ),
        "ReplicaCount" Core.=: replicaCount,
        "PrimaryAvailabilityZone"
          Core.=: primaryAvailabilityZone,
        "ReplicaAvailabilityZones"
          Core.=: Core.toQuery
            ( Core.toQueryList "AvailabilityZone"
                Prelude.<$> replicaAvailabilityZones
            ),
        "PrimaryOutpostArn" Core.=: primaryOutpostArn,
        "NodeGroupId" Core.=: nodeGroupId
      ]
