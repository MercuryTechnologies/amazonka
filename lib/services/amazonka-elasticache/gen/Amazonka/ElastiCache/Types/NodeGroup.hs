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
-- Module      : Amazonka.ElastiCache.Types.NodeGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.NodeGroup where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.Endpoint
import Amazonka.ElastiCache.Types.NodeGroupMember
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a collection of cache nodes in a replication group. One node
-- in the node group is the read\/write primary node. All the other nodes
-- are read-only Replica nodes.
--
-- /See:/ 'newNodeGroup' smart constructor.
data NodeGroup = NodeGroup'
  { -- | The current state of this replication group - @creating@, @available@,
    -- @modifying@, @deleting@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The endpoint of the primary node in this node group (shard).
    primaryEndpoint :: Prelude.Maybe Endpoint,
    -- | The keyspace for this node group (shard).
    slots :: Prelude.Maybe Prelude.Text,
    -- | A list containing information about individual nodes within the node
    -- group (shard).
    nodeGroupMembers :: Prelude.Maybe [NodeGroupMember],
    -- | The identifier for the node group (shard). A Redis (cluster mode
    -- disabled) replication group contains only 1 node group; therefore, the
    -- node group ID is 0001. A Redis (cluster mode enabled) replication group
    -- contains 1 to 90 node groups numbered 0001 to 0090. Optionally, the user
    -- can provide the id for a node group.
    nodeGroupId :: Prelude.Maybe Prelude.Text,
    -- | The endpoint of the replica nodes in this node group (shard).
    readerEndpoint :: Prelude.Maybe Endpoint
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'nodeGroup_status' - The current state of this replication group - @creating@, @available@,
-- @modifying@, @deleting@.
--
-- 'primaryEndpoint', 'nodeGroup_primaryEndpoint' - The endpoint of the primary node in this node group (shard).
--
-- 'slots', 'nodeGroup_slots' - The keyspace for this node group (shard).
--
-- 'nodeGroupMembers', 'nodeGroup_nodeGroupMembers' - A list containing information about individual nodes within the node
-- group (shard).
--
-- 'nodeGroupId', 'nodeGroup_nodeGroupId' - The identifier for the node group (shard). A Redis (cluster mode
-- disabled) replication group contains only 1 node group; therefore, the
-- node group ID is 0001. A Redis (cluster mode enabled) replication group
-- contains 1 to 90 node groups numbered 0001 to 0090. Optionally, the user
-- can provide the id for a node group.
--
-- 'readerEndpoint', 'nodeGroup_readerEndpoint' - The endpoint of the replica nodes in this node group (shard).
newNodeGroup ::
  NodeGroup
newNodeGroup =
  NodeGroup'
    { status = Prelude.Nothing,
      primaryEndpoint = Prelude.Nothing,
      slots = Prelude.Nothing,
      nodeGroupMembers = Prelude.Nothing,
      nodeGroupId = Prelude.Nothing,
      readerEndpoint = Prelude.Nothing
    }

-- | The current state of this replication group - @creating@, @available@,
-- @modifying@, @deleting@.
nodeGroup_status :: Lens.Lens' NodeGroup (Prelude.Maybe Prelude.Text)
nodeGroup_status = Lens.lens (\NodeGroup' {status} -> status) (\s@NodeGroup' {} a -> s {status = a} :: NodeGroup)

-- | The endpoint of the primary node in this node group (shard).
nodeGroup_primaryEndpoint :: Lens.Lens' NodeGroup (Prelude.Maybe Endpoint)
nodeGroup_primaryEndpoint = Lens.lens (\NodeGroup' {primaryEndpoint} -> primaryEndpoint) (\s@NodeGroup' {} a -> s {primaryEndpoint = a} :: NodeGroup)

-- | The keyspace for this node group (shard).
nodeGroup_slots :: Lens.Lens' NodeGroup (Prelude.Maybe Prelude.Text)
nodeGroup_slots = Lens.lens (\NodeGroup' {slots} -> slots) (\s@NodeGroup' {} a -> s {slots = a} :: NodeGroup)

-- | A list containing information about individual nodes within the node
-- group (shard).
nodeGroup_nodeGroupMembers :: Lens.Lens' NodeGroup (Prelude.Maybe [NodeGroupMember])
nodeGroup_nodeGroupMembers = Lens.lens (\NodeGroup' {nodeGroupMembers} -> nodeGroupMembers) (\s@NodeGroup' {} a -> s {nodeGroupMembers = a} :: NodeGroup) Prelude.. Lens.mapping Lens.coerced

-- | The identifier for the node group (shard). A Redis (cluster mode
-- disabled) replication group contains only 1 node group; therefore, the
-- node group ID is 0001. A Redis (cluster mode enabled) replication group
-- contains 1 to 90 node groups numbered 0001 to 0090. Optionally, the user
-- can provide the id for a node group.
nodeGroup_nodeGroupId :: Lens.Lens' NodeGroup (Prelude.Maybe Prelude.Text)
nodeGroup_nodeGroupId = Lens.lens (\NodeGroup' {nodeGroupId} -> nodeGroupId) (\s@NodeGroup' {} a -> s {nodeGroupId = a} :: NodeGroup)

-- | The endpoint of the replica nodes in this node group (shard).
nodeGroup_readerEndpoint :: Lens.Lens' NodeGroup (Prelude.Maybe Endpoint)
nodeGroup_readerEndpoint = Lens.lens (\NodeGroup' {readerEndpoint} -> readerEndpoint) (\s@NodeGroup' {} a -> s {readerEndpoint = a} :: NodeGroup)

instance Core.FromXML NodeGroup where
  parseXML x =
    NodeGroup'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "PrimaryEndpoint")
      Prelude.<*> (x Core..@? "Slots")
      Prelude.<*> ( x Core..@? "NodeGroupMembers"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "NodeGroupMember")
                  )
      Prelude.<*> (x Core..@? "NodeGroupId")
      Prelude.<*> (x Core..@? "ReaderEndpoint")

instance Prelude.Hashable NodeGroup where
  hashWithSalt salt' NodeGroup' {..} =
    salt' `Prelude.hashWithSalt` readerEndpoint
      `Prelude.hashWithSalt` nodeGroupId
      `Prelude.hashWithSalt` nodeGroupMembers
      `Prelude.hashWithSalt` slots
      `Prelude.hashWithSalt` primaryEndpoint
      `Prelude.hashWithSalt` status

instance Prelude.NFData NodeGroup where
  rnf NodeGroup' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf readerEndpoint
      `Prelude.seq` Prelude.rnf nodeGroupId
      `Prelude.seq` Prelude.rnf nodeGroupMembers
      `Prelude.seq` Prelude.rnf slots
      `Prelude.seq` Prelude.rnf primaryEndpoint
