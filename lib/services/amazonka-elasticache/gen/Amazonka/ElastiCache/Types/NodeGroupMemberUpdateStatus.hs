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
-- Module      : Amazonka.ElastiCache.Types.NodeGroupMemberUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.NodeGroupMemberUpdateStatus where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.NodeUpdateInitiatedBy
import Amazonka.ElastiCache.Types.NodeUpdateStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The status of the service update on the node group member
--
-- /See:/ 'newNodeGroupMemberUpdateStatus' smart constructor.
data NodeGroupMemberUpdateStatus = NodeGroupMemberUpdateStatus'
  { -- | The end date of the update for a node
    nodeUpdateEndDate :: Prelude.Maybe Core.ISO8601,
    -- | Reflects whether the update was initiated by the customer or
    -- automatically applied
    nodeUpdateInitiatedBy :: Prelude.Maybe NodeUpdateInitiatedBy,
    -- | The date when the NodeUpdateStatus was last modified
    nodeUpdateStatusModifiedDate :: Prelude.Maybe Core.ISO8601,
    -- | The cache cluster ID
    cacheClusterId :: Prelude.Maybe Prelude.Text,
    -- | The node ID of the cache cluster
    cacheNodeId :: Prelude.Maybe Prelude.Text,
    -- | The date when the update is triggered
    nodeUpdateInitiatedDate :: Prelude.Maybe Core.ISO8601,
    -- | The start date of the update for a node
    nodeUpdateStartDate :: Prelude.Maybe Core.ISO8601,
    -- | The update status of the node
    nodeUpdateStatus :: Prelude.Maybe NodeUpdateStatus,
    -- | The deletion date of the node
    nodeDeletionDate :: Prelude.Maybe Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NodeGroupMemberUpdateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nodeUpdateEndDate', 'nodeGroupMemberUpdateStatus_nodeUpdateEndDate' - The end date of the update for a node
--
-- 'nodeUpdateInitiatedBy', 'nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy' - Reflects whether the update was initiated by the customer or
-- automatically applied
--
-- 'nodeUpdateStatusModifiedDate', 'nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate' - The date when the NodeUpdateStatus was last modified
--
-- 'cacheClusterId', 'nodeGroupMemberUpdateStatus_cacheClusterId' - The cache cluster ID
--
-- 'cacheNodeId', 'nodeGroupMemberUpdateStatus_cacheNodeId' - The node ID of the cache cluster
--
-- 'nodeUpdateInitiatedDate', 'nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate' - The date when the update is triggered
--
-- 'nodeUpdateStartDate', 'nodeGroupMemberUpdateStatus_nodeUpdateStartDate' - The start date of the update for a node
--
-- 'nodeUpdateStatus', 'nodeGroupMemberUpdateStatus_nodeUpdateStatus' - The update status of the node
--
-- 'nodeDeletionDate', 'nodeGroupMemberUpdateStatus_nodeDeletionDate' - The deletion date of the node
newNodeGroupMemberUpdateStatus ::
  NodeGroupMemberUpdateStatus
newNodeGroupMemberUpdateStatus =
  NodeGroupMemberUpdateStatus'
    { nodeUpdateEndDate =
        Prelude.Nothing,
      nodeUpdateInitiatedBy = Prelude.Nothing,
      nodeUpdateStatusModifiedDate = Prelude.Nothing,
      cacheClusterId = Prelude.Nothing,
      cacheNodeId = Prelude.Nothing,
      nodeUpdateInitiatedDate = Prelude.Nothing,
      nodeUpdateStartDate = Prelude.Nothing,
      nodeUpdateStatus = Prelude.Nothing,
      nodeDeletionDate = Prelude.Nothing
    }

-- | The end date of the update for a node
nodeGroupMemberUpdateStatus_nodeUpdateEndDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateEndDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateEndDate} -> nodeUpdateEndDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateEndDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Core._Time

-- | Reflects whether the update was initiated by the customer or
-- automatically applied
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe NodeUpdateInitiatedBy)
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedBy = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateInitiatedBy} -> nodeUpdateInitiatedBy) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateInitiatedBy = a} :: NodeGroupMemberUpdateStatus)

-- | The date when the NodeUpdateStatus was last modified
nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateStatusModifiedDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStatusModifiedDate} -> nodeUpdateStatusModifiedDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStatusModifiedDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Core._Time

-- | The cache cluster ID
nodeGroupMemberUpdateStatus_cacheClusterId :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.Text)
nodeGroupMemberUpdateStatus_cacheClusterId = Lens.lens (\NodeGroupMemberUpdateStatus' {cacheClusterId} -> cacheClusterId) (\s@NodeGroupMemberUpdateStatus' {} a -> s {cacheClusterId = a} :: NodeGroupMemberUpdateStatus)

-- | The node ID of the cache cluster
nodeGroupMemberUpdateStatus_cacheNodeId :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.Text)
nodeGroupMemberUpdateStatus_cacheNodeId = Lens.lens (\NodeGroupMemberUpdateStatus' {cacheNodeId} -> cacheNodeId) (\s@NodeGroupMemberUpdateStatus' {} a -> s {cacheNodeId = a} :: NodeGroupMemberUpdateStatus)

-- | The date when the update is triggered
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateInitiatedDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateInitiatedDate} -> nodeUpdateInitiatedDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateInitiatedDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Core._Time

-- | The start date of the update for a node
nodeGroupMemberUpdateStatus_nodeUpdateStartDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeUpdateStartDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStartDate} -> nodeUpdateStartDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStartDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Core._Time

-- | The update status of the node
nodeGroupMemberUpdateStatus_nodeUpdateStatus :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe NodeUpdateStatus)
nodeGroupMemberUpdateStatus_nodeUpdateStatus = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeUpdateStatus} -> nodeUpdateStatus) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeUpdateStatus = a} :: NodeGroupMemberUpdateStatus)

-- | The deletion date of the node
nodeGroupMemberUpdateStatus_nodeDeletionDate :: Lens.Lens' NodeGroupMemberUpdateStatus (Prelude.Maybe Prelude.UTCTime)
nodeGroupMemberUpdateStatus_nodeDeletionDate = Lens.lens (\NodeGroupMemberUpdateStatus' {nodeDeletionDate} -> nodeDeletionDate) (\s@NodeGroupMemberUpdateStatus' {} a -> s {nodeDeletionDate = a} :: NodeGroupMemberUpdateStatus) Prelude.. Lens.mapping Core._Time

instance Core.FromXML NodeGroupMemberUpdateStatus where
  parseXML x =
    NodeGroupMemberUpdateStatus'
      Prelude.<$> (x Core..@? "NodeUpdateEndDate")
      Prelude.<*> (x Core..@? "NodeUpdateInitiatedBy")
      Prelude.<*> (x Core..@? "NodeUpdateStatusModifiedDate")
      Prelude.<*> (x Core..@? "CacheClusterId")
      Prelude.<*> (x Core..@? "CacheNodeId")
      Prelude.<*> (x Core..@? "NodeUpdateInitiatedDate")
      Prelude.<*> (x Core..@? "NodeUpdateStartDate")
      Prelude.<*> (x Core..@? "NodeUpdateStatus")
      Prelude.<*> (x Core..@? "NodeDeletionDate")

instance Prelude.Hashable NodeGroupMemberUpdateStatus where
  hashWithSalt salt' NodeGroupMemberUpdateStatus' {..} =
    salt' `Prelude.hashWithSalt` nodeDeletionDate
      `Prelude.hashWithSalt` nodeUpdateStatus
      `Prelude.hashWithSalt` nodeUpdateStartDate
      `Prelude.hashWithSalt` nodeUpdateInitiatedDate
      `Prelude.hashWithSalt` cacheNodeId
      `Prelude.hashWithSalt` cacheClusterId
      `Prelude.hashWithSalt` nodeUpdateStatusModifiedDate
      `Prelude.hashWithSalt` nodeUpdateInitiatedBy
      `Prelude.hashWithSalt` nodeUpdateEndDate

instance Prelude.NFData NodeGroupMemberUpdateStatus where
  rnf NodeGroupMemberUpdateStatus' {..} =
    Prelude.rnf nodeUpdateEndDate
      `Prelude.seq` Prelude.rnf nodeDeletionDate
      `Prelude.seq` Prelude.rnf nodeUpdateStatus
      `Prelude.seq` Prelude.rnf nodeUpdateStartDate
      `Prelude.seq` Prelude.rnf nodeUpdateInitiatedDate
      `Prelude.seq` Prelude.rnf cacheNodeId
      `Prelude.seq` Prelude.rnf cacheClusterId
      `Prelude.seq` Prelude.rnf nodeUpdateStatusModifiedDate
      `Prelude.seq` Prelude.rnf nodeUpdateInitiatedBy
