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
-- Module      : Amazonka.Panorama.Types.Node
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.Node where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Panorama.Types.NodeCategory
import qualified Amazonka.Prelude as Prelude

-- | An application node that represents a camera stream, a model, code, or
-- output.
--
-- /See:/ 'newNode' smart constructor.
data Node = Node'
  { -- | The node\'s ARN.
    packageArn :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the node\'s owner.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The node\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The node\'s ID.
    nodeId :: Prelude.Text,
    -- | The node\'s name.
    name :: Prelude.Text,
    -- | The node\'s category.
    category :: NodeCategory,
    -- | The node\'s package name.
    packageName :: Prelude.Text,
    -- | The node\'s package ID.
    packageId :: Prelude.Text,
    -- | The node\'s package version.
    packageVersion :: Prelude.Text,
    -- | The node\'s patch version.
    patchVersion :: Prelude.Text,
    -- | When the node was created.
    createdTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Node' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'packageArn', 'node_packageArn' - The node\'s ARN.
--
-- 'ownerAccount', 'node_ownerAccount' - The account ID of the node\'s owner.
--
-- 'description', 'node_description' - The node\'s description.
--
-- 'nodeId', 'node_nodeId' - The node\'s ID.
--
-- 'name', 'node_name' - The node\'s name.
--
-- 'category', 'node_category' - The node\'s category.
--
-- 'packageName', 'node_packageName' - The node\'s package name.
--
-- 'packageId', 'node_packageId' - The node\'s package ID.
--
-- 'packageVersion', 'node_packageVersion' - The node\'s package version.
--
-- 'patchVersion', 'node_patchVersion' - The node\'s patch version.
--
-- 'createdTime', 'node_createdTime' - When the node was created.
newNode ::
  -- | 'nodeId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'category'
  NodeCategory ->
  -- | 'packageName'
  Prelude.Text ->
  -- | 'packageId'
  Prelude.Text ->
  -- | 'packageVersion'
  Prelude.Text ->
  -- | 'patchVersion'
  Prelude.Text ->
  -- | 'createdTime'
  Prelude.UTCTime ->
  Node
newNode
  pNodeId_
  pName_
  pCategory_
  pPackageName_
  pPackageId_
  pPackageVersion_
  pPatchVersion_
  pCreatedTime_ =
    Node'
      { packageArn = Prelude.Nothing,
        ownerAccount = Prelude.Nothing,
        description = Prelude.Nothing,
        nodeId = pNodeId_,
        name = pName_,
        category = pCategory_,
        packageName = pPackageName_,
        packageId = pPackageId_,
        packageVersion = pPackageVersion_,
        patchVersion = pPatchVersion_,
        createdTime = Core._Time Lens.# pCreatedTime_
      }

-- | The node\'s ARN.
node_packageArn :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_packageArn = Lens.lens (\Node' {packageArn} -> packageArn) (\s@Node' {} a -> s {packageArn = a} :: Node)

-- | The account ID of the node\'s owner.
node_ownerAccount :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_ownerAccount = Lens.lens (\Node' {ownerAccount} -> ownerAccount) (\s@Node' {} a -> s {ownerAccount = a} :: Node)

-- | The node\'s description.
node_description :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_description = Lens.lens (\Node' {description} -> description) (\s@Node' {} a -> s {description = a} :: Node)

-- | The node\'s ID.
node_nodeId :: Lens.Lens' Node Prelude.Text
node_nodeId = Lens.lens (\Node' {nodeId} -> nodeId) (\s@Node' {} a -> s {nodeId = a} :: Node)

-- | The node\'s name.
node_name :: Lens.Lens' Node Prelude.Text
node_name = Lens.lens (\Node' {name} -> name) (\s@Node' {} a -> s {name = a} :: Node)

-- | The node\'s category.
node_category :: Lens.Lens' Node NodeCategory
node_category = Lens.lens (\Node' {category} -> category) (\s@Node' {} a -> s {category = a} :: Node)

-- | The node\'s package name.
node_packageName :: Lens.Lens' Node Prelude.Text
node_packageName = Lens.lens (\Node' {packageName} -> packageName) (\s@Node' {} a -> s {packageName = a} :: Node)

-- | The node\'s package ID.
node_packageId :: Lens.Lens' Node Prelude.Text
node_packageId = Lens.lens (\Node' {packageId} -> packageId) (\s@Node' {} a -> s {packageId = a} :: Node)

-- | The node\'s package version.
node_packageVersion :: Lens.Lens' Node Prelude.Text
node_packageVersion = Lens.lens (\Node' {packageVersion} -> packageVersion) (\s@Node' {} a -> s {packageVersion = a} :: Node)

-- | The node\'s patch version.
node_patchVersion :: Lens.Lens' Node Prelude.Text
node_patchVersion = Lens.lens (\Node' {patchVersion} -> patchVersion) (\s@Node' {} a -> s {patchVersion = a} :: Node)

-- | When the node was created.
node_createdTime :: Lens.Lens' Node Prelude.UTCTime
node_createdTime = Lens.lens (\Node' {createdTime} -> createdTime) (\s@Node' {} a -> s {createdTime = a} :: Node) Prelude.. Core._Time

instance Core.FromJSON Node where
  parseJSON =
    Core.withObject
      "Node"
      ( \x ->
          Node'
            Prelude.<$> (x Core..:? "PackageArn")
            Prelude.<*> (x Core..:? "OwnerAccount")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..: "NodeId")
            Prelude.<*> (x Core..: "Name")
            Prelude.<*> (x Core..: "Category")
            Prelude.<*> (x Core..: "PackageName")
            Prelude.<*> (x Core..: "PackageId")
            Prelude.<*> (x Core..: "PackageVersion")
            Prelude.<*> (x Core..: "PatchVersion")
            Prelude.<*> (x Core..: "CreatedTime")
      )

instance Prelude.Hashable Node where
  hashWithSalt salt' Node' {..} =
    salt' `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` patchVersion
      `Prelude.hashWithSalt` packageVersion
      `Prelude.hashWithSalt` packageId
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` category
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nodeId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` packageArn

instance Prelude.NFData Node where
  rnf Node' {..} =
    Prelude.rnf packageArn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf patchVersion
      `Prelude.seq` Prelude.rnf packageVersion
      `Prelude.seq` Prelude.rnf packageId
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf category
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nodeId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf ownerAccount
