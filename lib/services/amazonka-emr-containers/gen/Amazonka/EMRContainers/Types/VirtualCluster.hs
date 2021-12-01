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
-- Module      : Amazonka.EMRContainers.Types.VirtualCluster
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.VirtualCluster where

import qualified Amazonka.Core as Core
import Amazonka.EMRContainers.Types.ContainerProvider
import Amazonka.EMRContainers.Types.VirtualClusterState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | This entity describes a virtual cluster. A virtual cluster is a
-- Kubernetes namespace that Amazon EMR is registered with. Amazon EMR uses
-- virtual clusters to run jobs and host endpoints. Multiple virtual
-- clusters can be backed by the same physical cluster. However, each
-- virtual cluster maps to one namespace on an EKS cluster. Virtual
-- clusters do not create any active resources that contribute to your bill
-- or that require lifecycle management outside the service.
--
-- /See:/ 'newVirtualCluster' smart constructor.
data VirtualCluster = VirtualCluster'
  { -- | The state of the virtual cluster.
    state :: Prelude.Maybe VirtualClusterState,
    -- | The ARN of the virtual cluster.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the virtual cluster is created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The name of the virtual cluster.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual cluster.
    id :: Prelude.Maybe Prelude.Text,
    -- | The container provider of the virtual cluster.
    containerProvider :: Prelude.Maybe ContainerProvider,
    -- | The assigned tags of the virtual cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'virtualCluster_state' - The state of the virtual cluster.
--
-- 'arn', 'virtualCluster_arn' - The ARN of the virtual cluster.
--
-- 'createdAt', 'virtualCluster_createdAt' - The date and time when the virtual cluster is created.
--
-- 'name', 'virtualCluster_name' - The name of the virtual cluster.
--
-- 'id', 'virtualCluster_id' - The ID of the virtual cluster.
--
-- 'containerProvider', 'virtualCluster_containerProvider' - The container provider of the virtual cluster.
--
-- 'tags', 'virtualCluster_tags' - The assigned tags of the virtual cluster.
newVirtualCluster ::
  VirtualCluster
newVirtualCluster =
  VirtualCluster'
    { state = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      containerProvider = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The state of the virtual cluster.
virtualCluster_state :: Lens.Lens' VirtualCluster (Prelude.Maybe VirtualClusterState)
virtualCluster_state = Lens.lens (\VirtualCluster' {state} -> state) (\s@VirtualCluster' {} a -> s {state = a} :: VirtualCluster)

-- | The ARN of the virtual cluster.
virtualCluster_arn :: Lens.Lens' VirtualCluster (Prelude.Maybe Prelude.Text)
virtualCluster_arn = Lens.lens (\VirtualCluster' {arn} -> arn) (\s@VirtualCluster' {} a -> s {arn = a} :: VirtualCluster)

-- | The date and time when the virtual cluster is created.
virtualCluster_createdAt :: Lens.Lens' VirtualCluster (Prelude.Maybe Prelude.UTCTime)
virtualCluster_createdAt = Lens.lens (\VirtualCluster' {createdAt} -> createdAt) (\s@VirtualCluster' {} a -> s {createdAt = a} :: VirtualCluster) Prelude.. Lens.mapping Core._Time

-- | The name of the virtual cluster.
virtualCluster_name :: Lens.Lens' VirtualCluster (Prelude.Maybe Prelude.Text)
virtualCluster_name = Lens.lens (\VirtualCluster' {name} -> name) (\s@VirtualCluster' {} a -> s {name = a} :: VirtualCluster)

-- | The ID of the virtual cluster.
virtualCluster_id :: Lens.Lens' VirtualCluster (Prelude.Maybe Prelude.Text)
virtualCluster_id = Lens.lens (\VirtualCluster' {id} -> id) (\s@VirtualCluster' {} a -> s {id = a} :: VirtualCluster)

-- | The container provider of the virtual cluster.
virtualCluster_containerProvider :: Lens.Lens' VirtualCluster (Prelude.Maybe ContainerProvider)
virtualCluster_containerProvider = Lens.lens (\VirtualCluster' {containerProvider} -> containerProvider) (\s@VirtualCluster' {} a -> s {containerProvider = a} :: VirtualCluster)

-- | The assigned tags of the virtual cluster.
virtualCluster_tags :: Lens.Lens' VirtualCluster (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
virtualCluster_tags = Lens.lens (\VirtualCluster' {tags} -> tags) (\s@VirtualCluster' {} a -> s {tags = a} :: VirtualCluster) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON VirtualCluster where
  parseJSON =
    Core.withObject
      "VirtualCluster"
      ( \x ->
          VirtualCluster'
            Prelude.<$> (x Core..:? "state")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "containerProvider")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable VirtualCluster where
  hashWithSalt salt' VirtualCluster' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` containerProvider
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state

instance Prelude.NFData VirtualCluster where
  rnf VirtualCluster' {..} =
    Prelude.rnf state `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf containerProvider
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
