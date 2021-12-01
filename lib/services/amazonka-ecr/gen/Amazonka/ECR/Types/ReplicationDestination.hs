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
-- Module      : Amazonka.ECR.Types.ReplicationDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ReplicationDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An array of objects representing the destination for a replication rule.
--
-- /See:/ 'newReplicationDestination' smart constructor.
data ReplicationDestination = ReplicationDestination'
  { -- | The Region to replicate to.
    region :: Prelude.Text,
    -- | The Amazon Web Services account ID of the Amazon ECR private registry to
    -- replicate to. When configuring cross-Region replication within your own
    -- registry, specify your own account ID.
    registryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'region', 'replicationDestination_region' - The Region to replicate to.
--
-- 'registryId', 'replicationDestination_registryId' - The Amazon Web Services account ID of the Amazon ECR private registry to
-- replicate to. When configuring cross-Region replication within your own
-- registry, specify your own account ID.
newReplicationDestination ::
  -- | 'region'
  Prelude.Text ->
  -- | 'registryId'
  Prelude.Text ->
  ReplicationDestination
newReplicationDestination pRegion_ pRegistryId_ =
  ReplicationDestination'
    { region = pRegion_,
      registryId = pRegistryId_
    }

-- | The Region to replicate to.
replicationDestination_region :: Lens.Lens' ReplicationDestination Prelude.Text
replicationDestination_region = Lens.lens (\ReplicationDestination' {region} -> region) (\s@ReplicationDestination' {} a -> s {region = a} :: ReplicationDestination)

-- | The Amazon Web Services account ID of the Amazon ECR private registry to
-- replicate to. When configuring cross-Region replication within your own
-- registry, specify your own account ID.
replicationDestination_registryId :: Lens.Lens' ReplicationDestination Prelude.Text
replicationDestination_registryId = Lens.lens (\ReplicationDestination' {registryId} -> registryId) (\s@ReplicationDestination' {} a -> s {registryId = a} :: ReplicationDestination)

instance Core.FromJSON ReplicationDestination where
  parseJSON =
    Core.withObject
      "ReplicationDestination"
      ( \x ->
          ReplicationDestination'
            Prelude.<$> (x Core..: "region")
            Prelude.<*> (x Core..: "registryId")
      )

instance Prelude.Hashable ReplicationDestination where
  hashWithSalt salt' ReplicationDestination' {..} =
    salt' `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` region

instance Prelude.NFData ReplicationDestination where
  rnf ReplicationDestination' {..} =
    Prelude.rnf region
      `Prelude.seq` Prelude.rnf registryId

instance Core.ToJSON ReplicationDestination where
  toJSON ReplicationDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("region" Core..= region),
            Prelude.Just ("registryId" Core..= registryId)
          ]
      )
