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
-- Module      : Amazonka.ECR.Types.ReplicationRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ReplicationRule where

import qualified Amazonka.Core as Core
import Amazonka.ECR.Types.ReplicationDestination
import Amazonka.ECR.Types.RepositoryFilter
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An array of objects representing the replication destinations and
-- repository filters for a replication configuration.
--
-- /See:/ 'newReplicationRule' smart constructor.
data ReplicationRule = ReplicationRule'
  { -- | An array of objects representing the filters for a replication rule.
    -- Specifying a repository filter for a replication rule provides a method
    -- for controlling which repositories in a private registry are replicated.
    repositoryFilters :: Prelude.Maybe (Prelude.NonEmpty RepositoryFilter),
    -- | An array of objects representing the destination for a replication rule.
    destinations :: [ReplicationDestination]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryFilters', 'replicationRule_repositoryFilters' - An array of objects representing the filters for a replication rule.
-- Specifying a repository filter for a replication rule provides a method
-- for controlling which repositories in a private registry are replicated.
--
-- 'destinations', 'replicationRule_destinations' - An array of objects representing the destination for a replication rule.
newReplicationRule ::
  ReplicationRule
newReplicationRule =
  ReplicationRule'
    { repositoryFilters =
        Prelude.Nothing,
      destinations = Prelude.mempty
    }

-- | An array of objects representing the filters for a replication rule.
-- Specifying a repository filter for a replication rule provides a method
-- for controlling which repositories in a private registry are replicated.
replicationRule_repositoryFilters :: Lens.Lens' ReplicationRule (Prelude.Maybe (Prelude.NonEmpty RepositoryFilter))
replicationRule_repositoryFilters = Lens.lens (\ReplicationRule' {repositoryFilters} -> repositoryFilters) (\s@ReplicationRule' {} a -> s {repositoryFilters = a} :: ReplicationRule) Prelude.. Lens.mapping Lens.coerced

-- | An array of objects representing the destination for a replication rule.
replicationRule_destinations :: Lens.Lens' ReplicationRule [ReplicationDestination]
replicationRule_destinations = Lens.lens (\ReplicationRule' {destinations} -> destinations) (\s@ReplicationRule' {} a -> s {destinations = a} :: ReplicationRule) Prelude.. Lens.coerced

instance Core.FromJSON ReplicationRule where
  parseJSON =
    Core.withObject
      "ReplicationRule"
      ( \x ->
          ReplicationRule'
            Prelude.<$> (x Core..:? "repositoryFilters")
            Prelude.<*> (x Core..:? "destinations" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ReplicationRule where
  hashWithSalt salt' ReplicationRule' {..} =
    salt' `Prelude.hashWithSalt` destinations
      `Prelude.hashWithSalt` repositoryFilters

instance Prelude.NFData ReplicationRule where
  rnf ReplicationRule' {..} =
    Prelude.rnf repositoryFilters
      `Prelude.seq` Prelude.rnf destinations

instance Core.ToJSON ReplicationRule where
  toJSON ReplicationRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("repositoryFilters" Core..=)
              Prelude.<$> repositoryFilters,
            Prelude.Just ("destinations" Core..= destinations)
          ]
      )
