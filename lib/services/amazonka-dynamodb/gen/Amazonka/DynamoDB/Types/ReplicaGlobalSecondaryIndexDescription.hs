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
-- Module      : Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexDescription where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.ProvisionedThroughputOverride
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the properties of a replica global secondary index.
--
-- /See:/ 'newReplicaGlobalSecondaryIndexDescription' smart constructor.
data ReplicaGlobalSecondaryIndexDescription = ReplicaGlobalSecondaryIndexDescription'
  { -- | If not described, uses the source table GSI\'s read capacity settings.
    provisionedThroughputOverride :: Prelude.Maybe ProvisionedThroughputOverride,
    -- | The name of the global secondary index.
    indexName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaGlobalSecondaryIndexDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedThroughputOverride', 'replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride' - If not described, uses the source table GSI\'s read capacity settings.
--
-- 'indexName', 'replicaGlobalSecondaryIndexDescription_indexName' - The name of the global secondary index.
newReplicaGlobalSecondaryIndexDescription ::
  ReplicaGlobalSecondaryIndexDescription
newReplicaGlobalSecondaryIndexDescription =
  ReplicaGlobalSecondaryIndexDescription'
    { provisionedThroughputOverride =
        Prelude.Nothing,
      indexName = Prelude.Nothing
    }

-- | If not described, uses the source table GSI\'s read capacity settings.
replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride :: Lens.Lens' ReplicaGlobalSecondaryIndexDescription (Prelude.Maybe ProvisionedThroughputOverride)
replicaGlobalSecondaryIndexDescription_provisionedThroughputOverride = Lens.lens (\ReplicaGlobalSecondaryIndexDescription' {provisionedThroughputOverride} -> provisionedThroughputOverride) (\s@ReplicaGlobalSecondaryIndexDescription' {} a -> s {provisionedThroughputOverride = a} :: ReplicaGlobalSecondaryIndexDescription)

-- | The name of the global secondary index.
replicaGlobalSecondaryIndexDescription_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndexDescription (Prelude.Maybe Prelude.Text)
replicaGlobalSecondaryIndexDescription_indexName = Lens.lens (\ReplicaGlobalSecondaryIndexDescription' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndexDescription' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexDescription)

instance
  Core.FromJSON
    ReplicaGlobalSecondaryIndexDescription
  where
  parseJSON =
    Core.withObject
      "ReplicaGlobalSecondaryIndexDescription"
      ( \x ->
          ReplicaGlobalSecondaryIndexDescription'
            Prelude.<$> (x Core..:? "ProvisionedThroughputOverride")
            Prelude.<*> (x Core..:? "IndexName")
      )

instance
  Prelude.Hashable
    ReplicaGlobalSecondaryIndexDescription
  where
  hashWithSalt
    salt'
    ReplicaGlobalSecondaryIndexDescription' {..} =
      salt' `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` provisionedThroughputOverride

instance
  Prelude.NFData
    ReplicaGlobalSecondaryIndexDescription
  where
  rnf ReplicaGlobalSecondaryIndexDescription' {..} =
    Prelude.rnf provisionedThroughputOverride
      `Prelude.seq` Prelude.rnf indexName
