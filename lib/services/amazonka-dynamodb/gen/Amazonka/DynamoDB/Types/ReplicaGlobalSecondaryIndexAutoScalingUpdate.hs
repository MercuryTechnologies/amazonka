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
-- Module      : Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.ReplicaGlobalSecondaryIndexAutoScalingUpdate where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.Types.AutoScalingSettingsUpdate
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the auto scaling settings of a global secondary index for a
-- replica that will be modified.
--
-- /See:/ 'newReplicaGlobalSecondaryIndexAutoScalingUpdate' smart constructor.
data ReplicaGlobalSecondaryIndexAutoScalingUpdate = ReplicaGlobalSecondaryIndexAutoScalingUpdate'
  { provisionedReadCapacityAutoScalingUpdate :: Prelude.Maybe AutoScalingSettingsUpdate,
    -- | The name of the global secondary index.
    indexName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReplicaGlobalSecondaryIndexAutoScalingUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'provisionedReadCapacityAutoScalingUpdate', 'replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate' - Undocumented member.
--
-- 'indexName', 'replicaGlobalSecondaryIndexAutoScalingUpdate_indexName' - The name of the global secondary index.
newReplicaGlobalSecondaryIndexAutoScalingUpdate ::
  ReplicaGlobalSecondaryIndexAutoScalingUpdate
newReplicaGlobalSecondaryIndexAutoScalingUpdate =
  ReplicaGlobalSecondaryIndexAutoScalingUpdate'
    { provisionedReadCapacityAutoScalingUpdate =
        Prelude.Nothing,
      indexName = Prelude.Nothing
    }

-- | Undocumented member.
replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingUpdate (Prelude.Maybe AutoScalingSettingsUpdate)
replicaGlobalSecondaryIndexAutoScalingUpdate_provisionedReadCapacityAutoScalingUpdate = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingUpdate' {provisionedReadCapacityAutoScalingUpdate} -> provisionedReadCapacityAutoScalingUpdate) (\s@ReplicaGlobalSecondaryIndexAutoScalingUpdate' {} a -> s {provisionedReadCapacityAutoScalingUpdate = a} :: ReplicaGlobalSecondaryIndexAutoScalingUpdate)

-- | The name of the global secondary index.
replicaGlobalSecondaryIndexAutoScalingUpdate_indexName :: Lens.Lens' ReplicaGlobalSecondaryIndexAutoScalingUpdate (Prelude.Maybe Prelude.Text)
replicaGlobalSecondaryIndexAutoScalingUpdate_indexName = Lens.lens (\ReplicaGlobalSecondaryIndexAutoScalingUpdate' {indexName} -> indexName) (\s@ReplicaGlobalSecondaryIndexAutoScalingUpdate' {} a -> s {indexName = a} :: ReplicaGlobalSecondaryIndexAutoScalingUpdate)

instance
  Prelude.Hashable
    ReplicaGlobalSecondaryIndexAutoScalingUpdate
  where
  hashWithSalt
    salt'
    ReplicaGlobalSecondaryIndexAutoScalingUpdate' {..} =
      salt' `Prelude.hashWithSalt` indexName
        `Prelude.hashWithSalt` provisionedReadCapacityAutoScalingUpdate

instance
  Prelude.NFData
    ReplicaGlobalSecondaryIndexAutoScalingUpdate
  where
  rnf ReplicaGlobalSecondaryIndexAutoScalingUpdate' {..} =
    Prelude.rnf
      provisionedReadCapacityAutoScalingUpdate
      `Prelude.seq` Prelude.rnf indexName

instance
  Core.ToJSON
    ReplicaGlobalSecondaryIndexAutoScalingUpdate
  where
  toJSON
    ReplicaGlobalSecondaryIndexAutoScalingUpdate' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("ProvisionedReadCapacityAutoScalingUpdate" Core..=)
                Prelude.<$> provisionedReadCapacityAutoScalingUpdate,
              ("IndexName" Core..=) Prelude.<$> indexName
            ]
        )
