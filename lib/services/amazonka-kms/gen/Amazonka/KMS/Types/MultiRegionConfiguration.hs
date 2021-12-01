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
-- Module      : Amazonka.KMS.Types.MultiRegionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KMS.Types.MultiRegionConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.KMS.Types.MultiRegionKey
import Amazonka.KMS.Types.MultiRegionKeyType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of this multi-Region key. This field appears
-- only when the KMS key is a primary or replica of a multi-Region key.
--
-- For more information about any listed KMS key, use the DescribeKey
-- operation.
--
-- /See:/ 'newMultiRegionConfiguration' smart constructor.
data MultiRegionConfiguration = MultiRegionConfiguration'
  { -- | Displays the key ARN and Region of the primary key. This field includes
    -- the current KMS key if it is the primary key.
    primaryKey :: Prelude.Maybe MultiRegionKey,
    -- | displays the key ARNs and Regions of all replica keys. This field
    -- includes the current KMS key if it is a replica key.
    replicaKeys :: Prelude.Maybe [MultiRegionKey],
    -- | Indicates whether the KMS key is a @PRIMARY@ or @REPLICA@ key.
    multiRegionKeyType :: Prelude.Maybe MultiRegionKeyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiRegionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'primaryKey', 'multiRegionConfiguration_primaryKey' - Displays the key ARN and Region of the primary key. This field includes
-- the current KMS key if it is the primary key.
--
-- 'replicaKeys', 'multiRegionConfiguration_replicaKeys' - displays the key ARNs and Regions of all replica keys. This field
-- includes the current KMS key if it is a replica key.
--
-- 'multiRegionKeyType', 'multiRegionConfiguration_multiRegionKeyType' - Indicates whether the KMS key is a @PRIMARY@ or @REPLICA@ key.
newMultiRegionConfiguration ::
  MultiRegionConfiguration
newMultiRegionConfiguration =
  MultiRegionConfiguration'
    { primaryKey =
        Prelude.Nothing,
      replicaKeys = Prelude.Nothing,
      multiRegionKeyType = Prelude.Nothing
    }

-- | Displays the key ARN and Region of the primary key. This field includes
-- the current KMS key if it is the primary key.
multiRegionConfiguration_primaryKey :: Lens.Lens' MultiRegionConfiguration (Prelude.Maybe MultiRegionKey)
multiRegionConfiguration_primaryKey = Lens.lens (\MultiRegionConfiguration' {primaryKey} -> primaryKey) (\s@MultiRegionConfiguration' {} a -> s {primaryKey = a} :: MultiRegionConfiguration)

-- | displays the key ARNs and Regions of all replica keys. This field
-- includes the current KMS key if it is a replica key.
multiRegionConfiguration_replicaKeys :: Lens.Lens' MultiRegionConfiguration (Prelude.Maybe [MultiRegionKey])
multiRegionConfiguration_replicaKeys = Lens.lens (\MultiRegionConfiguration' {replicaKeys} -> replicaKeys) (\s@MultiRegionConfiguration' {} a -> s {replicaKeys = a} :: MultiRegionConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the KMS key is a @PRIMARY@ or @REPLICA@ key.
multiRegionConfiguration_multiRegionKeyType :: Lens.Lens' MultiRegionConfiguration (Prelude.Maybe MultiRegionKeyType)
multiRegionConfiguration_multiRegionKeyType = Lens.lens (\MultiRegionConfiguration' {multiRegionKeyType} -> multiRegionKeyType) (\s@MultiRegionConfiguration' {} a -> s {multiRegionKeyType = a} :: MultiRegionConfiguration)

instance Core.FromJSON MultiRegionConfiguration where
  parseJSON =
    Core.withObject
      "MultiRegionConfiguration"
      ( \x ->
          MultiRegionConfiguration'
            Prelude.<$> (x Core..:? "PrimaryKey")
            Prelude.<*> (x Core..:? "ReplicaKeys" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MultiRegionKeyType")
      )

instance Prelude.Hashable MultiRegionConfiguration where
  hashWithSalt salt' MultiRegionConfiguration' {..} =
    salt' `Prelude.hashWithSalt` multiRegionKeyType
      `Prelude.hashWithSalt` replicaKeys
      `Prelude.hashWithSalt` primaryKey

instance Prelude.NFData MultiRegionConfiguration where
  rnf MultiRegionConfiguration' {..} =
    Prelude.rnf primaryKey
      `Prelude.seq` Prelude.rnf multiRegionKeyType
      `Prelude.seq` Prelude.rnf replicaKeys
