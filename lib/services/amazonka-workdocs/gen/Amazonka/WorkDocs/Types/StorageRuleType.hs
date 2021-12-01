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
-- Module      : Amazonka.WorkDocs.Types.StorageRuleType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkDocs.Types.StorageRuleType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkDocs.Types.StorageType

-- | Describes the storage for a user.
--
-- /See:/ 'newStorageRuleType' smart constructor.
data StorageRuleType = StorageRuleType'
  { -- | The amount of storage allocated, in bytes.
    storageAllocatedInBytes :: Prelude.Maybe Prelude.Natural,
    -- | The type of storage.
    storageType :: Prelude.Maybe StorageType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StorageRuleType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageAllocatedInBytes', 'storageRuleType_storageAllocatedInBytes' - The amount of storage allocated, in bytes.
--
-- 'storageType', 'storageRuleType_storageType' - The type of storage.
newStorageRuleType ::
  StorageRuleType
newStorageRuleType =
  StorageRuleType'
    { storageAllocatedInBytes =
        Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | The amount of storage allocated, in bytes.
storageRuleType_storageAllocatedInBytes :: Lens.Lens' StorageRuleType (Prelude.Maybe Prelude.Natural)
storageRuleType_storageAllocatedInBytes = Lens.lens (\StorageRuleType' {storageAllocatedInBytes} -> storageAllocatedInBytes) (\s@StorageRuleType' {} a -> s {storageAllocatedInBytes = a} :: StorageRuleType)

-- | The type of storage.
storageRuleType_storageType :: Lens.Lens' StorageRuleType (Prelude.Maybe StorageType)
storageRuleType_storageType = Lens.lens (\StorageRuleType' {storageType} -> storageType) (\s@StorageRuleType' {} a -> s {storageType = a} :: StorageRuleType)

instance Core.FromJSON StorageRuleType where
  parseJSON =
    Core.withObject
      "StorageRuleType"
      ( \x ->
          StorageRuleType'
            Prelude.<$> (x Core..:? "StorageAllocatedInBytes")
            Prelude.<*> (x Core..:? "StorageType")
      )

instance Prelude.Hashable StorageRuleType where
  hashWithSalt salt' StorageRuleType' {..} =
    salt' `Prelude.hashWithSalt` storageType
      `Prelude.hashWithSalt` storageAllocatedInBytes

instance Prelude.NFData StorageRuleType where
  rnf StorageRuleType' {..} =
    Prelude.rnf storageAllocatedInBytes
      `Prelude.seq` Prelude.rnf storageType

instance Core.ToJSON StorageRuleType where
  toJSON StorageRuleType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StorageAllocatedInBytes" Core..=)
              Prelude.<$> storageAllocatedInBytes,
            ("StorageType" Core..=) Prelude.<$> storageType
          ]
      )
