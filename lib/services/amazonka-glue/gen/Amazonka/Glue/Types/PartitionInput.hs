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
-- Module      : Amazonka.Glue.Types.PartitionInput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PartitionInput where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.StorageDescriptor
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The structure used to create and update a partition.
--
-- /See:/ 'newPartitionInput' smart constructor.
data PartitionInput = PartitionInput'
  { -- | The values of the partition. Although this parameter is not required by
    -- the SDK, you must specify this parameter for a valid input.
    --
    -- The values for the keys for the new partition must be passed as an array
    -- of String objects that must be ordered in the same order as the
    -- partition keys appearing in the Amazon S3 prefix. Otherwise Glue will
    -- add the values to the wrong keys.
    values :: Prelude.Maybe [Prelude.Text],
    -- | The last time at which column statistics were computed for this
    -- partition.
    lastAnalyzedTime :: Prelude.Maybe Core.POSIX,
    -- | Provides information about the physical location where the partition is
    -- stored.
    storageDescriptor :: Prelude.Maybe StorageDescriptor,
    -- | These key-value pairs define partition parameters.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The last time at which the partition was accessed.
    lastAccessTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartitionInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'partitionInput_values' - The values of the partition. Although this parameter is not required by
-- the SDK, you must specify this parameter for a valid input.
--
-- The values for the keys for the new partition must be passed as an array
-- of String objects that must be ordered in the same order as the
-- partition keys appearing in the Amazon S3 prefix. Otherwise Glue will
-- add the values to the wrong keys.
--
-- 'lastAnalyzedTime', 'partitionInput_lastAnalyzedTime' - The last time at which column statistics were computed for this
-- partition.
--
-- 'storageDescriptor', 'partitionInput_storageDescriptor' - Provides information about the physical location where the partition is
-- stored.
--
-- 'parameters', 'partitionInput_parameters' - These key-value pairs define partition parameters.
--
-- 'lastAccessTime', 'partitionInput_lastAccessTime' - The last time at which the partition was accessed.
newPartitionInput ::
  PartitionInput
newPartitionInput =
  PartitionInput'
    { values = Prelude.Nothing,
      lastAnalyzedTime = Prelude.Nothing,
      storageDescriptor = Prelude.Nothing,
      parameters = Prelude.Nothing,
      lastAccessTime = Prelude.Nothing
    }

-- | The values of the partition. Although this parameter is not required by
-- the SDK, you must specify this parameter for a valid input.
--
-- The values for the keys for the new partition must be passed as an array
-- of String objects that must be ordered in the same order as the
-- partition keys appearing in the Amazon S3 prefix. Otherwise Glue will
-- add the values to the wrong keys.
partitionInput_values :: Lens.Lens' PartitionInput (Prelude.Maybe [Prelude.Text])
partitionInput_values = Lens.lens (\PartitionInput' {values} -> values) (\s@PartitionInput' {} a -> s {values = a} :: PartitionInput) Prelude.. Lens.mapping Lens.coerced

-- | The last time at which column statistics were computed for this
-- partition.
partitionInput_lastAnalyzedTime :: Lens.Lens' PartitionInput (Prelude.Maybe Prelude.UTCTime)
partitionInput_lastAnalyzedTime = Lens.lens (\PartitionInput' {lastAnalyzedTime} -> lastAnalyzedTime) (\s@PartitionInput' {} a -> s {lastAnalyzedTime = a} :: PartitionInput) Prelude.. Lens.mapping Core._Time

-- | Provides information about the physical location where the partition is
-- stored.
partitionInput_storageDescriptor :: Lens.Lens' PartitionInput (Prelude.Maybe StorageDescriptor)
partitionInput_storageDescriptor = Lens.lens (\PartitionInput' {storageDescriptor} -> storageDescriptor) (\s@PartitionInput' {} a -> s {storageDescriptor = a} :: PartitionInput)

-- | These key-value pairs define partition parameters.
partitionInput_parameters :: Lens.Lens' PartitionInput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
partitionInput_parameters = Lens.lens (\PartitionInput' {parameters} -> parameters) (\s@PartitionInput' {} a -> s {parameters = a} :: PartitionInput) Prelude.. Lens.mapping Lens.coerced

-- | The last time at which the partition was accessed.
partitionInput_lastAccessTime :: Lens.Lens' PartitionInput (Prelude.Maybe Prelude.UTCTime)
partitionInput_lastAccessTime = Lens.lens (\PartitionInput' {lastAccessTime} -> lastAccessTime) (\s@PartitionInput' {} a -> s {lastAccessTime = a} :: PartitionInput) Prelude.. Lens.mapping Core._Time

instance Prelude.Hashable PartitionInput where
  hashWithSalt salt' PartitionInput' {..} =
    salt' `Prelude.hashWithSalt` lastAccessTime
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` storageDescriptor
      `Prelude.hashWithSalt` lastAnalyzedTime
      `Prelude.hashWithSalt` values

instance Prelude.NFData PartitionInput where
  rnf PartitionInput' {..} =
    Prelude.rnf values
      `Prelude.seq` Prelude.rnf lastAccessTime
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf storageDescriptor
      `Prelude.seq` Prelude.rnf lastAnalyzedTime

instance Core.ToJSON PartitionInput where
  toJSON PartitionInput' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Values" Core..=) Prelude.<$> values,
            ("LastAnalyzedTime" Core..=)
              Prelude.<$> lastAnalyzedTime,
            ("StorageDescriptor" Core..=)
              Prelude.<$> storageDescriptor,
            ("Parameters" Core..=) Prelude.<$> parameters,
            ("LastAccessTime" Core..=)
              Prelude.<$> lastAccessTime
          ]
      )
