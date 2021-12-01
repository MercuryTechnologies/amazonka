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
-- Module      : Amazonka.Glue.Types.PartitionError
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.PartitionError where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.ErrorDetail
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a partition error.
--
-- /See:/ 'newPartitionError' smart constructor.
data PartitionError = PartitionError'
  { -- | The values that define the partition.
    partitionValues :: Prelude.Maybe [Prelude.Text],
    -- | The details about the partition error.
    errorDetail :: Prelude.Maybe ErrorDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PartitionError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionValues', 'partitionError_partitionValues' - The values that define the partition.
--
-- 'errorDetail', 'partitionError_errorDetail' - The details about the partition error.
newPartitionError ::
  PartitionError
newPartitionError =
  PartitionError'
    { partitionValues = Prelude.Nothing,
      errorDetail = Prelude.Nothing
    }

-- | The values that define the partition.
partitionError_partitionValues :: Lens.Lens' PartitionError (Prelude.Maybe [Prelude.Text])
partitionError_partitionValues = Lens.lens (\PartitionError' {partitionValues} -> partitionValues) (\s@PartitionError' {} a -> s {partitionValues = a} :: PartitionError) Prelude.. Lens.mapping Lens.coerced

-- | The details about the partition error.
partitionError_errorDetail :: Lens.Lens' PartitionError (Prelude.Maybe ErrorDetail)
partitionError_errorDetail = Lens.lens (\PartitionError' {errorDetail} -> errorDetail) (\s@PartitionError' {} a -> s {errorDetail = a} :: PartitionError)

instance Core.FromJSON PartitionError where
  parseJSON =
    Core.withObject
      "PartitionError"
      ( \x ->
          PartitionError'
            Prelude.<$> ( x Core..:? "PartitionValues"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ErrorDetail")
      )

instance Prelude.Hashable PartitionError where
  hashWithSalt salt' PartitionError' {..} =
    salt' `Prelude.hashWithSalt` errorDetail
      `Prelude.hashWithSalt` partitionValues

instance Prelude.NFData PartitionError where
  rnf PartitionError' {..} =
    Prelude.rnf partitionValues
      `Prelude.seq` Prelude.rnf errorDetail
