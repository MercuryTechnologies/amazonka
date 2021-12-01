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
-- Module      : Amazonka.Glue.Types.StartingEventBatchCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.StartingEventBatchCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The batch condition that started the workflow run. Either the number of
-- events in the batch size arrived, in which case the BatchSize member is
-- non-zero, or the batch window expired, in which case the BatchWindow
-- member is non-zero.
--
-- /See:/ 'newStartingEventBatchCondition' smart constructor.
data StartingEventBatchCondition = StartingEventBatchCondition'
  { -- | Duration of the batch window in seconds.
    batchWindow :: Prelude.Maybe Prelude.Int,
    -- | Number of events in the batch.
    batchSize :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartingEventBatchCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'batchWindow', 'startingEventBatchCondition_batchWindow' - Duration of the batch window in seconds.
--
-- 'batchSize', 'startingEventBatchCondition_batchSize' - Number of events in the batch.
newStartingEventBatchCondition ::
  StartingEventBatchCondition
newStartingEventBatchCondition =
  StartingEventBatchCondition'
    { batchWindow =
        Prelude.Nothing,
      batchSize = Prelude.Nothing
    }

-- | Duration of the batch window in seconds.
startingEventBatchCondition_batchWindow :: Lens.Lens' StartingEventBatchCondition (Prelude.Maybe Prelude.Int)
startingEventBatchCondition_batchWindow = Lens.lens (\StartingEventBatchCondition' {batchWindow} -> batchWindow) (\s@StartingEventBatchCondition' {} a -> s {batchWindow = a} :: StartingEventBatchCondition)

-- | Number of events in the batch.
startingEventBatchCondition_batchSize :: Lens.Lens' StartingEventBatchCondition (Prelude.Maybe Prelude.Int)
startingEventBatchCondition_batchSize = Lens.lens (\StartingEventBatchCondition' {batchSize} -> batchSize) (\s@StartingEventBatchCondition' {} a -> s {batchSize = a} :: StartingEventBatchCondition)

instance Core.FromJSON StartingEventBatchCondition where
  parseJSON =
    Core.withObject
      "StartingEventBatchCondition"
      ( \x ->
          StartingEventBatchCondition'
            Prelude.<$> (x Core..:? "BatchWindow")
            Prelude.<*> (x Core..:? "BatchSize")
      )

instance Prelude.Hashable StartingEventBatchCondition where
  hashWithSalt salt' StartingEventBatchCondition' {..} =
    salt' `Prelude.hashWithSalt` batchSize
      `Prelude.hashWithSalt` batchWindow

instance Prelude.NFData StartingEventBatchCondition where
  rnf StartingEventBatchCondition' {..} =
    Prelude.rnf batchWindow
      `Prelude.seq` Prelude.rnf batchSize
