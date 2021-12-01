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
-- Module      : Amazonka.MediaLive.Types.BatchScheduleActionDeleteRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.BatchScheduleActionDeleteRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of schedule actions to delete.
--
-- /See:/ 'newBatchScheduleActionDeleteRequest' smart constructor.
data BatchScheduleActionDeleteRequest = BatchScheduleActionDeleteRequest'
  { -- | A list of schedule actions to delete.
    actionNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchScheduleActionDeleteRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actionNames', 'batchScheduleActionDeleteRequest_actionNames' - A list of schedule actions to delete.
newBatchScheduleActionDeleteRequest ::
  BatchScheduleActionDeleteRequest
newBatchScheduleActionDeleteRequest =
  BatchScheduleActionDeleteRequest'
    { actionNames =
        Prelude.mempty
    }

-- | A list of schedule actions to delete.
batchScheduleActionDeleteRequest_actionNames :: Lens.Lens' BatchScheduleActionDeleteRequest [Prelude.Text]
batchScheduleActionDeleteRequest_actionNames = Lens.lens (\BatchScheduleActionDeleteRequest' {actionNames} -> actionNames) (\s@BatchScheduleActionDeleteRequest' {} a -> s {actionNames = a} :: BatchScheduleActionDeleteRequest) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    BatchScheduleActionDeleteRequest
  where
  hashWithSalt
    salt'
    BatchScheduleActionDeleteRequest' {..} =
      salt' `Prelude.hashWithSalt` actionNames

instance
  Prelude.NFData
    BatchScheduleActionDeleteRequest
  where
  rnf BatchScheduleActionDeleteRequest' {..} =
    Prelude.rnf actionNames

instance Core.ToJSON BatchScheduleActionDeleteRequest where
  toJSON BatchScheduleActionDeleteRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("actionNames" Core..= actionNames)]
      )
