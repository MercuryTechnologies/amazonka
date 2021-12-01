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
-- Module      : Amazonka.StepFunctions.Types.ExecutionTimedOutEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ExecutionTimedOutEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the execution timeout that occurred during the
-- execution.
--
-- /See:/ 'newExecutionTimedOutEventDetails' smart constructor.
data ExecutionTimedOutEventDetails = ExecutionTimedOutEventDetails'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the timeout.
    cause :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionTimedOutEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'executionTimedOutEventDetails_error' - The error code of the failure.
--
-- 'cause', 'executionTimedOutEventDetails_cause' - A more detailed explanation of the cause of the timeout.
newExecutionTimedOutEventDetails ::
  ExecutionTimedOutEventDetails
newExecutionTimedOutEventDetails =
  ExecutionTimedOutEventDetails'
    { error =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | The error code of the failure.
executionTimedOutEventDetails_error :: Lens.Lens' ExecutionTimedOutEventDetails (Prelude.Maybe Prelude.Text)
executionTimedOutEventDetails_error = Lens.lens (\ExecutionTimedOutEventDetails' {error} -> error) (\s@ExecutionTimedOutEventDetails' {} a -> s {error = a} :: ExecutionTimedOutEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the timeout.
executionTimedOutEventDetails_cause :: Lens.Lens' ExecutionTimedOutEventDetails (Prelude.Maybe Prelude.Text)
executionTimedOutEventDetails_cause = Lens.lens (\ExecutionTimedOutEventDetails' {cause} -> cause) (\s@ExecutionTimedOutEventDetails' {} a -> s {cause = a} :: ExecutionTimedOutEventDetails) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON ExecutionTimedOutEventDetails where
  parseJSON =
    Core.withObject
      "ExecutionTimedOutEventDetails"
      ( \x ->
          ExecutionTimedOutEventDetails'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "cause")
      )

instance
  Prelude.Hashable
    ExecutionTimedOutEventDetails
  where
  hashWithSalt salt' ExecutionTimedOutEventDetails' {..} =
    salt' `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` error

instance Prelude.NFData ExecutionTimedOutEventDetails where
  rnf ExecutionTimedOutEventDetails' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf cause
