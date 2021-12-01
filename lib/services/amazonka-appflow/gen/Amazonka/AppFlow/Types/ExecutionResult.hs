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
-- Module      : Amazonka.AppFlow.Types.ExecutionResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.ExecutionResult where

import Amazonka.AppFlow.Types.ErrorInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the end result of the flow run.
--
-- /See:/ 'newExecutionResult' smart constructor.
data ExecutionResult = ExecutionResult'
  { -- | The number of records processed in the flow run.
    recordsProcessed :: Prelude.Maybe Prelude.Integer,
    -- | The total number of bytes written as a result of the flow run.
    bytesWritten :: Prelude.Maybe Prelude.Integer,
    -- | The total number of bytes processed by the flow run.
    bytesProcessed :: Prelude.Maybe Prelude.Integer,
    -- | Provides any error message information related to the flow run.
    errorInfo :: Prelude.Maybe ErrorInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExecutionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'recordsProcessed', 'executionResult_recordsProcessed' - The number of records processed in the flow run.
--
-- 'bytesWritten', 'executionResult_bytesWritten' - The total number of bytes written as a result of the flow run.
--
-- 'bytesProcessed', 'executionResult_bytesProcessed' - The total number of bytes processed by the flow run.
--
-- 'errorInfo', 'executionResult_errorInfo' - Provides any error message information related to the flow run.
newExecutionResult ::
  ExecutionResult
newExecutionResult =
  ExecutionResult'
    { recordsProcessed =
        Prelude.Nothing,
      bytesWritten = Prelude.Nothing,
      bytesProcessed = Prelude.Nothing,
      errorInfo = Prelude.Nothing
    }

-- | The number of records processed in the flow run.
executionResult_recordsProcessed :: Lens.Lens' ExecutionResult (Prelude.Maybe Prelude.Integer)
executionResult_recordsProcessed = Lens.lens (\ExecutionResult' {recordsProcessed} -> recordsProcessed) (\s@ExecutionResult' {} a -> s {recordsProcessed = a} :: ExecutionResult)

-- | The total number of bytes written as a result of the flow run.
executionResult_bytesWritten :: Lens.Lens' ExecutionResult (Prelude.Maybe Prelude.Integer)
executionResult_bytesWritten = Lens.lens (\ExecutionResult' {bytesWritten} -> bytesWritten) (\s@ExecutionResult' {} a -> s {bytesWritten = a} :: ExecutionResult)

-- | The total number of bytes processed by the flow run.
executionResult_bytesProcessed :: Lens.Lens' ExecutionResult (Prelude.Maybe Prelude.Integer)
executionResult_bytesProcessed = Lens.lens (\ExecutionResult' {bytesProcessed} -> bytesProcessed) (\s@ExecutionResult' {} a -> s {bytesProcessed = a} :: ExecutionResult)

-- | Provides any error message information related to the flow run.
executionResult_errorInfo :: Lens.Lens' ExecutionResult (Prelude.Maybe ErrorInfo)
executionResult_errorInfo = Lens.lens (\ExecutionResult' {errorInfo} -> errorInfo) (\s@ExecutionResult' {} a -> s {errorInfo = a} :: ExecutionResult)

instance Core.FromJSON ExecutionResult where
  parseJSON =
    Core.withObject
      "ExecutionResult"
      ( \x ->
          ExecutionResult'
            Prelude.<$> (x Core..:? "recordsProcessed")
            Prelude.<*> (x Core..:? "bytesWritten")
            Prelude.<*> (x Core..:? "bytesProcessed")
            Prelude.<*> (x Core..:? "errorInfo")
      )

instance Prelude.Hashable ExecutionResult where
  hashWithSalt salt' ExecutionResult' {..} =
    salt' `Prelude.hashWithSalt` errorInfo
      `Prelude.hashWithSalt` bytesProcessed
      `Prelude.hashWithSalt` bytesWritten
      `Prelude.hashWithSalt` recordsProcessed

instance Prelude.NFData ExecutionResult where
  rnf ExecutionResult' {..} =
    Prelude.rnf recordsProcessed
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf bytesProcessed
      `Prelude.seq` Prelude.rnf bytesWritten
