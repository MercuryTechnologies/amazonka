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
-- Module      : Amazonka.SWF.Types.WorkflowExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.WorkflowExecution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a workflow execution.
--
-- /See:/ 'newWorkflowExecution' smart constructor.
data WorkflowExecution = WorkflowExecution'
  { -- | The user defined identifier associated with the workflow execution.
    workflowId :: Prelude.Text,
    -- | A system-generated unique identifier for the workflow execution.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowId', 'workflowExecution_workflowId' - The user defined identifier associated with the workflow execution.
--
-- 'runId', 'workflowExecution_runId' - A system-generated unique identifier for the workflow execution.
newWorkflowExecution ::
  -- | 'workflowId'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  WorkflowExecution
newWorkflowExecution pWorkflowId_ pRunId_ =
  WorkflowExecution'
    { workflowId = pWorkflowId_,
      runId = pRunId_
    }

-- | The user defined identifier associated with the workflow execution.
workflowExecution_workflowId :: Lens.Lens' WorkflowExecution Prelude.Text
workflowExecution_workflowId = Lens.lens (\WorkflowExecution' {workflowId} -> workflowId) (\s@WorkflowExecution' {} a -> s {workflowId = a} :: WorkflowExecution)

-- | A system-generated unique identifier for the workflow execution.
workflowExecution_runId :: Lens.Lens' WorkflowExecution Prelude.Text
workflowExecution_runId = Lens.lens (\WorkflowExecution' {runId} -> runId) (\s@WorkflowExecution' {} a -> s {runId = a} :: WorkflowExecution)

instance Core.FromJSON WorkflowExecution where
  parseJSON =
    Core.withObject
      "WorkflowExecution"
      ( \x ->
          WorkflowExecution'
            Prelude.<$> (x Core..: "workflowId")
            Prelude.<*> (x Core..: "runId")
      )

instance Prelude.Hashable WorkflowExecution where
  hashWithSalt salt' WorkflowExecution' {..} =
    salt' `Prelude.hashWithSalt` runId
      `Prelude.hashWithSalt` workflowId

instance Prelude.NFData WorkflowExecution where
  rnf WorkflowExecution' {..} =
    Prelude.rnf workflowId
      `Prelude.seq` Prelude.rnf runId

instance Core.ToJSON WorkflowExecution where
  toJSON WorkflowExecution' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("workflowId" Core..= workflowId),
            Prelude.Just ("runId" Core..= runId)
          ]
      )
