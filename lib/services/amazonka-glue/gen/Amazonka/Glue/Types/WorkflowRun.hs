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
-- Module      : Amazonka.Glue.Types.WorkflowRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.WorkflowRun where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.StartingEventBatchCondition
import Amazonka.Glue.Types.WorkflowGraph
import Amazonka.Glue.Types.WorkflowRunStatistics
import Amazonka.Glue.Types.WorkflowRunStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A workflow run is an execution of a workflow providing all the runtime
-- information.
--
-- /See:/ 'newWorkflowRun' smart constructor.
data WorkflowRun = WorkflowRun'
  { -- | The date and time when the workflow run completed.
    completedOn :: Prelude.Maybe Core.POSIX,
    -- | The status of the workflow run.
    status :: Prelude.Maybe WorkflowRunStatus,
    -- | The graph representing all the Glue components that belong to the
    -- workflow as nodes and directed connections between them as edges.
    graph :: Prelude.Maybe WorkflowGraph,
    -- | The date and time when the workflow run was started.
    startedOn :: Prelude.Maybe Core.POSIX,
    -- | The ID of this workflow run.
    workflowRunId :: Prelude.Maybe Prelude.Text,
    -- | Name of the workflow that was run.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the previous workflow run.
    previousRunId :: Prelude.Maybe Prelude.Text,
    -- | The statistics of the run.
    statistics :: Prelude.Maybe WorkflowRunStatistics,
    -- | The batch condition that started the workflow run.
    startingEventBatchCondition :: Prelude.Maybe StartingEventBatchCondition,
    -- | This error message describes any error that may have occurred in
    -- starting the workflow run. Currently the only error message is
    -- \"Concurrent runs exceeded for workflow: @foo@.\"
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The workflow run properties which were set during the run.
    workflowRunProperties :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkflowRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completedOn', 'workflowRun_completedOn' - The date and time when the workflow run completed.
--
-- 'status', 'workflowRun_status' - The status of the workflow run.
--
-- 'graph', 'workflowRun_graph' - The graph representing all the Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
--
-- 'startedOn', 'workflowRun_startedOn' - The date and time when the workflow run was started.
--
-- 'workflowRunId', 'workflowRun_workflowRunId' - The ID of this workflow run.
--
-- 'name', 'workflowRun_name' - Name of the workflow that was run.
--
-- 'previousRunId', 'workflowRun_previousRunId' - The ID of the previous workflow run.
--
-- 'statistics', 'workflowRun_statistics' - The statistics of the run.
--
-- 'startingEventBatchCondition', 'workflowRun_startingEventBatchCondition' - The batch condition that started the workflow run.
--
-- 'errorMessage', 'workflowRun_errorMessage' - This error message describes any error that may have occurred in
-- starting the workflow run. Currently the only error message is
-- \"Concurrent runs exceeded for workflow: @foo@.\"
--
-- 'workflowRunProperties', 'workflowRun_workflowRunProperties' - The workflow run properties which were set during the run.
newWorkflowRun ::
  WorkflowRun
newWorkflowRun =
  WorkflowRun'
    { completedOn = Prelude.Nothing,
      status = Prelude.Nothing,
      graph = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      workflowRunId = Prelude.Nothing,
      name = Prelude.Nothing,
      previousRunId = Prelude.Nothing,
      statistics = Prelude.Nothing,
      startingEventBatchCondition = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      workflowRunProperties = Prelude.Nothing
    }

-- | The date and time when the workflow run completed.
workflowRun_completedOn :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.UTCTime)
workflowRun_completedOn = Lens.lens (\WorkflowRun' {completedOn} -> completedOn) (\s@WorkflowRun' {} a -> s {completedOn = a} :: WorkflowRun) Prelude.. Lens.mapping Core._Time

-- | The status of the workflow run.
workflowRun_status :: Lens.Lens' WorkflowRun (Prelude.Maybe WorkflowRunStatus)
workflowRun_status = Lens.lens (\WorkflowRun' {status} -> status) (\s@WorkflowRun' {} a -> s {status = a} :: WorkflowRun)

-- | The graph representing all the Glue components that belong to the
-- workflow as nodes and directed connections between them as edges.
workflowRun_graph :: Lens.Lens' WorkflowRun (Prelude.Maybe WorkflowGraph)
workflowRun_graph = Lens.lens (\WorkflowRun' {graph} -> graph) (\s@WorkflowRun' {} a -> s {graph = a} :: WorkflowRun)

-- | The date and time when the workflow run was started.
workflowRun_startedOn :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.UTCTime)
workflowRun_startedOn = Lens.lens (\WorkflowRun' {startedOn} -> startedOn) (\s@WorkflowRun' {} a -> s {startedOn = a} :: WorkflowRun) Prelude.. Lens.mapping Core._Time

-- | The ID of this workflow run.
workflowRun_workflowRunId :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_workflowRunId = Lens.lens (\WorkflowRun' {workflowRunId} -> workflowRunId) (\s@WorkflowRun' {} a -> s {workflowRunId = a} :: WorkflowRun)

-- | Name of the workflow that was run.
workflowRun_name :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_name = Lens.lens (\WorkflowRun' {name} -> name) (\s@WorkflowRun' {} a -> s {name = a} :: WorkflowRun)

-- | The ID of the previous workflow run.
workflowRun_previousRunId :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_previousRunId = Lens.lens (\WorkflowRun' {previousRunId} -> previousRunId) (\s@WorkflowRun' {} a -> s {previousRunId = a} :: WorkflowRun)

-- | The statistics of the run.
workflowRun_statistics :: Lens.Lens' WorkflowRun (Prelude.Maybe WorkflowRunStatistics)
workflowRun_statistics = Lens.lens (\WorkflowRun' {statistics} -> statistics) (\s@WorkflowRun' {} a -> s {statistics = a} :: WorkflowRun)

-- | The batch condition that started the workflow run.
workflowRun_startingEventBatchCondition :: Lens.Lens' WorkflowRun (Prelude.Maybe StartingEventBatchCondition)
workflowRun_startingEventBatchCondition = Lens.lens (\WorkflowRun' {startingEventBatchCondition} -> startingEventBatchCondition) (\s@WorkflowRun' {} a -> s {startingEventBatchCondition = a} :: WorkflowRun)

-- | This error message describes any error that may have occurred in
-- starting the workflow run. Currently the only error message is
-- \"Concurrent runs exceeded for workflow: @foo@.\"
workflowRun_errorMessage :: Lens.Lens' WorkflowRun (Prelude.Maybe Prelude.Text)
workflowRun_errorMessage = Lens.lens (\WorkflowRun' {errorMessage} -> errorMessage) (\s@WorkflowRun' {} a -> s {errorMessage = a} :: WorkflowRun)

-- | The workflow run properties which were set during the run.
workflowRun_workflowRunProperties :: Lens.Lens' WorkflowRun (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
workflowRun_workflowRunProperties = Lens.lens (\WorkflowRun' {workflowRunProperties} -> workflowRunProperties) (\s@WorkflowRun' {} a -> s {workflowRunProperties = a} :: WorkflowRun) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON WorkflowRun where
  parseJSON =
    Core.withObject
      "WorkflowRun"
      ( \x ->
          WorkflowRun'
            Prelude.<$> (x Core..:? "CompletedOn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Graph")
            Prelude.<*> (x Core..:? "StartedOn")
            Prelude.<*> (x Core..:? "WorkflowRunId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "PreviousRunId")
            Prelude.<*> (x Core..:? "Statistics")
            Prelude.<*> (x Core..:? "StartingEventBatchCondition")
            Prelude.<*> (x Core..:? "ErrorMessage")
            Prelude.<*> ( x Core..:? "WorkflowRunProperties"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable WorkflowRun where
  hashWithSalt salt' WorkflowRun' {..} =
    salt' `Prelude.hashWithSalt` workflowRunProperties
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` startingEventBatchCondition
      `Prelude.hashWithSalt` statistics
      `Prelude.hashWithSalt` previousRunId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` workflowRunId
      `Prelude.hashWithSalt` startedOn
      `Prelude.hashWithSalt` graph
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` completedOn

instance Prelude.NFData WorkflowRun where
  rnf WorkflowRun' {..} =
    Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf workflowRunProperties
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf startingEventBatchCondition
      `Prelude.seq` Prelude.rnf statistics
      `Prelude.seq` Prelude.rnf previousRunId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf workflowRunId
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf graph
      `Prelude.seq` Prelude.rnf status
