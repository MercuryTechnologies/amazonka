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
-- Module      : Amazonka.Glue.Types.BlueprintRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.BlueprintRun where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.BlueprintRunState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of a blueprint run.
--
-- /See:/ 'newBlueprintRun' smart constructor.
data BlueprintRun = BlueprintRun'
  { -- | The name of a workflow that is created as a result of a successful
    -- blueprint run. If a blueprint run has an error, there will not be a
    -- workflow created.
    workflowName :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the blueprint run completed.
    completedOn :: Prelude.Maybe Core.POSIX,
    -- | The state of the blueprint run. Possible values are:
    --
    -- -   Running — The blueprint run is in progress.
    --
    -- -   Succeeded — The blueprint run completed successfully.
    --
    -- -   Failed — The blueprint run failed and rollback is complete.
    --
    -- -   Rolling Back — The blueprint run failed and rollback is in progress.
    state :: Prelude.Maybe BlueprintRunState,
    -- | If there are any errors while creating the entities of a workflow, we
    -- try to roll back the created entities until that point and delete them.
    -- This attribute indicates the errors seen while trying to delete the
    -- entities that are created.
    rollbackErrorMessage :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the blueprint run started.
    startedOn :: Prelude.Maybe Core.POSIX,
    -- | The run ID for this blueprint run.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The blueprint parameters as a string. You will have to provide a value
    -- for each key that is required from the parameter spec that is defined in
    -- the @Blueprint$ParameterSpec@.
    parameters :: Prelude.Maybe Prelude.Text,
    -- | The name of the blueprint.
    blueprintName :: Prelude.Maybe Prelude.Text,
    -- | Indicates any errors that are seen while running the blueprint.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The role ARN. This role will be assumed by the Glue service and will be
    -- used to create the workflow and other entities of a workflow.
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BlueprintRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowName', 'blueprintRun_workflowName' - The name of a workflow that is created as a result of a successful
-- blueprint run. If a blueprint run has an error, there will not be a
-- workflow created.
--
-- 'completedOn', 'blueprintRun_completedOn' - The date and time that the blueprint run completed.
--
-- 'state', 'blueprintRun_state' - The state of the blueprint run. Possible values are:
--
-- -   Running — The blueprint run is in progress.
--
-- -   Succeeded — The blueprint run completed successfully.
--
-- -   Failed — The blueprint run failed and rollback is complete.
--
-- -   Rolling Back — The blueprint run failed and rollback is in progress.
--
-- 'rollbackErrorMessage', 'blueprintRun_rollbackErrorMessage' - If there are any errors while creating the entities of a workflow, we
-- try to roll back the created entities until that point and delete them.
-- This attribute indicates the errors seen while trying to delete the
-- entities that are created.
--
-- 'startedOn', 'blueprintRun_startedOn' - The date and time that the blueprint run started.
--
-- 'runId', 'blueprintRun_runId' - The run ID for this blueprint run.
--
-- 'parameters', 'blueprintRun_parameters' - The blueprint parameters as a string. You will have to provide a value
-- for each key that is required from the parameter spec that is defined in
-- the @Blueprint$ParameterSpec@.
--
-- 'blueprintName', 'blueprintRun_blueprintName' - The name of the blueprint.
--
-- 'errorMessage', 'blueprintRun_errorMessage' - Indicates any errors that are seen while running the blueprint.
--
-- 'roleArn', 'blueprintRun_roleArn' - The role ARN. This role will be assumed by the Glue service and will be
-- used to create the workflow and other entities of a workflow.
newBlueprintRun ::
  BlueprintRun
newBlueprintRun =
  BlueprintRun'
    { workflowName = Prelude.Nothing,
      completedOn = Prelude.Nothing,
      state = Prelude.Nothing,
      rollbackErrorMessage = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      runId = Prelude.Nothing,
      parameters = Prelude.Nothing,
      blueprintName = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The name of a workflow that is created as a result of a successful
-- blueprint run. If a blueprint run has an error, there will not be a
-- workflow created.
blueprintRun_workflowName :: Lens.Lens' BlueprintRun (Prelude.Maybe Prelude.Text)
blueprintRun_workflowName = Lens.lens (\BlueprintRun' {workflowName} -> workflowName) (\s@BlueprintRun' {} a -> s {workflowName = a} :: BlueprintRun)

-- | The date and time that the blueprint run completed.
blueprintRun_completedOn :: Lens.Lens' BlueprintRun (Prelude.Maybe Prelude.UTCTime)
blueprintRun_completedOn = Lens.lens (\BlueprintRun' {completedOn} -> completedOn) (\s@BlueprintRun' {} a -> s {completedOn = a} :: BlueprintRun) Prelude.. Lens.mapping Core._Time

-- | The state of the blueprint run. Possible values are:
--
-- -   Running — The blueprint run is in progress.
--
-- -   Succeeded — The blueprint run completed successfully.
--
-- -   Failed — The blueprint run failed and rollback is complete.
--
-- -   Rolling Back — The blueprint run failed and rollback is in progress.
blueprintRun_state :: Lens.Lens' BlueprintRun (Prelude.Maybe BlueprintRunState)
blueprintRun_state = Lens.lens (\BlueprintRun' {state} -> state) (\s@BlueprintRun' {} a -> s {state = a} :: BlueprintRun)

-- | If there are any errors while creating the entities of a workflow, we
-- try to roll back the created entities until that point and delete them.
-- This attribute indicates the errors seen while trying to delete the
-- entities that are created.
blueprintRun_rollbackErrorMessage :: Lens.Lens' BlueprintRun (Prelude.Maybe Prelude.Text)
blueprintRun_rollbackErrorMessage = Lens.lens (\BlueprintRun' {rollbackErrorMessage} -> rollbackErrorMessage) (\s@BlueprintRun' {} a -> s {rollbackErrorMessage = a} :: BlueprintRun)

-- | The date and time that the blueprint run started.
blueprintRun_startedOn :: Lens.Lens' BlueprintRun (Prelude.Maybe Prelude.UTCTime)
blueprintRun_startedOn = Lens.lens (\BlueprintRun' {startedOn} -> startedOn) (\s@BlueprintRun' {} a -> s {startedOn = a} :: BlueprintRun) Prelude.. Lens.mapping Core._Time

-- | The run ID for this blueprint run.
blueprintRun_runId :: Lens.Lens' BlueprintRun (Prelude.Maybe Prelude.Text)
blueprintRun_runId = Lens.lens (\BlueprintRun' {runId} -> runId) (\s@BlueprintRun' {} a -> s {runId = a} :: BlueprintRun)

-- | The blueprint parameters as a string. You will have to provide a value
-- for each key that is required from the parameter spec that is defined in
-- the @Blueprint$ParameterSpec@.
blueprintRun_parameters :: Lens.Lens' BlueprintRun (Prelude.Maybe Prelude.Text)
blueprintRun_parameters = Lens.lens (\BlueprintRun' {parameters} -> parameters) (\s@BlueprintRun' {} a -> s {parameters = a} :: BlueprintRun)

-- | The name of the blueprint.
blueprintRun_blueprintName :: Lens.Lens' BlueprintRun (Prelude.Maybe Prelude.Text)
blueprintRun_blueprintName = Lens.lens (\BlueprintRun' {blueprintName} -> blueprintName) (\s@BlueprintRun' {} a -> s {blueprintName = a} :: BlueprintRun)

-- | Indicates any errors that are seen while running the blueprint.
blueprintRun_errorMessage :: Lens.Lens' BlueprintRun (Prelude.Maybe Prelude.Text)
blueprintRun_errorMessage = Lens.lens (\BlueprintRun' {errorMessage} -> errorMessage) (\s@BlueprintRun' {} a -> s {errorMessage = a} :: BlueprintRun)

-- | The role ARN. This role will be assumed by the Glue service and will be
-- used to create the workflow and other entities of a workflow.
blueprintRun_roleArn :: Lens.Lens' BlueprintRun (Prelude.Maybe Prelude.Text)
blueprintRun_roleArn = Lens.lens (\BlueprintRun' {roleArn} -> roleArn) (\s@BlueprintRun' {} a -> s {roleArn = a} :: BlueprintRun)

instance Core.FromJSON BlueprintRun where
  parseJSON =
    Core.withObject
      "BlueprintRun"
      ( \x ->
          BlueprintRun'
            Prelude.<$> (x Core..:? "WorkflowName")
            Prelude.<*> (x Core..:? "CompletedOn")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "RollbackErrorMessage")
            Prelude.<*> (x Core..:? "StartedOn")
            Prelude.<*> (x Core..:? "RunId")
            Prelude.<*> (x Core..:? "Parameters")
            Prelude.<*> (x Core..:? "BlueprintName")
            Prelude.<*> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "RoleArn")
      )

instance Prelude.Hashable BlueprintRun where
  hashWithSalt salt' BlueprintRun' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` blueprintName
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` runId
      `Prelude.hashWithSalt` startedOn
      `Prelude.hashWithSalt` rollbackErrorMessage
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` completedOn
      `Prelude.hashWithSalt` workflowName

instance Prelude.NFData BlueprintRun where
  rnf BlueprintRun' {..} =
    Prelude.rnf workflowName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf blueprintName
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf rollbackErrorMessage
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf completedOn
