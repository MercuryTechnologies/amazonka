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
-- Module      : Amazonka.IoT.Types.DetectMitigationActionExecution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.DetectMitigationActionExecution where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.DetectMitigationActionExecutionStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes which mitigation actions should be executed.
--
-- /See:/ 'newDetectMitigationActionExecution' smart constructor.
data DetectMitigationActionExecution = DetectMitigationActionExecution'
  { -- | The status of a mitigation action.
    status :: Prelude.Maybe DetectMitigationActionExecutionStatus,
    -- | The unique identifier of the task.
    taskId :: Prelude.Maybe Prelude.Text,
    -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the violation.
    violationId :: Prelude.Maybe Prelude.Text,
    -- | The error code of a mitigation action.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The message of a mitigation action.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The date a mitigation action was started.
    executionStartDate :: Prelude.Maybe Core.POSIX,
    -- | The date a mitigation action ended.
    executionEndDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectMitigationActionExecution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'detectMitigationActionExecution_status' - The status of a mitigation action.
--
-- 'taskId', 'detectMitigationActionExecution_taskId' - The unique identifier of the task.
--
-- 'actionName', 'detectMitigationActionExecution_actionName' - The friendly name that uniquely identifies the mitigation action.
--
-- 'violationId', 'detectMitigationActionExecution_violationId' - The unique identifier of the violation.
--
-- 'errorCode', 'detectMitigationActionExecution_errorCode' - The error code of a mitigation action.
--
-- 'message', 'detectMitigationActionExecution_message' - The message of a mitigation action.
--
-- 'thingName', 'detectMitigationActionExecution_thingName' - The name of the thing.
--
-- 'executionStartDate', 'detectMitigationActionExecution_executionStartDate' - The date a mitigation action was started.
--
-- 'executionEndDate', 'detectMitigationActionExecution_executionEndDate' - The date a mitigation action ended.
newDetectMitigationActionExecution ::
  DetectMitigationActionExecution
newDetectMitigationActionExecution =
  DetectMitigationActionExecution'
    { status =
        Prelude.Nothing,
      taskId = Prelude.Nothing,
      actionName = Prelude.Nothing,
      violationId = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      message = Prelude.Nothing,
      thingName = Prelude.Nothing,
      executionStartDate = Prelude.Nothing,
      executionEndDate = Prelude.Nothing
    }

-- | The status of a mitigation action.
detectMitigationActionExecution_status :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe DetectMitigationActionExecutionStatus)
detectMitigationActionExecution_status = Lens.lens (\DetectMitigationActionExecution' {status} -> status) (\s@DetectMitigationActionExecution' {} a -> s {status = a} :: DetectMitigationActionExecution)

-- | The unique identifier of the task.
detectMitigationActionExecution_taskId :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
detectMitigationActionExecution_taskId = Lens.lens (\DetectMitigationActionExecution' {taskId} -> taskId) (\s@DetectMitigationActionExecution' {} a -> s {taskId = a} :: DetectMitigationActionExecution)

-- | The friendly name that uniquely identifies the mitigation action.
detectMitigationActionExecution_actionName :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
detectMitigationActionExecution_actionName = Lens.lens (\DetectMitigationActionExecution' {actionName} -> actionName) (\s@DetectMitigationActionExecution' {} a -> s {actionName = a} :: DetectMitigationActionExecution)

-- | The unique identifier of the violation.
detectMitigationActionExecution_violationId :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
detectMitigationActionExecution_violationId = Lens.lens (\DetectMitigationActionExecution' {violationId} -> violationId) (\s@DetectMitigationActionExecution' {} a -> s {violationId = a} :: DetectMitigationActionExecution)

-- | The error code of a mitigation action.
detectMitigationActionExecution_errorCode :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
detectMitigationActionExecution_errorCode = Lens.lens (\DetectMitigationActionExecution' {errorCode} -> errorCode) (\s@DetectMitigationActionExecution' {} a -> s {errorCode = a} :: DetectMitigationActionExecution)

-- | The message of a mitigation action.
detectMitigationActionExecution_message :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
detectMitigationActionExecution_message = Lens.lens (\DetectMitigationActionExecution' {message} -> message) (\s@DetectMitigationActionExecution' {} a -> s {message = a} :: DetectMitigationActionExecution)

-- | The name of the thing.
detectMitigationActionExecution_thingName :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.Text)
detectMitigationActionExecution_thingName = Lens.lens (\DetectMitigationActionExecution' {thingName} -> thingName) (\s@DetectMitigationActionExecution' {} a -> s {thingName = a} :: DetectMitigationActionExecution)

-- | The date a mitigation action was started.
detectMitigationActionExecution_executionStartDate :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.UTCTime)
detectMitigationActionExecution_executionStartDate = Lens.lens (\DetectMitigationActionExecution' {executionStartDate} -> executionStartDate) (\s@DetectMitigationActionExecution' {} a -> s {executionStartDate = a} :: DetectMitigationActionExecution) Prelude.. Lens.mapping Core._Time

-- | The date a mitigation action ended.
detectMitigationActionExecution_executionEndDate :: Lens.Lens' DetectMitigationActionExecution (Prelude.Maybe Prelude.UTCTime)
detectMitigationActionExecution_executionEndDate = Lens.lens (\DetectMitigationActionExecution' {executionEndDate} -> executionEndDate) (\s@DetectMitigationActionExecution' {} a -> s {executionEndDate = a} :: DetectMitigationActionExecution) Prelude.. Lens.mapping Core._Time

instance
  Core.FromJSON
    DetectMitigationActionExecution
  where
  parseJSON =
    Core.withObject
      "DetectMitigationActionExecution"
      ( \x ->
          DetectMitigationActionExecution'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "taskId")
            Prelude.<*> (x Core..:? "actionName")
            Prelude.<*> (x Core..:? "violationId")
            Prelude.<*> (x Core..:? "errorCode")
            Prelude.<*> (x Core..:? "message")
            Prelude.<*> (x Core..:? "thingName")
            Prelude.<*> (x Core..:? "executionStartDate")
            Prelude.<*> (x Core..:? "executionEndDate")
      )

instance
  Prelude.Hashable
    DetectMitigationActionExecution
  where
  hashWithSalt
    salt'
    DetectMitigationActionExecution' {..} =
      salt' `Prelude.hashWithSalt` executionEndDate
        `Prelude.hashWithSalt` executionStartDate
        `Prelude.hashWithSalt` thingName
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` errorCode
        `Prelude.hashWithSalt` violationId
        `Prelude.hashWithSalt` actionName
        `Prelude.hashWithSalt` taskId
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    DetectMitigationActionExecution
  where
  rnf DetectMitigationActionExecution' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf executionEndDate
      `Prelude.seq` Prelude.rnf executionStartDate
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf violationId
      `Prelude.seq` Prelude.rnf actionName
      `Prelude.seq` Prelude.rnf taskId
