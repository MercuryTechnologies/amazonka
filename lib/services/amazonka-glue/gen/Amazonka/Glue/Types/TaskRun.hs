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
-- Module      : Amazonka.Glue.Types.TaskRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.TaskRun where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.TaskRunProperties
import Amazonka.Glue.Types.TaskStatusType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The sampling parameters that are associated with the machine learning
-- transform.
--
-- /See:/ 'newTaskRun' smart constructor.
data TaskRun = TaskRun'
  { -- | The last point in time that the requested task run was completed.
    completedOn :: Prelude.Maybe Core.POSIX,
    -- | The current status of the requested task run.
    status :: Prelude.Maybe TaskStatusType,
    -- | The last point in time that the requested task run was updated.
    lastModifiedOn :: Prelude.Maybe Core.POSIX,
    -- | The list of error strings associated with this task run.
    errorString :: Prelude.Maybe Prelude.Text,
    -- | The date and time that this task run started.
    startedOn :: Prelude.Maybe Core.POSIX,
    -- | The names of the log group for secure logging, associated with this task
    -- run.
    logGroupName :: Prelude.Maybe Prelude.Text,
    -- | The amount of time (in seconds) that the task run consumed resources.
    executionTime :: Prelude.Maybe Prelude.Int,
    -- | Specifies configuration properties associated with this task run.
    properties :: Prelude.Maybe TaskRunProperties,
    -- | The unique identifier for the transform.
    transformId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for this task run.
    taskRunId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completedOn', 'taskRun_completedOn' - The last point in time that the requested task run was completed.
--
-- 'status', 'taskRun_status' - The current status of the requested task run.
--
-- 'lastModifiedOn', 'taskRun_lastModifiedOn' - The last point in time that the requested task run was updated.
--
-- 'errorString', 'taskRun_errorString' - The list of error strings associated with this task run.
--
-- 'startedOn', 'taskRun_startedOn' - The date and time that this task run started.
--
-- 'logGroupName', 'taskRun_logGroupName' - The names of the log group for secure logging, associated with this task
-- run.
--
-- 'executionTime', 'taskRun_executionTime' - The amount of time (in seconds) that the task run consumed resources.
--
-- 'properties', 'taskRun_properties' - Specifies configuration properties associated with this task run.
--
-- 'transformId', 'taskRun_transformId' - The unique identifier for the transform.
--
-- 'taskRunId', 'taskRun_taskRunId' - The unique identifier for this task run.
newTaskRun ::
  TaskRun
newTaskRun =
  TaskRun'
    { completedOn = Prelude.Nothing,
      status = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      errorString = Prelude.Nothing,
      startedOn = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      executionTime = Prelude.Nothing,
      properties = Prelude.Nothing,
      transformId = Prelude.Nothing,
      taskRunId = Prelude.Nothing
    }

-- | The last point in time that the requested task run was completed.
taskRun_completedOn :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.UTCTime)
taskRun_completedOn = Lens.lens (\TaskRun' {completedOn} -> completedOn) (\s@TaskRun' {} a -> s {completedOn = a} :: TaskRun) Prelude.. Lens.mapping Core._Time

-- | The current status of the requested task run.
taskRun_status :: Lens.Lens' TaskRun (Prelude.Maybe TaskStatusType)
taskRun_status = Lens.lens (\TaskRun' {status} -> status) (\s@TaskRun' {} a -> s {status = a} :: TaskRun)

-- | The last point in time that the requested task run was updated.
taskRun_lastModifiedOn :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.UTCTime)
taskRun_lastModifiedOn = Lens.lens (\TaskRun' {lastModifiedOn} -> lastModifiedOn) (\s@TaskRun' {} a -> s {lastModifiedOn = a} :: TaskRun) Prelude.. Lens.mapping Core._Time

-- | The list of error strings associated with this task run.
taskRun_errorString :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.Text)
taskRun_errorString = Lens.lens (\TaskRun' {errorString} -> errorString) (\s@TaskRun' {} a -> s {errorString = a} :: TaskRun)

-- | The date and time that this task run started.
taskRun_startedOn :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.UTCTime)
taskRun_startedOn = Lens.lens (\TaskRun' {startedOn} -> startedOn) (\s@TaskRun' {} a -> s {startedOn = a} :: TaskRun) Prelude.. Lens.mapping Core._Time

-- | The names of the log group for secure logging, associated with this task
-- run.
taskRun_logGroupName :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.Text)
taskRun_logGroupName = Lens.lens (\TaskRun' {logGroupName} -> logGroupName) (\s@TaskRun' {} a -> s {logGroupName = a} :: TaskRun)

-- | The amount of time (in seconds) that the task run consumed resources.
taskRun_executionTime :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.Int)
taskRun_executionTime = Lens.lens (\TaskRun' {executionTime} -> executionTime) (\s@TaskRun' {} a -> s {executionTime = a} :: TaskRun)

-- | Specifies configuration properties associated with this task run.
taskRun_properties :: Lens.Lens' TaskRun (Prelude.Maybe TaskRunProperties)
taskRun_properties = Lens.lens (\TaskRun' {properties} -> properties) (\s@TaskRun' {} a -> s {properties = a} :: TaskRun)

-- | The unique identifier for the transform.
taskRun_transformId :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.Text)
taskRun_transformId = Lens.lens (\TaskRun' {transformId} -> transformId) (\s@TaskRun' {} a -> s {transformId = a} :: TaskRun)

-- | The unique identifier for this task run.
taskRun_taskRunId :: Lens.Lens' TaskRun (Prelude.Maybe Prelude.Text)
taskRun_taskRunId = Lens.lens (\TaskRun' {taskRunId} -> taskRunId) (\s@TaskRun' {} a -> s {taskRunId = a} :: TaskRun)

instance Core.FromJSON TaskRun where
  parseJSON =
    Core.withObject
      "TaskRun"
      ( \x ->
          TaskRun'
            Prelude.<$> (x Core..:? "CompletedOn")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LastModifiedOn")
            Prelude.<*> (x Core..:? "ErrorString")
            Prelude.<*> (x Core..:? "StartedOn")
            Prelude.<*> (x Core..:? "LogGroupName")
            Prelude.<*> (x Core..:? "ExecutionTime")
            Prelude.<*> (x Core..:? "Properties")
            Prelude.<*> (x Core..:? "TransformId")
            Prelude.<*> (x Core..:? "TaskRunId")
      )

instance Prelude.Hashable TaskRun where
  hashWithSalt salt' TaskRun' {..} =
    salt' `Prelude.hashWithSalt` taskRunId
      `Prelude.hashWithSalt` transformId
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` executionTime
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` startedOn
      `Prelude.hashWithSalt` errorString
      `Prelude.hashWithSalt` lastModifiedOn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` completedOn

instance Prelude.NFData TaskRun where
  rnf TaskRun' {..} =
    Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf taskRunId
      `Prelude.seq` Prelude.rnf transformId
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf executionTime
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf errorString
      `Prelude.seq` Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf status
