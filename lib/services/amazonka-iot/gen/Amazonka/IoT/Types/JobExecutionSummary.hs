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
-- Module      : Amazonka.IoT.Types.JobExecutionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.JobExecutionSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.JobExecutionStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The job execution summary.
--
-- /See:/ 'newJobExecutionSummary' smart constructor.
data JobExecutionSummary = JobExecutionSummary'
  { -- | The status of the job execution.
    status :: Prelude.Maybe JobExecutionStatus,
    -- | The time, in seconds since the epoch, when the job execution was last
    -- updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The time, in seconds since the epoch, when the job execution was queued.
    queuedAt :: Prelude.Maybe Core.POSIX,
    -- | A string (consisting of the digits \"0\" through \"9\") which identifies
    -- this particular job execution on this particular device. It can be used
    -- later in commands which return or update job execution information.
    executionNumber :: Prelude.Maybe Prelude.Integer,
    -- | The time, in seconds since the epoch, when the job execution started.
    startedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'jobExecutionSummary_status' - The status of the job execution.
--
-- 'lastUpdatedAt', 'jobExecutionSummary_lastUpdatedAt' - The time, in seconds since the epoch, when the job execution was last
-- updated.
--
-- 'queuedAt', 'jobExecutionSummary_queuedAt' - The time, in seconds since the epoch, when the job execution was queued.
--
-- 'executionNumber', 'jobExecutionSummary_executionNumber' - A string (consisting of the digits \"0\" through \"9\") which identifies
-- this particular job execution on this particular device. It can be used
-- later in commands which return or update job execution information.
--
-- 'startedAt', 'jobExecutionSummary_startedAt' - The time, in seconds since the epoch, when the job execution started.
newJobExecutionSummary ::
  JobExecutionSummary
newJobExecutionSummary =
  JobExecutionSummary'
    { status = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      queuedAt = Prelude.Nothing,
      executionNumber = Prelude.Nothing,
      startedAt = Prelude.Nothing
    }

-- | The status of the job execution.
jobExecutionSummary_status :: Lens.Lens' JobExecutionSummary (Prelude.Maybe JobExecutionStatus)
jobExecutionSummary_status = Lens.lens (\JobExecutionSummary' {status} -> status) (\s@JobExecutionSummary' {} a -> s {status = a} :: JobExecutionSummary)

-- | The time, in seconds since the epoch, when the job execution was last
-- updated.
jobExecutionSummary_lastUpdatedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.UTCTime)
jobExecutionSummary_lastUpdatedAt = Lens.lens (\JobExecutionSummary' {lastUpdatedAt} -> lastUpdatedAt) (\s@JobExecutionSummary' {} a -> s {lastUpdatedAt = a} :: JobExecutionSummary) Prelude.. Lens.mapping Core._Time

-- | The time, in seconds since the epoch, when the job execution was queued.
jobExecutionSummary_queuedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.UTCTime)
jobExecutionSummary_queuedAt = Lens.lens (\JobExecutionSummary' {queuedAt} -> queuedAt) (\s@JobExecutionSummary' {} a -> s {queuedAt = a} :: JobExecutionSummary) Prelude.. Lens.mapping Core._Time

-- | A string (consisting of the digits \"0\" through \"9\") which identifies
-- this particular job execution on this particular device. It can be used
-- later in commands which return or update job execution information.
jobExecutionSummary_executionNumber :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.Integer)
jobExecutionSummary_executionNumber = Lens.lens (\JobExecutionSummary' {executionNumber} -> executionNumber) (\s@JobExecutionSummary' {} a -> s {executionNumber = a} :: JobExecutionSummary)

-- | The time, in seconds since the epoch, when the job execution started.
jobExecutionSummary_startedAt :: Lens.Lens' JobExecutionSummary (Prelude.Maybe Prelude.UTCTime)
jobExecutionSummary_startedAt = Lens.lens (\JobExecutionSummary' {startedAt} -> startedAt) (\s@JobExecutionSummary' {} a -> s {startedAt = a} :: JobExecutionSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON JobExecutionSummary where
  parseJSON =
    Core.withObject
      "JobExecutionSummary"
      ( \x ->
          JobExecutionSummary'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdatedAt")
            Prelude.<*> (x Core..:? "queuedAt")
            Prelude.<*> (x Core..:? "executionNumber")
            Prelude.<*> (x Core..:? "startedAt")
      )

instance Prelude.Hashable JobExecutionSummary where
  hashWithSalt salt' JobExecutionSummary' {..} =
    salt' `Prelude.hashWithSalt` startedAt
      `Prelude.hashWithSalt` executionNumber
      `Prelude.hashWithSalt` queuedAt
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData JobExecutionSummary where
  rnf JobExecutionSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf startedAt
      `Prelude.seq` Prelude.rnf executionNumber
      `Prelude.seq` Prelude.rnf queuedAt
      `Prelude.seq` Prelude.rnf lastUpdatedAt
