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
-- Module      : Amazonka.AccessAnalyzer.Types.JobDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.JobDetails where

import Amazonka.AccessAnalyzer.Types.JobError
import Amazonka.AccessAnalyzer.Types.JobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the policy generation request.
--
-- /See:/ 'newJobDetails' smart constructor.
data JobDetails = JobDetails'
  { -- | A timestamp of when the job was completed.
    completedOn :: Prelude.Maybe Core.POSIX,
    -- | The job error for the policy generation request.
    jobError :: Prelude.Maybe JobError,
    -- | The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
    -- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
    -- generated policies or used with @CancelPolicyGeneration@ to cancel the
    -- policy generation request.
    jobId :: Prelude.Text,
    -- | A timestamp of when the job was started.
    startedOn :: Core.POSIX,
    -- | The status of the job request.
    status :: JobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completedOn', 'jobDetails_completedOn' - A timestamp of when the job was completed.
--
-- 'jobError', 'jobDetails_jobError' - The job error for the policy generation request.
--
-- 'jobId', 'jobDetails_jobId' - The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
-- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
-- generated policies or used with @CancelPolicyGeneration@ to cancel the
-- policy generation request.
--
-- 'startedOn', 'jobDetails_startedOn' - A timestamp of when the job was started.
--
-- 'status', 'jobDetails_status' - The status of the job request.
newJobDetails ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'startedOn'
  Prelude.UTCTime ->
  -- | 'status'
  JobStatus ->
  JobDetails
newJobDetails pJobId_ pStartedOn_ pStatus_ =
  JobDetails'
    { completedOn = Prelude.Nothing,
      jobError = Prelude.Nothing,
      jobId = pJobId_,
      startedOn = Core._Time Lens.# pStartedOn_,
      status = pStatus_
    }

-- | A timestamp of when the job was completed.
jobDetails_completedOn :: Lens.Lens' JobDetails (Prelude.Maybe Prelude.UTCTime)
jobDetails_completedOn = Lens.lens (\JobDetails' {completedOn} -> completedOn) (\s@JobDetails' {} a -> s {completedOn = a} :: JobDetails) Prelude.. Lens.mapping Core._Time

-- | The job error for the policy generation request.
jobDetails_jobError :: Lens.Lens' JobDetails (Prelude.Maybe JobError)
jobDetails_jobError = Lens.lens (\JobDetails' {jobError} -> jobError) (\s@JobDetails' {} a -> s {jobError = a} :: JobDetails)

-- | The @JobId@ that is returned by the @StartPolicyGeneration@ operation.
-- The @JobId@ can be used with @GetGeneratedPolicy@ to retrieve the
-- generated policies or used with @CancelPolicyGeneration@ to cancel the
-- policy generation request.
jobDetails_jobId :: Lens.Lens' JobDetails Prelude.Text
jobDetails_jobId = Lens.lens (\JobDetails' {jobId} -> jobId) (\s@JobDetails' {} a -> s {jobId = a} :: JobDetails)

-- | A timestamp of when the job was started.
jobDetails_startedOn :: Lens.Lens' JobDetails Prelude.UTCTime
jobDetails_startedOn = Lens.lens (\JobDetails' {startedOn} -> startedOn) (\s@JobDetails' {} a -> s {startedOn = a} :: JobDetails) Prelude.. Core._Time

-- | The status of the job request.
jobDetails_status :: Lens.Lens' JobDetails JobStatus
jobDetails_status = Lens.lens (\JobDetails' {status} -> status) (\s@JobDetails' {} a -> s {status = a} :: JobDetails)

instance Core.FromJSON JobDetails where
  parseJSON =
    Core.withObject
      "JobDetails"
      ( \x ->
          JobDetails'
            Prelude.<$> (x Core..:? "completedOn")
            Prelude.<*> (x Core..:? "jobError")
            Prelude.<*> (x Core..: "jobId")
            Prelude.<*> (x Core..: "startedOn")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable JobDetails where
  hashWithSalt salt' JobDetails' {..} =
    salt' `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` startedOn
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` jobError
      `Prelude.hashWithSalt` completedOn

instance Prelude.NFData JobDetails where
  rnf JobDetails' {..} =
    Prelude.rnf completedOn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf startedOn
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf jobError
