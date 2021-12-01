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
-- Module      : Amazonka.Glue.Types.JobBookmarkEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.JobBookmarkEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Defines a point that a job can resume processing.
--
-- /See:/ 'newJobBookmarkEntry' smart constructor.
data JobBookmarkEntry = JobBookmarkEntry'
  { -- | The name of the job in question.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The run ID number.
    run :: Prelude.Maybe Prelude.Int,
    -- | The run ID number.
    runId :: Prelude.Maybe Prelude.Text,
    -- | The version of the job.
    version :: Prelude.Maybe Prelude.Int,
    -- | The unique run identifier associated with the previous job run.
    previousRunId :: Prelude.Maybe Prelude.Text,
    -- | The attempt ID number.
    attempt :: Prelude.Maybe Prelude.Int,
    -- | The bookmark itself.
    jobBookmark :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobBookmarkEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobName', 'jobBookmarkEntry_jobName' - The name of the job in question.
--
-- 'run', 'jobBookmarkEntry_run' - The run ID number.
--
-- 'runId', 'jobBookmarkEntry_runId' - The run ID number.
--
-- 'version', 'jobBookmarkEntry_version' - The version of the job.
--
-- 'previousRunId', 'jobBookmarkEntry_previousRunId' - The unique run identifier associated with the previous job run.
--
-- 'attempt', 'jobBookmarkEntry_attempt' - The attempt ID number.
--
-- 'jobBookmark', 'jobBookmarkEntry_jobBookmark' - The bookmark itself.
newJobBookmarkEntry ::
  JobBookmarkEntry
newJobBookmarkEntry =
  JobBookmarkEntry'
    { jobName = Prelude.Nothing,
      run = Prelude.Nothing,
      runId = Prelude.Nothing,
      version = Prelude.Nothing,
      previousRunId = Prelude.Nothing,
      attempt = Prelude.Nothing,
      jobBookmark = Prelude.Nothing
    }

-- | The name of the job in question.
jobBookmarkEntry_jobName :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_jobName = Lens.lens (\JobBookmarkEntry' {jobName} -> jobName) (\s@JobBookmarkEntry' {} a -> s {jobName = a} :: JobBookmarkEntry)

-- | The run ID number.
jobBookmarkEntry_run :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Int)
jobBookmarkEntry_run = Lens.lens (\JobBookmarkEntry' {run} -> run) (\s@JobBookmarkEntry' {} a -> s {run = a} :: JobBookmarkEntry)

-- | The run ID number.
jobBookmarkEntry_runId :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_runId = Lens.lens (\JobBookmarkEntry' {runId} -> runId) (\s@JobBookmarkEntry' {} a -> s {runId = a} :: JobBookmarkEntry)

-- | The version of the job.
jobBookmarkEntry_version :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Int)
jobBookmarkEntry_version = Lens.lens (\JobBookmarkEntry' {version} -> version) (\s@JobBookmarkEntry' {} a -> s {version = a} :: JobBookmarkEntry)

-- | The unique run identifier associated with the previous job run.
jobBookmarkEntry_previousRunId :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_previousRunId = Lens.lens (\JobBookmarkEntry' {previousRunId} -> previousRunId) (\s@JobBookmarkEntry' {} a -> s {previousRunId = a} :: JobBookmarkEntry)

-- | The attempt ID number.
jobBookmarkEntry_attempt :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Int)
jobBookmarkEntry_attempt = Lens.lens (\JobBookmarkEntry' {attempt} -> attempt) (\s@JobBookmarkEntry' {} a -> s {attempt = a} :: JobBookmarkEntry)

-- | The bookmark itself.
jobBookmarkEntry_jobBookmark :: Lens.Lens' JobBookmarkEntry (Prelude.Maybe Prelude.Text)
jobBookmarkEntry_jobBookmark = Lens.lens (\JobBookmarkEntry' {jobBookmark} -> jobBookmark) (\s@JobBookmarkEntry' {} a -> s {jobBookmark = a} :: JobBookmarkEntry)

instance Core.FromJSON JobBookmarkEntry where
  parseJSON =
    Core.withObject
      "JobBookmarkEntry"
      ( \x ->
          JobBookmarkEntry'
            Prelude.<$> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "Run")
            Prelude.<*> (x Core..:? "RunId")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "PreviousRunId")
            Prelude.<*> (x Core..:? "Attempt")
            Prelude.<*> (x Core..:? "JobBookmark")
      )

instance Prelude.Hashable JobBookmarkEntry where
  hashWithSalt salt' JobBookmarkEntry' {..} =
    salt' `Prelude.hashWithSalt` jobBookmark
      `Prelude.hashWithSalt` attempt
      `Prelude.hashWithSalt` previousRunId
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` runId
      `Prelude.hashWithSalt` run
      `Prelude.hashWithSalt` jobName

instance Prelude.NFData JobBookmarkEntry where
  rnf JobBookmarkEntry' {..} =
    Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobBookmark
      `Prelude.seq` Prelude.rnf attempt
      `Prelude.seq` Prelude.rnf previousRunId
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf runId
      `Prelude.seq` Prelude.rnf run
