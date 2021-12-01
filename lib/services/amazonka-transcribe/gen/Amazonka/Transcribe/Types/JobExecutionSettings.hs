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
-- Module      : Amazonka.Transcribe.Types.JobExecutionSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transcribe.Types.JobExecutionSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about when a transcription job should be executed.
--
-- /See:/ 'newJobExecutionSettings' smart constructor.
data JobExecutionSettings = JobExecutionSettings'
  { -- | The Amazon Resource Name (ARN) of a role that has access to the S3
    -- bucket that contains the input files. Amazon Transcribe assumes this
    -- role to read queued media files. If you have specified an output S3
    -- bucket for the transcription results, this role should have access to
    -- the output bucket as well.
    --
    -- If you specify the @AllowDeferredExecution@ field, you must specify the
    -- @DataAccessRoleArn@ field.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a job should be queued by Amazon Transcribe when the
    -- concurrent execution limit is exceeded. When the
    -- @AllowDeferredExecution@ field is true, jobs are queued and executed
    -- when the number of executing jobs falls below the concurrent execution
    -- limit. If the field is false, Amazon Transcribe returns a
    -- @LimitExceededException@ exception.
    --
    -- Note that job queuing is enabled by default for call analytics jobs.
    --
    -- If you specify the @AllowDeferredExecution@ field, you must specify the
    -- @DataAccessRoleArn@ field.
    allowDeferredExecution :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobExecutionSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataAccessRoleArn', 'jobExecutionSettings_dataAccessRoleArn' - The Amazon Resource Name (ARN) of a role that has access to the S3
-- bucket that contains the input files. Amazon Transcribe assumes this
-- role to read queued media files. If you have specified an output S3
-- bucket for the transcription results, this role should have access to
-- the output bucket as well.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the
-- @DataAccessRoleArn@ field.
--
-- 'allowDeferredExecution', 'jobExecutionSettings_allowDeferredExecution' - Indicates whether a job should be queued by Amazon Transcribe when the
-- concurrent execution limit is exceeded. When the
-- @AllowDeferredExecution@ field is true, jobs are queued and executed
-- when the number of executing jobs falls below the concurrent execution
-- limit. If the field is false, Amazon Transcribe returns a
-- @LimitExceededException@ exception.
--
-- Note that job queuing is enabled by default for call analytics jobs.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the
-- @DataAccessRoleArn@ field.
newJobExecutionSettings ::
  JobExecutionSettings
newJobExecutionSettings =
  JobExecutionSettings'
    { dataAccessRoleArn =
        Prelude.Nothing,
      allowDeferredExecution = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of a role that has access to the S3
-- bucket that contains the input files. Amazon Transcribe assumes this
-- role to read queued media files. If you have specified an output S3
-- bucket for the transcription results, this role should have access to
-- the output bucket as well.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the
-- @DataAccessRoleArn@ field.
jobExecutionSettings_dataAccessRoleArn :: Lens.Lens' JobExecutionSettings (Prelude.Maybe Prelude.Text)
jobExecutionSettings_dataAccessRoleArn = Lens.lens (\JobExecutionSettings' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@JobExecutionSettings' {} a -> s {dataAccessRoleArn = a} :: JobExecutionSettings)

-- | Indicates whether a job should be queued by Amazon Transcribe when the
-- concurrent execution limit is exceeded. When the
-- @AllowDeferredExecution@ field is true, jobs are queued and executed
-- when the number of executing jobs falls below the concurrent execution
-- limit. If the field is false, Amazon Transcribe returns a
-- @LimitExceededException@ exception.
--
-- Note that job queuing is enabled by default for call analytics jobs.
--
-- If you specify the @AllowDeferredExecution@ field, you must specify the
-- @DataAccessRoleArn@ field.
jobExecutionSettings_allowDeferredExecution :: Lens.Lens' JobExecutionSettings (Prelude.Maybe Prelude.Bool)
jobExecutionSettings_allowDeferredExecution = Lens.lens (\JobExecutionSettings' {allowDeferredExecution} -> allowDeferredExecution) (\s@JobExecutionSettings' {} a -> s {allowDeferredExecution = a} :: JobExecutionSettings)

instance Core.FromJSON JobExecutionSettings where
  parseJSON =
    Core.withObject
      "JobExecutionSettings"
      ( \x ->
          JobExecutionSettings'
            Prelude.<$> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "AllowDeferredExecution")
      )

instance Prelude.Hashable JobExecutionSettings where
  hashWithSalt salt' JobExecutionSettings' {..} =
    salt' `Prelude.hashWithSalt` allowDeferredExecution
      `Prelude.hashWithSalt` dataAccessRoleArn

instance Prelude.NFData JobExecutionSettings where
  rnf JobExecutionSettings' {..} =
    Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf allowDeferredExecution

instance Core.ToJSON JobExecutionSettings where
  toJSON JobExecutionSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DataAccessRoleArn" Core..=)
              Prelude.<$> dataAccessRoleArn,
            ("AllowDeferredExecution" Core..=)
              Prelude.<$> allowDeferredExecution
          ]
      )
