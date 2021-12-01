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
-- Module      : Amazonka.VoiceId.Types.SpeakerEnrollmentJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VoiceId.Types.SpeakerEnrollmentJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.VoiceId.Types.EnrollmentConfig
import Amazonka.VoiceId.Types.FailureDetails
import Amazonka.VoiceId.Types.InputDataConfig
import Amazonka.VoiceId.Types.JobProgress
import Amazonka.VoiceId.Types.OutputDataConfig
import Amazonka.VoiceId.Types.SpeakerEnrollmentJobStatus

-- | Contains all the information about a speaker enrollment job.
--
-- /See:/ 'newSpeakerEnrollmentJob' smart constructor.
data SpeakerEnrollmentJob = SpeakerEnrollmentJob'
  { -- | Contains details that are populated when an entire batch job fails. In
    -- cases of individual registration job failures, the batch job as a whole
    -- doesn\'t fail; it is completed with a @JobStatus@ of
    -- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
    -- individual registration requests that failed.
    failureDetails :: Prelude.Maybe FailureDetails,
    -- | The service-generated identifier for the speaker enrollment job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | A timestamp showing the creation of the speaker enrollment job.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The client-provided name for the speaker enrollment job.
    jobName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A timestamp showing when the speaker enrollment job ended.
    endedAt :: Prelude.Maybe Core.POSIX,
    -- | The configuration that defines the action to take when the speaker is
    -- already enrolled in Voice ID, and the @FraudDetectionConfig@ to use.
    enrollmentConfig :: Prelude.Maybe EnrollmentConfig,
    -- | The input data config containing an S3 URI for the input manifest file
    -- that contains the list of speaker enrollment job requests.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | Provides details on job progress. This field shows the completed
    -- percentage of registration requests listed in the input file.
    jobProgress :: Prelude.Maybe JobProgress,
    -- | The output data config containing the S3 location where Voice ID writes
    -- the job output file; you must also include a KMS Key ID to encrypt the
    -- file.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
    -- to access customer\'s buckets to read the input manifest file and write
    -- the job output file.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the domain that contains the speaker enrollment job.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the speaker enrollment job.
    jobStatus :: Prelude.Maybe SpeakerEnrollmentJobStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpeakerEnrollmentJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureDetails', 'speakerEnrollmentJob_failureDetails' - Contains details that are populated when an entire batch job fails. In
-- cases of individual registration job failures, the batch job as a whole
-- doesn\'t fail; it is completed with a @JobStatus@ of
-- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
-- individual registration requests that failed.
--
-- 'jobId', 'speakerEnrollmentJob_jobId' - The service-generated identifier for the speaker enrollment job.
--
-- 'createdAt', 'speakerEnrollmentJob_createdAt' - A timestamp showing the creation of the speaker enrollment job.
--
-- 'jobName', 'speakerEnrollmentJob_jobName' - The client-provided name for the speaker enrollment job.
--
-- 'endedAt', 'speakerEnrollmentJob_endedAt' - A timestamp showing when the speaker enrollment job ended.
--
-- 'enrollmentConfig', 'speakerEnrollmentJob_enrollmentConfig' - The configuration that defines the action to take when the speaker is
-- already enrolled in Voice ID, and the @FraudDetectionConfig@ to use.
--
-- 'inputDataConfig', 'speakerEnrollmentJob_inputDataConfig' - The input data config containing an S3 URI for the input manifest file
-- that contains the list of speaker enrollment job requests.
--
-- 'jobProgress', 'speakerEnrollmentJob_jobProgress' - Provides details on job progress. This field shows the completed
-- percentage of registration requests listed in the input file.
--
-- 'outputDataConfig', 'speakerEnrollmentJob_outputDataConfig' - The output data config containing the S3 location where Voice ID writes
-- the job output file; you must also include a KMS Key ID to encrypt the
-- file.
--
-- 'dataAccessRoleArn', 'speakerEnrollmentJob_dataAccessRoleArn' - The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
-- to access customer\'s buckets to read the input manifest file and write
-- the job output file.
--
-- 'domainId', 'speakerEnrollmentJob_domainId' - The identifier of the domain that contains the speaker enrollment job.
--
-- 'jobStatus', 'speakerEnrollmentJob_jobStatus' - The current status of the speaker enrollment job.
newSpeakerEnrollmentJob ::
  SpeakerEnrollmentJob
newSpeakerEnrollmentJob =
  SpeakerEnrollmentJob'
    { failureDetails =
        Prelude.Nothing,
      jobId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      jobName = Prelude.Nothing,
      endedAt = Prelude.Nothing,
      enrollmentConfig = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      jobProgress = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      domainId = Prelude.Nothing,
      jobStatus = Prelude.Nothing
    }

-- | Contains details that are populated when an entire batch job fails. In
-- cases of individual registration job failures, the batch job as a whole
-- doesn\'t fail; it is completed with a @JobStatus@ of
-- @COMPLETED_WITH_ERRORS@. You can use the job output file to identify the
-- individual registration requests that failed.
speakerEnrollmentJob_failureDetails :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe FailureDetails)
speakerEnrollmentJob_failureDetails = Lens.lens (\SpeakerEnrollmentJob' {failureDetails} -> failureDetails) (\s@SpeakerEnrollmentJob' {} a -> s {failureDetails = a} :: SpeakerEnrollmentJob)

-- | The service-generated identifier for the speaker enrollment job.
speakerEnrollmentJob_jobId :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe Prelude.Text)
speakerEnrollmentJob_jobId = Lens.lens (\SpeakerEnrollmentJob' {jobId} -> jobId) (\s@SpeakerEnrollmentJob' {} a -> s {jobId = a} :: SpeakerEnrollmentJob)

-- | A timestamp showing the creation of the speaker enrollment job.
speakerEnrollmentJob_createdAt :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe Prelude.UTCTime)
speakerEnrollmentJob_createdAt = Lens.lens (\SpeakerEnrollmentJob' {createdAt} -> createdAt) (\s@SpeakerEnrollmentJob' {} a -> s {createdAt = a} :: SpeakerEnrollmentJob) Prelude.. Lens.mapping Core._Time

-- | The client-provided name for the speaker enrollment job.
speakerEnrollmentJob_jobName :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe Prelude.Text)
speakerEnrollmentJob_jobName = Lens.lens (\SpeakerEnrollmentJob' {jobName} -> jobName) (\s@SpeakerEnrollmentJob' {} a -> s {jobName = a} :: SpeakerEnrollmentJob) Prelude.. Lens.mapping Core._Sensitive

-- | A timestamp showing when the speaker enrollment job ended.
speakerEnrollmentJob_endedAt :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe Prelude.UTCTime)
speakerEnrollmentJob_endedAt = Lens.lens (\SpeakerEnrollmentJob' {endedAt} -> endedAt) (\s@SpeakerEnrollmentJob' {} a -> s {endedAt = a} :: SpeakerEnrollmentJob) Prelude.. Lens.mapping Core._Time

-- | The configuration that defines the action to take when the speaker is
-- already enrolled in Voice ID, and the @FraudDetectionConfig@ to use.
speakerEnrollmentJob_enrollmentConfig :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe EnrollmentConfig)
speakerEnrollmentJob_enrollmentConfig = Lens.lens (\SpeakerEnrollmentJob' {enrollmentConfig} -> enrollmentConfig) (\s@SpeakerEnrollmentJob' {} a -> s {enrollmentConfig = a} :: SpeakerEnrollmentJob)

-- | The input data config containing an S3 URI for the input manifest file
-- that contains the list of speaker enrollment job requests.
speakerEnrollmentJob_inputDataConfig :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe InputDataConfig)
speakerEnrollmentJob_inputDataConfig = Lens.lens (\SpeakerEnrollmentJob' {inputDataConfig} -> inputDataConfig) (\s@SpeakerEnrollmentJob' {} a -> s {inputDataConfig = a} :: SpeakerEnrollmentJob)

-- | Provides details on job progress. This field shows the completed
-- percentage of registration requests listed in the input file.
speakerEnrollmentJob_jobProgress :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe JobProgress)
speakerEnrollmentJob_jobProgress = Lens.lens (\SpeakerEnrollmentJob' {jobProgress} -> jobProgress) (\s@SpeakerEnrollmentJob' {} a -> s {jobProgress = a} :: SpeakerEnrollmentJob)

-- | The output data config containing the S3 location where Voice ID writes
-- the job output file; you must also include a KMS Key ID to encrypt the
-- file.
speakerEnrollmentJob_outputDataConfig :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe OutputDataConfig)
speakerEnrollmentJob_outputDataConfig = Lens.lens (\SpeakerEnrollmentJob' {outputDataConfig} -> outputDataConfig) (\s@SpeakerEnrollmentJob' {} a -> s {outputDataConfig = a} :: SpeakerEnrollmentJob)

-- | The IAM role Amazon Resource Name (ARN) that grants Voice ID permissions
-- to access customer\'s buckets to read the input manifest file and write
-- the job output file.
speakerEnrollmentJob_dataAccessRoleArn :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe Prelude.Text)
speakerEnrollmentJob_dataAccessRoleArn = Lens.lens (\SpeakerEnrollmentJob' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@SpeakerEnrollmentJob' {} a -> s {dataAccessRoleArn = a} :: SpeakerEnrollmentJob)

-- | The identifier of the domain that contains the speaker enrollment job.
speakerEnrollmentJob_domainId :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe Prelude.Text)
speakerEnrollmentJob_domainId = Lens.lens (\SpeakerEnrollmentJob' {domainId} -> domainId) (\s@SpeakerEnrollmentJob' {} a -> s {domainId = a} :: SpeakerEnrollmentJob)

-- | The current status of the speaker enrollment job.
speakerEnrollmentJob_jobStatus :: Lens.Lens' SpeakerEnrollmentJob (Prelude.Maybe SpeakerEnrollmentJobStatus)
speakerEnrollmentJob_jobStatus = Lens.lens (\SpeakerEnrollmentJob' {jobStatus} -> jobStatus) (\s@SpeakerEnrollmentJob' {} a -> s {jobStatus = a} :: SpeakerEnrollmentJob)

instance Core.FromJSON SpeakerEnrollmentJob where
  parseJSON =
    Core.withObject
      "SpeakerEnrollmentJob"
      ( \x ->
          SpeakerEnrollmentJob'
            Prelude.<$> (x Core..:? "FailureDetails")
            Prelude.<*> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "EndedAt")
            Prelude.<*> (x Core..:? "EnrollmentConfig")
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> (x Core..:? "JobProgress")
            Prelude.<*> (x Core..:? "OutputDataConfig")
            Prelude.<*> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "DomainId")
            Prelude.<*> (x Core..:? "JobStatus")
      )

instance Prelude.Hashable SpeakerEnrollmentJob where
  hashWithSalt salt' SpeakerEnrollmentJob' {..} =
    salt' `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` outputDataConfig
      `Prelude.hashWithSalt` jobProgress
      `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` enrollmentConfig
      `Prelude.hashWithSalt` endedAt
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` failureDetails

instance Prelude.NFData SpeakerEnrollmentJob where
  rnf SpeakerEnrollmentJob' {..} =
    Prelude.rnf failureDetails
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf jobProgress
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf enrollmentConfig
      `Prelude.seq` Prelude.rnf endedAt
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf jobId
