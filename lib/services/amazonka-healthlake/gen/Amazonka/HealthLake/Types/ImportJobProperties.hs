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
-- Module      : Amazonka.HealthLake.Types.ImportJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.HealthLake.Types.ImportJobProperties where

import qualified Amazonka.Core as Core
import Amazonka.HealthLake.Types.InputDataConfig
import Amazonka.HealthLake.Types.JobStatus
import Amazonka.HealthLake.Types.OutputDataConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Displays the properties of the import job, including the ID, Arn, Name,
-- and the status of the Data Store.
--
-- /See:/ 'newImportJobProperties' smart constructor.
data ImportJobProperties = ImportJobProperties'
  { jobOutputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The user-generated name for an Import job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The time that the Import job was completed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) that gives Amazon HealthLake access to
    -- your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | An explanation of any errors that may have occurred during the FHIR
    -- import job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The AWS-generated id number for the Import job.
    jobId :: Prelude.Text,
    -- | The job status for an Import job. Possible statuses are SUBMITTED,
    -- IN_PROGRESS, COMPLETED, FAILED.
    jobStatus :: JobStatus,
    -- | The time that the Import job was submitted for processing.
    submitTime :: Core.POSIX,
    -- | The datastore id used when the Import job was created.
    datastoreId :: Prelude.Text,
    -- | The input data configuration that was supplied when the Import job was
    -- created.
    inputDataConfig :: InputDataConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobOutputDataConfig', 'importJobProperties_jobOutputDataConfig' - Undocumented member.
--
-- 'jobName', 'importJobProperties_jobName' - The user-generated name for an Import job.
--
-- 'endTime', 'importJobProperties_endTime' - The time that the Import job was completed.
--
-- 'dataAccessRoleArn', 'importJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon HealthLake access to
-- your input data.
--
-- 'message', 'importJobProperties_message' - An explanation of any errors that may have occurred during the FHIR
-- import job.
--
-- 'jobId', 'importJobProperties_jobId' - The AWS-generated id number for the Import job.
--
-- 'jobStatus', 'importJobProperties_jobStatus' - The job status for an Import job. Possible statuses are SUBMITTED,
-- IN_PROGRESS, COMPLETED, FAILED.
--
-- 'submitTime', 'importJobProperties_submitTime' - The time that the Import job was submitted for processing.
--
-- 'datastoreId', 'importJobProperties_datastoreId' - The datastore id used when the Import job was created.
--
-- 'inputDataConfig', 'importJobProperties_inputDataConfig' - The input data configuration that was supplied when the Import job was
-- created.
newImportJobProperties ::
  -- | 'jobId'
  Prelude.Text ->
  -- | 'jobStatus'
  JobStatus ->
  -- | 'submitTime'
  Prelude.UTCTime ->
  -- | 'datastoreId'
  Prelude.Text ->
  -- | 'inputDataConfig'
  InputDataConfig ->
  ImportJobProperties
newImportJobProperties
  pJobId_
  pJobStatus_
  pSubmitTime_
  pDatastoreId_
  pInputDataConfig_ =
    ImportJobProperties'
      { jobOutputDataConfig =
          Prelude.Nothing,
        jobName = Prelude.Nothing,
        endTime = Prelude.Nothing,
        dataAccessRoleArn = Prelude.Nothing,
        message = Prelude.Nothing,
        jobId = pJobId_,
        jobStatus = pJobStatus_,
        submitTime = Core._Time Lens.# pSubmitTime_,
        datastoreId = pDatastoreId_,
        inputDataConfig = pInputDataConfig_
      }

-- | Undocumented member.
importJobProperties_jobOutputDataConfig :: Lens.Lens' ImportJobProperties (Prelude.Maybe OutputDataConfig)
importJobProperties_jobOutputDataConfig = Lens.lens (\ImportJobProperties' {jobOutputDataConfig} -> jobOutputDataConfig) (\s@ImportJobProperties' {} a -> s {jobOutputDataConfig = a} :: ImportJobProperties)

-- | The user-generated name for an Import job.
importJobProperties_jobName :: Lens.Lens' ImportJobProperties (Prelude.Maybe Prelude.Text)
importJobProperties_jobName = Lens.lens (\ImportJobProperties' {jobName} -> jobName) (\s@ImportJobProperties' {} a -> s {jobName = a} :: ImportJobProperties)

-- | The time that the Import job was completed.
importJobProperties_endTime :: Lens.Lens' ImportJobProperties (Prelude.Maybe Prelude.UTCTime)
importJobProperties_endTime = Lens.lens (\ImportJobProperties' {endTime} -> endTime) (\s@ImportJobProperties' {} a -> s {endTime = a} :: ImportJobProperties) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) that gives Amazon HealthLake access to
-- your input data.
importJobProperties_dataAccessRoleArn :: Lens.Lens' ImportJobProperties (Prelude.Maybe Prelude.Text)
importJobProperties_dataAccessRoleArn = Lens.lens (\ImportJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@ImportJobProperties' {} a -> s {dataAccessRoleArn = a} :: ImportJobProperties)

-- | An explanation of any errors that may have occurred during the FHIR
-- import job.
importJobProperties_message :: Lens.Lens' ImportJobProperties (Prelude.Maybe Prelude.Text)
importJobProperties_message = Lens.lens (\ImportJobProperties' {message} -> message) (\s@ImportJobProperties' {} a -> s {message = a} :: ImportJobProperties)

-- | The AWS-generated id number for the Import job.
importJobProperties_jobId :: Lens.Lens' ImportJobProperties Prelude.Text
importJobProperties_jobId = Lens.lens (\ImportJobProperties' {jobId} -> jobId) (\s@ImportJobProperties' {} a -> s {jobId = a} :: ImportJobProperties)

-- | The job status for an Import job. Possible statuses are SUBMITTED,
-- IN_PROGRESS, COMPLETED, FAILED.
importJobProperties_jobStatus :: Lens.Lens' ImportJobProperties JobStatus
importJobProperties_jobStatus = Lens.lens (\ImportJobProperties' {jobStatus} -> jobStatus) (\s@ImportJobProperties' {} a -> s {jobStatus = a} :: ImportJobProperties)

-- | The time that the Import job was submitted for processing.
importJobProperties_submitTime :: Lens.Lens' ImportJobProperties Prelude.UTCTime
importJobProperties_submitTime = Lens.lens (\ImportJobProperties' {submitTime} -> submitTime) (\s@ImportJobProperties' {} a -> s {submitTime = a} :: ImportJobProperties) Prelude.. Core._Time

-- | The datastore id used when the Import job was created.
importJobProperties_datastoreId :: Lens.Lens' ImportJobProperties Prelude.Text
importJobProperties_datastoreId = Lens.lens (\ImportJobProperties' {datastoreId} -> datastoreId) (\s@ImportJobProperties' {} a -> s {datastoreId = a} :: ImportJobProperties)

-- | The input data configuration that was supplied when the Import job was
-- created.
importJobProperties_inputDataConfig :: Lens.Lens' ImportJobProperties InputDataConfig
importJobProperties_inputDataConfig = Lens.lens (\ImportJobProperties' {inputDataConfig} -> inputDataConfig) (\s@ImportJobProperties' {} a -> s {inputDataConfig = a} :: ImportJobProperties)

instance Core.FromJSON ImportJobProperties where
  parseJSON =
    Core.withObject
      "ImportJobProperties"
      ( \x ->
          ImportJobProperties'
            Prelude.<$> (x Core..:? "JobOutputDataConfig")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "Message")
            Prelude.<*> (x Core..: "JobId")
            Prelude.<*> (x Core..: "JobStatus")
            Prelude.<*> (x Core..: "SubmitTime")
            Prelude.<*> (x Core..: "DatastoreId")
            Prelude.<*> (x Core..: "InputDataConfig")
      )

instance Prelude.Hashable ImportJobProperties where
  hashWithSalt salt' ImportJobProperties' {..} =
    salt' `Prelude.hashWithSalt` inputDataConfig
      `Prelude.hashWithSalt` datastoreId
      `Prelude.hashWithSalt` submitTime
      `Prelude.hashWithSalt` jobStatus
      `Prelude.hashWithSalt` jobId
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` dataAccessRoleArn
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` jobName
      `Prelude.hashWithSalt` jobOutputDataConfig

instance Prelude.NFData ImportJobProperties where
  rnf ImportJobProperties' {..} =
    Prelude.rnf jobOutputDataConfig
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf datastoreId
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf jobName
