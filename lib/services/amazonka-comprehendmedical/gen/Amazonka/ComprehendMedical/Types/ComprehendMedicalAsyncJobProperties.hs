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
-- Module      : Amazonka.ComprehendMedical.Types.ComprehendMedicalAsyncJobProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComprehendMedical.Types.ComprehendMedicalAsyncJobProperties where

import Amazonka.ComprehendMedical.Types.InputDataConfig
import Amazonka.ComprehendMedical.Types.JobStatus
import Amazonka.ComprehendMedical.Types.LanguageCode
import Amazonka.ComprehendMedical.Types.OutputDataConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a detection job.
--
-- /See:/ 'newComprehendMedicalAsyncJobProperties' smart constructor.
data ComprehendMedicalAsyncJobProperties = ComprehendMedicalAsyncJobProperties'
  { -- | The language code of the input documents.
    languageCode :: Prelude.Maybe LanguageCode,
    -- | The AWS Key Management Service key, if any, used to encrypt the output
    -- files.
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The identifier assigned to the detection job.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned to the detection job.
    jobName :: Prelude.Maybe Prelude.Text,
    -- | The path to the file that describes the results of a batch job.
    manifestFilePath :: Prelude.Maybe Prelude.Text,
    -- | The input data configuration that you supplied when you created the
    -- detection job.
    inputDataConfig :: Prelude.Maybe InputDataConfig,
    -- | The time that the detection job completed.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | The output data configuration that you supplied when you created the
    -- detection job.
    outputDataConfig :: Prelude.Maybe OutputDataConfig,
    -- | The version of the model used to analyze the documents. The version
    -- number looks like X.X.X. You can use this information to track the model
    -- used for a particular batch of documents.
    modelVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that gives Amazon Comprehend Medical read
    -- access to your input data.
    dataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The current status of the detection job. If the status is @FAILED@, the
    -- @Message@ field shows the reason for the failure.
    jobStatus :: Prelude.Maybe JobStatus,
    -- | A description of the status of a job.
    message :: Prelude.Maybe Prelude.Text,
    -- | The date and time that job metadata is deleted from the server. Output
    -- files in your S3 bucket will not be deleted. After the metadata is
    -- deleted, the job will no longer appear in the results of the
    -- @ListEntitiesDetectionV2Job@ or the @ListPHIDetectionJobs@ operation.
    expirationTime :: Prelude.Maybe Core.POSIX,
    -- | The time that the detection job was submitted for processing.
    submitTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ComprehendMedicalAsyncJobProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'comprehendMedicalAsyncJobProperties_languageCode' - The language code of the input documents.
--
-- 'kmsKey', 'comprehendMedicalAsyncJobProperties_kmsKey' - The AWS Key Management Service key, if any, used to encrypt the output
-- files.
--
-- 'jobId', 'comprehendMedicalAsyncJobProperties_jobId' - The identifier assigned to the detection job.
--
-- 'jobName', 'comprehendMedicalAsyncJobProperties_jobName' - The name that you assigned to the detection job.
--
-- 'manifestFilePath', 'comprehendMedicalAsyncJobProperties_manifestFilePath' - The path to the file that describes the results of a batch job.
--
-- 'inputDataConfig', 'comprehendMedicalAsyncJobProperties_inputDataConfig' - The input data configuration that you supplied when you created the
-- detection job.
--
-- 'endTime', 'comprehendMedicalAsyncJobProperties_endTime' - The time that the detection job completed.
--
-- 'outputDataConfig', 'comprehendMedicalAsyncJobProperties_outputDataConfig' - The output data configuration that you supplied when you created the
-- detection job.
--
-- 'modelVersion', 'comprehendMedicalAsyncJobProperties_modelVersion' - The version of the model used to analyze the documents. The version
-- number looks like X.X.X. You can use this information to track the model
-- used for a particular batch of documents.
--
-- 'dataAccessRoleArn', 'comprehendMedicalAsyncJobProperties_dataAccessRoleArn' - The Amazon Resource Name (ARN) that gives Amazon Comprehend Medical read
-- access to your input data.
--
-- 'jobStatus', 'comprehendMedicalAsyncJobProperties_jobStatus' - The current status of the detection job. If the status is @FAILED@, the
-- @Message@ field shows the reason for the failure.
--
-- 'message', 'comprehendMedicalAsyncJobProperties_message' - A description of the status of a job.
--
-- 'expirationTime', 'comprehendMedicalAsyncJobProperties_expirationTime' - The date and time that job metadata is deleted from the server. Output
-- files in your S3 bucket will not be deleted. After the metadata is
-- deleted, the job will no longer appear in the results of the
-- @ListEntitiesDetectionV2Job@ or the @ListPHIDetectionJobs@ operation.
--
-- 'submitTime', 'comprehendMedicalAsyncJobProperties_submitTime' - The time that the detection job was submitted for processing.
newComprehendMedicalAsyncJobProperties ::
  ComprehendMedicalAsyncJobProperties
newComprehendMedicalAsyncJobProperties =
  ComprehendMedicalAsyncJobProperties'
    { languageCode =
        Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      jobId = Prelude.Nothing,
      jobName = Prelude.Nothing,
      manifestFilePath = Prelude.Nothing,
      inputDataConfig = Prelude.Nothing,
      endTime = Prelude.Nothing,
      outputDataConfig = Prelude.Nothing,
      modelVersion = Prelude.Nothing,
      dataAccessRoleArn = Prelude.Nothing,
      jobStatus = Prelude.Nothing,
      message = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      submitTime = Prelude.Nothing
    }

-- | The language code of the input documents.
comprehendMedicalAsyncJobProperties_languageCode :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe LanguageCode)
comprehendMedicalAsyncJobProperties_languageCode = Lens.lens (\ComprehendMedicalAsyncJobProperties' {languageCode} -> languageCode) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {languageCode = a} :: ComprehendMedicalAsyncJobProperties)

-- | The AWS Key Management Service key, if any, used to encrypt the output
-- files.
comprehendMedicalAsyncJobProperties_kmsKey :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_kmsKey = Lens.lens (\ComprehendMedicalAsyncJobProperties' {kmsKey} -> kmsKey) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {kmsKey = a} :: ComprehendMedicalAsyncJobProperties)

-- | The identifier assigned to the detection job.
comprehendMedicalAsyncJobProperties_jobId :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_jobId = Lens.lens (\ComprehendMedicalAsyncJobProperties' {jobId} -> jobId) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {jobId = a} :: ComprehendMedicalAsyncJobProperties)

-- | The name that you assigned to the detection job.
comprehendMedicalAsyncJobProperties_jobName :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_jobName = Lens.lens (\ComprehendMedicalAsyncJobProperties' {jobName} -> jobName) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {jobName = a} :: ComprehendMedicalAsyncJobProperties)

-- | The path to the file that describes the results of a batch job.
comprehendMedicalAsyncJobProperties_manifestFilePath :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_manifestFilePath = Lens.lens (\ComprehendMedicalAsyncJobProperties' {manifestFilePath} -> manifestFilePath) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {manifestFilePath = a} :: ComprehendMedicalAsyncJobProperties)

-- | The input data configuration that you supplied when you created the
-- detection job.
comprehendMedicalAsyncJobProperties_inputDataConfig :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe InputDataConfig)
comprehendMedicalAsyncJobProperties_inputDataConfig = Lens.lens (\ComprehendMedicalAsyncJobProperties' {inputDataConfig} -> inputDataConfig) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {inputDataConfig = a} :: ComprehendMedicalAsyncJobProperties)

-- | The time that the detection job completed.
comprehendMedicalAsyncJobProperties_endTime :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.UTCTime)
comprehendMedicalAsyncJobProperties_endTime = Lens.lens (\ComprehendMedicalAsyncJobProperties' {endTime} -> endTime) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {endTime = a} :: ComprehendMedicalAsyncJobProperties) Prelude.. Lens.mapping Core._Time

-- | The output data configuration that you supplied when you created the
-- detection job.
comprehendMedicalAsyncJobProperties_outputDataConfig :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe OutputDataConfig)
comprehendMedicalAsyncJobProperties_outputDataConfig = Lens.lens (\ComprehendMedicalAsyncJobProperties' {outputDataConfig} -> outputDataConfig) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {outputDataConfig = a} :: ComprehendMedicalAsyncJobProperties)

-- | The version of the model used to analyze the documents. The version
-- number looks like X.X.X. You can use this information to track the model
-- used for a particular batch of documents.
comprehendMedicalAsyncJobProperties_modelVersion :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_modelVersion = Lens.lens (\ComprehendMedicalAsyncJobProperties' {modelVersion} -> modelVersion) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {modelVersion = a} :: ComprehendMedicalAsyncJobProperties)

-- | The Amazon Resource Name (ARN) that gives Amazon Comprehend Medical read
-- access to your input data.
comprehendMedicalAsyncJobProperties_dataAccessRoleArn :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_dataAccessRoleArn = Lens.lens (\ComprehendMedicalAsyncJobProperties' {dataAccessRoleArn} -> dataAccessRoleArn) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {dataAccessRoleArn = a} :: ComprehendMedicalAsyncJobProperties)

-- | The current status of the detection job. If the status is @FAILED@, the
-- @Message@ field shows the reason for the failure.
comprehendMedicalAsyncJobProperties_jobStatus :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe JobStatus)
comprehendMedicalAsyncJobProperties_jobStatus = Lens.lens (\ComprehendMedicalAsyncJobProperties' {jobStatus} -> jobStatus) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {jobStatus = a} :: ComprehendMedicalAsyncJobProperties)

-- | A description of the status of a job.
comprehendMedicalAsyncJobProperties_message :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.Text)
comprehendMedicalAsyncJobProperties_message = Lens.lens (\ComprehendMedicalAsyncJobProperties' {message} -> message) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {message = a} :: ComprehendMedicalAsyncJobProperties)

-- | The date and time that job metadata is deleted from the server. Output
-- files in your S3 bucket will not be deleted. After the metadata is
-- deleted, the job will no longer appear in the results of the
-- @ListEntitiesDetectionV2Job@ or the @ListPHIDetectionJobs@ operation.
comprehendMedicalAsyncJobProperties_expirationTime :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.UTCTime)
comprehendMedicalAsyncJobProperties_expirationTime = Lens.lens (\ComprehendMedicalAsyncJobProperties' {expirationTime} -> expirationTime) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {expirationTime = a} :: ComprehendMedicalAsyncJobProperties) Prelude.. Lens.mapping Core._Time

-- | The time that the detection job was submitted for processing.
comprehendMedicalAsyncJobProperties_submitTime :: Lens.Lens' ComprehendMedicalAsyncJobProperties (Prelude.Maybe Prelude.UTCTime)
comprehendMedicalAsyncJobProperties_submitTime = Lens.lens (\ComprehendMedicalAsyncJobProperties' {submitTime} -> submitTime) (\s@ComprehendMedicalAsyncJobProperties' {} a -> s {submitTime = a} :: ComprehendMedicalAsyncJobProperties) Prelude.. Lens.mapping Core._Time

instance
  Core.FromJSON
    ComprehendMedicalAsyncJobProperties
  where
  parseJSON =
    Core.withObject
      "ComprehendMedicalAsyncJobProperties"
      ( \x ->
          ComprehendMedicalAsyncJobProperties'
            Prelude.<$> (x Core..:? "LanguageCode")
            Prelude.<*> (x Core..:? "KMSKey")
            Prelude.<*> (x Core..:? "JobId")
            Prelude.<*> (x Core..:? "JobName")
            Prelude.<*> (x Core..:? "ManifestFilePath")
            Prelude.<*> (x Core..:? "InputDataConfig")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "OutputDataConfig")
            Prelude.<*> (x Core..:? "ModelVersion")
            Prelude.<*> (x Core..:? "DataAccessRoleArn")
            Prelude.<*> (x Core..:? "JobStatus")
            Prelude.<*> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "ExpirationTime")
            Prelude.<*> (x Core..:? "SubmitTime")
      )

instance
  Prelude.Hashable
    ComprehendMedicalAsyncJobProperties
  where
  hashWithSalt
    salt'
    ComprehendMedicalAsyncJobProperties' {..} =
      salt' `Prelude.hashWithSalt` submitTime
        `Prelude.hashWithSalt` expirationTime
        `Prelude.hashWithSalt` message
        `Prelude.hashWithSalt` jobStatus
        `Prelude.hashWithSalt` dataAccessRoleArn
        `Prelude.hashWithSalt` modelVersion
        `Prelude.hashWithSalt` outputDataConfig
        `Prelude.hashWithSalt` endTime
        `Prelude.hashWithSalt` inputDataConfig
        `Prelude.hashWithSalt` manifestFilePath
        `Prelude.hashWithSalt` jobName
        `Prelude.hashWithSalt` jobId
        `Prelude.hashWithSalt` kmsKey
        `Prelude.hashWithSalt` languageCode

instance
  Prelude.NFData
    ComprehendMedicalAsyncJobProperties
  where
  rnf ComprehendMedicalAsyncJobProperties' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf submitTime
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf jobStatus
      `Prelude.seq` Prelude.rnf dataAccessRoleArn
      `Prelude.seq` Prelude.rnf modelVersion
      `Prelude.seq` Prelude.rnf outputDataConfig
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf inputDataConfig
      `Prelude.seq` Prelude.rnf manifestFilePath
      `Prelude.seq` Prelude.rnf jobName
      `Prelude.seq` Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf kmsKey
