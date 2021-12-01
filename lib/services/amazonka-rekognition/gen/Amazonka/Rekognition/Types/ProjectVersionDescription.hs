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
-- Module      : Amazonka.Rekognition.Types.ProjectVersionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Rekognition.Types.ProjectVersionDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Rekognition.Types.EvaluationResult
import Amazonka.Rekognition.Types.GroundTruthManifest
import Amazonka.Rekognition.Types.OutputConfig
import Amazonka.Rekognition.Types.ProjectVersionStatus
import Amazonka.Rekognition.Types.TestingDataResult
import Amazonka.Rekognition.Types.TrainingDataResult

-- | The description of a version of a model.
--
-- /See:/ 'newProjectVersionDescription' smart constructor.
data ProjectVersionDescription = ProjectVersionDescription'
  { -- | The minimum number of inference units used by the model. For more
    -- information, see StartProjectVersion.
    minInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | The current status of the model version.
    status :: Prelude.Maybe ProjectVersionStatus,
    -- | The training results. @EvaluationResult@ is only returned if training is
    -- successful.
    evaluationResult :: Prelude.Maybe EvaluationResult,
    -- | The location of the summary manifest. The summary manifest provides
    -- aggregate data validation results for the training and test datasets.
    manifestSummary :: Prelude.Maybe GroundTruthManifest,
    -- | The identifer for the AWS Key Management Service (AWS KMS) customer
    -- master key that was used to encrypt the model during training.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Contains information about the testing results.
    testingDataResult :: Prelude.Maybe TestingDataResult,
    -- | A descriptive message for an error or warning that occurred.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The Unix datetime for the date and time that training started.
    creationTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the model version.
    projectVersionArn :: Prelude.Maybe Prelude.Text,
    -- | The location where training results are saved.
    outputConfig :: Prelude.Maybe OutputConfig,
    -- | The duration, in seconds, that the model version has been billed for
    -- training. This value is only returned if the model version has been
    -- successfully trained.
    billableTrainingTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Unix date and time that training of the model ended.
    trainingEndTimestamp :: Prelude.Maybe Core.POSIX,
    -- | Contains information about the training results.
    trainingDataResult :: Prelude.Maybe TrainingDataResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProjectVersionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'minInferenceUnits', 'projectVersionDescription_minInferenceUnits' - The minimum number of inference units used by the model. For more
-- information, see StartProjectVersion.
--
-- 'status', 'projectVersionDescription_status' - The current status of the model version.
--
-- 'evaluationResult', 'projectVersionDescription_evaluationResult' - The training results. @EvaluationResult@ is only returned if training is
-- successful.
--
-- 'manifestSummary', 'projectVersionDescription_manifestSummary' - The location of the summary manifest. The summary manifest provides
-- aggregate data validation results for the training and test datasets.
--
-- 'kmsKeyId', 'projectVersionDescription_kmsKeyId' - The identifer for the AWS Key Management Service (AWS KMS) customer
-- master key that was used to encrypt the model during training.
--
-- 'testingDataResult', 'projectVersionDescription_testingDataResult' - Contains information about the testing results.
--
-- 'statusMessage', 'projectVersionDescription_statusMessage' - A descriptive message for an error or warning that occurred.
--
-- 'creationTimestamp', 'projectVersionDescription_creationTimestamp' - The Unix datetime for the date and time that training started.
--
-- 'projectVersionArn', 'projectVersionDescription_projectVersionArn' - The Amazon Resource Name (ARN) of the model version.
--
-- 'outputConfig', 'projectVersionDescription_outputConfig' - The location where training results are saved.
--
-- 'billableTrainingTimeInSeconds', 'projectVersionDescription_billableTrainingTimeInSeconds' - The duration, in seconds, that the model version has been billed for
-- training. This value is only returned if the model version has been
-- successfully trained.
--
-- 'trainingEndTimestamp', 'projectVersionDescription_trainingEndTimestamp' - The Unix date and time that training of the model ended.
--
-- 'trainingDataResult', 'projectVersionDescription_trainingDataResult' - Contains information about the training results.
newProjectVersionDescription ::
  ProjectVersionDescription
newProjectVersionDescription =
  ProjectVersionDescription'
    { minInferenceUnits =
        Prelude.Nothing,
      status = Prelude.Nothing,
      evaluationResult = Prelude.Nothing,
      manifestSummary = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      testingDataResult = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      projectVersionArn = Prelude.Nothing,
      outputConfig = Prelude.Nothing,
      billableTrainingTimeInSeconds = Prelude.Nothing,
      trainingEndTimestamp = Prelude.Nothing,
      trainingDataResult = Prelude.Nothing
    }

-- | The minimum number of inference units used by the model. For more
-- information, see StartProjectVersion.
projectVersionDescription_minInferenceUnits :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Natural)
projectVersionDescription_minInferenceUnits = Lens.lens (\ProjectVersionDescription' {minInferenceUnits} -> minInferenceUnits) (\s@ProjectVersionDescription' {} a -> s {minInferenceUnits = a} :: ProjectVersionDescription)

-- | The current status of the model version.
projectVersionDescription_status :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe ProjectVersionStatus)
projectVersionDescription_status = Lens.lens (\ProjectVersionDescription' {status} -> status) (\s@ProjectVersionDescription' {} a -> s {status = a} :: ProjectVersionDescription)

-- | The training results. @EvaluationResult@ is only returned if training is
-- successful.
projectVersionDescription_evaluationResult :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe EvaluationResult)
projectVersionDescription_evaluationResult = Lens.lens (\ProjectVersionDescription' {evaluationResult} -> evaluationResult) (\s@ProjectVersionDescription' {} a -> s {evaluationResult = a} :: ProjectVersionDescription)

-- | The location of the summary manifest. The summary manifest provides
-- aggregate data validation results for the training and test datasets.
projectVersionDescription_manifestSummary :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe GroundTruthManifest)
projectVersionDescription_manifestSummary = Lens.lens (\ProjectVersionDescription' {manifestSummary} -> manifestSummary) (\s@ProjectVersionDescription' {} a -> s {manifestSummary = a} :: ProjectVersionDescription)

-- | The identifer for the AWS Key Management Service (AWS KMS) customer
-- master key that was used to encrypt the model during training.
projectVersionDescription_kmsKeyId :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Text)
projectVersionDescription_kmsKeyId = Lens.lens (\ProjectVersionDescription' {kmsKeyId} -> kmsKeyId) (\s@ProjectVersionDescription' {} a -> s {kmsKeyId = a} :: ProjectVersionDescription)

-- | Contains information about the testing results.
projectVersionDescription_testingDataResult :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe TestingDataResult)
projectVersionDescription_testingDataResult = Lens.lens (\ProjectVersionDescription' {testingDataResult} -> testingDataResult) (\s@ProjectVersionDescription' {} a -> s {testingDataResult = a} :: ProjectVersionDescription)

-- | A descriptive message for an error or warning that occurred.
projectVersionDescription_statusMessage :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Text)
projectVersionDescription_statusMessage = Lens.lens (\ProjectVersionDescription' {statusMessage} -> statusMessage) (\s@ProjectVersionDescription' {} a -> s {statusMessage = a} :: ProjectVersionDescription)

-- | The Unix datetime for the date and time that training started.
projectVersionDescription_creationTimestamp :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.UTCTime)
projectVersionDescription_creationTimestamp = Lens.lens (\ProjectVersionDescription' {creationTimestamp} -> creationTimestamp) (\s@ProjectVersionDescription' {} a -> s {creationTimestamp = a} :: ProjectVersionDescription) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the model version.
projectVersionDescription_projectVersionArn :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Text)
projectVersionDescription_projectVersionArn = Lens.lens (\ProjectVersionDescription' {projectVersionArn} -> projectVersionArn) (\s@ProjectVersionDescription' {} a -> s {projectVersionArn = a} :: ProjectVersionDescription)

-- | The location where training results are saved.
projectVersionDescription_outputConfig :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe OutputConfig)
projectVersionDescription_outputConfig = Lens.lens (\ProjectVersionDescription' {outputConfig} -> outputConfig) (\s@ProjectVersionDescription' {} a -> s {outputConfig = a} :: ProjectVersionDescription)

-- | The duration, in seconds, that the model version has been billed for
-- training. This value is only returned if the model version has been
-- successfully trained.
projectVersionDescription_billableTrainingTimeInSeconds :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.Natural)
projectVersionDescription_billableTrainingTimeInSeconds = Lens.lens (\ProjectVersionDescription' {billableTrainingTimeInSeconds} -> billableTrainingTimeInSeconds) (\s@ProjectVersionDescription' {} a -> s {billableTrainingTimeInSeconds = a} :: ProjectVersionDescription)

-- | The Unix date and time that training of the model ended.
projectVersionDescription_trainingEndTimestamp :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe Prelude.UTCTime)
projectVersionDescription_trainingEndTimestamp = Lens.lens (\ProjectVersionDescription' {trainingEndTimestamp} -> trainingEndTimestamp) (\s@ProjectVersionDescription' {} a -> s {trainingEndTimestamp = a} :: ProjectVersionDescription) Prelude.. Lens.mapping Core._Time

-- | Contains information about the training results.
projectVersionDescription_trainingDataResult :: Lens.Lens' ProjectVersionDescription (Prelude.Maybe TrainingDataResult)
projectVersionDescription_trainingDataResult = Lens.lens (\ProjectVersionDescription' {trainingDataResult} -> trainingDataResult) (\s@ProjectVersionDescription' {} a -> s {trainingDataResult = a} :: ProjectVersionDescription)

instance Core.FromJSON ProjectVersionDescription where
  parseJSON =
    Core.withObject
      "ProjectVersionDescription"
      ( \x ->
          ProjectVersionDescription'
            Prelude.<$> (x Core..:? "MinInferenceUnits")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "EvaluationResult")
            Prelude.<*> (x Core..:? "ManifestSummary")
            Prelude.<*> (x Core..:? "KmsKeyId")
            Prelude.<*> (x Core..:? "TestingDataResult")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "CreationTimestamp")
            Prelude.<*> (x Core..:? "ProjectVersionArn")
            Prelude.<*> (x Core..:? "OutputConfig")
            Prelude.<*> (x Core..:? "BillableTrainingTimeInSeconds")
            Prelude.<*> (x Core..:? "TrainingEndTimestamp")
            Prelude.<*> (x Core..:? "TrainingDataResult")
      )

instance Prelude.Hashable ProjectVersionDescription where
  hashWithSalt salt' ProjectVersionDescription' {..} =
    salt' `Prelude.hashWithSalt` trainingDataResult
      `Prelude.hashWithSalt` trainingEndTimestamp
      `Prelude.hashWithSalt` billableTrainingTimeInSeconds
      `Prelude.hashWithSalt` outputConfig
      `Prelude.hashWithSalt` projectVersionArn
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` testingDataResult
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` manifestSummary
      `Prelude.hashWithSalt` evaluationResult
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` minInferenceUnits

instance Prelude.NFData ProjectVersionDescription where
  rnf ProjectVersionDescription' {..} =
    Prelude.rnf minInferenceUnits
      `Prelude.seq` Prelude.rnf trainingDataResult
      `Prelude.seq` Prelude.rnf trainingEndTimestamp
      `Prelude.seq` Prelude.rnf billableTrainingTimeInSeconds
      `Prelude.seq` Prelude.rnf outputConfig
      `Prelude.seq` Prelude.rnf projectVersionArn
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf testingDataResult
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf manifestSummary
      `Prelude.seq` Prelude.rnf evaluationResult
      `Prelude.seq` Prelude.rnf status
