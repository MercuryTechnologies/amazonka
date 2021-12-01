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
-- Module      : Amazonka.LookoutEquipment.Types.InferenceExecutionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutEquipment.Types.InferenceExecutionSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutEquipment.Types.InferenceExecutionStatus
import Amazonka.LookoutEquipment.Types.InferenceInputConfiguration
import Amazonka.LookoutEquipment.Types.InferenceOutputConfiguration
import Amazonka.LookoutEquipment.Types.S3Object
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the specific inference execution, including
-- input and output data configuration, inference scheduling information,
-- status, and so on.
--
-- /See:/ 'newInferenceExecutionSummary' smart constructor.
data InferenceExecutionSummary = InferenceExecutionSummary'
  { -- | Indicates the status of the inference execution.
    status :: Prelude.Maybe InferenceExecutionStatus,
    -- | Specifies the reason for failure when an inference execution has failed.
    failedReason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the ML model used for the inference
    -- execution.
    modelArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates the time reference in the dataset at which the inference
    -- execution began.
    dataStartTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the ML model being used for the inference execution.
    modelName :: Prelude.Maybe Prelude.Text,
    customerResultObject :: Prelude.Maybe S3Object,
    -- | The Amazon Resource Name (ARN) of the inference scheduler being used for
    -- the inference execution.
    inferenceSchedulerArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates the start time at which the inference scheduler began the
    -- specific inference execution.
    scheduledStartTime :: Prelude.Maybe Core.POSIX,
    -- | Specifies configuration information for the output results from for the
    -- inference execution, including the output S3 location.
    dataOutputConfiguration :: Prelude.Maybe InferenceOutputConfiguration,
    -- | Indicates the time reference in the dataset at which the inference
    -- execution stopped.
    dataEndTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the inference scheduler being used for the inference
    -- execution.
    inferenceSchedulerName :: Prelude.Maybe Prelude.Text,
    -- | Specifies configuration information for the input data for the inference
    -- scheduler, including delimiter, format, and dataset location.
    dataInputConfiguration :: Prelude.Maybe InferenceInputConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InferenceExecutionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'inferenceExecutionSummary_status' - Indicates the status of the inference execution.
--
-- 'failedReason', 'inferenceExecutionSummary_failedReason' - Specifies the reason for failure when an inference execution has failed.
--
-- 'modelArn', 'inferenceExecutionSummary_modelArn' - The Amazon Resource Name (ARN) of the ML model used for the inference
-- execution.
--
-- 'dataStartTime', 'inferenceExecutionSummary_dataStartTime' - Indicates the time reference in the dataset at which the inference
-- execution began.
--
-- 'modelName', 'inferenceExecutionSummary_modelName' - The name of the ML model being used for the inference execution.
--
-- 'customerResultObject', 'inferenceExecutionSummary_customerResultObject' -
--
-- 'inferenceSchedulerArn', 'inferenceExecutionSummary_inferenceSchedulerArn' - The Amazon Resource Name (ARN) of the inference scheduler being used for
-- the inference execution.
--
-- 'scheduledStartTime', 'inferenceExecutionSummary_scheduledStartTime' - Indicates the start time at which the inference scheduler began the
-- specific inference execution.
--
-- 'dataOutputConfiguration', 'inferenceExecutionSummary_dataOutputConfiguration' - Specifies configuration information for the output results from for the
-- inference execution, including the output S3 location.
--
-- 'dataEndTime', 'inferenceExecutionSummary_dataEndTime' - Indicates the time reference in the dataset at which the inference
-- execution stopped.
--
-- 'inferenceSchedulerName', 'inferenceExecutionSummary_inferenceSchedulerName' - The name of the inference scheduler being used for the inference
-- execution.
--
-- 'dataInputConfiguration', 'inferenceExecutionSummary_dataInputConfiguration' - Specifies configuration information for the input data for the inference
-- scheduler, including delimiter, format, and dataset location.
newInferenceExecutionSummary ::
  InferenceExecutionSummary
newInferenceExecutionSummary =
  InferenceExecutionSummary'
    { status =
        Prelude.Nothing,
      failedReason = Prelude.Nothing,
      modelArn = Prelude.Nothing,
      dataStartTime = Prelude.Nothing,
      modelName = Prelude.Nothing,
      customerResultObject = Prelude.Nothing,
      inferenceSchedulerArn = Prelude.Nothing,
      scheduledStartTime = Prelude.Nothing,
      dataOutputConfiguration = Prelude.Nothing,
      dataEndTime = Prelude.Nothing,
      inferenceSchedulerName = Prelude.Nothing,
      dataInputConfiguration = Prelude.Nothing
    }

-- | Indicates the status of the inference execution.
inferenceExecutionSummary_status :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe InferenceExecutionStatus)
inferenceExecutionSummary_status = Lens.lens (\InferenceExecutionSummary' {status} -> status) (\s@InferenceExecutionSummary' {} a -> s {status = a} :: InferenceExecutionSummary)

-- | Specifies the reason for failure when an inference execution has failed.
inferenceExecutionSummary_failedReason :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe Prelude.Text)
inferenceExecutionSummary_failedReason = Lens.lens (\InferenceExecutionSummary' {failedReason} -> failedReason) (\s@InferenceExecutionSummary' {} a -> s {failedReason = a} :: InferenceExecutionSummary)

-- | The Amazon Resource Name (ARN) of the ML model used for the inference
-- execution.
inferenceExecutionSummary_modelArn :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe Prelude.Text)
inferenceExecutionSummary_modelArn = Lens.lens (\InferenceExecutionSummary' {modelArn} -> modelArn) (\s@InferenceExecutionSummary' {} a -> s {modelArn = a} :: InferenceExecutionSummary)

-- | Indicates the time reference in the dataset at which the inference
-- execution began.
inferenceExecutionSummary_dataStartTime :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe Prelude.UTCTime)
inferenceExecutionSummary_dataStartTime = Lens.lens (\InferenceExecutionSummary' {dataStartTime} -> dataStartTime) (\s@InferenceExecutionSummary' {} a -> s {dataStartTime = a} :: InferenceExecutionSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the ML model being used for the inference execution.
inferenceExecutionSummary_modelName :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe Prelude.Text)
inferenceExecutionSummary_modelName = Lens.lens (\InferenceExecutionSummary' {modelName} -> modelName) (\s@InferenceExecutionSummary' {} a -> s {modelName = a} :: InferenceExecutionSummary)

-- |
inferenceExecutionSummary_customerResultObject :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe S3Object)
inferenceExecutionSummary_customerResultObject = Lens.lens (\InferenceExecutionSummary' {customerResultObject} -> customerResultObject) (\s@InferenceExecutionSummary' {} a -> s {customerResultObject = a} :: InferenceExecutionSummary)

-- | The Amazon Resource Name (ARN) of the inference scheduler being used for
-- the inference execution.
inferenceExecutionSummary_inferenceSchedulerArn :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe Prelude.Text)
inferenceExecutionSummary_inferenceSchedulerArn = Lens.lens (\InferenceExecutionSummary' {inferenceSchedulerArn} -> inferenceSchedulerArn) (\s@InferenceExecutionSummary' {} a -> s {inferenceSchedulerArn = a} :: InferenceExecutionSummary)

-- | Indicates the start time at which the inference scheduler began the
-- specific inference execution.
inferenceExecutionSummary_scheduledStartTime :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe Prelude.UTCTime)
inferenceExecutionSummary_scheduledStartTime = Lens.lens (\InferenceExecutionSummary' {scheduledStartTime} -> scheduledStartTime) (\s@InferenceExecutionSummary' {} a -> s {scheduledStartTime = a} :: InferenceExecutionSummary) Prelude.. Lens.mapping Core._Time

-- | Specifies configuration information for the output results from for the
-- inference execution, including the output S3 location.
inferenceExecutionSummary_dataOutputConfiguration :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe InferenceOutputConfiguration)
inferenceExecutionSummary_dataOutputConfiguration = Lens.lens (\InferenceExecutionSummary' {dataOutputConfiguration} -> dataOutputConfiguration) (\s@InferenceExecutionSummary' {} a -> s {dataOutputConfiguration = a} :: InferenceExecutionSummary)

-- | Indicates the time reference in the dataset at which the inference
-- execution stopped.
inferenceExecutionSummary_dataEndTime :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe Prelude.UTCTime)
inferenceExecutionSummary_dataEndTime = Lens.lens (\InferenceExecutionSummary' {dataEndTime} -> dataEndTime) (\s@InferenceExecutionSummary' {} a -> s {dataEndTime = a} :: InferenceExecutionSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the inference scheduler being used for the inference
-- execution.
inferenceExecutionSummary_inferenceSchedulerName :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe Prelude.Text)
inferenceExecutionSummary_inferenceSchedulerName = Lens.lens (\InferenceExecutionSummary' {inferenceSchedulerName} -> inferenceSchedulerName) (\s@InferenceExecutionSummary' {} a -> s {inferenceSchedulerName = a} :: InferenceExecutionSummary)

-- | Specifies configuration information for the input data for the inference
-- scheduler, including delimiter, format, and dataset location.
inferenceExecutionSummary_dataInputConfiguration :: Lens.Lens' InferenceExecutionSummary (Prelude.Maybe InferenceInputConfiguration)
inferenceExecutionSummary_dataInputConfiguration = Lens.lens (\InferenceExecutionSummary' {dataInputConfiguration} -> dataInputConfiguration) (\s@InferenceExecutionSummary' {} a -> s {dataInputConfiguration = a} :: InferenceExecutionSummary)

instance Core.FromJSON InferenceExecutionSummary where
  parseJSON =
    Core.withObject
      "InferenceExecutionSummary"
      ( \x ->
          InferenceExecutionSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "FailedReason")
            Prelude.<*> (x Core..:? "ModelArn")
            Prelude.<*> (x Core..:? "DataStartTime")
            Prelude.<*> (x Core..:? "ModelName")
            Prelude.<*> (x Core..:? "CustomerResultObject")
            Prelude.<*> (x Core..:? "InferenceSchedulerArn")
            Prelude.<*> (x Core..:? "ScheduledStartTime")
            Prelude.<*> (x Core..:? "DataOutputConfiguration")
            Prelude.<*> (x Core..:? "DataEndTime")
            Prelude.<*> (x Core..:? "InferenceSchedulerName")
            Prelude.<*> (x Core..:? "DataInputConfiguration")
      )

instance Prelude.Hashable InferenceExecutionSummary where
  hashWithSalt salt' InferenceExecutionSummary' {..} =
    salt' `Prelude.hashWithSalt` dataInputConfiguration
      `Prelude.hashWithSalt` inferenceSchedulerName
      `Prelude.hashWithSalt` dataEndTime
      `Prelude.hashWithSalt` dataOutputConfiguration
      `Prelude.hashWithSalt` scheduledStartTime
      `Prelude.hashWithSalt` inferenceSchedulerArn
      `Prelude.hashWithSalt` customerResultObject
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` dataStartTime
      `Prelude.hashWithSalt` modelArn
      `Prelude.hashWithSalt` failedReason
      `Prelude.hashWithSalt` status

instance Prelude.NFData InferenceExecutionSummary where
  rnf InferenceExecutionSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf dataInputConfiguration
      `Prelude.seq` Prelude.rnf inferenceSchedulerName
      `Prelude.seq` Prelude.rnf dataEndTime
      `Prelude.seq` Prelude.rnf dataOutputConfiguration
      `Prelude.seq` Prelude.rnf scheduledStartTime
      `Prelude.seq` Prelude.rnf inferenceSchedulerArn
      `Prelude.seq` Prelude.rnf customerResultObject
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf dataStartTime
      `Prelude.seq` Prelude.rnf modelArn
      `Prelude.seq` Prelude.rnf failedReason
