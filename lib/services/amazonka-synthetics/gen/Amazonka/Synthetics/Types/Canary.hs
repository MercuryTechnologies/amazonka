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
-- Module      : Amazonka.Synthetics.Types.Canary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.Canary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Synthetics.Types.ArtifactConfigOutput
import Amazonka.Synthetics.Types.CanaryCodeOutput
import Amazonka.Synthetics.Types.CanaryRunConfigOutput
import Amazonka.Synthetics.Types.CanaryScheduleOutput
import Amazonka.Synthetics.Types.CanaryStatus
import Amazonka.Synthetics.Types.CanaryTimeline
import Amazonka.Synthetics.Types.VisualReferenceOutput
import Amazonka.Synthetics.Types.VpcConfigOutput

-- | This structure contains all information about one canary in your
-- account.
--
-- /See:/ 'newCanary' smart constructor.
data Canary = Canary'
  { -- | A structure that contains information about the canary\'s status.
    status :: Prelude.Maybe CanaryStatus,
    -- | The number of days to retain data about successful runs of this canary.
    successRetentionPeriodInDays :: Prelude.Maybe Prelude.Natural,
    -- | A structure that contains information about how often the canary is to
    -- run, and when these runs are to stop.
    schedule :: Prelude.Maybe CanaryScheduleOutput,
    -- | The location in Amazon S3 where Synthetics stores artifacts from the
    -- runs of this canary. Artifacts include the log file, screenshots, and
    -- HAR files.
    artifactS3Location :: Prelude.Maybe Prelude.Text,
    runConfig :: Prelude.Maybe CanaryRunConfigOutput,
    -- | The ARN of the IAM role used to run the canary. This role must include
    -- @lambda.amazonaws.com@ as a principal in the trust policy.
    executionRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the runtime version to use for the canary. For more
    -- information about runtime versions, see
    -- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Library.html Canary Runtime Versions>.
    runtimeVersion :: Prelude.Maybe Prelude.Text,
    -- | The number of days to retain data about failed runs of this canary.
    failureRetentionPeriodInDays :: Prelude.Maybe Prelude.Natural,
    -- | A structure that contains the configuration for canary artifacts,
    -- including the encryption-at-rest settings for artifacts that the canary
    -- uploads to Amazon S3.
    artifactConfig :: Prelude.Maybe ArtifactConfigOutput,
    vpcConfig :: Prelude.Maybe VpcConfigOutput,
    -- | If this canary performs visual monitoring by comparing screenshots, this
    -- structure contains the ID of the canary run to use as the baseline for
    -- screenshots, and the coordinates of any parts of the screen to ignore
    -- during the visual monitoring comparison.
    visualReference :: Prelude.Maybe VisualReferenceOutput,
    -- | The name of the canary.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of this canary.
    id :: Prelude.Maybe Prelude.Text,
    code :: Prelude.Maybe CanaryCodeOutput,
    -- | A structure that contains information about when the canary was created,
    -- modified, and most recently run.
    timeline :: Prelude.Maybe CanaryTimeline,
    -- | The ARN of the Lambda function that is used as your canary\'s engine.
    -- For more information about Lambda ARN format, see
    -- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-api-permissions-ref.html Resources and Conditions for Lambda Actions>.
    engineArn :: Prelude.Maybe Prelude.Text,
    -- | The list of key-value pairs that are associated with the canary.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Canary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'canary_status' - A structure that contains information about the canary\'s status.
--
-- 'successRetentionPeriodInDays', 'canary_successRetentionPeriodInDays' - The number of days to retain data about successful runs of this canary.
--
-- 'schedule', 'canary_schedule' - A structure that contains information about how often the canary is to
-- run, and when these runs are to stop.
--
-- 'artifactS3Location', 'canary_artifactS3Location' - The location in Amazon S3 where Synthetics stores artifacts from the
-- runs of this canary. Artifacts include the log file, screenshots, and
-- HAR files.
--
-- 'runConfig', 'canary_runConfig' - Undocumented member.
--
-- 'executionRoleArn', 'canary_executionRoleArn' - The ARN of the IAM role used to run the canary. This role must include
-- @lambda.amazonaws.com@ as a principal in the trust policy.
--
-- 'runtimeVersion', 'canary_runtimeVersion' - Specifies the runtime version to use for the canary. For more
-- information about runtime versions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Library.html Canary Runtime Versions>.
--
-- 'failureRetentionPeriodInDays', 'canary_failureRetentionPeriodInDays' - The number of days to retain data about failed runs of this canary.
--
-- 'artifactConfig', 'canary_artifactConfig' - A structure that contains the configuration for canary artifacts,
-- including the encryption-at-rest settings for artifacts that the canary
-- uploads to Amazon S3.
--
-- 'vpcConfig', 'canary_vpcConfig' - Undocumented member.
--
-- 'visualReference', 'canary_visualReference' - If this canary performs visual monitoring by comparing screenshots, this
-- structure contains the ID of the canary run to use as the baseline for
-- screenshots, and the coordinates of any parts of the screen to ignore
-- during the visual monitoring comparison.
--
-- 'name', 'canary_name' - The name of the canary.
--
-- 'id', 'canary_id' - The unique ID of this canary.
--
-- 'code', 'canary_code' - Undocumented member.
--
-- 'timeline', 'canary_timeline' - A structure that contains information about when the canary was created,
-- modified, and most recently run.
--
-- 'engineArn', 'canary_engineArn' - The ARN of the Lambda function that is used as your canary\'s engine.
-- For more information about Lambda ARN format, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-api-permissions-ref.html Resources and Conditions for Lambda Actions>.
--
-- 'tags', 'canary_tags' - The list of key-value pairs that are associated with the canary.
newCanary ::
  Canary
newCanary =
  Canary'
    { status = Prelude.Nothing,
      successRetentionPeriodInDays = Prelude.Nothing,
      schedule = Prelude.Nothing,
      artifactS3Location = Prelude.Nothing,
      runConfig = Prelude.Nothing,
      executionRoleArn = Prelude.Nothing,
      runtimeVersion = Prelude.Nothing,
      failureRetentionPeriodInDays = Prelude.Nothing,
      artifactConfig = Prelude.Nothing,
      vpcConfig = Prelude.Nothing,
      visualReference = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      code = Prelude.Nothing,
      timeline = Prelude.Nothing,
      engineArn = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | A structure that contains information about the canary\'s status.
canary_status :: Lens.Lens' Canary (Prelude.Maybe CanaryStatus)
canary_status = Lens.lens (\Canary' {status} -> status) (\s@Canary' {} a -> s {status = a} :: Canary)

-- | The number of days to retain data about successful runs of this canary.
canary_successRetentionPeriodInDays :: Lens.Lens' Canary (Prelude.Maybe Prelude.Natural)
canary_successRetentionPeriodInDays = Lens.lens (\Canary' {successRetentionPeriodInDays} -> successRetentionPeriodInDays) (\s@Canary' {} a -> s {successRetentionPeriodInDays = a} :: Canary)

-- | A structure that contains information about how often the canary is to
-- run, and when these runs are to stop.
canary_schedule :: Lens.Lens' Canary (Prelude.Maybe CanaryScheduleOutput)
canary_schedule = Lens.lens (\Canary' {schedule} -> schedule) (\s@Canary' {} a -> s {schedule = a} :: Canary)

-- | The location in Amazon S3 where Synthetics stores artifacts from the
-- runs of this canary. Artifacts include the log file, screenshots, and
-- HAR files.
canary_artifactS3Location :: Lens.Lens' Canary (Prelude.Maybe Prelude.Text)
canary_artifactS3Location = Lens.lens (\Canary' {artifactS3Location} -> artifactS3Location) (\s@Canary' {} a -> s {artifactS3Location = a} :: Canary)

-- | Undocumented member.
canary_runConfig :: Lens.Lens' Canary (Prelude.Maybe CanaryRunConfigOutput)
canary_runConfig = Lens.lens (\Canary' {runConfig} -> runConfig) (\s@Canary' {} a -> s {runConfig = a} :: Canary)

-- | The ARN of the IAM role used to run the canary. This role must include
-- @lambda.amazonaws.com@ as a principal in the trust policy.
canary_executionRoleArn :: Lens.Lens' Canary (Prelude.Maybe Prelude.Text)
canary_executionRoleArn = Lens.lens (\Canary' {executionRoleArn} -> executionRoleArn) (\s@Canary' {} a -> s {executionRoleArn = a} :: Canary)

-- | Specifies the runtime version to use for the canary. For more
-- information about runtime versions, see
-- <https://docs.aws.amazon.com/AmazonCloudWatch/latest/monitoring/CloudWatch_Synthetics_Canaries_Library.html Canary Runtime Versions>.
canary_runtimeVersion :: Lens.Lens' Canary (Prelude.Maybe Prelude.Text)
canary_runtimeVersion = Lens.lens (\Canary' {runtimeVersion} -> runtimeVersion) (\s@Canary' {} a -> s {runtimeVersion = a} :: Canary)

-- | The number of days to retain data about failed runs of this canary.
canary_failureRetentionPeriodInDays :: Lens.Lens' Canary (Prelude.Maybe Prelude.Natural)
canary_failureRetentionPeriodInDays = Lens.lens (\Canary' {failureRetentionPeriodInDays} -> failureRetentionPeriodInDays) (\s@Canary' {} a -> s {failureRetentionPeriodInDays = a} :: Canary)

-- | A structure that contains the configuration for canary artifacts,
-- including the encryption-at-rest settings for artifacts that the canary
-- uploads to Amazon S3.
canary_artifactConfig :: Lens.Lens' Canary (Prelude.Maybe ArtifactConfigOutput)
canary_artifactConfig = Lens.lens (\Canary' {artifactConfig} -> artifactConfig) (\s@Canary' {} a -> s {artifactConfig = a} :: Canary)

-- | Undocumented member.
canary_vpcConfig :: Lens.Lens' Canary (Prelude.Maybe VpcConfigOutput)
canary_vpcConfig = Lens.lens (\Canary' {vpcConfig} -> vpcConfig) (\s@Canary' {} a -> s {vpcConfig = a} :: Canary)

-- | If this canary performs visual monitoring by comparing screenshots, this
-- structure contains the ID of the canary run to use as the baseline for
-- screenshots, and the coordinates of any parts of the screen to ignore
-- during the visual monitoring comparison.
canary_visualReference :: Lens.Lens' Canary (Prelude.Maybe VisualReferenceOutput)
canary_visualReference = Lens.lens (\Canary' {visualReference} -> visualReference) (\s@Canary' {} a -> s {visualReference = a} :: Canary)

-- | The name of the canary.
canary_name :: Lens.Lens' Canary (Prelude.Maybe Prelude.Text)
canary_name = Lens.lens (\Canary' {name} -> name) (\s@Canary' {} a -> s {name = a} :: Canary)

-- | The unique ID of this canary.
canary_id :: Lens.Lens' Canary (Prelude.Maybe Prelude.Text)
canary_id = Lens.lens (\Canary' {id} -> id) (\s@Canary' {} a -> s {id = a} :: Canary)

-- | Undocumented member.
canary_code :: Lens.Lens' Canary (Prelude.Maybe CanaryCodeOutput)
canary_code = Lens.lens (\Canary' {code} -> code) (\s@Canary' {} a -> s {code = a} :: Canary)

-- | A structure that contains information about when the canary was created,
-- modified, and most recently run.
canary_timeline :: Lens.Lens' Canary (Prelude.Maybe CanaryTimeline)
canary_timeline = Lens.lens (\Canary' {timeline} -> timeline) (\s@Canary' {} a -> s {timeline = a} :: Canary)

-- | The ARN of the Lambda function that is used as your canary\'s engine.
-- For more information about Lambda ARN format, see
-- <https://docs.aws.amazon.com/lambda/latest/dg/lambda-api-permissions-ref.html Resources and Conditions for Lambda Actions>.
canary_engineArn :: Lens.Lens' Canary (Prelude.Maybe Prelude.Text)
canary_engineArn = Lens.lens (\Canary' {engineArn} -> engineArn) (\s@Canary' {} a -> s {engineArn = a} :: Canary)

-- | The list of key-value pairs that are associated with the canary.
canary_tags :: Lens.Lens' Canary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
canary_tags = Lens.lens (\Canary' {tags} -> tags) (\s@Canary' {} a -> s {tags = a} :: Canary) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Canary where
  parseJSON =
    Core.withObject
      "Canary"
      ( \x ->
          Canary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "SuccessRetentionPeriodInDays")
            Prelude.<*> (x Core..:? "Schedule")
            Prelude.<*> (x Core..:? "ArtifactS3Location")
            Prelude.<*> (x Core..:? "RunConfig")
            Prelude.<*> (x Core..:? "ExecutionRoleArn")
            Prelude.<*> (x Core..:? "RuntimeVersion")
            Prelude.<*> (x Core..:? "FailureRetentionPeriodInDays")
            Prelude.<*> (x Core..:? "ArtifactConfig")
            Prelude.<*> (x Core..:? "VpcConfig")
            Prelude.<*> (x Core..:? "VisualReference")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Code")
            Prelude.<*> (x Core..:? "Timeline")
            Prelude.<*> (x Core..:? "EngineArn")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Canary where
  hashWithSalt salt' Canary' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` engineArn
      `Prelude.hashWithSalt` timeline
      `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` visualReference
      `Prelude.hashWithSalt` vpcConfig
      `Prelude.hashWithSalt` artifactConfig
      `Prelude.hashWithSalt` failureRetentionPeriodInDays
      `Prelude.hashWithSalt` runtimeVersion
      `Prelude.hashWithSalt` executionRoleArn
      `Prelude.hashWithSalt` runConfig
      `Prelude.hashWithSalt` artifactS3Location
      `Prelude.hashWithSalt` schedule
      `Prelude.hashWithSalt` successRetentionPeriodInDays
      `Prelude.hashWithSalt` status

instance Prelude.NFData Canary where
  rnf Canary' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf engineArn
      `Prelude.seq` Prelude.rnf timeline
      `Prelude.seq` Prelude.rnf code
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf visualReference
      `Prelude.seq` Prelude.rnf vpcConfig
      `Prelude.seq` Prelude.rnf artifactConfig
      `Prelude.seq` Prelude.rnf failureRetentionPeriodInDays
      `Prelude.seq` Prelude.rnf runtimeVersion
      `Prelude.seq` Prelude.rnf executionRoleArn
      `Prelude.seq` Prelude.rnf runConfig
      `Prelude.seq` Prelude.rnf artifactS3Location
      `Prelude.seq` Prelude.rnf schedule
      `Prelude.seq` Prelude.rnf successRetentionPeriodInDays
