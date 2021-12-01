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
-- Module      : Amazonka.SageMaker.Types.DebugRuleConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.DebugRuleConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProcessingInstanceType

-- | Configuration information for SageMaker Debugger rules for debugging. To
-- learn more about how to configure the @DebugRuleConfiguration@
-- parameter, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/debugger-createtrainingjob-api.html Use the SageMaker and Debugger Configuration API Operations to Create, Update, and Debug Your Training Job>.
--
-- /See:/ 'newDebugRuleConfiguration' smart constructor.
data DebugRuleConfiguration = DebugRuleConfiguration'
  { -- | Runtime configuration for rule container.
    ruleParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Path to Amazon S3 storage location for rules.
    s3OutputPath :: Prelude.Maybe Prelude.Text,
    -- | Path to local storage location for output of rules. Defaults to
    -- @\/opt\/ml\/processing\/output\/rule\/@.
    localPath :: Prelude.Maybe Prelude.Text,
    -- | The instance type to deploy a Debugger custom rule for debugging a
    -- training job.
    instanceType :: Prelude.Maybe ProcessingInstanceType,
    -- | The size, in GB, of the ML storage volume attached to the processing
    -- instance.
    volumeSizeInGB :: Prelude.Maybe Prelude.Natural,
    -- | The name of the rule configuration. It must be unique relative to other
    -- rule configuration names.
    ruleConfigurationName :: Prelude.Text,
    -- | The Amazon Elastic Container (ECR) Image for the managed rule
    -- evaluation.
    ruleEvaluatorImage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DebugRuleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleParameters', 'debugRuleConfiguration_ruleParameters' - Runtime configuration for rule container.
--
-- 's3OutputPath', 'debugRuleConfiguration_s3OutputPath' - Path to Amazon S3 storage location for rules.
--
-- 'localPath', 'debugRuleConfiguration_localPath' - Path to local storage location for output of rules. Defaults to
-- @\/opt\/ml\/processing\/output\/rule\/@.
--
-- 'instanceType', 'debugRuleConfiguration_instanceType' - The instance type to deploy a Debugger custom rule for debugging a
-- training job.
--
-- 'volumeSizeInGB', 'debugRuleConfiguration_volumeSizeInGB' - The size, in GB, of the ML storage volume attached to the processing
-- instance.
--
-- 'ruleConfigurationName', 'debugRuleConfiguration_ruleConfigurationName' - The name of the rule configuration. It must be unique relative to other
-- rule configuration names.
--
-- 'ruleEvaluatorImage', 'debugRuleConfiguration_ruleEvaluatorImage' - The Amazon Elastic Container (ECR) Image for the managed rule
-- evaluation.
newDebugRuleConfiguration ::
  -- | 'ruleConfigurationName'
  Prelude.Text ->
  -- | 'ruleEvaluatorImage'
  Prelude.Text ->
  DebugRuleConfiguration
newDebugRuleConfiguration
  pRuleConfigurationName_
  pRuleEvaluatorImage_ =
    DebugRuleConfiguration'
      { ruleParameters =
          Prelude.Nothing,
        s3OutputPath = Prelude.Nothing,
        localPath = Prelude.Nothing,
        instanceType = Prelude.Nothing,
        volumeSizeInGB = Prelude.Nothing,
        ruleConfigurationName = pRuleConfigurationName_,
        ruleEvaluatorImage = pRuleEvaluatorImage_
      }

-- | Runtime configuration for rule container.
debugRuleConfiguration_ruleParameters :: Lens.Lens' DebugRuleConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
debugRuleConfiguration_ruleParameters = Lens.lens (\DebugRuleConfiguration' {ruleParameters} -> ruleParameters) (\s@DebugRuleConfiguration' {} a -> s {ruleParameters = a} :: DebugRuleConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Path to Amazon S3 storage location for rules.
debugRuleConfiguration_s3OutputPath :: Lens.Lens' DebugRuleConfiguration (Prelude.Maybe Prelude.Text)
debugRuleConfiguration_s3OutputPath = Lens.lens (\DebugRuleConfiguration' {s3OutputPath} -> s3OutputPath) (\s@DebugRuleConfiguration' {} a -> s {s3OutputPath = a} :: DebugRuleConfiguration)

-- | Path to local storage location for output of rules. Defaults to
-- @\/opt\/ml\/processing\/output\/rule\/@.
debugRuleConfiguration_localPath :: Lens.Lens' DebugRuleConfiguration (Prelude.Maybe Prelude.Text)
debugRuleConfiguration_localPath = Lens.lens (\DebugRuleConfiguration' {localPath} -> localPath) (\s@DebugRuleConfiguration' {} a -> s {localPath = a} :: DebugRuleConfiguration)

-- | The instance type to deploy a Debugger custom rule for debugging a
-- training job.
debugRuleConfiguration_instanceType :: Lens.Lens' DebugRuleConfiguration (Prelude.Maybe ProcessingInstanceType)
debugRuleConfiguration_instanceType = Lens.lens (\DebugRuleConfiguration' {instanceType} -> instanceType) (\s@DebugRuleConfiguration' {} a -> s {instanceType = a} :: DebugRuleConfiguration)

-- | The size, in GB, of the ML storage volume attached to the processing
-- instance.
debugRuleConfiguration_volumeSizeInGB :: Lens.Lens' DebugRuleConfiguration (Prelude.Maybe Prelude.Natural)
debugRuleConfiguration_volumeSizeInGB = Lens.lens (\DebugRuleConfiguration' {volumeSizeInGB} -> volumeSizeInGB) (\s@DebugRuleConfiguration' {} a -> s {volumeSizeInGB = a} :: DebugRuleConfiguration)

-- | The name of the rule configuration. It must be unique relative to other
-- rule configuration names.
debugRuleConfiguration_ruleConfigurationName :: Lens.Lens' DebugRuleConfiguration Prelude.Text
debugRuleConfiguration_ruleConfigurationName = Lens.lens (\DebugRuleConfiguration' {ruleConfigurationName} -> ruleConfigurationName) (\s@DebugRuleConfiguration' {} a -> s {ruleConfigurationName = a} :: DebugRuleConfiguration)

-- | The Amazon Elastic Container (ECR) Image for the managed rule
-- evaluation.
debugRuleConfiguration_ruleEvaluatorImage :: Lens.Lens' DebugRuleConfiguration Prelude.Text
debugRuleConfiguration_ruleEvaluatorImage = Lens.lens (\DebugRuleConfiguration' {ruleEvaluatorImage} -> ruleEvaluatorImage) (\s@DebugRuleConfiguration' {} a -> s {ruleEvaluatorImage = a} :: DebugRuleConfiguration)

instance Core.FromJSON DebugRuleConfiguration where
  parseJSON =
    Core.withObject
      "DebugRuleConfiguration"
      ( \x ->
          DebugRuleConfiguration'
            Prelude.<$> (x Core..:? "RuleParameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "S3OutputPath")
            Prelude.<*> (x Core..:? "LocalPath")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "VolumeSizeInGB")
            Prelude.<*> (x Core..: "RuleConfigurationName")
            Prelude.<*> (x Core..: "RuleEvaluatorImage")
      )

instance Prelude.Hashable DebugRuleConfiguration where
  hashWithSalt salt' DebugRuleConfiguration' {..} =
    salt' `Prelude.hashWithSalt` ruleEvaluatorImage
      `Prelude.hashWithSalt` ruleConfigurationName
      `Prelude.hashWithSalt` volumeSizeInGB
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` localPath
      `Prelude.hashWithSalt` s3OutputPath
      `Prelude.hashWithSalt` ruleParameters

instance Prelude.NFData DebugRuleConfiguration where
  rnf DebugRuleConfiguration' {..} =
    Prelude.rnf ruleParameters
      `Prelude.seq` Prelude.rnf ruleEvaluatorImage
      `Prelude.seq` Prelude.rnf ruleConfigurationName
      `Prelude.seq` Prelude.rnf volumeSizeInGB
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf localPath
      `Prelude.seq` Prelude.rnf s3OutputPath

instance Core.ToJSON DebugRuleConfiguration where
  toJSON DebugRuleConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RuleParameters" Core..=)
              Prelude.<$> ruleParameters,
            ("S3OutputPath" Core..=) Prelude.<$> s3OutputPath,
            ("LocalPath" Core..=) Prelude.<$> localPath,
            ("InstanceType" Core..=) Prelude.<$> instanceType,
            ("VolumeSizeInGB" Core..=)
              Prelude.<$> volumeSizeInGB,
            Prelude.Just
              ( "RuleConfigurationName"
                  Core..= ruleConfigurationName
              ),
            Prelude.Just
              ("RuleEvaluatorImage" Core..= ruleEvaluatorImage)
          ]
      )
