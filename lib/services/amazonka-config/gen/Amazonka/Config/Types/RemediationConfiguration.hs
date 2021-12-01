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
-- Module      : Amazonka.Config.Types.RemediationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RemediationConfiguration where

import Amazonka.Config.Types.ExecutionControls
import Amazonka.Config.Types.RemediationParameterValue
import Amazonka.Config.Types.RemediationTargetType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the details about the remediation
-- configuration that includes the remediation action, parameters, and data
-- to execute the action.
--
-- /See:/ 'newRemediationConfiguration' smart constructor.
data RemediationConfiguration = RemediationConfiguration'
  { -- | The type of a resource.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Name (ARN) of remediation configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The remediation is triggered automatically.
    automatic :: Prelude.Maybe Prelude.Bool,
    -- | Name of the service that owns the service linked rule, if applicable.
    createdByService :: Prelude.Maybe Prelude.Text,
    -- | Maximum time in seconds that Config runs auto-remediation. If you do not
    -- select a number, the default is 60 seconds.
    --
    -- For example, if you specify RetryAttemptSeconds as 50 seconds and
    -- MaximumAutomaticAttempts as 5, Config will run auto-remediations 5 times
    -- within 50 seconds before throwing an exception.
    retryAttemptSeconds :: Prelude.Maybe Prelude.Natural,
    -- | An ExecutionControls object.
    executionControls :: Prelude.Maybe ExecutionControls,
    -- | An object of the RemediationParameterValue.
    parameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text RemediationParameterValue),
    -- | The maximum number of failed attempts for auto-remediation. If you do
    -- not select a number, the default is 5.
    --
    -- For example, if you specify MaximumAutomaticAttempts as 5 with
    -- RetryAttemptSeconds as 50 seconds, Config will put a
    -- RemediationException on your behalf for the failing resource after the
    -- 5th failed attempt within 50 seconds.
    maximumAutomaticAttempts :: Prelude.Maybe Prelude.Natural,
    -- | Version of the target. For example, version of the SSM document.
    --
    -- If you make backward incompatible changes to the SSM document, you must
    -- call PutRemediationConfiguration API again to ensure the remediations
    -- can run.
    targetVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the Config rule.
    configRuleName :: Prelude.Text,
    -- | The type of the target. Target executes remediation. For example, SSM
    -- document.
    targetType :: RemediationTargetType,
    -- | Target ID is the name of the public document.
    targetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemediationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'remediationConfiguration_resourceType' - The type of a resource.
--
-- 'arn', 'remediationConfiguration_arn' - Amazon Resource Name (ARN) of remediation configuration.
--
-- 'automatic', 'remediationConfiguration_automatic' - The remediation is triggered automatically.
--
-- 'createdByService', 'remediationConfiguration_createdByService' - Name of the service that owns the service linked rule, if applicable.
--
-- 'retryAttemptSeconds', 'remediationConfiguration_retryAttemptSeconds' - Maximum time in seconds that Config runs auto-remediation. If you do not
-- select a number, the default is 60 seconds.
--
-- For example, if you specify RetryAttemptSeconds as 50 seconds and
-- MaximumAutomaticAttempts as 5, Config will run auto-remediations 5 times
-- within 50 seconds before throwing an exception.
--
-- 'executionControls', 'remediationConfiguration_executionControls' - An ExecutionControls object.
--
-- 'parameters', 'remediationConfiguration_parameters' - An object of the RemediationParameterValue.
--
-- 'maximumAutomaticAttempts', 'remediationConfiguration_maximumAutomaticAttempts' - The maximum number of failed attempts for auto-remediation. If you do
-- not select a number, the default is 5.
--
-- For example, if you specify MaximumAutomaticAttempts as 5 with
-- RetryAttemptSeconds as 50 seconds, Config will put a
-- RemediationException on your behalf for the failing resource after the
-- 5th failed attempt within 50 seconds.
--
-- 'targetVersion', 'remediationConfiguration_targetVersion' - Version of the target. For example, version of the SSM document.
--
-- If you make backward incompatible changes to the SSM document, you must
-- call PutRemediationConfiguration API again to ensure the remediations
-- can run.
--
-- 'configRuleName', 'remediationConfiguration_configRuleName' - The name of the Config rule.
--
-- 'targetType', 'remediationConfiguration_targetType' - The type of the target. Target executes remediation. For example, SSM
-- document.
--
-- 'targetId', 'remediationConfiguration_targetId' - Target ID is the name of the public document.
newRemediationConfiguration ::
  -- | 'configRuleName'
  Prelude.Text ->
  -- | 'targetType'
  RemediationTargetType ->
  -- | 'targetId'
  Prelude.Text ->
  RemediationConfiguration
newRemediationConfiguration
  pConfigRuleName_
  pTargetType_
  pTargetId_ =
    RemediationConfiguration'
      { resourceType =
          Prelude.Nothing,
        arn = Prelude.Nothing,
        automatic = Prelude.Nothing,
        createdByService = Prelude.Nothing,
        retryAttemptSeconds = Prelude.Nothing,
        executionControls = Prelude.Nothing,
        parameters = Prelude.Nothing,
        maximumAutomaticAttempts = Prelude.Nothing,
        targetVersion = Prelude.Nothing,
        configRuleName = pConfigRuleName_,
        targetType = pTargetType_,
        targetId = pTargetId_
      }

-- | The type of a resource.
remediationConfiguration_resourceType :: Lens.Lens' RemediationConfiguration (Prelude.Maybe Prelude.Text)
remediationConfiguration_resourceType = Lens.lens (\RemediationConfiguration' {resourceType} -> resourceType) (\s@RemediationConfiguration' {} a -> s {resourceType = a} :: RemediationConfiguration)

-- | Amazon Resource Name (ARN) of remediation configuration.
remediationConfiguration_arn :: Lens.Lens' RemediationConfiguration (Prelude.Maybe Prelude.Text)
remediationConfiguration_arn = Lens.lens (\RemediationConfiguration' {arn} -> arn) (\s@RemediationConfiguration' {} a -> s {arn = a} :: RemediationConfiguration)

-- | The remediation is triggered automatically.
remediationConfiguration_automatic :: Lens.Lens' RemediationConfiguration (Prelude.Maybe Prelude.Bool)
remediationConfiguration_automatic = Lens.lens (\RemediationConfiguration' {automatic} -> automatic) (\s@RemediationConfiguration' {} a -> s {automatic = a} :: RemediationConfiguration)

-- | Name of the service that owns the service linked rule, if applicable.
remediationConfiguration_createdByService :: Lens.Lens' RemediationConfiguration (Prelude.Maybe Prelude.Text)
remediationConfiguration_createdByService = Lens.lens (\RemediationConfiguration' {createdByService} -> createdByService) (\s@RemediationConfiguration' {} a -> s {createdByService = a} :: RemediationConfiguration)

-- | Maximum time in seconds that Config runs auto-remediation. If you do not
-- select a number, the default is 60 seconds.
--
-- For example, if you specify RetryAttemptSeconds as 50 seconds and
-- MaximumAutomaticAttempts as 5, Config will run auto-remediations 5 times
-- within 50 seconds before throwing an exception.
remediationConfiguration_retryAttemptSeconds :: Lens.Lens' RemediationConfiguration (Prelude.Maybe Prelude.Natural)
remediationConfiguration_retryAttemptSeconds = Lens.lens (\RemediationConfiguration' {retryAttemptSeconds} -> retryAttemptSeconds) (\s@RemediationConfiguration' {} a -> s {retryAttemptSeconds = a} :: RemediationConfiguration)

-- | An ExecutionControls object.
remediationConfiguration_executionControls :: Lens.Lens' RemediationConfiguration (Prelude.Maybe ExecutionControls)
remediationConfiguration_executionControls = Lens.lens (\RemediationConfiguration' {executionControls} -> executionControls) (\s@RemediationConfiguration' {} a -> s {executionControls = a} :: RemediationConfiguration)

-- | An object of the RemediationParameterValue.
remediationConfiguration_parameters :: Lens.Lens' RemediationConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text RemediationParameterValue))
remediationConfiguration_parameters = Lens.lens (\RemediationConfiguration' {parameters} -> parameters) (\s@RemediationConfiguration' {} a -> s {parameters = a} :: RemediationConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of failed attempts for auto-remediation. If you do
-- not select a number, the default is 5.
--
-- For example, if you specify MaximumAutomaticAttempts as 5 with
-- RetryAttemptSeconds as 50 seconds, Config will put a
-- RemediationException on your behalf for the failing resource after the
-- 5th failed attempt within 50 seconds.
remediationConfiguration_maximumAutomaticAttempts :: Lens.Lens' RemediationConfiguration (Prelude.Maybe Prelude.Natural)
remediationConfiguration_maximumAutomaticAttempts = Lens.lens (\RemediationConfiguration' {maximumAutomaticAttempts} -> maximumAutomaticAttempts) (\s@RemediationConfiguration' {} a -> s {maximumAutomaticAttempts = a} :: RemediationConfiguration)

-- | Version of the target. For example, version of the SSM document.
--
-- If you make backward incompatible changes to the SSM document, you must
-- call PutRemediationConfiguration API again to ensure the remediations
-- can run.
remediationConfiguration_targetVersion :: Lens.Lens' RemediationConfiguration (Prelude.Maybe Prelude.Text)
remediationConfiguration_targetVersion = Lens.lens (\RemediationConfiguration' {targetVersion} -> targetVersion) (\s@RemediationConfiguration' {} a -> s {targetVersion = a} :: RemediationConfiguration)

-- | The name of the Config rule.
remediationConfiguration_configRuleName :: Lens.Lens' RemediationConfiguration Prelude.Text
remediationConfiguration_configRuleName = Lens.lens (\RemediationConfiguration' {configRuleName} -> configRuleName) (\s@RemediationConfiguration' {} a -> s {configRuleName = a} :: RemediationConfiguration)

-- | The type of the target. Target executes remediation. For example, SSM
-- document.
remediationConfiguration_targetType :: Lens.Lens' RemediationConfiguration RemediationTargetType
remediationConfiguration_targetType = Lens.lens (\RemediationConfiguration' {targetType} -> targetType) (\s@RemediationConfiguration' {} a -> s {targetType = a} :: RemediationConfiguration)

-- | Target ID is the name of the public document.
remediationConfiguration_targetId :: Lens.Lens' RemediationConfiguration Prelude.Text
remediationConfiguration_targetId = Lens.lens (\RemediationConfiguration' {targetId} -> targetId) (\s@RemediationConfiguration' {} a -> s {targetId = a} :: RemediationConfiguration)

instance Core.FromJSON RemediationConfiguration where
  parseJSON =
    Core.withObject
      "RemediationConfiguration"
      ( \x ->
          RemediationConfiguration'
            Prelude.<$> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Automatic")
            Prelude.<*> (x Core..:? "CreatedByService")
            Prelude.<*> (x Core..:? "RetryAttemptSeconds")
            Prelude.<*> (x Core..:? "ExecutionControls")
            Prelude.<*> (x Core..:? "Parameters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "MaximumAutomaticAttempts")
            Prelude.<*> (x Core..:? "TargetVersion")
            Prelude.<*> (x Core..: "ConfigRuleName")
            Prelude.<*> (x Core..: "TargetType")
            Prelude.<*> (x Core..: "TargetId")
      )

instance Prelude.Hashable RemediationConfiguration where
  hashWithSalt salt' RemediationConfiguration' {..} =
    salt' `Prelude.hashWithSalt` targetId
      `Prelude.hashWithSalt` targetType
      `Prelude.hashWithSalt` configRuleName
      `Prelude.hashWithSalt` targetVersion
      `Prelude.hashWithSalt` maximumAutomaticAttempts
      `Prelude.hashWithSalt` parameters
      `Prelude.hashWithSalt` executionControls
      `Prelude.hashWithSalt` retryAttemptSeconds
      `Prelude.hashWithSalt` createdByService
      `Prelude.hashWithSalt` automatic
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData RemediationConfiguration where
  rnf RemediationConfiguration' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf targetId
      `Prelude.seq` Prelude.rnf targetType
      `Prelude.seq` Prelude.rnf configRuleName
      `Prelude.seq` Prelude.rnf targetVersion
      `Prelude.seq` Prelude.rnf maximumAutomaticAttempts
      `Prelude.seq` Prelude.rnf parameters
      `Prelude.seq` Prelude.rnf executionControls
      `Prelude.seq` Prelude.rnf retryAttemptSeconds
      `Prelude.seq` Prelude.rnf createdByService
      `Prelude.seq` Prelude.rnf automatic
      `Prelude.seq` Prelude.rnf arn

instance Core.ToJSON RemediationConfiguration where
  toJSON RemediationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResourceType" Core..=) Prelude.<$> resourceType,
            ("Arn" Core..=) Prelude.<$> arn,
            ("Automatic" Core..=) Prelude.<$> automatic,
            ("CreatedByService" Core..=)
              Prelude.<$> createdByService,
            ("RetryAttemptSeconds" Core..=)
              Prelude.<$> retryAttemptSeconds,
            ("ExecutionControls" Core..=)
              Prelude.<$> executionControls,
            ("Parameters" Core..=) Prelude.<$> parameters,
            ("MaximumAutomaticAttempts" Core..=)
              Prelude.<$> maximumAutomaticAttempts,
            ("TargetVersion" Core..=) Prelude.<$> targetVersion,
            Prelude.Just
              ("ConfigRuleName" Core..= configRuleName),
            Prelude.Just ("TargetType" Core..= targetType),
            Prelude.Just ("TargetId" Core..= targetId)
          ]
      )
