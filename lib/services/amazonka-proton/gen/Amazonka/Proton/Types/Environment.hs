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
-- Module      : Amazonka.Proton.Types.Environment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.Environment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.DeploymentStatus
import Amazonka.Proton.Types.Provisioning

-- | The environment detail data. An AWS Proton environment is a set
-- resources shared across an AWS Proton service.
--
-- /See:/ 'newEnvironment' smart constructor.
data Environment = Environment'
  { -- | An environment deployment status message.
    deploymentStatusMessage :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The ID of the environment account that the environment infrastructure
    -- resources are provisioned in.
    environmentAccountId :: Prelude.Maybe Prelude.Text,
    -- | When included, indicates that the environment template is for customer
    -- provisioned and managed infrastructure.
    provisioning :: Prelude.Maybe Provisioning,
    -- | The Amazon Resource Name (ARN) of the AWS Proton service role that
    -- allows AWS Proton to make calls to other services on your behalf.
    protonServiceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment account connection that\'s used to provision
    -- infrastructure resources in an environment account.
    environmentAccountConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The environment spec.
    spec :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The description of the environment.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Prelude.Text,
    -- | The time when the environment was created.
    createdAt :: Core.POSIX,
    -- | The environment deployment status.
    deploymentStatus :: DeploymentStatus,
    -- | The time when a deployment of the environment was last attempted.
    lastDeploymentAttemptedAt :: Core.POSIX,
    -- | The time when the environment was last deployed successfully.
    lastDeploymentSucceededAt :: Core.POSIX,
    -- | The name of the environment.
    name :: Prelude.Text,
    -- | The ID of the major version of the environment template.
    templateMajorVersion :: Prelude.Text,
    -- | The ID of the minor version of the environment template.
    templateMinorVersion :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment template.
    templateName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Environment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deploymentStatusMessage', 'environment_deploymentStatusMessage' - An environment deployment status message.
--
-- 'environmentAccountId', 'environment_environmentAccountId' - The ID of the environment account that the environment infrastructure
-- resources are provisioned in.
--
-- 'provisioning', 'environment_provisioning' - When included, indicates that the environment template is for customer
-- provisioned and managed infrastructure.
--
-- 'protonServiceRoleArn', 'environment_protonServiceRoleArn' - The Amazon Resource Name (ARN) of the AWS Proton service role that
-- allows AWS Proton to make calls to other services on your behalf.
--
-- 'environmentAccountConnectionId', 'environment_environmentAccountConnectionId' - The ID of the environment account connection that\'s used to provision
-- infrastructure resources in an environment account.
--
-- 'spec', 'environment_spec' - The environment spec.
--
-- 'description', 'environment_description' - The description of the environment.
--
-- 'arn', 'environment_arn' - The Amazon Resource Name (ARN) of the environment.
--
-- 'createdAt', 'environment_createdAt' - The time when the environment was created.
--
-- 'deploymentStatus', 'environment_deploymentStatus' - The environment deployment status.
--
-- 'lastDeploymentAttemptedAt', 'environment_lastDeploymentAttemptedAt' - The time when a deployment of the environment was last attempted.
--
-- 'lastDeploymentSucceededAt', 'environment_lastDeploymentSucceededAt' - The time when the environment was last deployed successfully.
--
-- 'name', 'environment_name' - The name of the environment.
--
-- 'templateMajorVersion', 'environment_templateMajorVersion' - The ID of the major version of the environment template.
--
-- 'templateMinorVersion', 'environment_templateMinorVersion' - The ID of the minor version of the environment template.
--
-- 'templateName', 'environment_templateName' - The Amazon Resource Name (ARN) of the environment template.
newEnvironment ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'deploymentStatus'
  DeploymentStatus ->
  -- | 'lastDeploymentAttemptedAt'
  Prelude.UTCTime ->
  -- | 'lastDeploymentSucceededAt'
  Prelude.UTCTime ->
  -- | 'name'
  Prelude.Text ->
  -- | 'templateMajorVersion'
  Prelude.Text ->
  -- | 'templateMinorVersion'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  Environment
newEnvironment
  pArn_
  pCreatedAt_
  pDeploymentStatus_
  pLastDeploymentAttemptedAt_
  pLastDeploymentSucceededAt_
  pName_
  pTemplateMajorVersion_
  pTemplateMinorVersion_
  pTemplateName_ =
    Environment'
      { deploymentStatusMessage =
          Prelude.Nothing,
        environmentAccountId = Prelude.Nothing,
        provisioning = Prelude.Nothing,
        protonServiceRoleArn = Prelude.Nothing,
        environmentAccountConnectionId = Prelude.Nothing,
        spec = Prelude.Nothing,
        description = Prelude.Nothing,
        arn = pArn_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        deploymentStatus = pDeploymentStatus_,
        lastDeploymentAttemptedAt =
          Core._Time Lens.# pLastDeploymentAttemptedAt_,
        lastDeploymentSucceededAt =
          Core._Time Lens.# pLastDeploymentSucceededAt_,
        name = pName_,
        templateMajorVersion = pTemplateMajorVersion_,
        templateMinorVersion = pTemplateMinorVersion_,
        templateName = pTemplateName_
      }

-- | An environment deployment status message.
environment_deploymentStatusMessage :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_deploymentStatusMessage = Lens.lens (\Environment' {deploymentStatusMessage} -> deploymentStatusMessage) (\s@Environment' {} a -> s {deploymentStatusMessage = a} :: Environment) Prelude.. Lens.mapping Core._Sensitive

-- | The ID of the environment account that the environment infrastructure
-- resources are provisioned in.
environment_environmentAccountId :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_environmentAccountId = Lens.lens (\Environment' {environmentAccountId} -> environmentAccountId) (\s@Environment' {} a -> s {environmentAccountId = a} :: Environment)

-- | When included, indicates that the environment template is for customer
-- provisioned and managed infrastructure.
environment_provisioning :: Lens.Lens' Environment (Prelude.Maybe Provisioning)
environment_provisioning = Lens.lens (\Environment' {provisioning} -> provisioning) (\s@Environment' {} a -> s {provisioning = a} :: Environment)

-- | The Amazon Resource Name (ARN) of the AWS Proton service role that
-- allows AWS Proton to make calls to other services on your behalf.
environment_protonServiceRoleArn :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_protonServiceRoleArn = Lens.lens (\Environment' {protonServiceRoleArn} -> protonServiceRoleArn) (\s@Environment' {} a -> s {protonServiceRoleArn = a} :: Environment)

-- | The ID of the environment account connection that\'s used to provision
-- infrastructure resources in an environment account.
environment_environmentAccountConnectionId :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_environmentAccountConnectionId = Lens.lens (\Environment' {environmentAccountConnectionId} -> environmentAccountConnectionId) (\s@Environment' {} a -> s {environmentAccountConnectionId = a} :: Environment)

-- | The environment spec.
environment_spec :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_spec = Lens.lens (\Environment' {spec} -> spec) (\s@Environment' {} a -> s {spec = a} :: Environment) Prelude.. Lens.mapping Core._Sensitive

-- | The description of the environment.
environment_description :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_description = Lens.lens (\Environment' {description} -> description) (\s@Environment' {} a -> s {description = a} :: Environment) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the environment.
environment_arn :: Lens.Lens' Environment Prelude.Text
environment_arn = Lens.lens (\Environment' {arn} -> arn) (\s@Environment' {} a -> s {arn = a} :: Environment)

-- | The time when the environment was created.
environment_createdAt :: Lens.Lens' Environment Prelude.UTCTime
environment_createdAt = Lens.lens (\Environment' {createdAt} -> createdAt) (\s@Environment' {} a -> s {createdAt = a} :: Environment) Prelude.. Core._Time

-- | The environment deployment status.
environment_deploymentStatus :: Lens.Lens' Environment DeploymentStatus
environment_deploymentStatus = Lens.lens (\Environment' {deploymentStatus} -> deploymentStatus) (\s@Environment' {} a -> s {deploymentStatus = a} :: Environment)

-- | The time when a deployment of the environment was last attempted.
environment_lastDeploymentAttemptedAt :: Lens.Lens' Environment Prelude.UTCTime
environment_lastDeploymentAttemptedAt = Lens.lens (\Environment' {lastDeploymentAttemptedAt} -> lastDeploymentAttemptedAt) (\s@Environment' {} a -> s {lastDeploymentAttemptedAt = a} :: Environment) Prelude.. Core._Time

-- | The time when the environment was last deployed successfully.
environment_lastDeploymentSucceededAt :: Lens.Lens' Environment Prelude.UTCTime
environment_lastDeploymentSucceededAt = Lens.lens (\Environment' {lastDeploymentSucceededAt} -> lastDeploymentSucceededAt) (\s@Environment' {} a -> s {lastDeploymentSucceededAt = a} :: Environment) Prelude.. Core._Time

-- | The name of the environment.
environment_name :: Lens.Lens' Environment Prelude.Text
environment_name = Lens.lens (\Environment' {name} -> name) (\s@Environment' {} a -> s {name = a} :: Environment)

-- | The ID of the major version of the environment template.
environment_templateMajorVersion :: Lens.Lens' Environment Prelude.Text
environment_templateMajorVersion = Lens.lens (\Environment' {templateMajorVersion} -> templateMajorVersion) (\s@Environment' {} a -> s {templateMajorVersion = a} :: Environment)

-- | The ID of the minor version of the environment template.
environment_templateMinorVersion :: Lens.Lens' Environment Prelude.Text
environment_templateMinorVersion = Lens.lens (\Environment' {templateMinorVersion} -> templateMinorVersion) (\s@Environment' {} a -> s {templateMinorVersion = a} :: Environment)

-- | The Amazon Resource Name (ARN) of the environment template.
environment_templateName :: Lens.Lens' Environment Prelude.Text
environment_templateName = Lens.lens (\Environment' {templateName} -> templateName) (\s@Environment' {} a -> s {templateName = a} :: Environment)

instance Core.FromJSON Environment where
  parseJSON =
    Core.withObject
      "Environment"
      ( \x ->
          Environment'
            Prelude.<$> (x Core..:? "deploymentStatusMessage")
            Prelude.<*> (x Core..:? "environmentAccountId")
            Prelude.<*> (x Core..:? "provisioning")
            Prelude.<*> (x Core..:? "protonServiceRoleArn")
            Prelude.<*> (x Core..:? "environmentAccountConnectionId")
            Prelude.<*> (x Core..:? "spec")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "createdAt")
            Prelude.<*> (x Core..: "deploymentStatus")
            Prelude.<*> (x Core..: "lastDeploymentAttemptedAt")
            Prelude.<*> (x Core..: "lastDeploymentSucceededAt")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "templateMajorVersion")
            Prelude.<*> (x Core..: "templateMinorVersion")
            Prelude.<*> (x Core..: "templateName")
      )

instance Prelude.Hashable Environment where
  hashWithSalt salt' Environment' {..} =
    salt' `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateMinorVersion
      `Prelude.hashWithSalt` templateMajorVersion
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastDeploymentSucceededAt
      `Prelude.hashWithSalt` lastDeploymentAttemptedAt
      `Prelude.hashWithSalt` deploymentStatus
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` spec
      `Prelude.hashWithSalt` environmentAccountConnectionId
      `Prelude.hashWithSalt` protonServiceRoleArn
      `Prelude.hashWithSalt` provisioning
      `Prelude.hashWithSalt` environmentAccountId
      `Prelude.hashWithSalt` deploymentStatusMessage

instance Prelude.NFData Environment where
  rnf Environment' {..} =
    Prelude.rnf deploymentStatusMessage
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateMinorVersion
      `Prelude.seq` Prelude.rnf templateMajorVersion
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastDeploymentSucceededAt
      `Prelude.seq` Prelude.rnf lastDeploymentAttemptedAt
      `Prelude.seq` Prelude.rnf deploymentStatus
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf spec
      `Prelude.seq` Prelude.rnf environmentAccountConnectionId
      `Prelude.seq` Prelude.rnf protonServiceRoleArn
      `Prelude.seq` Prelude.rnf provisioning
      `Prelude.seq` Prelude.rnf environmentAccountId
