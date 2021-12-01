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
-- Module      : Amazonka.CodeDeploy.Types.DeploymentTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.DeploymentTarget where

import Amazonka.CodeDeploy.Types.CloudFormationTarget
import Amazonka.CodeDeploy.Types.DeploymentTargetType
import Amazonka.CodeDeploy.Types.ECSTarget
import Amazonka.CodeDeploy.Types.InstanceTarget
import Amazonka.CodeDeploy.Types.LambdaTarget
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the deployment target.
--
-- /See:/ 'newDeploymentTarget' smart constructor.
data DeploymentTarget = DeploymentTarget'
  { -- | Information about the target for a deployment that uses the
    -- EC2\/On-premises compute platform.
    instanceTarget :: Prelude.Maybe InstanceTarget,
    cloudFormationTarget :: Prelude.Maybe CloudFormationTarget,
    -- | Information about the target for a deployment that uses the Amazon ECS
    -- compute platform.
    ecsTarget :: Prelude.Maybe ECSTarget,
    -- | The deployment type that is specific to the deployment\'s compute
    -- platform or deployments initiated by a CloudFormation stack update.
    deploymentTargetType :: Prelude.Maybe DeploymentTargetType,
    -- | Information about the target for a deployment that uses the AWS Lambda
    -- compute platform.
    lambdaTarget :: Prelude.Maybe LambdaTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeploymentTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTarget', 'deploymentTarget_instanceTarget' - Information about the target for a deployment that uses the
-- EC2\/On-premises compute platform.
--
-- 'cloudFormationTarget', 'deploymentTarget_cloudFormationTarget' - Undocumented member.
--
-- 'ecsTarget', 'deploymentTarget_ecsTarget' - Information about the target for a deployment that uses the Amazon ECS
-- compute platform.
--
-- 'deploymentTargetType', 'deploymentTarget_deploymentTargetType' - The deployment type that is specific to the deployment\'s compute
-- platform or deployments initiated by a CloudFormation stack update.
--
-- 'lambdaTarget', 'deploymentTarget_lambdaTarget' - Information about the target for a deployment that uses the AWS Lambda
-- compute platform.
newDeploymentTarget ::
  DeploymentTarget
newDeploymentTarget =
  DeploymentTarget'
    { instanceTarget = Prelude.Nothing,
      cloudFormationTarget = Prelude.Nothing,
      ecsTarget = Prelude.Nothing,
      deploymentTargetType = Prelude.Nothing,
      lambdaTarget = Prelude.Nothing
    }

-- | Information about the target for a deployment that uses the
-- EC2\/On-premises compute platform.
deploymentTarget_instanceTarget :: Lens.Lens' DeploymentTarget (Prelude.Maybe InstanceTarget)
deploymentTarget_instanceTarget = Lens.lens (\DeploymentTarget' {instanceTarget} -> instanceTarget) (\s@DeploymentTarget' {} a -> s {instanceTarget = a} :: DeploymentTarget)

-- | Undocumented member.
deploymentTarget_cloudFormationTarget :: Lens.Lens' DeploymentTarget (Prelude.Maybe CloudFormationTarget)
deploymentTarget_cloudFormationTarget = Lens.lens (\DeploymentTarget' {cloudFormationTarget} -> cloudFormationTarget) (\s@DeploymentTarget' {} a -> s {cloudFormationTarget = a} :: DeploymentTarget)

-- | Information about the target for a deployment that uses the Amazon ECS
-- compute platform.
deploymentTarget_ecsTarget :: Lens.Lens' DeploymentTarget (Prelude.Maybe ECSTarget)
deploymentTarget_ecsTarget = Lens.lens (\DeploymentTarget' {ecsTarget} -> ecsTarget) (\s@DeploymentTarget' {} a -> s {ecsTarget = a} :: DeploymentTarget)

-- | The deployment type that is specific to the deployment\'s compute
-- platform or deployments initiated by a CloudFormation stack update.
deploymentTarget_deploymentTargetType :: Lens.Lens' DeploymentTarget (Prelude.Maybe DeploymentTargetType)
deploymentTarget_deploymentTargetType = Lens.lens (\DeploymentTarget' {deploymentTargetType} -> deploymentTargetType) (\s@DeploymentTarget' {} a -> s {deploymentTargetType = a} :: DeploymentTarget)

-- | Information about the target for a deployment that uses the AWS Lambda
-- compute platform.
deploymentTarget_lambdaTarget :: Lens.Lens' DeploymentTarget (Prelude.Maybe LambdaTarget)
deploymentTarget_lambdaTarget = Lens.lens (\DeploymentTarget' {lambdaTarget} -> lambdaTarget) (\s@DeploymentTarget' {} a -> s {lambdaTarget = a} :: DeploymentTarget)

instance Core.FromJSON DeploymentTarget where
  parseJSON =
    Core.withObject
      "DeploymentTarget"
      ( \x ->
          DeploymentTarget'
            Prelude.<$> (x Core..:? "instanceTarget")
            Prelude.<*> (x Core..:? "cloudFormationTarget")
            Prelude.<*> (x Core..:? "ecsTarget")
            Prelude.<*> (x Core..:? "deploymentTargetType")
            Prelude.<*> (x Core..:? "lambdaTarget")
      )

instance Prelude.Hashable DeploymentTarget where
  hashWithSalt salt' DeploymentTarget' {..} =
    salt' `Prelude.hashWithSalt` lambdaTarget
      `Prelude.hashWithSalt` deploymentTargetType
      `Prelude.hashWithSalt` ecsTarget
      `Prelude.hashWithSalt` cloudFormationTarget
      `Prelude.hashWithSalt` instanceTarget

instance Prelude.NFData DeploymentTarget where
  rnf DeploymentTarget' {..} =
    Prelude.rnf instanceTarget
      `Prelude.seq` Prelude.rnf lambdaTarget
      `Prelude.seq` Prelude.rnf deploymentTargetType
      `Prelude.seq` Prelude.rnf ecsTarget
      `Prelude.seq` Prelude.rnf cloudFormationTarget
