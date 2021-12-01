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
-- Module      : Amazonka.IAM.Types.ResourceSpecificResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.ResourceSpecificResult where

import qualified Amazonka.Core as Core
import Amazonka.IAM.Types.PermissionsBoundaryDecisionDetail
import Amazonka.IAM.Types.PolicyEvaluationDecisionType
import Amazonka.IAM.Types.Statement
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains the result of the simulation of a single API operation call on
-- a single resource.
--
-- This data type is used by a member of the EvaluationResult data type.
--
-- /See:/ 'newResourceSpecificResult' smart constructor.
data ResourceSpecificResult = ResourceSpecificResult'
  { -- | A list of the statements in the input policies that determine the result
    -- for this part of the simulation. Remember that even if multiple
    -- statements allow the operation on the resource, if /any/ statement
    -- denies that operation, then the explicit deny overrides any allow. In
    -- addition, the deny statement is the only entry included in the result.
    matchedStatements :: Prelude.Maybe [Statement],
    -- | Additional details about the results of the evaluation decision on a
    -- single resource. This parameter is returned only for cross-account
    -- simulations. This parameter explains how each policy type contributes to
    -- the resource-specific evaluation decision.
    evalDecisionDetails :: Prelude.Maybe (Prelude.HashMap Prelude.Text PolicyEvaluationDecisionType),
    -- | A list of context keys that are required by the included input policies
    -- but that were not provided by one of the input parameters. This list is
    -- used when a list of ARNs is included in the @ResourceArns@ parameter
    -- instead of \"*\". If you do not specify individual resources, by setting
    -- @ResourceArns@ to \"*\" or by not including the @ResourceArns@
    -- parameter, then any missing context values are instead included under
    -- the @EvaluationResults@ section. To discover the context keys used by a
    -- set of policies, you can call GetContextKeysForCustomPolicy or
    -- GetContextKeysForPrincipalPolicy.
    missingContextValues :: Prelude.Maybe [Prelude.Text],
    -- | Contains information about the effect that a permissions boundary has on
    -- a policy simulation when that boundary is applied to an IAM entity.
    permissionsBoundaryDecisionDetail :: Prelude.Maybe PermissionsBoundaryDecisionDetail,
    -- | The name of the simulated resource, in Amazon Resource Name (ARN)
    -- format.
    evalResourceName :: Prelude.Text,
    -- | The result of the simulation of the simulated API operation on the
    -- resource specified in @EvalResourceName@.
    evalResourceDecision :: PolicyEvaluationDecisionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceSpecificResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'matchedStatements', 'resourceSpecificResult_matchedStatements' - A list of the statements in the input policies that determine the result
-- for this part of the simulation. Remember that even if multiple
-- statements allow the operation on the resource, if /any/ statement
-- denies that operation, then the explicit deny overrides any allow. In
-- addition, the deny statement is the only entry included in the result.
--
-- 'evalDecisionDetails', 'resourceSpecificResult_evalDecisionDetails' - Additional details about the results of the evaluation decision on a
-- single resource. This parameter is returned only for cross-account
-- simulations. This parameter explains how each policy type contributes to
-- the resource-specific evaluation decision.
--
-- 'missingContextValues', 'resourceSpecificResult_missingContextValues' - A list of context keys that are required by the included input policies
-- but that were not provided by one of the input parameters. This list is
-- used when a list of ARNs is included in the @ResourceArns@ parameter
-- instead of \"*\". If you do not specify individual resources, by setting
-- @ResourceArns@ to \"*\" or by not including the @ResourceArns@
-- parameter, then any missing context values are instead included under
-- the @EvaluationResults@ section. To discover the context keys used by a
-- set of policies, you can call GetContextKeysForCustomPolicy or
-- GetContextKeysForPrincipalPolicy.
--
-- 'permissionsBoundaryDecisionDetail', 'resourceSpecificResult_permissionsBoundaryDecisionDetail' - Contains information about the effect that a permissions boundary has on
-- a policy simulation when that boundary is applied to an IAM entity.
--
-- 'evalResourceName', 'resourceSpecificResult_evalResourceName' - The name of the simulated resource, in Amazon Resource Name (ARN)
-- format.
--
-- 'evalResourceDecision', 'resourceSpecificResult_evalResourceDecision' - The result of the simulation of the simulated API operation on the
-- resource specified in @EvalResourceName@.
newResourceSpecificResult ::
  -- | 'evalResourceName'
  Prelude.Text ->
  -- | 'evalResourceDecision'
  PolicyEvaluationDecisionType ->
  ResourceSpecificResult
newResourceSpecificResult
  pEvalResourceName_
  pEvalResourceDecision_ =
    ResourceSpecificResult'
      { matchedStatements =
          Prelude.Nothing,
        evalDecisionDetails = Prelude.Nothing,
        missingContextValues = Prelude.Nothing,
        permissionsBoundaryDecisionDetail = Prelude.Nothing,
        evalResourceName = pEvalResourceName_,
        evalResourceDecision = pEvalResourceDecision_
      }

-- | A list of the statements in the input policies that determine the result
-- for this part of the simulation. Remember that even if multiple
-- statements allow the operation on the resource, if /any/ statement
-- denies that operation, then the explicit deny overrides any allow. In
-- addition, the deny statement is the only entry included in the result.
resourceSpecificResult_matchedStatements :: Lens.Lens' ResourceSpecificResult (Prelude.Maybe [Statement])
resourceSpecificResult_matchedStatements = Lens.lens (\ResourceSpecificResult' {matchedStatements} -> matchedStatements) (\s@ResourceSpecificResult' {} a -> s {matchedStatements = a} :: ResourceSpecificResult) Prelude.. Lens.mapping Lens.coerced

-- | Additional details about the results of the evaluation decision on a
-- single resource. This parameter is returned only for cross-account
-- simulations. This parameter explains how each policy type contributes to
-- the resource-specific evaluation decision.
resourceSpecificResult_evalDecisionDetails :: Lens.Lens' ResourceSpecificResult (Prelude.Maybe (Prelude.HashMap Prelude.Text PolicyEvaluationDecisionType))
resourceSpecificResult_evalDecisionDetails = Lens.lens (\ResourceSpecificResult' {evalDecisionDetails} -> evalDecisionDetails) (\s@ResourceSpecificResult' {} a -> s {evalDecisionDetails = a} :: ResourceSpecificResult) Prelude.. Lens.mapping Lens.coerced

-- | A list of context keys that are required by the included input policies
-- but that were not provided by one of the input parameters. This list is
-- used when a list of ARNs is included in the @ResourceArns@ parameter
-- instead of \"*\". If you do not specify individual resources, by setting
-- @ResourceArns@ to \"*\" or by not including the @ResourceArns@
-- parameter, then any missing context values are instead included under
-- the @EvaluationResults@ section. To discover the context keys used by a
-- set of policies, you can call GetContextKeysForCustomPolicy or
-- GetContextKeysForPrincipalPolicy.
resourceSpecificResult_missingContextValues :: Lens.Lens' ResourceSpecificResult (Prelude.Maybe [Prelude.Text])
resourceSpecificResult_missingContextValues = Lens.lens (\ResourceSpecificResult' {missingContextValues} -> missingContextValues) (\s@ResourceSpecificResult' {} a -> s {missingContextValues = a} :: ResourceSpecificResult) Prelude.. Lens.mapping Lens.coerced

-- | Contains information about the effect that a permissions boundary has on
-- a policy simulation when that boundary is applied to an IAM entity.
resourceSpecificResult_permissionsBoundaryDecisionDetail :: Lens.Lens' ResourceSpecificResult (Prelude.Maybe PermissionsBoundaryDecisionDetail)
resourceSpecificResult_permissionsBoundaryDecisionDetail = Lens.lens (\ResourceSpecificResult' {permissionsBoundaryDecisionDetail} -> permissionsBoundaryDecisionDetail) (\s@ResourceSpecificResult' {} a -> s {permissionsBoundaryDecisionDetail = a} :: ResourceSpecificResult)

-- | The name of the simulated resource, in Amazon Resource Name (ARN)
-- format.
resourceSpecificResult_evalResourceName :: Lens.Lens' ResourceSpecificResult Prelude.Text
resourceSpecificResult_evalResourceName = Lens.lens (\ResourceSpecificResult' {evalResourceName} -> evalResourceName) (\s@ResourceSpecificResult' {} a -> s {evalResourceName = a} :: ResourceSpecificResult)

-- | The result of the simulation of the simulated API operation on the
-- resource specified in @EvalResourceName@.
resourceSpecificResult_evalResourceDecision :: Lens.Lens' ResourceSpecificResult PolicyEvaluationDecisionType
resourceSpecificResult_evalResourceDecision = Lens.lens (\ResourceSpecificResult' {evalResourceDecision} -> evalResourceDecision) (\s@ResourceSpecificResult' {} a -> s {evalResourceDecision = a} :: ResourceSpecificResult)

instance Core.FromXML ResourceSpecificResult where
  parseXML x =
    ResourceSpecificResult'
      Prelude.<$> ( x Core..@? "MatchedStatements"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "EvalDecisionDetails"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLMap "entry" "key" "value")
                  )
      Prelude.<*> ( x Core..@? "MissingContextValues"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "PermissionsBoundaryDecisionDetail")
      Prelude.<*> (x Core..@ "EvalResourceName")
      Prelude.<*> (x Core..@ "EvalResourceDecision")

instance Prelude.Hashable ResourceSpecificResult where
  hashWithSalt salt' ResourceSpecificResult' {..} =
    salt' `Prelude.hashWithSalt` evalResourceDecision
      `Prelude.hashWithSalt` evalResourceName
      `Prelude.hashWithSalt` permissionsBoundaryDecisionDetail
      `Prelude.hashWithSalt` missingContextValues
      `Prelude.hashWithSalt` evalDecisionDetails
      `Prelude.hashWithSalt` matchedStatements

instance Prelude.NFData ResourceSpecificResult where
  rnf ResourceSpecificResult' {..} =
    Prelude.rnf matchedStatements
      `Prelude.seq` Prelude.rnf evalResourceDecision
      `Prelude.seq` Prelude.rnf evalResourceName
      `Prelude.seq` Prelude.rnf permissionsBoundaryDecisionDetail
      `Prelude.seq` Prelude.rnf missingContextValues
      `Prelude.seq` Prelude.rnf evalDecisionDetails
