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
-- Module      : Amazonka.Config.Types.EvaluationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.EvaluationResult where

import Amazonka.Config.Types.ComplianceType
import Amazonka.Config.Types.EvaluationResultIdentifier
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of an Config evaluation. Provides the Amazon Web Services
-- resource that was evaluated, the compliance of the resource, related
-- time stamps, and supplementary information.
--
-- /See:/ 'newEvaluationResult' smart constructor.
data EvaluationResult = EvaluationResult'
  { -- | Uniquely identifies the evaluation result.
    evaluationResultIdentifier :: Prelude.Maybe EvaluationResultIdentifier,
    -- | Supplementary information about how the evaluation determined the
    -- compliance.
    annotation :: Prelude.Maybe Prelude.Text,
    -- | The time when the Config rule evaluated the Amazon Web Services
    -- resource.
    configRuleInvokedTime :: Prelude.Maybe Core.POSIX,
    -- | The time when Config recorded the evaluation result.
    resultRecordedTime :: Prelude.Maybe Core.POSIX,
    -- | An encrypted token that associates an evaluation with an Config rule.
    -- The token identifies the rule, the Amazon Web Services resource being
    -- evaluated, and the event that triggered the evaluation.
    resultToken :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the Amazon Web Services resource complies with the
    -- Config rule that evaluated it.
    --
    -- For the @EvaluationResult@ data type, Config supports only the
    -- @COMPLIANT@, @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. Config does
    -- not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@
    -- data type.
    complianceType :: Prelude.Maybe ComplianceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EvaluationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'evaluationResultIdentifier', 'evaluationResult_evaluationResultIdentifier' - Uniquely identifies the evaluation result.
--
-- 'annotation', 'evaluationResult_annotation' - Supplementary information about how the evaluation determined the
-- compliance.
--
-- 'configRuleInvokedTime', 'evaluationResult_configRuleInvokedTime' - The time when the Config rule evaluated the Amazon Web Services
-- resource.
--
-- 'resultRecordedTime', 'evaluationResult_resultRecordedTime' - The time when Config recorded the evaluation result.
--
-- 'resultToken', 'evaluationResult_resultToken' - An encrypted token that associates an evaluation with an Config rule.
-- The token identifies the rule, the Amazon Web Services resource being
-- evaluated, and the event that triggered the evaluation.
--
-- 'complianceType', 'evaluationResult_complianceType' - Indicates whether the Amazon Web Services resource complies with the
-- Config rule that evaluated it.
--
-- For the @EvaluationResult@ data type, Config supports only the
-- @COMPLIANT@, @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. Config does
-- not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@
-- data type.
newEvaluationResult ::
  EvaluationResult
newEvaluationResult =
  EvaluationResult'
    { evaluationResultIdentifier =
        Prelude.Nothing,
      annotation = Prelude.Nothing,
      configRuleInvokedTime = Prelude.Nothing,
      resultRecordedTime = Prelude.Nothing,
      resultToken = Prelude.Nothing,
      complianceType = Prelude.Nothing
    }

-- | Uniquely identifies the evaluation result.
evaluationResult_evaluationResultIdentifier :: Lens.Lens' EvaluationResult (Prelude.Maybe EvaluationResultIdentifier)
evaluationResult_evaluationResultIdentifier = Lens.lens (\EvaluationResult' {evaluationResultIdentifier} -> evaluationResultIdentifier) (\s@EvaluationResult' {} a -> s {evaluationResultIdentifier = a} :: EvaluationResult)

-- | Supplementary information about how the evaluation determined the
-- compliance.
evaluationResult_annotation :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Text)
evaluationResult_annotation = Lens.lens (\EvaluationResult' {annotation} -> annotation) (\s@EvaluationResult' {} a -> s {annotation = a} :: EvaluationResult)

-- | The time when the Config rule evaluated the Amazon Web Services
-- resource.
evaluationResult_configRuleInvokedTime :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.UTCTime)
evaluationResult_configRuleInvokedTime = Lens.lens (\EvaluationResult' {configRuleInvokedTime} -> configRuleInvokedTime) (\s@EvaluationResult' {} a -> s {configRuleInvokedTime = a} :: EvaluationResult) Prelude.. Lens.mapping Core._Time

-- | The time when Config recorded the evaluation result.
evaluationResult_resultRecordedTime :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.UTCTime)
evaluationResult_resultRecordedTime = Lens.lens (\EvaluationResult' {resultRecordedTime} -> resultRecordedTime) (\s@EvaluationResult' {} a -> s {resultRecordedTime = a} :: EvaluationResult) Prelude.. Lens.mapping Core._Time

-- | An encrypted token that associates an evaluation with an Config rule.
-- The token identifies the rule, the Amazon Web Services resource being
-- evaluated, and the event that triggered the evaluation.
evaluationResult_resultToken :: Lens.Lens' EvaluationResult (Prelude.Maybe Prelude.Text)
evaluationResult_resultToken = Lens.lens (\EvaluationResult' {resultToken} -> resultToken) (\s@EvaluationResult' {} a -> s {resultToken = a} :: EvaluationResult)

-- | Indicates whether the Amazon Web Services resource complies with the
-- Config rule that evaluated it.
--
-- For the @EvaluationResult@ data type, Config supports only the
-- @COMPLIANT@, @NON_COMPLIANT@, and @NOT_APPLICABLE@ values. Config does
-- not support the @INSUFFICIENT_DATA@ value for the @EvaluationResult@
-- data type.
evaluationResult_complianceType :: Lens.Lens' EvaluationResult (Prelude.Maybe ComplianceType)
evaluationResult_complianceType = Lens.lens (\EvaluationResult' {complianceType} -> complianceType) (\s@EvaluationResult' {} a -> s {complianceType = a} :: EvaluationResult)

instance Core.FromJSON EvaluationResult where
  parseJSON =
    Core.withObject
      "EvaluationResult"
      ( \x ->
          EvaluationResult'
            Prelude.<$> (x Core..:? "EvaluationResultIdentifier")
            Prelude.<*> (x Core..:? "Annotation")
            Prelude.<*> (x Core..:? "ConfigRuleInvokedTime")
            Prelude.<*> (x Core..:? "ResultRecordedTime")
            Prelude.<*> (x Core..:? "ResultToken")
            Prelude.<*> (x Core..:? "ComplianceType")
      )

instance Prelude.Hashable EvaluationResult where
  hashWithSalt salt' EvaluationResult' {..} =
    salt' `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` resultToken
      `Prelude.hashWithSalt` resultRecordedTime
      `Prelude.hashWithSalt` configRuleInvokedTime
      `Prelude.hashWithSalt` annotation
      `Prelude.hashWithSalt` evaluationResultIdentifier

instance Prelude.NFData EvaluationResult where
  rnf EvaluationResult' {..} =
    Prelude.rnf evaluationResultIdentifier
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf resultToken
      `Prelude.seq` Prelude.rnf resultRecordedTime
      `Prelude.seq` Prelude.rnf configRuleInvokedTime
      `Prelude.seq` Prelude.rnf annotation
