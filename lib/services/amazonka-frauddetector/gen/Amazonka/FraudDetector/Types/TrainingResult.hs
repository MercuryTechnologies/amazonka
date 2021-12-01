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
-- Module      : Amazonka.FraudDetector.Types.TrainingResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.TrainingResult where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types.DataValidationMetrics
import Amazonka.FraudDetector.Types.TrainingMetrics
import Amazonka.FraudDetector.Types.VariableImportanceMetrics
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The training result details.
--
-- /See:/ 'newTrainingResult' smart constructor.
data TrainingResult = TrainingResult'
  { -- | The validation metrics.
    dataValidationMetrics :: Prelude.Maybe DataValidationMetrics,
    -- | The training metric details.
    trainingMetrics :: Prelude.Maybe TrainingMetrics,
    -- | The variable importance metrics.
    variableImportanceMetrics :: Prelude.Maybe VariableImportanceMetrics
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrainingResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataValidationMetrics', 'trainingResult_dataValidationMetrics' - The validation metrics.
--
-- 'trainingMetrics', 'trainingResult_trainingMetrics' - The training metric details.
--
-- 'variableImportanceMetrics', 'trainingResult_variableImportanceMetrics' - The variable importance metrics.
newTrainingResult ::
  TrainingResult
newTrainingResult =
  TrainingResult'
    { dataValidationMetrics =
        Prelude.Nothing,
      trainingMetrics = Prelude.Nothing,
      variableImportanceMetrics = Prelude.Nothing
    }

-- | The validation metrics.
trainingResult_dataValidationMetrics :: Lens.Lens' TrainingResult (Prelude.Maybe DataValidationMetrics)
trainingResult_dataValidationMetrics = Lens.lens (\TrainingResult' {dataValidationMetrics} -> dataValidationMetrics) (\s@TrainingResult' {} a -> s {dataValidationMetrics = a} :: TrainingResult)

-- | The training metric details.
trainingResult_trainingMetrics :: Lens.Lens' TrainingResult (Prelude.Maybe TrainingMetrics)
trainingResult_trainingMetrics = Lens.lens (\TrainingResult' {trainingMetrics} -> trainingMetrics) (\s@TrainingResult' {} a -> s {trainingMetrics = a} :: TrainingResult)

-- | The variable importance metrics.
trainingResult_variableImportanceMetrics :: Lens.Lens' TrainingResult (Prelude.Maybe VariableImportanceMetrics)
trainingResult_variableImportanceMetrics = Lens.lens (\TrainingResult' {variableImportanceMetrics} -> variableImportanceMetrics) (\s@TrainingResult' {} a -> s {variableImportanceMetrics = a} :: TrainingResult)

instance Core.FromJSON TrainingResult where
  parseJSON =
    Core.withObject
      "TrainingResult"
      ( \x ->
          TrainingResult'
            Prelude.<$> (x Core..:? "dataValidationMetrics")
            Prelude.<*> (x Core..:? "trainingMetrics")
            Prelude.<*> (x Core..:? "variableImportanceMetrics")
      )

instance Prelude.Hashable TrainingResult where
  hashWithSalt salt' TrainingResult' {..} =
    salt'
      `Prelude.hashWithSalt` variableImportanceMetrics
      `Prelude.hashWithSalt` trainingMetrics
      `Prelude.hashWithSalt` dataValidationMetrics

instance Prelude.NFData TrainingResult where
  rnf TrainingResult' {..} =
    Prelude.rnf dataValidationMetrics
      `Prelude.seq` Prelude.rnf variableImportanceMetrics
      `Prelude.seq` Prelude.rnf trainingMetrics
