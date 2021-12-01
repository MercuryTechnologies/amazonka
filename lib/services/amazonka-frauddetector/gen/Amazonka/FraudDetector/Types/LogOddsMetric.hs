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
-- Module      : Amazonka.FraudDetector.Types.LogOddsMetric
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.LogOddsMetric where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The log odds metric details.
--
-- /See:/ 'newLogOddsMetric' smart constructor.
data LogOddsMetric = LogOddsMetric'
  { -- | The name of the variable.
    variableName :: Prelude.Text,
    -- | The type of variable.
    variableType :: Prelude.Text,
    -- | The relative importance of the variable. For more information, see
    -- <https://docs.aws.amazon.com/frauddetector/latest/ug/model-variable-importance.html Model variable importance>.
    variableImportance :: Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogOddsMetric' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'variableName', 'logOddsMetric_variableName' - The name of the variable.
--
-- 'variableType', 'logOddsMetric_variableType' - The type of variable.
--
-- 'variableImportance', 'logOddsMetric_variableImportance' - The relative importance of the variable. For more information, see
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/model-variable-importance.html Model variable importance>.
newLogOddsMetric ::
  -- | 'variableName'
  Prelude.Text ->
  -- | 'variableType'
  Prelude.Text ->
  -- | 'variableImportance'
  Prelude.Double ->
  LogOddsMetric
newLogOddsMetric
  pVariableName_
  pVariableType_
  pVariableImportance_ =
    LogOddsMetric'
      { variableName = pVariableName_,
        variableType = pVariableType_,
        variableImportance = pVariableImportance_
      }

-- | The name of the variable.
logOddsMetric_variableName :: Lens.Lens' LogOddsMetric Prelude.Text
logOddsMetric_variableName = Lens.lens (\LogOddsMetric' {variableName} -> variableName) (\s@LogOddsMetric' {} a -> s {variableName = a} :: LogOddsMetric)

-- | The type of variable.
logOddsMetric_variableType :: Lens.Lens' LogOddsMetric Prelude.Text
logOddsMetric_variableType = Lens.lens (\LogOddsMetric' {variableType} -> variableType) (\s@LogOddsMetric' {} a -> s {variableType = a} :: LogOddsMetric)

-- | The relative importance of the variable. For more information, see
-- <https://docs.aws.amazon.com/frauddetector/latest/ug/model-variable-importance.html Model variable importance>.
logOddsMetric_variableImportance :: Lens.Lens' LogOddsMetric Prelude.Double
logOddsMetric_variableImportance = Lens.lens (\LogOddsMetric' {variableImportance} -> variableImportance) (\s@LogOddsMetric' {} a -> s {variableImportance = a} :: LogOddsMetric)

instance Core.FromJSON LogOddsMetric where
  parseJSON =
    Core.withObject
      "LogOddsMetric"
      ( \x ->
          LogOddsMetric'
            Prelude.<$> (x Core..: "variableName")
            Prelude.<*> (x Core..: "variableType")
            Prelude.<*> (x Core..: "variableImportance")
      )

instance Prelude.Hashable LogOddsMetric where
  hashWithSalt salt' LogOddsMetric' {..} =
    salt' `Prelude.hashWithSalt` variableImportance
      `Prelude.hashWithSalt` variableType
      `Prelude.hashWithSalt` variableName

instance Prelude.NFData LogOddsMetric where
  rnf LogOddsMetric' {..} =
    Prelude.rnf variableName
      `Prelude.seq` Prelude.rnf variableImportance
      `Prelude.seq` Prelude.rnf variableType
