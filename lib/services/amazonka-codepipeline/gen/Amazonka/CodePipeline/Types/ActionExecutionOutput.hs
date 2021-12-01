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
-- Module      : Amazonka.CodePipeline.Types.ActionExecutionOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionExecutionOutput where

import Amazonka.CodePipeline.Types.ActionExecutionResult
import Amazonka.CodePipeline.Types.ArtifactDetail
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Output details listed for an action execution, such as the action
-- execution result.
--
-- /See:/ 'newActionExecutionOutput' smart constructor.
data ActionExecutionOutput = ActionExecutionOutput'
  { -- | The outputVariables field shows the key-value pairs that were output as
    -- part of that execution.
    outputVariables :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Details of output artifacts of the action that correspond to the action
    -- execution.
    outputArtifacts :: Prelude.Maybe [ArtifactDetail],
    -- | Execution result information listed in the output details for an action
    -- execution.
    executionResult :: Prelude.Maybe ActionExecutionResult
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionExecutionOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'outputVariables', 'actionExecutionOutput_outputVariables' - The outputVariables field shows the key-value pairs that were output as
-- part of that execution.
--
-- 'outputArtifacts', 'actionExecutionOutput_outputArtifacts' - Details of output artifacts of the action that correspond to the action
-- execution.
--
-- 'executionResult', 'actionExecutionOutput_executionResult' - Execution result information listed in the output details for an action
-- execution.
newActionExecutionOutput ::
  ActionExecutionOutput
newActionExecutionOutput =
  ActionExecutionOutput'
    { outputVariables =
        Prelude.Nothing,
      outputArtifacts = Prelude.Nothing,
      executionResult = Prelude.Nothing
    }

-- | The outputVariables field shows the key-value pairs that were output as
-- part of that execution.
actionExecutionOutput_outputVariables :: Lens.Lens' ActionExecutionOutput (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
actionExecutionOutput_outputVariables = Lens.lens (\ActionExecutionOutput' {outputVariables} -> outputVariables) (\s@ActionExecutionOutput' {} a -> s {outputVariables = a} :: ActionExecutionOutput) Prelude.. Lens.mapping Lens.coerced

-- | Details of output artifacts of the action that correspond to the action
-- execution.
actionExecutionOutput_outputArtifacts :: Lens.Lens' ActionExecutionOutput (Prelude.Maybe [ArtifactDetail])
actionExecutionOutput_outputArtifacts = Lens.lens (\ActionExecutionOutput' {outputArtifacts} -> outputArtifacts) (\s@ActionExecutionOutput' {} a -> s {outputArtifacts = a} :: ActionExecutionOutput) Prelude.. Lens.mapping Lens.coerced

-- | Execution result information listed in the output details for an action
-- execution.
actionExecutionOutput_executionResult :: Lens.Lens' ActionExecutionOutput (Prelude.Maybe ActionExecutionResult)
actionExecutionOutput_executionResult = Lens.lens (\ActionExecutionOutput' {executionResult} -> executionResult) (\s@ActionExecutionOutput' {} a -> s {executionResult = a} :: ActionExecutionOutput)

instance Core.FromJSON ActionExecutionOutput where
  parseJSON =
    Core.withObject
      "ActionExecutionOutput"
      ( \x ->
          ActionExecutionOutput'
            Prelude.<$> ( x Core..:? "outputVariables"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "outputArtifacts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "executionResult")
      )

instance Prelude.Hashable ActionExecutionOutput where
  hashWithSalt salt' ActionExecutionOutput' {..} =
    salt' `Prelude.hashWithSalt` executionResult
      `Prelude.hashWithSalt` outputArtifacts
      `Prelude.hashWithSalt` outputVariables

instance Prelude.NFData ActionExecutionOutput where
  rnf ActionExecutionOutput' {..} =
    Prelude.rnf outputVariables
      `Prelude.seq` Prelude.rnf executionResult
      `Prelude.seq` Prelude.rnf outputArtifacts
