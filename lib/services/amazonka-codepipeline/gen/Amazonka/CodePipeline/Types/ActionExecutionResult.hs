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
-- Module      : Amazonka.CodePipeline.Types.ActionExecutionResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionExecutionResult where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Execution result information, such as the external execution ID.
--
-- /See:/ 'newActionExecutionResult' smart constructor.
data ActionExecutionResult = ActionExecutionResult'
  { -- | The deepest external link to the external resource (for example, a
    -- repository URL or deployment endpoint) that is used when running the
    -- action.
    externalExecutionUrl :: Prelude.Maybe Prelude.Text,
    -- | The action provider\'s external ID for the action execution.
    externalExecutionId :: Prelude.Maybe Prelude.Text,
    -- | The action provider\'s summary for the action execution.
    externalExecutionSummary :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionExecutionResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'externalExecutionUrl', 'actionExecutionResult_externalExecutionUrl' - The deepest external link to the external resource (for example, a
-- repository URL or deployment endpoint) that is used when running the
-- action.
--
-- 'externalExecutionId', 'actionExecutionResult_externalExecutionId' - The action provider\'s external ID for the action execution.
--
-- 'externalExecutionSummary', 'actionExecutionResult_externalExecutionSummary' - The action provider\'s summary for the action execution.
newActionExecutionResult ::
  ActionExecutionResult
newActionExecutionResult =
  ActionExecutionResult'
    { externalExecutionUrl =
        Prelude.Nothing,
      externalExecutionId = Prelude.Nothing,
      externalExecutionSummary = Prelude.Nothing
    }

-- | The deepest external link to the external resource (for example, a
-- repository URL or deployment endpoint) that is used when running the
-- action.
actionExecutionResult_externalExecutionUrl :: Lens.Lens' ActionExecutionResult (Prelude.Maybe Prelude.Text)
actionExecutionResult_externalExecutionUrl = Lens.lens (\ActionExecutionResult' {externalExecutionUrl} -> externalExecutionUrl) (\s@ActionExecutionResult' {} a -> s {externalExecutionUrl = a} :: ActionExecutionResult)

-- | The action provider\'s external ID for the action execution.
actionExecutionResult_externalExecutionId :: Lens.Lens' ActionExecutionResult (Prelude.Maybe Prelude.Text)
actionExecutionResult_externalExecutionId = Lens.lens (\ActionExecutionResult' {externalExecutionId} -> externalExecutionId) (\s@ActionExecutionResult' {} a -> s {externalExecutionId = a} :: ActionExecutionResult)

-- | The action provider\'s summary for the action execution.
actionExecutionResult_externalExecutionSummary :: Lens.Lens' ActionExecutionResult (Prelude.Maybe Prelude.Text)
actionExecutionResult_externalExecutionSummary = Lens.lens (\ActionExecutionResult' {externalExecutionSummary} -> externalExecutionSummary) (\s@ActionExecutionResult' {} a -> s {externalExecutionSummary = a} :: ActionExecutionResult)

instance Core.FromJSON ActionExecutionResult where
  parseJSON =
    Core.withObject
      "ActionExecutionResult"
      ( \x ->
          ActionExecutionResult'
            Prelude.<$> (x Core..:? "externalExecutionUrl")
            Prelude.<*> (x Core..:? "externalExecutionId")
            Prelude.<*> (x Core..:? "externalExecutionSummary")
      )

instance Prelude.Hashable ActionExecutionResult where
  hashWithSalt salt' ActionExecutionResult' {..} =
    salt'
      `Prelude.hashWithSalt` externalExecutionSummary
      `Prelude.hashWithSalt` externalExecutionId
      `Prelude.hashWithSalt` externalExecutionUrl

instance Prelude.NFData ActionExecutionResult where
  rnf ActionExecutionResult' {..} =
    Prelude.rnf externalExecutionUrl
      `Prelude.seq` Prelude.rnf externalExecutionSummary
      `Prelude.seq` Prelude.rnf externalExecutionId
