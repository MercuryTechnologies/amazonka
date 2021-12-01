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
-- Module      : Amazonka.Connect.Types.AnswerMachineDetectionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.AnswerMachineDetectionConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration of the answering machine detection.
--
-- /See:/ 'newAnswerMachineDetectionConfig' smart constructor.
data AnswerMachineDetectionConfig = AnswerMachineDetectionConfig'
  { -- | The flag to indicate if answer machine detection analysis needs to be
    -- performed for a voice call. If set to @true@, @TrafficType@ must be set
    -- as @CAMPAIGN@.
    enableAnswerMachineDetection :: Prelude.Maybe Prelude.Bool,
    -- | Wait for the answering machine prompt.
    awaitAnswerMachinePrompt :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnswerMachineDetectionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableAnswerMachineDetection', 'answerMachineDetectionConfig_enableAnswerMachineDetection' - The flag to indicate if answer machine detection analysis needs to be
-- performed for a voice call. If set to @true@, @TrafficType@ must be set
-- as @CAMPAIGN@.
--
-- 'awaitAnswerMachinePrompt', 'answerMachineDetectionConfig_awaitAnswerMachinePrompt' - Wait for the answering machine prompt.
newAnswerMachineDetectionConfig ::
  AnswerMachineDetectionConfig
newAnswerMachineDetectionConfig =
  AnswerMachineDetectionConfig'
    { enableAnswerMachineDetection =
        Prelude.Nothing,
      awaitAnswerMachinePrompt = Prelude.Nothing
    }

-- | The flag to indicate if answer machine detection analysis needs to be
-- performed for a voice call. If set to @true@, @TrafficType@ must be set
-- as @CAMPAIGN@.
answerMachineDetectionConfig_enableAnswerMachineDetection :: Lens.Lens' AnswerMachineDetectionConfig (Prelude.Maybe Prelude.Bool)
answerMachineDetectionConfig_enableAnswerMachineDetection = Lens.lens (\AnswerMachineDetectionConfig' {enableAnswerMachineDetection} -> enableAnswerMachineDetection) (\s@AnswerMachineDetectionConfig' {} a -> s {enableAnswerMachineDetection = a} :: AnswerMachineDetectionConfig)

-- | Wait for the answering machine prompt.
answerMachineDetectionConfig_awaitAnswerMachinePrompt :: Lens.Lens' AnswerMachineDetectionConfig (Prelude.Maybe Prelude.Bool)
answerMachineDetectionConfig_awaitAnswerMachinePrompt = Lens.lens (\AnswerMachineDetectionConfig' {awaitAnswerMachinePrompt} -> awaitAnswerMachinePrompt) (\s@AnswerMachineDetectionConfig' {} a -> s {awaitAnswerMachinePrompt = a} :: AnswerMachineDetectionConfig)

instance
  Prelude.Hashable
    AnswerMachineDetectionConfig
  where
  hashWithSalt salt' AnswerMachineDetectionConfig' {..} =
    salt'
      `Prelude.hashWithSalt` awaitAnswerMachinePrompt
      `Prelude.hashWithSalt` enableAnswerMachineDetection

instance Prelude.NFData AnswerMachineDetectionConfig where
  rnf AnswerMachineDetectionConfig' {..} =
    Prelude.rnf enableAnswerMachineDetection
      `Prelude.seq` Prelude.rnf awaitAnswerMachinePrompt

instance Core.ToJSON AnswerMachineDetectionConfig where
  toJSON AnswerMachineDetectionConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EnableAnswerMachineDetection" Core..=)
              Prelude.<$> enableAnswerMachineDetection,
            ("AwaitAnswerMachinePrompt" Core..=)
              Prelude.<$> awaitAnswerMachinePrompt
          ]
      )
