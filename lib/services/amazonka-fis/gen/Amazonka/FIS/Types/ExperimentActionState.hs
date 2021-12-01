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
-- Module      : Amazonka.FIS.Types.ExperimentActionState
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentActionState where

import qualified Amazonka.Core as Core
import Amazonka.FIS.Types.ExperimentActionStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of an action.
--
-- /See:/ 'newExperimentActionState' smart constructor.
data ExperimentActionState = ExperimentActionState'
  { -- | The state of the action.
    status :: Prelude.Maybe ExperimentActionStatus,
    -- | The reason for the state.
    reason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentActionState' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'experimentActionState_status' - The state of the action.
--
-- 'reason', 'experimentActionState_reason' - The reason for the state.
newExperimentActionState ::
  ExperimentActionState
newExperimentActionState =
  ExperimentActionState'
    { status = Prelude.Nothing,
      reason = Prelude.Nothing
    }

-- | The state of the action.
experimentActionState_status :: Lens.Lens' ExperimentActionState (Prelude.Maybe ExperimentActionStatus)
experimentActionState_status = Lens.lens (\ExperimentActionState' {status} -> status) (\s@ExperimentActionState' {} a -> s {status = a} :: ExperimentActionState)

-- | The reason for the state.
experimentActionState_reason :: Lens.Lens' ExperimentActionState (Prelude.Maybe Prelude.Text)
experimentActionState_reason = Lens.lens (\ExperimentActionState' {reason} -> reason) (\s@ExperimentActionState' {} a -> s {reason = a} :: ExperimentActionState)

instance Core.FromJSON ExperimentActionState where
  parseJSON =
    Core.withObject
      "ExperimentActionState"
      ( \x ->
          ExperimentActionState'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "reason")
      )

instance Prelude.Hashable ExperimentActionState where
  hashWithSalt salt' ExperimentActionState' {..} =
    salt' `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` status

instance Prelude.NFData ExperimentActionState where
  rnf ExperimentActionState' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf reason
