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
-- Module      : Amazonka.Config.Types.RemediationExecutionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.RemediationExecutionStatus where

import Amazonka.Config.Types.RemediationExecutionState
import Amazonka.Config.Types.RemediationExecutionStep
import Amazonka.Config.Types.ResourceKey
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides details of the current status of the invoked remediation action
-- for that resource.
--
-- /See:/ 'newRemediationExecutionStatus' smart constructor.
data RemediationExecutionStatus = RemediationExecutionStatus'
  { -- | ENUM of the values.
    state :: Prelude.Maybe RemediationExecutionState,
    -- | The time when the remediation execution was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    resourceKey :: Prelude.Maybe ResourceKey,
    -- | Details of every step.
    stepDetails :: Prelude.Maybe [RemediationExecutionStep],
    -- | Start time when the remediation was executed.
    invocationTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemediationExecutionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'remediationExecutionStatus_state' - ENUM of the values.
--
-- 'lastUpdatedTime', 'remediationExecutionStatus_lastUpdatedTime' - The time when the remediation execution was last updated.
--
-- 'resourceKey', 'remediationExecutionStatus_resourceKey' - Undocumented member.
--
-- 'stepDetails', 'remediationExecutionStatus_stepDetails' - Details of every step.
--
-- 'invocationTime', 'remediationExecutionStatus_invocationTime' - Start time when the remediation was executed.
newRemediationExecutionStatus ::
  RemediationExecutionStatus
newRemediationExecutionStatus =
  RemediationExecutionStatus'
    { state =
        Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      resourceKey = Prelude.Nothing,
      stepDetails = Prelude.Nothing,
      invocationTime = Prelude.Nothing
    }

-- | ENUM of the values.
remediationExecutionStatus_state :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe RemediationExecutionState)
remediationExecutionStatus_state = Lens.lens (\RemediationExecutionStatus' {state} -> state) (\s@RemediationExecutionStatus' {} a -> s {state = a} :: RemediationExecutionStatus)

-- | The time when the remediation execution was last updated.
remediationExecutionStatus_lastUpdatedTime :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe Prelude.UTCTime)
remediationExecutionStatus_lastUpdatedTime = Lens.lens (\RemediationExecutionStatus' {lastUpdatedTime} -> lastUpdatedTime) (\s@RemediationExecutionStatus' {} a -> s {lastUpdatedTime = a} :: RemediationExecutionStatus) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
remediationExecutionStatus_resourceKey :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe ResourceKey)
remediationExecutionStatus_resourceKey = Lens.lens (\RemediationExecutionStatus' {resourceKey} -> resourceKey) (\s@RemediationExecutionStatus' {} a -> s {resourceKey = a} :: RemediationExecutionStatus)

-- | Details of every step.
remediationExecutionStatus_stepDetails :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe [RemediationExecutionStep])
remediationExecutionStatus_stepDetails = Lens.lens (\RemediationExecutionStatus' {stepDetails} -> stepDetails) (\s@RemediationExecutionStatus' {} a -> s {stepDetails = a} :: RemediationExecutionStatus) Prelude.. Lens.mapping Lens.coerced

-- | Start time when the remediation was executed.
remediationExecutionStatus_invocationTime :: Lens.Lens' RemediationExecutionStatus (Prelude.Maybe Prelude.UTCTime)
remediationExecutionStatus_invocationTime = Lens.lens (\RemediationExecutionStatus' {invocationTime} -> invocationTime) (\s@RemediationExecutionStatus' {} a -> s {invocationTime = a} :: RemediationExecutionStatus) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON RemediationExecutionStatus where
  parseJSON =
    Core.withObject
      "RemediationExecutionStatus"
      ( \x ->
          RemediationExecutionStatus'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "ResourceKey")
            Prelude.<*> (x Core..:? "StepDetails" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "InvocationTime")
      )

instance Prelude.Hashable RemediationExecutionStatus where
  hashWithSalt salt' RemediationExecutionStatus' {..} =
    salt' `Prelude.hashWithSalt` invocationTime
      `Prelude.hashWithSalt` stepDetails
      `Prelude.hashWithSalt` resourceKey
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` state

instance Prelude.NFData RemediationExecutionStatus where
  rnf RemediationExecutionStatus' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf invocationTime
      `Prelude.seq` Prelude.rnf stepDetails
      `Prelude.seq` Prelude.rnf resourceKey
      `Prelude.seq` Prelude.rnf lastUpdatedTime
