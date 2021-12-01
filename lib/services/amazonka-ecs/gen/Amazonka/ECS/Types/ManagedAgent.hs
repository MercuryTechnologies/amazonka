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
-- Module      : Amazonka.ECS.Types.ManagedAgent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.ManagedAgent where

import qualified Amazonka.Core as Core
import Amazonka.ECS.Types.ManagedAgentName
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about the managed agent status for the container.
--
-- /See:/ 'newManagedAgent' smart constructor.
data ManagedAgent = ManagedAgent'
  { -- | The last known status of the managed agent.
    lastStatus :: Prelude.Maybe Prelude.Text,
    -- | The reason for why the managed agent is in the state it is in.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The name of the managed agent. When the execute command feature is
    -- enabled, the managed agent name is @ExecuteCommandAgent@.
    name :: Prelude.Maybe ManagedAgentName,
    -- | The Unix timestamp for when the managed agent was last started.
    lastStartedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedAgent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastStatus', 'managedAgent_lastStatus' - The last known status of the managed agent.
--
-- 'reason', 'managedAgent_reason' - The reason for why the managed agent is in the state it is in.
--
-- 'name', 'managedAgent_name' - The name of the managed agent. When the execute command feature is
-- enabled, the managed agent name is @ExecuteCommandAgent@.
--
-- 'lastStartedAt', 'managedAgent_lastStartedAt' - The Unix timestamp for when the managed agent was last started.
newManagedAgent ::
  ManagedAgent
newManagedAgent =
  ManagedAgent'
    { lastStatus = Prelude.Nothing,
      reason = Prelude.Nothing,
      name = Prelude.Nothing,
      lastStartedAt = Prelude.Nothing
    }

-- | The last known status of the managed agent.
managedAgent_lastStatus :: Lens.Lens' ManagedAgent (Prelude.Maybe Prelude.Text)
managedAgent_lastStatus = Lens.lens (\ManagedAgent' {lastStatus} -> lastStatus) (\s@ManagedAgent' {} a -> s {lastStatus = a} :: ManagedAgent)

-- | The reason for why the managed agent is in the state it is in.
managedAgent_reason :: Lens.Lens' ManagedAgent (Prelude.Maybe Prelude.Text)
managedAgent_reason = Lens.lens (\ManagedAgent' {reason} -> reason) (\s@ManagedAgent' {} a -> s {reason = a} :: ManagedAgent)

-- | The name of the managed agent. When the execute command feature is
-- enabled, the managed agent name is @ExecuteCommandAgent@.
managedAgent_name :: Lens.Lens' ManagedAgent (Prelude.Maybe ManagedAgentName)
managedAgent_name = Lens.lens (\ManagedAgent' {name} -> name) (\s@ManagedAgent' {} a -> s {name = a} :: ManagedAgent)

-- | The Unix timestamp for when the managed agent was last started.
managedAgent_lastStartedAt :: Lens.Lens' ManagedAgent (Prelude.Maybe Prelude.UTCTime)
managedAgent_lastStartedAt = Lens.lens (\ManagedAgent' {lastStartedAt} -> lastStartedAt) (\s@ManagedAgent' {} a -> s {lastStartedAt = a} :: ManagedAgent) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ManagedAgent where
  parseJSON =
    Core.withObject
      "ManagedAgent"
      ( \x ->
          ManagedAgent'
            Prelude.<$> (x Core..:? "lastStatus")
            Prelude.<*> (x Core..:? "reason")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "lastStartedAt")
      )

instance Prelude.Hashable ManagedAgent where
  hashWithSalt salt' ManagedAgent' {..} =
    salt' `Prelude.hashWithSalt` lastStartedAt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` lastStatus

instance Prelude.NFData ManagedAgent where
  rnf ManagedAgent' {..} =
    Prelude.rnf lastStatus
      `Prelude.seq` Prelude.rnf lastStartedAt
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf reason
