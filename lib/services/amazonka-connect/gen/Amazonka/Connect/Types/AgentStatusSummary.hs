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
-- Module      : Amazonka.Connect.Types.AgentStatusSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.AgentStatusSummary where

import Amazonka.Connect.Types.AgentStatusType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary information for an agent status.
--
-- /See:/ 'newAgentStatusSummary' smart constructor.
data AgentStatusSummary = AgentStatusSummary'
  { -- | The Amazon Resource Name (ARN) for the agent status.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the agent status.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier for an agent status.
    id :: Prelude.Maybe Prelude.Text,
    -- | The type of the agent status.
    type' :: Prelude.Maybe AgentStatusType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AgentStatusSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'agentStatusSummary_arn' - The Amazon Resource Name (ARN) for the agent status.
--
-- 'name', 'agentStatusSummary_name' - The name of the agent status.
--
-- 'id', 'agentStatusSummary_id' - The identifier for an agent status.
--
-- 'type'', 'agentStatusSummary_type' - The type of the agent status.
newAgentStatusSummary ::
  AgentStatusSummary
newAgentStatusSummary =
  AgentStatusSummary'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) for the agent status.
agentStatusSummary_arn :: Lens.Lens' AgentStatusSummary (Prelude.Maybe Prelude.Text)
agentStatusSummary_arn = Lens.lens (\AgentStatusSummary' {arn} -> arn) (\s@AgentStatusSummary' {} a -> s {arn = a} :: AgentStatusSummary)

-- | The name of the agent status.
agentStatusSummary_name :: Lens.Lens' AgentStatusSummary (Prelude.Maybe Prelude.Text)
agentStatusSummary_name = Lens.lens (\AgentStatusSummary' {name} -> name) (\s@AgentStatusSummary' {} a -> s {name = a} :: AgentStatusSummary)

-- | The identifier for an agent status.
agentStatusSummary_id :: Lens.Lens' AgentStatusSummary (Prelude.Maybe Prelude.Text)
agentStatusSummary_id = Lens.lens (\AgentStatusSummary' {id} -> id) (\s@AgentStatusSummary' {} a -> s {id = a} :: AgentStatusSummary)

-- | The type of the agent status.
agentStatusSummary_type :: Lens.Lens' AgentStatusSummary (Prelude.Maybe AgentStatusType)
agentStatusSummary_type = Lens.lens (\AgentStatusSummary' {type'} -> type') (\s@AgentStatusSummary' {} a -> s {type' = a} :: AgentStatusSummary)

instance Core.FromJSON AgentStatusSummary where
  parseJSON =
    Core.withObject
      "AgentStatusSummary"
      ( \x ->
          AgentStatusSummary'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable AgentStatusSummary where
  hashWithSalt salt' AgentStatusSummary' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn

instance Prelude.NFData AgentStatusSummary where
  rnf AgentStatusSummary' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
