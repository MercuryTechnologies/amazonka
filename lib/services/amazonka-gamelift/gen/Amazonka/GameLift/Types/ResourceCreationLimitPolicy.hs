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
-- Module      : Amazonka.GameLift.Types.ResourceCreationLimitPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.ResourceCreationLimitPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A policy that puts limits on the number of game sessions that a player
-- can create within a specified span of time. With this policy, you can
-- control players\' ability to consume available resources.
--
-- The policy is evaluated when a player tries to create a new game
-- session. On receiving a @CreateGameSession@ request, GameLift checks
-- that the player (identified by @CreatorId@) has created fewer than game
-- session limit in the specified time period.
--
-- The resource creation limit policy is included in FleetAttributes.
--
-- /See:/ 'newResourceCreationLimitPolicy' smart constructor.
data ResourceCreationLimitPolicy = ResourceCreationLimitPolicy'
  { -- | The maximum number of game sessions that an individual can create during
    -- the policy period.
    newGameSessionsPerCreator' :: Prelude.Maybe Prelude.Natural,
    -- | The time span used in evaluating the resource creation limit policy.
    policyPeriodInMinutes :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceCreationLimitPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newGameSessionsPerCreator'', 'resourceCreationLimitPolicy_newGameSessionsPerCreator' - The maximum number of game sessions that an individual can create during
-- the policy period.
--
-- 'policyPeriodInMinutes', 'resourceCreationLimitPolicy_policyPeriodInMinutes' - The time span used in evaluating the resource creation limit policy.
newResourceCreationLimitPolicy ::
  ResourceCreationLimitPolicy
newResourceCreationLimitPolicy =
  ResourceCreationLimitPolicy'
    { newGameSessionsPerCreator' =
        Prelude.Nothing,
      policyPeriodInMinutes = Prelude.Nothing
    }

-- | The maximum number of game sessions that an individual can create during
-- the policy period.
resourceCreationLimitPolicy_newGameSessionsPerCreator :: Lens.Lens' ResourceCreationLimitPolicy (Prelude.Maybe Prelude.Natural)
resourceCreationLimitPolicy_newGameSessionsPerCreator = Lens.lens (\ResourceCreationLimitPolicy' {newGameSessionsPerCreator'} -> newGameSessionsPerCreator') (\s@ResourceCreationLimitPolicy' {} a -> s {newGameSessionsPerCreator' = a} :: ResourceCreationLimitPolicy)

-- | The time span used in evaluating the resource creation limit policy.
resourceCreationLimitPolicy_policyPeriodInMinutes :: Lens.Lens' ResourceCreationLimitPolicy (Prelude.Maybe Prelude.Natural)
resourceCreationLimitPolicy_policyPeriodInMinutes = Lens.lens (\ResourceCreationLimitPolicy' {policyPeriodInMinutes} -> policyPeriodInMinutes) (\s@ResourceCreationLimitPolicy' {} a -> s {policyPeriodInMinutes = a} :: ResourceCreationLimitPolicy)

instance Core.FromJSON ResourceCreationLimitPolicy where
  parseJSON =
    Core.withObject
      "ResourceCreationLimitPolicy"
      ( \x ->
          ResourceCreationLimitPolicy'
            Prelude.<$> (x Core..:? "NewGameSessionsPerCreator")
            Prelude.<*> (x Core..:? "PolicyPeriodInMinutes")
      )

instance Prelude.Hashable ResourceCreationLimitPolicy where
  hashWithSalt salt' ResourceCreationLimitPolicy' {..} =
    salt' `Prelude.hashWithSalt` policyPeriodInMinutes
      `Prelude.hashWithSalt` newGameSessionsPerCreator'

instance Prelude.NFData ResourceCreationLimitPolicy where
  rnf ResourceCreationLimitPolicy' {..} =
    Prelude.rnf newGameSessionsPerCreator'
      `Prelude.seq` Prelude.rnf policyPeriodInMinutes

instance Core.ToJSON ResourceCreationLimitPolicy where
  toJSON ResourceCreationLimitPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NewGameSessionsPerCreator" Core..=)
              Prelude.<$> newGameSessionsPerCreator',
            ("PolicyPeriodInMinutes" Core..=)
              Prelude.<$> policyPeriodInMinutes
          ]
      )
