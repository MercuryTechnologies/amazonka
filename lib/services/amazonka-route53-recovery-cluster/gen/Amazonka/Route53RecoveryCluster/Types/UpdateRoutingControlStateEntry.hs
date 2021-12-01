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
-- Module      : Amazonka.Route53RecoveryCluster.Types.UpdateRoutingControlStateEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryCluster.Types.UpdateRoutingControlStateEntry where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryCluster.Types.RoutingControlState

-- | A routing control state.
--
-- /See:/ 'newUpdateRoutingControlStateEntry' smart constructor.
data UpdateRoutingControlStateEntry = UpdateRoutingControlStateEntry'
  { -- | The Amazon Resource Number (ARN) for the routing control state entry.
    routingControlArn :: Prelude.Text,
    -- | The routing control state in a set of routing control state entries.
    routingControlState :: RoutingControlState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingControlStateEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'routingControlArn', 'updateRoutingControlStateEntry_routingControlArn' - The Amazon Resource Number (ARN) for the routing control state entry.
--
-- 'routingControlState', 'updateRoutingControlStateEntry_routingControlState' - The routing control state in a set of routing control state entries.
newUpdateRoutingControlStateEntry ::
  -- | 'routingControlArn'
  Prelude.Text ->
  -- | 'routingControlState'
  RoutingControlState ->
  UpdateRoutingControlStateEntry
newUpdateRoutingControlStateEntry
  pRoutingControlArn_
  pRoutingControlState_ =
    UpdateRoutingControlStateEntry'
      { routingControlArn =
          pRoutingControlArn_,
        routingControlState = pRoutingControlState_
      }

-- | The Amazon Resource Number (ARN) for the routing control state entry.
updateRoutingControlStateEntry_routingControlArn :: Lens.Lens' UpdateRoutingControlStateEntry Prelude.Text
updateRoutingControlStateEntry_routingControlArn = Lens.lens (\UpdateRoutingControlStateEntry' {routingControlArn} -> routingControlArn) (\s@UpdateRoutingControlStateEntry' {} a -> s {routingControlArn = a} :: UpdateRoutingControlStateEntry)

-- | The routing control state in a set of routing control state entries.
updateRoutingControlStateEntry_routingControlState :: Lens.Lens' UpdateRoutingControlStateEntry RoutingControlState
updateRoutingControlStateEntry_routingControlState = Lens.lens (\UpdateRoutingControlStateEntry' {routingControlState} -> routingControlState) (\s@UpdateRoutingControlStateEntry' {} a -> s {routingControlState = a} :: UpdateRoutingControlStateEntry)

instance
  Prelude.Hashable
    UpdateRoutingControlStateEntry
  where
  hashWithSalt
    salt'
    UpdateRoutingControlStateEntry' {..} =
      salt' `Prelude.hashWithSalt` routingControlState
        `Prelude.hashWithSalt` routingControlArn

instance
  Prelude.NFData
    UpdateRoutingControlStateEntry
  where
  rnf UpdateRoutingControlStateEntry' {..} =
    Prelude.rnf routingControlArn
      `Prelude.seq` Prelude.rnf routingControlState

instance Core.ToJSON UpdateRoutingControlStateEntry where
  toJSON UpdateRoutingControlStateEntry' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RoutingControlArn" Core..= routingControlArn),
            Prelude.Just
              ("RoutingControlState" Core..= routingControlState)
          ]
      )
