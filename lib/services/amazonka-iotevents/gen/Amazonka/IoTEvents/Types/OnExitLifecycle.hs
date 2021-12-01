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
-- Module      : Amazonka.IoTEvents.Types.OnExitLifecycle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.OnExitLifecycle where

import qualified Amazonka.Core as Core
import Amazonka.IoTEvents.Types.Event
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | When exiting this state, perform these @actions@ if the specified
-- @condition@ is @TRUE@.
--
-- /See:/ 'newOnExitLifecycle' smart constructor.
data OnExitLifecycle = OnExitLifecycle'
  { -- | Specifies the @actions@ that are performed when the state is exited and
    -- the @condition@ is @TRUE@.
    events :: Prelude.Maybe [Event]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnExitLifecycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'events', 'onExitLifecycle_events' - Specifies the @actions@ that are performed when the state is exited and
-- the @condition@ is @TRUE@.
newOnExitLifecycle ::
  OnExitLifecycle
newOnExitLifecycle =
  OnExitLifecycle' {events = Prelude.Nothing}

-- | Specifies the @actions@ that are performed when the state is exited and
-- the @condition@ is @TRUE@.
onExitLifecycle_events :: Lens.Lens' OnExitLifecycle (Prelude.Maybe [Event])
onExitLifecycle_events = Lens.lens (\OnExitLifecycle' {events} -> events) (\s@OnExitLifecycle' {} a -> s {events = a} :: OnExitLifecycle) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON OnExitLifecycle where
  parseJSON =
    Core.withObject
      "OnExitLifecycle"
      ( \x ->
          OnExitLifecycle'
            Prelude.<$> (x Core..:? "events" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable OnExitLifecycle where
  hashWithSalt salt' OnExitLifecycle' {..} =
    salt' `Prelude.hashWithSalt` events

instance Prelude.NFData OnExitLifecycle where
  rnf OnExitLifecycle' {..} = Prelude.rnf events

instance Core.ToJSON OnExitLifecycle where
  toJSON OnExitLifecycle' {..} =
    Core.object
      ( Prelude.catMaybes
          [("events" Core..=) Prelude.<$> events]
      )
