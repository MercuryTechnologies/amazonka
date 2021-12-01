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
-- Module      : Amazonka.MGN.Types.LifeCycle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LifeCycle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MGN.Types.LifeCycleLastCutover
import Amazonka.MGN.Types.LifeCycleLastTest
import Amazonka.MGN.Types.LifeCycleState
import qualified Amazonka.Prelude as Prelude

-- | Lifecycle.
--
-- /See:/ 'newLifeCycle' smart constructor.
data LifeCycle = LifeCycle'
  { -- | Lifecycle last Test.
    lastTest :: Prelude.Maybe LifeCycleLastTest,
    -- | Lifecycle state.
    state :: Prelude.Maybe LifeCycleState,
    -- | Lifecycle elapsed time and duration.
    elapsedReplicationDuration :: Prelude.Maybe Prelude.Text,
    -- | Lifecycle last seen date and time.
    lastSeenByServiceDateTime :: Prelude.Maybe Prelude.Text,
    -- | Lifecycle added to service data and time.
    addedToServiceDateTime :: Prelude.Maybe Prelude.Text,
    -- | Lifecycle last Cutover.
    lastCutover :: Prelude.Maybe LifeCycleLastCutover,
    -- | Lifecycle replication initiation date and time.
    firstByteDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifeCycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastTest', 'lifeCycle_lastTest' - Lifecycle last Test.
--
-- 'state', 'lifeCycle_state' - Lifecycle state.
--
-- 'elapsedReplicationDuration', 'lifeCycle_elapsedReplicationDuration' - Lifecycle elapsed time and duration.
--
-- 'lastSeenByServiceDateTime', 'lifeCycle_lastSeenByServiceDateTime' - Lifecycle last seen date and time.
--
-- 'addedToServiceDateTime', 'lifeCycle_addedToServiceDateTime' - Lifecycle added to service data and time.
--
-- 'lastCutover', 'lifeCycle_lastCutover' - Lifecycle last Cutover.
--
-- 'firstByteDateTime', 'lifeCycle_firstByteDateTime' - Lifecycle replication initiation date and time.
newLifeCycle ::
  LifeCycle
newLifeCycle =
  LifeCycle'
    { lastTest = Prelude.Nothing,
      state = Prelude.Nothing,
      elapsedReplicationDuration = Prelude.Nothing,
      lastSeenByServiceDateTime = Prelude.Nothing,
      addedToServiceDateTime = Prelude.Nothing,
      lastCutover = Prelude.Nothing,
      firstByteDateTime = Prelude.Nothing
    }

-- | Lifecycle last Test.
lifeCycle_lastTest :: Lens.Lens' LifeCycle (Prelude.Maybe LifeCycleLastTest)
lifeCycle_lastTest = Lens.lens (\LifeCycle' {lastTest} -> lastTest) (\s@LifeCycle' {} a -> s {lastTest = a} :: LifeCycle)

-- | Lifecycle state.
lifeCycle_state :: Lens.Lens' LifeCycle (Prelude.Maybe LifeCycleState)
lifeCycle_state = Lens.lens (\LifeCycle' {state} -> state) (\s@LifeCycle' {} a -> s {state = a} :: LifeCycle)

-- | Lifecycle elapsed time and duration.
lifeCycle_elapsedReplicationDuration :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_elapsedReplicationDuration = Lens.lens (\LifeCycle' {elapsedReplicationDuration} -> elapsedReplicationDuration) (\s@LifeCycle' {} a -> s {elapsedReplicationDuration = a} :: LifeCycle)

-- | Lifecycle last seen date and time.
lifeCycle_lastSeenByServiceDateTime :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_lastSeenByServiceDateTime = Lens.lens (\LifeCycle' {lastSeenByServiceDateTime} -> lastSeenByServiceDateTime) (\s@LifeCycle' {} a -> s {lastSeenByServiceDateTime = a} :: LifeCycle)

-- | Lifecycle added to service data and time.
lifeCycle_addedToServiceDateTime :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_addedToServiceDateTime = Lens.lens (\LifeCycle' {addedToServiceDateTime} -> addedToServiceDateTime) (\s@LifeCycle' {} a -> s {addedToServiceDateTime = a} :: LifeCycle)

-- | Lifecycle last Cutover.
lifeCycle_lastCutover :: Lens.Lens' LifeCycle (Prelude.Maybe LifeCycleLastCutover)
lifeCycle_lastCutover = Lens.lens (\LifeCycle' {lastCutover} -> lastCutover) (\s@LifeCycle' {} a -> s {lastCutover = a} :: LifeCycle)

-- | Lifecycle replication initiation date and time.
lifeCycle_firstByteDateTime :: Lens.Lens' LifeCycle (Prelude.Maybe Prelude.Text)
lifeCycle_firstByteDateTime = Lens.lens (\LifeCycle' {firstByteDateTime} -> firstByteDateTime) (\s@LifeCycle' {} a -> s {firstByteDateTime = a} :: LifeCycle)

instance Core.FromJSON LifeCycle where
  parseJSON =
    Core.withObject
      "LifeCycle"
      ( \x ->
          LifeCycle'
            Prelude.<$> (x Core..:? "lastTest")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "elapsedReplicationDuration")
            Prelude.<*> (x Core..:? "lastSeenByServiceDateTime")
            Prelude.<*> (x Core..:? "addedToServiceDateTime")
            Prelude.<*> (x Core..:? "lastCutover")
            Prelude.<*> (x Core..:? "firstByteDateTime")
      )

instance Prelude.Hashable LifeCycle where
  hashWithSalt salt' LifeCycle' {..} =
    salt' `Prelude.hashWithSalt` firstByteDateTime
      `Prelude.hashWithSalt` lastCutover
      `Prelude.hashWithSalt` addedToServiceDateTime
      `Prelude.hashWithSalt` lastSeenByServiceDateTime
      `Prelude.hashWithSalt` elapsedReplicationDuration
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` lastTest

instance Prelude.NFData LifeCycle where
  rnf LifeCycle' {..} =
    Prelude.rnf lastTest
      `Prelude.seq` Prelude.rnf firstByteDateTime
      `Prelude.seq` Prelude.rnf lastCutover
      `Prelude.seq` Prelude.rnf addedToServiceDateTime
      `Prelude.seq` Prelude.rnf lastSeenByServiceDateTime
      `Prelude.seq` Prelude.rnf elapsedReplicationDuration
      `Prelude.seq` Prelude.rnf state
