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
-- Module      : Amazonka.IoTEventsData.Types.Timer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.Timer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The current state of a timer.
--
-- /See:/ 'newTimer' smart constructor.
data Timer = Timer'
  { -- | The name of the timer.
    name :: Prelude.Text,
    -- | The number of seconds which have elapsed on the timer.
    timestamp :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Timer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'timer_name' - The name of the timer.
--
-- 'timestamp', 'timer_timestamp' - The number of seconds which have elapsed on the timer.
newTimer ::
  -- | 'name'
  Prelude.Text ->
  -- | 'timestamp'
  Prelude.UTCTime ->
  Timer
newTimer pName_ pTimestamp_ =
  Timer'
    { name = pName_,
      timestamp = Core._Time Lens.# pTimestamp_
    }

-- | The name of the timer.
timer_name :: Lens.Lens' Timer Prelude.Text
timer_name = Lens.lens (\Timer' {name} -> name) (\s@Timer' {} a -> s {name = a} :: Timer)

-- | The number of seconds which have elapsed on the timer.
timer_timestamp :: Lens.Lens' Timer Prelude.UTCTime
timer_timestamp = Lens.lens (\Timer' {timestamp} -> timestamp) (\s@Timer' {} a -> s {timestamp = a} :: Timer) Prelude.. Core._Time

instance Core.FromJSON Timer where
  parseJSON =
    Core.withObject
      "Timer"
      ( \x ->
          Timer'
            Prelude.<$> (x Core..: "name")
            Prelude.<*> (x Core..: "timestamp")
      )

instance Prelude.Hashable Timer where
  hashWithSalt salt' Timer' {..} =
    salt' `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` name

instance Prelude.NFData Timer where
  rnf Timer' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf timestamp
