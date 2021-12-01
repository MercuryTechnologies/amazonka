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
-- Module      : Amazonka.Pinpoint.Types.QuietTime
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.QuietTime where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the start and end times that define a time range when messages
-- aren\'t sent to endpoints.
--
-- /See:/ 'newQuietTime' smart constructor.
data QuietTime = QuietTime'
  { -- | The specific time when quiet time begins. This value has to use 24-hour
    -- notation and be in HH:MM format, where HH is the hour (with a leading
    -- zero, if applicable) and MM is the minutes. For example, use 02:30 to
    -- represent 2:30 AM, or 14:30 to represent 2:30 PM.
    start :: Prelude.Maybe Prelude.Text,
    -- | The specific time when quiet time ends. This value has to use 24-hour
    -- notation and be in HH:MM format, where HH is the hour (with a leading
    -- zero, if applicable) and MM is the minutes. For example, use 02:30 to
    -- represent 2:30 AM, or 14:30 to represent 2:30 PM.
    end :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QuietTime' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'start', 'quietTime_start' - The specific time when quiet time begins. This value has to use 24-hour
-- notation and be in HH:MM format, where HH is the hour (with a leading
-- zero, if applicable) and MM is the minutes. For example, use 02:30 to
-- represent 2:30 AM, or 14:30 to represent 2:30 PM.
--
-- 'end', 'quietTime_end' - The specific time when quiet time ends. This value has to use 24-hour
-- notation and be in HH:MM format, where HH is the hour (with a leading
-- zero, if applicable) and MM is the minutes. For example, use 02:30 to
-- represent 2:30 AM, or 14:30 to represent 2:30 PM.
newQuietTime ::
  QuietTime
newQuietTime =
  QuietTime'
    { start = Prelude.Nothing,
      end = Prelude.Nothing
    }

-- | The specific time when quiet time begins. This value has to use 24-hour
-- notation and be in HH:MM format, where HH is the hour (with a leading
-- zero, if applicable) and MM is the minutes. For example, use 02:30 to
-- represent 2:30 AM, or 14:30 to represent 2:30 PM.
quietTime_start :: Lens.Lens' QuietTime (Prelude.Maybe Prelude.Text)
quietTime_start = Lens.lens (\QuietTime' {start} -> start) (\s@QuietTime' {} a -> s {start = a} :: QuietTime)

-- | The specific time when quiet time ends. This value has to use 24-hour
-- notation and be in HH:MM format, where HH is the hour (with a leading
-- zero, if applicable) and MM is the minutes. For example, use 02:30 to
-- represent 2:30 AM, or 14:30 to represent 2:30 PM.
quietTime_end :: Lens.Lens' QuietTime (Prelude.Maybe Prelude.Text)
quietTime_end = Lens.lens (\QuietTime' {end} -> end) (\s@QuietTime' {} a -> s {end = a} :: QuietTime)

instance Core.FromJSON QuietTime where
  parseJSON =
    Core.withObject
      "QuietTime"
      ( \x ->
          QuietTime'
            Prelude.<$> (x Core..:? "Start") Prelude.<*> (x Core..:? "End")
      )

instance Prelude.Hashable QuietTime where
  hashWithSalt salt' QuietTime' {..} =
    salt' `Prelude.hashWithSalt` end
      `Prelude.hashWithSalt` start

instance Prelude.NFData QuietTime where
  rnf QuietTime' {..} =
    Prelude.rnf start `Prelude.seq` Prelude.rnf end

instance Core.ToJSON QuietTime where
  toJSON QuietTime' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Start" Core..=) Prelude.<$> start,
            ("End" Core..=) Prelude.<$> end
          ]
      )
