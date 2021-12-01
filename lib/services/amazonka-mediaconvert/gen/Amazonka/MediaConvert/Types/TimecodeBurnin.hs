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
-- Module      : Amazonka.MediaConvert.Types.TimecodeBurnin
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.TimecodeBurnin where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.TimecodeBurninPosition
import qualified Amazonka.Prelude as Prelude

-- | Settings for burning the output timecode and specified prefix into the
-- output.
--
-- /See:/ 'newTimecodeBurnin' smart constructor.
data TimecodeBurnin = TimecodeBurnin'
  { -- | Use Prefix (Prefix) to place ASCII characters before any burned-in
    -- timecode. For example, a prefix of \"EZ-\" will result in the timecode
    -- \"EZ-00:00:00:00\". Provide either the characters themselves or the
    -- ASCII code equivalents. The supported range of characters is 0x20
    -- through 0x7e. This includes letters, numbers, and all special characters
    -- represented on a standard English keyboard.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | Use Font Size (FontSize) to set the font size of any burned-in timecode.
    -- Valid values are 10, 16, 32, 48.
    fontSize :: Prelude.Maybe Prelude.Natural,
    -- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to
    -- specify the location the burned-in timecode on output video.
    position :: Prelude.Maybe TimecodeBurninPosition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimecodeBurnin' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 'timecodeBurnin_prefix' - Use Prefix (Prefix) to place ASCII characters before any burned-in
-- timecode. For example, a prefix of \"EZ-\" will result in the timecode
-- \"EZ-00:00:00:00\". Provide either the characters themselves or the
-- ASCII code equivalents. The supported range of characters is 0x20
-- through 0x7e. This includes letters, numbers, and all special characters
-- represented on a standard English keyboard.
--
-- 'fontSize', 'timecodeBurnin_fontSize' - Use Font Size (FontSize) to set the font size of any burned-in timecode.
-- Valid values are 10, 16, 32, 48.
--
-- 'position', 'timecodeBurnin_position' - Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to
-- specify the location the burned-in timecode on output video.
newTimecodeBurnin ::
  TimecodeBurnin
newTimecodeBurnin =
  TimecodeBurnin'
    { prefix = Prelude.Nothing,
      fontSize = Prelude.Nothing,
      position = Prelude.Nothing
    }

-- | Use Prefix (Prefix) to place ASCII characters before any burned-in
-- timecode. For example, a prefix of \"EZ-\" will result in the timecode
-- \"EZ-00:00:00:00\". Provide either the characters themselves or the
-- ASCII code equivalents. The supported range of characters is 0x20
-- through 0x7e. This includes letters, numbers, and all special characters
-- represented on a standard English keyboard.
timecodeBurnin_prefix :: Lens.Lens' TimecodeBurnin (Prelude.Maybe Prelude.Text)
timecodeBurnin_prefix = Lens.lens (\TimecodeBurnin' {prefix} -> prefix) (\s@TimecodeBurnin' {} a -> s {prefix = a} :: TimecodeBurnin)

-- | Use Font Size (FontSize) to set the font size of any burned-in timecode.
-- Valid values are 10, 16, 32, 48.
timecodeBurnin_fontSize :: Lens.Lens' TimecodeBurnin (Prelude.Maybe Prelude.Natural)
timecodeBurnin_fontSize = Lens.lens (\TimecodeBurnin' {fontSize} -> fontSize) (\s@TimecodeBurnin' {} a -> s {fontSize = a} :: TimecodeBurnin)

-- | Use Position (Position) under under Timecode burn-in (TimecodeBurnIn) to
-- specify the location the burned-in timecode on output video.
timecodeBurnin_position :: Lens.Lens' TimecodeBurnin (Prelude.Maybe TimecodeBurninPosition)
timecodeBurnin_position = Lens.lens (\TimecodeBurnin' {position} -> position) (\s@TimecodeBurnin' {} a -> s {position = a} :: TimecodeBurnin)

instance Core.FromJSON TimecodeBurnin where
  parseJSON =
    Core.withObject
      "TimecodeBurnin"
      ( \x ->
          TimecodeBurnin'
            Prelude.<$> (x Core..:? "prefix")
            Prelude.<*> (x Core..:? "fontSize")
            Prelude.<*> (x Core..:? "position")
      )

instance Prelude.Hashable TimecodeBurnin where
  hashWithSalt salt' TimecodeBurnin' {..} =
    salt' `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` fontSize
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData TimecodeBurnin where
  rnf TimecodeBurnin' {..} =
    Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf fontSize

instance Core.ToJSON TimecodeBurnin where
  toJSON TimecodeBurnin' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("prefix" Core..=) Prelude.<$> prefix,
            ("fontSize" Core..=) Prelude.<$> fontSize,
            ("position" Core..=) Prelude.<$> position
          ]
      )
