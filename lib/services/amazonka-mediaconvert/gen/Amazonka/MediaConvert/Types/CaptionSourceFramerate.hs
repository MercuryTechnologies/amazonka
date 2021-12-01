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
-- Module      : Amazonka.MediaConvert.Types.CaptionSourceFramerate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.CaptionSourceFramerate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Ignore this setting unless your input captions format is SCC. To have
-- the service compensate for differing frame rates between your input
-- captions and input video, specify the frame rate of the captions file.
-- Specify this value as a fraction. When you work directly in your JSON
-- job specification, use the settings framerateNumerator and
-- framerateDenominator. For example, you might specify 24 \/ 1 for 24 fps,
-- 25 \/ 1 for 25 fps, 24000 \/ 1001 for 23.976 fps, or 30000 \/ 1001 for
-- 29.97 fps.
--
-- /See:/ 'newCaptionSourceFramerate' smart constructor.
data CaptionSourceFramerate = CaptionSourceFramerate'
  { -- | Specify the denominator of the fraction that represents the frame rate
    -- for the setting Caption source frame rate (CaptionSourceFramerate). Use
    -- this setting along with the setting Framerate numerator
    -- (framerateNumerator).
    framerateDenominator :: Prelude.Maybe Prelude.Natural,
    -- | Specify the numerator of the fraction that represents the frame rate for
    -- the setting Caption source frame rate (CaptionSourceFramerate). Use this
    -- setting along with the setting Framerate denominator
    -- (framerateDenominator).
    framerateNumerator :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionSourceFramerate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'framerateDenominator', 'captionSourceFramerate_framerateDenominator' - Specify the denominator of the fraction that represents the frame rate
-- for the setting Caption source frame rate (CaptionSourceFramerate). Use
-- this setting along with the setting Framerate numerator
-- (framerateNumerator).
--
-- 'framerateNumerator', 'captionSourceFramerate_framerateNumerator' - Specify the numerator of the fraction that represents the frame rate for
-- the setting Caption source frame rate (CaptionSourceFramerate). Use this
-- setting along with the setting Framerate denominator
-- (framerateDenominator).
newCaptionSourceFramerate ::
  CaptionSourceFramerate
newCaptionSourceFramerate =
  CaptionSourceFramerate'
    { framerateDenominator =
        Prelude.Nothing,
      framerateNumerator = Prelude.Nothing
    }

-- | Specify the denominator of the fraction that represents the frame rate
-- for the setting Caption source frame rate (CaptionSourceFramerate). Use
-- this setting along with the setting Framerate numerator
-- (framerateNumerator).
captionSourceFramerate_framerateDenominator :: Lens.Lens' CaptionSourceFramerate (Prelude.Maybe Prelude.Natural)
captionSourceFramerate_framerateDenominator = Lens.lens (\CaptionSourceFramerate' {framerateDenominator} -> framerateDenominator) (\s@CaptionSourceFramerate' {} a -> s {framerateDenominator = a} :: CaptionSourceFramerate)

-- | Specify the numerator of the fraction that represents the frame rate for
-- the setting Caption source frame rate (CaptionSourceFramerate). Use this
-- setting along with the setting Framerate denominator
-- (framerateDenominator).
captionSourceFramerate_framerateNumerator :: Lens.Lens' CaptionSourceFramerate (Prelude.Maybe Prelude.Natural)
captionSourceFramerate_framerateNumerator = Lens.lens (\CaptionSourceFramerate' {framerateNumerator} -> framerateNumerator) (\s@CaptionSourceFramerate' {} a -> s {framerateNumerator = a} :: CaptionSourceFramerate)

instance Core.FromJSON CaptionSourceFramerate where
  parseJSON =
    Core.withObject
      "CaptionSourceFramerate"
      ( \x ->
          CaptionSourceFramerate'
            Prelude.<$> (x Core..:? "framerateDenominator")
            Prelude.<*> (x Core..:? "framerateNumerator")
      )

instance Prelude.Hashable CaptionSourceFramerate where
  hashWithSalt salt' CaptionSourceFramerate' {..} =
    salt' `Prelude.hashWithSalt` framerateNumerator
      `Prelude.hashWithSalt` framerateDenominator

instance Prelude.NFData CaptionSourceFramerate where
  rnf CaptionSourceFramerate' {..} =
    Prelude.rnf framerateDenominator
      `Prelude.seq` Prelude.rnf framerateNumerator

instance Core.ToJSON CaptionSourceFramerate where
  toJSON CaptionSourceFramerate' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("framerateDenominator" Core..=)
              Prelude.<$> framerateDenominator,
            ("framerateNumerator" Core..=)
              Prelude.<$> framerateNumerator
          ]
      )
