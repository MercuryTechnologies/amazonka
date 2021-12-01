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
-- Module      : Amazonka.MediaConvert.Types.MovSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MovSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.MovClapAtom
import Amazonka.MediaConvert.Types.MovCslgAtom
import Amazonka.MediaConvert.Types.MovMpeg2FourCCControl
import Amazonka.MediaConvert.Types.MovPaddingControl
import Amazonka.MediaConvert.Types.MovReference
import qualified Amazonka.Prelude as Prelude

-- | These settings relate to your QuickTime MOV output container.
--
-- /See:/ 'newMovSettings' smart constructor.
data MovSettings = MovSettings'
  { -- | Always keep the default value (SELF_CONTAINED) for this setting.
    reference :: Prelude.Maybe MovReference,
    -- | When enabled, file composition times will start at zero, composition
    -- times in the \'ctts\' (composition time to sample) box for B-frames will
    -- be negative, and a \'cslg\' (composition shift least greatest) box will
    -- be included per 14496-1 amendment 1. This improves compatibility with
    -- Apple players and tools.
    cslgAtom :: Prelude.Maybe MovCslgAtom,
    -- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file
    -- using XDCAM fourcc codes. This increases compatibility with Apple
    -- editors and players, but may decrease compatibility with other players.
    -- Only applicable when the video codec is MPEG2.
    mpeg2FourCCControl :: Prelude.Maybe MovMpeg2FourCCControl,
    -- | To make this output compatible with Omenon, keep the default value,
    -- OMNEON. Unless you need Omneon compatibility, set this value to NONE.
    -- When you keep the default value, OMNEON, MediaConvert increases the
    -- length of the edit list atom. This might cause file rejections when a
    -- recipient of the output file doesn\'t expct this extra padding.
    paddingControl :: Prelude.Maybe MovPaddingControl,
    -- | When enabled, include \'clap\' atom if appropriate for the video output
    -- settings.
    clapAtom :: Prelude.Maybe MovClapAtom
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MovSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reference', 'movSettings_reference' - Always keep the default value (SELF_CONTAINED) for this setting.
--
-- 'cslgAtom', 'movSettings_cslgAtom' - When enabled, file composition times will start at zero, composition
-- times in the \'ctts\' (composition time to sample) box for B-frames will
-- be negative, and a \'cslg\' (composition shift least greatest) box will
-- be included per 14496-1 amendment 1. This improves compatibility with
-- Apple players and tools.
--
-- 'mpeg2FourCCControl', 'movSettings_mpeg2FourCCControl' - When set to XDCAM, writes MPEG2 video streams into the QuickTime file
-- using XDCAM fourcc codes. This increases compatibility with Apple
-- editors and players, but may decrease compatibility with other players.
-- Only applicable when the video codec is MPEG2.
--
-- 'paddingControl', 'movSettings_paddingControl' - To make this output compatible with Omenon, keep the default value,
-- OMNEON. Unless you need Omneon compatibility, set this value to NONE.
-- When you keep the default value, OMNEON, MediaConvert increases the
-- length of the edit list atom. This might cause file rejections when a
-- recipient of the output file doesn\'t expct this extra padding.
--
-- 'clapAtom', 'movSettings_clapAtom' - When enabled, include \'clap\' atom if appropriate for the video output
-- settings.
newMovSettings ::
  MovSettings
newMovSettings =
  MovSettings'
    { reference = Prelude.Nothing,
      cslgAtom = Prelude.Nothing,
      mpeg2FourCCControl = Prelude.Nothing,
      paddingControl = Prelude.Nothing,
      clapAtom = Prelude.Nothing
    }

-- | Always keep the default value (SELF_CONTAINED) for this setting.
movSettings_reference :: Lens.Lens' MovSettings (Prelude.Maybe MovReference)
movSettings_reference = Lens.lens (\MovSettings' {reference} -> reference) (\s@MovSettings' {} a -> s {reference = a} :: MovSettings)

-- | When enabled, file composition times will start at zero, composition
-- times in the \'ctts\' (composition time to sample) box for B-frames will
-- be negative, and a \'cslg\' (composition shift least greatest) box will
-- be included per 14496-1 amendment 1. This improves compatibility with
-- Apple players and tools.
movSettings_cslgAtom :: Lens.Lens' MovSettings (Prelude.Maybe MovCslgAtom)
movSettings_cslgAtom = Lens.lens (\MovSettings' {cslgAtom} -> cslgAtom) (\s@MovSettings' {} a -> s {cslgAtom = a} :: MovSettings)

-- | When set to XDCAM, writes MPEG2 video streams into the QuickTime file
-- using XDCAM fourcc codes. This increases compatibility with Apple
-- editors and players, but may decrease compatibility with other players.
-- Only applicable when the video codec is MPEG2.
movSettings_mpeg2FourCCControl :: Lens.Lens' MovSettings (Prelude.Maybe MovMpeg2FourCCControl)
movSettings_mpeg2FourCCControl = Lens.lens (\MovSettings' {mpeg2FourCCControl} -> mpeg2FourCCControl) (\s@MovSettings' {} a -> s {mpeg2FourCCControl = a} :: MovSettings)

-- | To make this output compatible with Omenon, keep the default value,
-- OMNEON. Unless you need Omneon compatibility, set this value to NONE.
-- When you keep the default value, OMNEON, MediaConvert increases the
-- length of the edit list atom. This might cause file rejections when a
-- recipient of the output file doesn\'t expct this extra padding.
movSettings_paddingControl :: Lens.Lens' MovSettings (Prelude.Maybe MovPaddingControl)
movSettings_paddingControl = Lens.lens (\MovSettings' {paddingControl} -> paddingControl) (\s@MovSettings' {} a -> s {paddingControl = a} :: MovSettings)

-- | When enabled, include \'clap\' atom if appropriate for the video output
-- settings.
movSettings_clapAtom :: Lens.Lens' MovSettings (Prelude.Maybe MovClapAtom)
movSettings_clapAtom = Lens.lens (\MovSettings' {clapAtom} -> clapAtom) (\s@MovSettings' {} a -> s {clapAtom = a} :: MovSettings)

instance Core.FromJSON MovSettings where
  parseJSON =
    Core.withObject
      "MovSettings"
      ( \x ->
          MovSettings'
            Prelude.<$> (x Core..:? "reference")
            Prelude.<*> (x Core..:? "cslgAtom")
            Prelude.<*> (x Core..:? "mpeg2FourCCControl")
            Prelude.<*> (x Core..:? "paddingControl")
            Prelude.<*> (x Core..:? "clapAtom")
      )

instance Prelude.Hashable MovSettings where
  hashWithSalt salt' MovSettings' {..} =
    salt' `Prelude.hashWithSalt` clapAtom
      `Prelude.hashWithSalt` paddingControl
      `Prelude.hashWithSalt` mpeg2FourCCControl
      `Prelude.hashWithSalt` cslgAtom
      `Prelude.hashWithSalt` reference

instance Prelude.NFData MovSettings where
  rnf MovSettings' {..} =
    Prelude.rnf reference
      `Prelude.seq` Prelude.rnf clapAtom
      `Prelude.seq` Prelude.rnf paddingControl
      `Prelude.seq` Prelude.rnf mpeg2FourCCControl
      `Prelude.seq` Prelude.rnf cslgAtom

instance Core.ToJSON MovSettings where
  toJSON MovSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("reference" Core..=) Prelude.<$> reference,
            ("cslgAtom" Core..=) Prelude.<$> cslgAtom,
            ("mpeg2FourCCControl" Core..=)
              Prelude.<$> mpeg2FourCCControl,
            ("paddingControl" Core..=)
              Prelude.<$> paddingControl,
            ("clapAtom" Core..=) Prelude.<$> clapAtom
          ]
      )
