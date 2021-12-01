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
-- Module      : Amazonka.MediaLive.Types.CaptionSelectorSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.CaptionSelectorSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.AncillarySourceSettings
import Amazonka.MediaLive.Types.AribSourceSettings
import Amazonka.MediaLive.Types.DvbSubSourceSettings
import Amazonka.MediaLive.Types.EmbeddedSourceSettings
import Amazonka.MediaLive.Types.Scte20SourceSettings
import Amazonka.MediaLive.Types.Scte27SourceSettings
import Amazonka.MediaLive.Types.TeletextSourceSettings
import qualified Amazonka.Prelude as Prelude

-- | Caption Selector Settings
--
-- /See:/ 'newCaptionSelectorSettings' smart constructor.
data CaptionSelectorSettings = CaptionSelectorSettings'
  { teletextSourceSettings :: Prelude.Maybe TeletextSourceSettings,
    aribSourceSettings :: Prelude.Maybe AribSourceSettings,
    scte27SourceSettings :: Prelude.Maybe Scte27SourceSettings,
    dvbSubSourceSettings :: Prelude.Maybe DvbSubSourceSettings,
    ancillarySourceSettings :: Prelude.Maybe AncillarySourceSettings,
    scte20SourceSettings :: Prelude.Maybe Scte20SourceSettings,
    embeddedSourceSettings :: Prelude.Maybe EmbeddedSourceSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionSelectorSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'teletextSourceSettings', 'captionSelectorSettings_teletextSourceSettings' - Undocumented member.
--
-- 'aribSourceSettings', 'captionSelectorSettings_aribSourceSettings' - Undocumented member.
--
-- 'scte27SourceSettings', 'captionSelectorSettings_scte27SourceSettings' - Undocumented member.
--
-- 'dvbSubSourceSettings', 'captionSelectorSettings_dvbSubSourceSettings' - Undocumented member.
--
-- 'ancillarySourceSettings', 'captionSelectorSettings_ancillarySourceSettings' - Undocumented member.
--
-- 'scte20SourceSettings', 'captionSelectorSettings_scte20SourceSettings' - Undocumented member.
--
-- 'embeddedSourceSettings', 'captionSelectorSettings_embeddedSourceSettings' - Undocumented member.
newCaptionSelectorSettings ::
  CaptionSelectorSettings
newCaptionSelectorSettings =
  CaptionSelectorSettings'
    { teletextSourceSettings =
        Prelude.Nothing,
      aribSourceSettings = Prelude.Nothing,
      scte27SourceSettings = Prelude.Nothing,
      dvbSubSourceSettings = Prelude.Nothing,
      ancillarySourceSettings = Prelude.Nothing,
      scte20SourceSettings = Prelude.Nothing,
      embeddedSourceSettings = Prelude.Nothing
    }

-- | Undocumented member.
captionSelectorSettings_teletextSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe TeletextSourceSettings)
captionSelectorSettings_teletextSourceSettings = Lens.lens (\CaptionSelectorSettings' {teletextSourceSettings} -> teletextSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {teletextSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_aribSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe AribSourceSettings)
captionSelectorSettings_aribSourceSettings = Lens.lens (\CaptionSelectorSettings' {aribSourceSettings} -> aribSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {aribSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_scte27SourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe Scte27SourceSettings)
captionSelectorSettings_scte27SourceSettings = Lens.lens (\CaptionSelectorSettings' {scte27SourceSettings} -> scte27SourceSettings) (\s@CaptionSelectorSettings' {} a -> s {scte27SourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_dvbSubSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe DvbSubSourceSettings)
captionSelectorSettings_dvbSubSourceSettings = Lens.lens (\CaptionSelectorSettings' {dvbSubSourceSettings} -> dvbSubSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {dvbSubSourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_ancillarySourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe AncillarySourceSettings)
captionSelectorSettings_ancillarySourceSettings = Lens.lens (\CaptionSelectorSettings' {ancillarySourceSettings} -> ancillarySourceSettings) (\s@CaptionSelectorSettings' {} a -> s {ancillarySourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_scte20SourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe Scte20SourceSettings)
captionSelectorSettings_scte20SourceSettings = Lens.lens (\CaptionSelectorSettings' {scte20SourceSettings} -> scte20SourceSettings) (\s@CaptionSelectorSettings' {} a -> s {scte20SourceSettings = a} :: CaptionSelectorSettings)

-- | Undocumented member.
captionSelectorSettings_embeddedSourceSettings :: Lens.Lens' CaptionSelectorSettings (Prelude.Maybe EmbeddedSourceSettings)
captionSelectorSettings_embeddedSourceSettings = Lens.lens (\CaptionSelectorSettings' {embeddedSourceSettings} -> embeddedSourceSettings) (\s@CaptionSelectorSettings' {} a -> s {embeddedSourceSettings = a} :: CaptionSelectorSettings)

instance Core.FromJSON CaptionSelectorSettings where
  parseJSON =
    Core.withObject
      "CaptionSelectorSettings"
      ( \x ->
          CaptionSelectorSettings'
            Prelude.<$> (x Core..:? "teletextSourceSettings")
            Prelude.<*> (x Core..:? "aribSourceSettings")
            Prelude.<*> (x Core..:? "scte27SourceSettings")
            Prelude.<*> (x Core..:? "dvbSubSourceSettings")
            Prelude.<*> (x Core..:? "ancillarySourceSettings")
            Prelude.<*> (x Core..:? "scte20SourceSettings")
            Prelude.<*> (x Core..:? "embeddedSourceSettings")
      )

instance Prelude.Hashable CaptionSelectorSettings where
  hashWithSalt salt' CaptionSelectorSettings' {..} =
    salt' `Prelude.hashWithSalt` embeddedSourceSettings
      `Prelude.hashWithSalt` scte20SourceSettings
      `Prelude.hashWithSalt` ancillarySourceSettings
      `Prelude.hashWithSalt` dvbSubSourceSettings
      `Prelude.hashWithSalt` scte27SourceSettings
      `Prelude.hashWithSalt` aribSourceSettings
      `Prelude.hashWithSalt` teletextSourceSettings

instance Prelude.NFData CaptionSelectorSettings where
  rnf CaptionSelectorSettings' {..} =
    Prelude.rnf teletextSourceSettings
      `Prelude.seq` Prelude.rnf embeddedSourceSettings
      `Prelude.seq` Prelude.rnf scte20SourceSettings
      `Prelude.seq` Prelude.rnf ancillarySourceSettings
      `Prelude.seq` Prelude.rnf dvbSubSourceSettings
      `Prelude.seq` Prelude.rnf scte27SourceSettings
      `Prelude.seq` Prelude.rnf aribSourceSettings

instance Core.ToJSON CaptionSelectorSettings where
  toJSON CaptionSelectorSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("teletextSourceSettings" Core..=)
              Prelude.<$> teletextSourceSettings,
            ("aribSourceSettings" Core..=)
              Prelude.<$> aribSourceSettings,
            ("scte27SourceSettings" Core..=)
              Prelude.<$> scte27SourceSettings,
            ("dvbSubSourceSettings" Core..=)
              Prelude.<$> dvbSubSourceSettings,
            ("ancillarySourceSettings" Core..=)
              Prelude.<$> ancillarySourceSettings,
            ("scte20SourceSettings" Core..=)
              Prelude.<$> scte20SourceSettings,
            ("embeddedSourceSettings" Core..=)
              Prelude.<$> embeddedSourceSettings
          ]
      )
