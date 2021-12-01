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
-- Module      : Amazonka.MediaConvert.Types.ContainerSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.ContainerSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.CmfcSettings
import Amazonka.MediaConvert.Types.ContainerType
import Amazonka.MediaConvert.Types.F4vSettings
import Amazonka.MediaConvert.Types.M2tsSettings
import Amazonka.MediaConvert.Types.M3u8Settings
import Amazonka.MediaConvert.Types.MovSettings
import Amazonka.MediaConvert.Types.Mp4Settings
import Amazonka.MediaConvert.Types.MpdSettings
import Amazonka.MediaConvert.Types.MxfSettings
import qualified Amazonka.Prelude as Prelude

-- | Container specific settings.
--
-- /See:/ 'newContainerSettings' smart constructor.
data ContainerSettings = ContainerSettings'
  { -- | MPEG-2 TS container settings. These apply to outputs in a File output
    -- group when the output\'s container (ContainerType) is MPEG-2 Transport
    -- Stream (M2TS). In these assets, data is organized by the program map
    -- table (PMT). Each transport stream program contains subsets of data,
    -- including audio, video, and metadata. Each of these subsets of data has
    -- a numerical label called a packet identifier (PID). Each transport
    -- stream program corresponds to one MediaConvert output. The PMT lists the
    -- types of data in a program along with their PID. Downstream systems and
    -- players use the program map table to look up the PID for each type of
    -- data it accesses and then uses the PIDs to locate specific data within
    -- the asset.
    m2tsSettings :: Prelude.Maybe M2tsSettings,
    -- | These settings relate to your MXF output container.
    mxfSettings :: Prelude.Maybe MxfSettings,
    -- | These settings relate to the MPEG-2 transport stream (MPEG2-TS)
    -- container for the MPEG2-TS segments in your HLS outputs.
    m3u8Settings :: Prelude.Maybe M3u8Settings,
    -- | These settings relate to the fragmented MP4 container for the segments
    -- in your CMAF outputs.
    cmfcSettings :: Prelude.Maybe CmfcSettings,
    -- | These settings relate to your QuickTime MOV output container.
    movSettings :: Prelude.Maybe MovSettings,
    -- | These settings relate to your MP4 output container. You can create audio
    -- only outputs with this container. For more information, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/supported-codecs-containers-audio-only.html#output-codecs-and-containers-supported-for-audio-only.
    mp4Settings :: Prelude.Maybe Mp4Settings,
    -- | These settings relate to the fragmented MP4 container for the segments
    -- in your DASH outputs.
    mpdSettings :: Prelude.Maybe MpdSettings,
    -- | Container for this output. Some containers require a container settings
    -- object. If not specified, the default object will be created.
    container :: Prelude.Maybe ContainerType,
    -- | Settings for F4v container
    f4vSettings :: Prelude.Maybe F4vSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'm2tsSettings', 'containerSettings_m2tsSettings' - MPEG-2 TS container settings. These apply to outputs in a File output
-- group when the output\'s container (ContainerType) is MPEG-2 Transport
-- Stream (M2TS). In these assets, data is organized by the program map
-- table (PMT). Each transport stream program contains subsets of data,
-- including audio, video, and metadata. Each of these subsets of data has
-- a numerical label called a packet identifier (PID). Each transport
-- stream program corresponds to one MediaConvert output. The PMT lists the
-- types of data in a program along with their PID. Downstream systems and
-- players use the program map table to look up the PID for each type of
-- data it accesses and then uses the PIDs to locate specific data within
-- the asset.
--
-- 'mxfSettings', 'containerSettings_mxfSettings' - These settings relate to your MXF output container.
--
-- 'm3u8Settings', 'containerSettings_m3u8Settings' - These settings relate to the MPEG-2 transport stream (MPEG2-TS)
-- container for the MPEG2-TS segments in your HLS outputs.
--
-- 'cmfcSettings', 'containerSettings_cmfcSettings' - These settings relate to the fragmented MP4 container for the segments
-- in your CMAF outputs.
--
-- 'movSettings', 'containerSettings_movSettings' - These settings relate to your QuickTime MOV output container.
--
-- 'mp4Settings', 'containerSettings_mp4Settings' - These settings relate to your MP4 output container. You can create audio
-- only outputs with this container. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/supported-codecs-containers-audio-only.html#output-codecs-and-containers-supported-for-audio-only.
--
-- 'mpdSettings', 'containerSettings_mpdSettings' - These settings relate to the fragmented MP4 container for the segments
-- in your DASH outputs.
--
-- 'container', 'containerSettings_container' - Container for this output. Some containers require a container settings
-- object. If not specified, the default object will be created.
--
-- 'f4vSettings', 'containerSettings_f4vSettings' - Settings for F4v container
newContainerSettings ::
  ContainerSettings
newContainerSettings =
  ContainerSettings'
    { m2tsSettings = Prelude.Nothing,
      mxfSettings = Prelude.Nothing,
      m3u8Settings = Prelude.Nothing,
      cmfcSettings = Prelude.Nothing,
      movSettings = Prelude.Nothing,
      mp4Settings = Prelude.Nothing,
      mpdSettings = Prelude.Nothing,
      container = Prelude.Nothing,
      f4vSettings = Prelude.Nothing
    }

-- | MPEG-2 TS container settings. These apply to outputs in a File output
-- group when the output\'s container (ContainerType) is MPEG-2 Transport
-- Stream (M2TS). In these assets, data is organized by the program map
-- table (PMT). Each transport stream program contains subsets of data,
-- including audio, video, and metadata. Each of these subsets of data has
-- a numerical label called a packet identifier (PID). Each transport
-- stream program corresponds to one MediaConvert output. The PMT lists the
-- types of data in a program along with their PID. Downstream systems and
-- players use the program map table to look up the PID for each type of
-- data it accesses and then uses the PIDs to locate specific data within
-- the asset.
containerSettings_m2tsSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe M2tsSettings)
containerSettings_m2tsSettings = Lens.lens (\ContainerSettings' {m2tsSettings} -> m2tsSettings) (\s@ContainerSettings' {} a -> s {m2tsSettings = a} :: ContainerSettings)

-- | These settings relate to your MXF output container.
containerSettings_mxfSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe MxfSettings)
containerSettings_mxfSettings = Lens.lens (\ContainerSettings' {mxfSettings} -> mxfSettings) (\s@ContainerSettings' {} a -> s {mxfSettings = a} :: ContainerSettings)

-- | These settings relate to the MPEG-2 transport stream (MPEG2-TS)
-- container for the MPEG2-TS segments in your HLS outputs.
containerSettings_m3u8Settings :: Lens.Lens' ContainerSettings (Prelude.Maybe M3u8Settings)
containerSettings_m3u8Settings = Lens.lens (\ContainerSettings' {m3u8Settings} -> m3u8Settings) (\s@ContainerSettings' {} a -> s {m3u8Settings = a} :: ContainerSettings)

-- | These settings relate to the fragmented MP4 container for the segments
-- in your CMAF outputs.
containerSettings_cmfcSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe CmfcSettings)
containerSettings_cmfcSettings = Lens.lens (\ContainerSettings' {cmfcSettings} -> cmfcSettings) (\s@ContainerSettings' {} a -> s {cmfcSettings = a} :: ContainerSettings)

-- | These settings relate to your QuickTime MOV output container.
containerSettings_movSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe MovSettings)
containerSettings_movSettings = Lens.lens (\ContainerSettings' {movSettings} -> movSettings) (\s@ContainerSettings' {} a -> s {movSettings = a} :: ContainerSettings)

-- | These settings relate to your MP4 output container. You can create audio
-- only outputs with this container. For more information, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/supported-codecs-containers-audio-only.html#output-codecs-and-containers-supported-for-audio-only.
containerSettings_mp4Settings :: Lens.Lens' ContainerSettings (Prelude.Maybe Mp4Settings)
containerSettings_mp4Settings = Lens.lens (\ContainerSettings' {mp4Settings} -> mp4Settings) (\s@ContainerSettings' {} a -> s {mp4Settings = a} :: ContainerSettings)

-- | These settings relate to the fragmented MP4 container for the segments
-- in your DASH outputs.
containerSettings_mpdSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe MpdSettings)
containerSettings_mpdSettings = Lens.lens (\ContainerSettings' {mpdSettings} -> mpdSettings) (\s@ContainerSettings' {} a -> s {mpdSettings = a} :: ContainerSettings)

-- | Container for this output. Some containers require a container settings
-- object. If not specified, the default object will be created.
containerSettings_container :: Lens.Lens' ContainerSettings (Prelude.Maybe ContainerType)
containerSettings_container = Lens.lens (\ContainerSettings' {container} -> container) (\s@ContainerSettings' {} a -> s {container = a} :: ContainerSettings)

-- | Settings for F4v container
containerSettings_f4vSettings :: Lens.Lens' ContainerSettings (Prelude.Maybe F4vSettings)
containerSettings_f4vSettings = Lens.lens (\ContainerSettings' {f4vSettings} -> f4vSettings) (\s@ContainerSettings' {} a -> s {f4vSettings = a} :: ContainerSettings)

instance Core.FromJSON ContainerSettings where
  parseJSON =
    Core.withObject
      "ContainerSettings"
      ( \x ->
          ContainerSettings'
            Prelude.<$> (x Core..:? "m2tsSettings")
            Prelude.<*> (x Core..:? "mxfSettings")
            Prelude.<*> (x Core..:? "m3u8Settings")
            Prelude.<*> (x Core..:? "cmfcSettings")
            Prelude.<*> (x Core..:? "movSettings")
            Prelude.<*> (x Core..:? "mp4Settings")
            Prelude.<*> (x Core..:? "mpdSettings")
            Prelude.<*> (x Core..:? "container")
            Prelude.<*> (x Core..:? "f4vSettings")
      )

instance Prelude.Hashable ContainerSettings where
  hashWithSalt salt' ContainerSettings' {..} =
    salt' `Prelude.hashWithSalt` f4vSettings
      `Prelude.hashWithSalt` container
      `Prelude.hashWithSalt` mpdSettings
      `Prelude.hashWithSalt` mp4Settings
      `Prelude.hashWithSalt` movSettings
      `Prelude.hashWithSalt` cmfcSettings
      `Prelude.hashWithSalt` m3u8Settings
      `Prelude.hashWithSalt` mxfSettings
      `Prelude.hashWithSalt` m2tsSettings

instance Prelude.NFData ContainerSettings where
  rnf ContainerSettings' {..} =
    Prelude.rnf m2tsSettings
      `Prelude.seq` Prelude.rnf f4vSettings
      `Prelude.seq` Prelude.rnf container
      `Prelude.seq` Prelude.rnf mpdSettings
      `Prelude.seq` Prelude.rnf mp4Settings
      `Prelude.seq` Prelude.rnf movSettings
      `Prelude.seq` Prelude.rnf cmfcSettings
      `Prelude.seq` Prelude.rnf m3u8Settings
      `Prelude.seq` Prelude.rnf mxfSettings

instance Core.ToJSON ContainerSettings where
  toJSON ContainerSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("m2tsSettings" Core..=) Prelude.<$> m2tsSettings,
            ("mxfSettings" Core..=) Prelude.<$> mxfSettings,
            ("m3u8Settings" Core..=) Prelude.<$> m3u8Settings,
            ("cmfcSettings" Core..=) Prelude.<$> cmfcSettings,
            ("movSettings" Core..=) Prelude.<$> movSettings,
            ("mp4Settings" Core..=) Prelude.<$> mp4Settings,
            ("mpdSettings" Core..=) Prelude.<$> mpdSettings,
            ("container" Core..=) Prelude.<$> container,
            ("f4vSettings" Core..=) Prelude.<$> f4vSettings
          ]
      )
