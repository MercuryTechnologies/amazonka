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
-- Module      : Amazonka.MediaConvert.Types.AudioCodecSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.AudioCodecSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.AacSettings
import Amazonka.MediaConvert.Types.Ac3Settings
import Amazonka.MediaConvert.Types.AiffSettings
import Amazonka.MediaConvert.Types.AudioCodec
import Amazonka.MediaConvert.Types.Eac3AtmosSettings
import Amazonka.MediaConvert.Types.Eac3Settings
import Amazonka.MediaConvert.Types.Mp2Settings
import Amazonka.MediaConvert.Types.Mp3Settings
import Amazonka.MediaConvert.Types.OpusSettings
import Amazonka.MediaConvert.Types.VorbisSettings
import Amazonka.MediaConvert.Types.WavSettings
import qualified Amazonka.Prelude as Prelude

-- | Settings related to audio encoding. The settings in this group vary
-- depending on the value that you choose for your audio codec.
--
-- /See:/ 'newAudioCodecSettings' smart constructor.
data AudioCodecSettings = AudioCodecSettings'
  { -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value AIFF.
    aiffSettings :: Prelude.Maybe AiffSettings,
    -- | Choose the audio codec for this output. Note that the option Dolby
    -- Digital passthrough (PASSTHROUGH) applies only to Dolby Digital and
    -- Dolby Digital Plus audio inputs. Make sure that you choose a codec
    -- that\'s supported with your output container:
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#reference-codecs-containers-output-audio
    -- For audio-only outputs, make sure that both your input audio codec and
    -- your output audio codec are supported for audio-only workflows. For more
    -- information, see:
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers-input.html#reference-codecs-containers-input-audio-only
    -- and
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#audio-only-output
    codec :: Prelude.Maybe AudioCodec,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value AC3.
    ac3Settings :: Prelude.Maybe Ac3Settings,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value OPUS.
    opusSettings :: Prelude.Maybe OpusSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value MP2.
    mp2Settings :: Prelude.Maybe Mp2Settings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value WAV.
    wavSettings :: Prelude.Maybe WavSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value EAC3_ATMOS.
    eac3AtmosSettings :: Prelude.Maybe Eac3AtmosSettings,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value MP3.
    mp3Settings :: Prelude.Maybe Mp3Settings,
    -- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
    -- the value Vorbis.
    vorbisSettings :: Prelude.Maybe VorbisSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value AAC. The service accepts one of two mutually exclusive
    -- groups of AAC settings--VBR and CBR. To select one of these modes, set
    -- the value of Bitrate control mode (rateControlMode) to \"VBR\" or
    -- \"CBR\". In VBR mode, you control the audio quality with the setting VBR
    -- quality (vbrQuality). In CBR mode, you use the setting Bitrate
    -- (bitrate). Defaults and valid values depend on the rate control mode.
    aacSettings :: Prelude.Maybe AacSettings,
    -- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
    -- to the value EAC3.
    eac3Settings :: Prelude.Maybe Eac3Settings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AudioCodecSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aiffSettings', 'audioCodecSettings_aiffSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AIFF.
--
-- 'codec', 'audioCodecSettings_codec' - Choose the audio codec for this output. Note that the option Dolby
-- Digital passthrough (PASSTHROUGH) applies only to Dolby Digital and
-- Dolby Digital Plus audio inputs. Make sure that you choose a codec
-- that\'s supported with your output container:
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#reference-codecs-containers-output-audio
-- For audio-only outputs, make sure that both your input audio codec and
-- your output audio codec are supported for audio-only workflows. For more
-- information, see:
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers-input.html#reference-codecs-containers-input-audio-only
-- and
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#audio-only-output
--
-- 'ac3Settings', 'audioCodecSettings_ac3Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AC3.
--
-- 'opusSettings', 'audioCodecSettings_opusSettings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value OPUS.
--
-- 'mp2Settings', 'audioCodecSettings_mp2Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value MP2.
--
-- 'wavSettings', 'audioCodecSettings_wavSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value WAV.
--
-- 'eac3AtmosSettings', 'audioCodecSettings_eac3AtmosSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3_ATMOS.
--
-- 'mp3Settings', 'audioCodecSettings_mp3Settings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value MP3.
--
-- 'vorbisSettings', 'audioCodecSettings_vorbisSettings' - Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value Vorbis.
--
-- 'aacSettings', 'audioCodecSettings_aacSettings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AAC. The service accepts one of two mutually exclusive
-- groups of AAC settings--VBR and CBR. To select one of these modes, set
-- the value of Bitrate control mode (rateControlMode) to \"VBR\" or
-- \"CBR\". In VBR mode, you control the audio quality with the setting VBR
-- quality (vbrQuality). In CBR mode, you use the setting Bitrate
-- (bitrate). Defaults and valid values depend on the rate control mode.
--
-- 'eac3Settings', 'audioCodecSettings_eac3Settings' - Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3.
newAudioCodecSettings ::
  AudioCodecSettings
newAudioCodecSettings =
  AudioCodecSettings'
    { aiffSettings = Prelude.Nothing,
      codec = Prelude.Nothing,
      ac3Settings = Prelude.Nothing,
      opusSettings = Prelude.Nothing,
      mp2Settings = Prelude.Nothing,
      wavSettings = Prelude.Nothing,
      eac3AtmosSettings = Prelude.Nothing,
      mp3Settings = Prelude.Nothing,
      vorbisSettings = Prelude.Nothing,
      aacSettings = Prelude.Nothing,
      eac3Settings = Prelude.Nothing
    }

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AIFF.
audioCodecSettings_aiffSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe AiffSettings)
audioCodecSettings_aiffSettings = Lens.lens (\AudioCodecSettings' {aiffSettings} -> aiffSettings) (\s@AudioCodecSettings' {} a -> s {aiffSettings = a} :: AudioCodecSettings)

-- | Choose the audio codec for this output. Note that the option Dolby
-- Digital passthrough (PASSTHROUGH) applies only to Dolby Digital and
-- Dolby Digital Plus audio inputs. Make sure that you choose a codec
-- that\'s supported with your output container:
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#reference-codecs-containers-output-audio
-- For audio-only outputs, make sure that both your input audio codec and
-- your output audio codec are supported for audio-only workflows. For more
-- information, see:
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers-input.html#reference-codecs-containers-input-audio-only
-- and
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/reference-codecs-containers.html#audio-only-output
audioCodecSettings_codec :: Lens.Lens' AudioCodecSettings (Prelude.Maybe AudioCodec)
audioCodecSettings_codec = Lens.lens (\AudioCodecSettings' {codec} -> codec) (\s@AudioCodecSettings' {} a -> s {codec = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AC3.
audioCodecSettings_ac3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Ac3Settings)
audioCodecSettings_ac3Settings = Lens.lens (\AudioCodecSettings' {ac3Settings} -> ac3Settings) (\s@AudioCodecSettings' {} a -> s {ac3Settings = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value OPUS.
audioCodecSettings_opusSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe OpusSettings)
audioCodecSettings_opusSettings = Lens.lens (\AudioCodecSettings' {opusSettings} -> opusSettings) (\s@AudioCodecSettings' {} a -> s {opusSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value MP2.
audioCodecSettings_mp2Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Mp2Settings)
audioCodecSettings_mp2Settings = Lens.lens (\AudioCodecSettings' {mp2Settings} -> mp2Settings) (\s@AudioCodecSettings' {} a -> s {mp2Settings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value WAV.
audioCodecSettings_wavSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe WavSettings)
audioCodecSettings_wavSettings = Lens.lens (\AudioCodecSettings' {wavSettings} -> wavSettings) (\s@AudioCodecSettings' {} a -> s {wavSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3_ATMOS.
audioCodecSettings_eac3AtmosSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Eac3AtmosSettings)
audioCodecSettings_eac3AtmosSettings = Lens.lens (\AudioCodecSettings' {eac3AtmosSettings} -> eac3AtmosSettings) (\s@AudioCodecSettings' {} a -> s {eac3AtmosSettings = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value MP3.
audioCodecSettings_mp3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Mp3Settings)
audioCodecSettings_mp3Settings = Lens.lens (\AudioCodecSettings' {mp3Settings} -> mp3Settings) (\s@AudioCodecSettings' {} a -> s {mp3Settings = a} :: AudioCodecSettings)

-- | Required when you set Codec, under AudioDescriptions>CodecSettings, to
-- the value Vorbis.
audioCodecSettings_vorbisSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe VorbisSettings)
audioCodecSettings_vorbisSettings = Lens.lens (\AudioCodecSettings' {vorbisSettings} -> vorbisSettings) (\s@AudioCodecSettings' {} a -> s {vorbisSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value AAC. The service accepts one of two mutually exclusive
-- groups of AAC settings--VBR and CBR. To select one of these modes, set
-- the value of Bitrate control mode (rateControlMode) to \"VBR\" or
-- \"CBR\". In VBR mode, you control the audio quality with the setting VBR
-- quality (vbrQuality). In CBR mode, you use the setting Bitrate
-- (bitrate). Defaults and valid values depend on the rate control mode.
audioCodecSettings_aacSettings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe AacSettings)
audioCodecSettings_aacSettings = Lens.lens (\AudioCodecSettings' {aacSettings} -> aacSettings) (\s@AudioCodecSettings' {} a -> s {aacSettings = a} :: AudioCodecSettings)

-- | Required when you set (Codec) under (AudioDescriptions)>(CodecSettings)
-- to the value EAC3.
audioCodecSettings_eac3Settings :: Lens.Lens' AudioCodecSettings (Prelude.Maybe Eac3Settings)
audioCodecSettings_eac3Settings = Lens.lens (\AudioCodecSettings' {eac3Settings} -> eac3Settings) (\s@AudioCodecSettings' {} a -> s {eac3Settings = a} :: AudioCodecSettings)

instance Core.FromJSON AudioCodecSettings where
  parseJSON =
    Core.withObject
      "AudioCodecSettings"
      ( \x ->
          AudioCodecSettings'
            Prelude.<$> (x Core..:? "aiffSettings")
            Prelude.<*> (x Core..:? "codec")
            Prelude.<*> (x Core..:? "ac3Settings")
            Prelude.<*> (x Core..:? "opusSettings")
            Prelude.<*> (x Core..:? "mp2Settings")
            Prelude.<*> (x Core..:? "wavSettings")
            Prelude.<*> (x Core..:? "eac3AtmosSettings")
            Prelude.<*> (x Core..:? "mp3Settings")
            Prelude.<*> (x Core..:? "vorbisSettings")
            Prelude.<*> (x Core..:? "aacSettings")
            Prelude.<*> (x Core..:? "eac3Settings")
      )

instance Prelude.Hashable AudioCodecSettings where
  hashWithSalt salt' AudioCodecSettings' {..} =
    salt' `Prelude.hashWithSalt` eac3Settings
      `Prelude.hashWithSalt` aacSettings
      `Prelude.hashWithSalt` vorbisSettings
      `Prelude.hashWithSalt` mp3Settings
      `Prelude.hashWithSalt` eac3AtmosSettings
      `Prelude.hashWithSalt` wavSettings
      `Prelude.hashWithSalt` mp2Settings
      `Prelude.hashWithSalt` opusSettings
      `Prelude.hashWithSalt` ac3Settings
      `Prelude.hashWithSalt` codec
      `Prelude.hashWithSalt` aiffSettings

instance Prelude.NFData AudioCodecSettings where
  rnf AudioCodecSettings' {..} =
    Prelude.rnf aiffSettings
      `Prelude.seq` Prelude.rnf eac3Settings
      `Prelude.seq` Prelude.rnf aacSettings
      `Prelude.seq` Prelude.rnf vorbisSettings
      `Prelude.seq` Prelude.rnf mp3Settings
      `Prelude.seq` Prelude.rnf eac3AtmosSettings
      `Prelude.seq` Prelude.rnf wavSettings
      `Prelude.seq` Prelude.rnf mp2Settings
      `Prelude.seq` Prelude.rnf opusSettings
      `Prelude.seq` Prelude.rnf ac3Settings
      `Prelude.seq` Prelude.rnf codec

instance Core.ToJSON AudioCodecSettings where
  toJSON AudioCodecSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("aiffSettings" Core..=) Prelude.<$> aiffSettings,
            ("codec" Core..=) Prelude.<$> codec,
            ("ac3Settings" Core..=) Prelude.<$> ac3Settings,
            ("opusSettings" Core..=) Prelude.<$> opusSettings,
            ("mp2Settings" Core..=) Prelude.<$> mp2Settings,
            ("wavSettings" Core..=) Prelude.<$> wavSettings,
            ("eac3AtmosSettings" Core..=)
              Prelude.<$> eac3AtmosSettings,
            ("mp3Settings" Core..=) Prelude.<$> mp3Settings,
            ("vorbisSettings" Core..=)
              Prelude.<$> vorbisSettings,
            ("aacSettings" Core..=) Prelude.<$> aacSettings,
            ("eac3Settings" Core..=) Prelude.<$> eac3Settings
          ]
      )
