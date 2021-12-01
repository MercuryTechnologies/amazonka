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
-- Module      : Amazonka.MediaConvert.Types.H264QvbrSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.H264QvbrSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Settings for quality-defined variable bitrate encoding with the H.265
-- codec. Use these settings only when you set QVBR for Rate control mode
-- (RateControlMode).
--
-- /See:/ 'newH264QvbrSettings' smart constructor.
data H264QvbrSettings = H264QvbrSettings'
  { -- | Optional. Specify a value here to set the QVBR quality to a level that
    -- is between whole numbers. For example, if you want your QVBR quality
    -- level to be 7.33, set qvbrQualityLevel to 7 and set
    -- qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality
    -- level to the nearest third of a whole number. For example, if you set
    -- qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your
    -- actual QVBR quality level is 7.33.
    qvbrQualityLevelFineTune :: Prelude.Maybe Prelude.Double,
    -- | Use this setting only when Rate control mode is QVBR and Quality tuning
    -- level is Multi-pass HQ. For Max average bitrate values suited to the
    -- complexity of your input video, the service limits the average bitrate
    -- of the video part of this output to the value that you choose. That is,
    -- the total size of the video element is less than or equal to the value
    -- you set multiplied by the number of seconds of encoded output.
    maxAverageBitrate :: Prelude.Maybe Prelude.Natural,
    -- | Use this setting only when you set Rate control mode (RateControlMode)
    -- to QVBR. Specify the target quality level for this output. MediaConvert
    -- determines the right number of bits to use for each part of the video to
    -- maintain the video quality that you specify. When you keep the default
    -- value, AUTO, MediaConvert picks a quality level for you, based on
    -- characteristics of your input video. If you prefer to specify a quality
    -- level, specify a number from 1 through 10. Use higher numbers for
    -- greater quality. Level 10 results in nearly lossless compression. The
    -- quality level for most broadcast-quality transcodes is between 6 and 9.
    -- Optionally, to specify a value between whole numbers, also provide a
    -- value for the setting qvbrQualityLevelFineTune. For example, if you want
    -- your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set
    -- qvbrQualityLevelFineTune to .33.
    qvbrQualityLevel :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'H264QvbrSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qvbrQualityLevelFineTune', 'h264QvbrSettings_qvbrQualityLevelFineTune' - Optional. Specify a value here to set the QVBR quality to a level that
-- is between whole numbers. For example, if you want your QVBR quality
-- level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality
-- level to the nearest third of a whole number. For example, if you set
-- qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your
-- actual QVBR quality level is 7.33.
--
-- 'maxAverageBitrate', 'h264QvbrSettings_maxAverageBitrate' - Use this setting only when Rate control mode is QVBR and Quality tuning
-- level is Multi-pass HQ. For Max average bitrate values suited to the
-- complexity of your input video, the service limits the average bitrate
-- of the video part of this output to the value that you choose. That is,
-- the total size of the video element is less than or equal to the value
-- you set multiplied by the number of seconds of encoded output.
--
-- 'qvbrQualityLevel', 'h264QvbrSettings_qvbrQualityLevel' - Use this setting only when you set Rate control mode (RateControlMode)
-- to QVBR. Specify the target quality level for this output. MediaConvert
-- determines the right number of bits to use for each part of the video to
-- maintain the video quality that you specify. When you keep the default
-- value, AUTO, MediaConvert picks a quality level for you, based on
-- characteristics of your input video. If you prefer to specify a quality
-- level, specify a number from 1 through 10. Use higher numbers for
-- greater quality. Level 10 results in nearly lossless compression. The
-- quality level for most broadcast-quality transcodes is between 6 and 9.
-- Optionally, to specify a value between whole numbers, also provide a
-- value for the setting qvbrQualityLevelFineTune. For example, if you want
-- your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33.
newH264QvbrSettings ::
  H264QvbrSettings
newH264QvbrSettings =
  H264QvbrSettings'
    { qvbrQualityLevelFineTune =
        Prelude.Nothing,
      maxAverageBitrate = Prelude.Nothing,
      qvbrQualityLevel = Prelude.Nothing
    }

-- | Optional. Specify a value here to set the QVBR quality to a level that
-- is between whole numbers. For example, if you want your QVBR quality
-- level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33. MediaConvert rounds your QVBR quality
-- level to the nearest third of a whole number. For example, if you set
-- qvbrQualityLevel to 7 and you set qvbrQualityLevelFineTune to .25, your
-- actual QVBR quality level is 7.33.
h264QvbrSettings_qvbrQualityLevelFineTune :: Lens.Lens' H264QvbrSettings (Prelude.Maybe Prelude.Double)
h264QvbrSettings_qvbrQualityLevelFineTune = Lens.lens (\H264QvbrSettings' {qvbrQualityLevelFineTune} -> qvbrQualityLevelFineTune) (\s@H264QvbrSettings' {} a -> s {qvbrQualityLevelFineTune = a} :: H264QvbrSettings)

-- | Use this setting only when Rate control mode is QVBR and Quality tuning
-- level is Multi-pass HQ. For Max average bitrate values suited to the
-- complexity of your input video, the service limits the average bitrate
-- of the video part of this output to the value that you choose. That is,
-- the total size of the video element is less than or equal to the value
-- you set multiplied by the number of seconds of encoded output.
h264QvbrSettings_maxAverageBitrate :: Lens.Lens' H264QvbrSettings (Prelude.Maybe Prelude.Natural)
h264QvbrSettings_maxAverageBitrate = Lens.lens (\H264QvbrSettings' {maxAverageBitrate} -> maxAverageBitrate) (\s@H264QvbrSettings' {} a -> s {maxAverageBitrate = a} :: H264QvbrSettings)

-- | Use this setting only when you set Rate control mode (RateControlMode)
-- to QVBR. Specify the target quality level for this output. MediaConvert
-- determines the right number of bits to use for each part of the video to
-- maintain the video quality that you specify. When you keep the default
-- value, AUTO, MediaConvert picks a quality level for you, based on
-- characteristics of your input video. If you prefer to specify a quality
-- level, specify a number from 1 through 10. Use higher numbers for
-- greater quality. Level 10 results in nearly lossless compression. The
-- quality level for most broadcast-quality transcodes is between 6 and 9.
-- Optionally, to specify a value between whole numbers, also provide a
-- value for the setting qvbrQualityLevelFineTune. For example, if you want
-- your QVBR quality level to be 7.33, set qvbrQualityLevel to 7 and set
-- qvbrQualityLevelFineTune to .33.
h264QvbrSettings_qvbrQualityLevel :: Lens.Lens' H264QvbrSettings (Prelude.Maybe Prelude.Natural)
h264QvbrSettings_qvbrQualityLevel = Lens.lens (\H264QvbrSettings' {qvbrQualityLevel} -> qvbrQualityLevel) (\s@H264QvbrSettings' {} a -> s {qvbrQualityLevel = a} :: H264QvbrSettings)

instance Core.FromJSON H264QvbrSettings where
  parseJSON =
    Core.withObject
      "H264QvbrSettings"
      ( \x ->
          H264QvbrSettings'
            Prelude.<$> (x Core..:? "qvbrQualityLevelFineTune")
            Prelude.<*> (x Core..:? "maxAverageBitrate")
            Prelude.<*> (x Core..:? "qvbrQualityLevel")
      )

instance Prelude.Hashable H264QvbrSettings where
  hashWithSalt salt' H264QvbrSettings' {..} =
    salt' `Prelude.hashWithSalt` qvbrQualityLevel
      `Prelude.hashWithSalt` maxAverageBitrate
      `Prelude.hashWithSalt` qvbrQualityLevelFineTune

instance Prelude.NFData H264QvbrSettings where
  rnf H264QvbrSettings' {..} =
    Prelude.rnf qvbrQualityLevelFineTune
      `Prelude.seq` Prelude.rnf qvbrQualityLevel
      `Prelude.seq` Prelude.rnf maxAverageBitrate

instance Core.ToJSON H264QvbrSettings where
  toJSON H264QvbrSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("qvbrQualityLevelFineTune" Core..=)
              Prelude.<$> qvbrQualityLevelFineTune,
            ("maxAverageBitrate" Core..=)
              Prelude.<$> maxAverageBitrate,
            ("qvbrQualityLevel" Core..=)
              Prelude.<$> qvbrQualityLevel
          ]
      )
