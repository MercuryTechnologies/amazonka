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
-- Module      : Amazonka.MediaConvert.Types.VideoSelector
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.VideoSelector where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.AlphaBehavior
import Amazonka.MediaConvert.Types.ColorSpace
import Amazonka.MediaConvert.Types.ColorSpaceUsage
import Amazonka.MediaConvert.Types.Hdr10Metadata
import Amazonka.MediaConvert.Types.InputRotate
import Amazonka.MediaConvert.Types.InputSampleRange
import qualified Amazonka.Prelude as Prelude

-- | Input video selectors contain the video settings for the input. Each of
-- your inputs can have up to one video selector.
--
-- /See:/ 'newVideoSelector' smart constructor.
data VideoSelector = VideoSelector'
  { -- | Selects a specific program from within a multi-program transport stream.
    -- Note that Quad 4K is not currently supported.
    programNumber :: Prelude.Maybe Prelude.Int,
    -- | Ignore this setting unless this input is a QuickTime animation with an
    -- alpha channel. Use this setting to create separate Key and Fill outputs.
    -- In each output, specify which part of the input MediaConvert uses. Leave
    -- this setting at the default value DISCARD to delete the alpha channel
    -- and preserve the video. Set it to REMAP_TO_LUMA to delete the video and
    -- map the alpha channel to the luma channel of your outputs.
    alphaBehavior :: Prelude.Maybe AlphaBehavior,
    -- | There are two sources for color metadata, the input file and the job
    -- input settings Color space (ColorSpace) and HDR master display
    -- information settings(Hdr10Metadata). The Color space usage setting
    -- determines which takes precedence. Choose Force (FORCE) to use color
    -- metadata from the input job settings. If you don\'t specify values for
    -- those settings, the service defaults to using metadata from your input.
    -- FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the
    -- source when it is present. If there\'s no color metadata in your input
    -- file, the service defaults to using values you specify in the input
    -- settings.
    colorSpaceUsage :: Prelude.Maybe ColorSpaceUsage,
    -- | Use these settings to provide HDR 10 metadata that is missing or
    -- inaccurate in your input video. Appropriate values vary depending on the
    -- input video and must be provided by a color grader. The color grader
    -- generates these values during the HDR 10 mastering process. The valid
    -- range for each of these settings is 0 to 50,000. Each increment
    -- represents 0.00002 in CIE1931 color coordinate. Related settings - When
    -- you specify these values, you must also set Color space (ColorSpace) to
    -- HDR 10 (HDR10). To specify whether the the values you specify here take
    -- precedence over the values in the metadata of your input file, set Color
    -- space usage (ColorSpaceUsage). To specify whether color metadata is
    -- included in an output, set Color metadata (ColorMetadata). For more
    -- information about MediaConvert HDR jobs, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
    hdr10Metadata :: Prelude.Maybe Hdr10Metadata,
    -- | Use PID (Pid) to select specific video data from an input file. Specify
    -- this value as an integer; the system automatically converts it to the
    -- hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet
    -- identifier, is an identifier for a set of data in an MPEG-2 transport
    -- stream container.
    pid :: Prelude.Maybe Prelude.Natural,
    -- | Use Rotate (InputRotate) to specify how the service rotates your video.
    -- You can choose automatic rotation or specify a rotation. You can specify
    -- a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video
    -- container is .mov or .mp4 and your input has rotation metadata, you can
    -- choose Automatic to have the service rotate your video according to the
    -- rotation specified in the metadata. The rotation must be within one
    -- degree of 90, 180, or 270 degrees. If the rotation metadata specifies
    -- any other rotation, the service will default to no rotation. By default,
    -- the service does no rotation, even if your input video has rotation
    -- metadata. The service doesn\'t pass through rotation metadata.
    rotate :: Prelude.Maybe InputRotate,
    -- | If your input video has accurate color space metadata, or if you don\'t
    -- know about color space, leave this set to the default value Follow
    -- (FOLLOW). The service will automatically detect your input color space.
    -- If your input video has metadata indicating the wrong color space,
    -- specify the accurate color space here. If your input video is HDR 10 and
    -- the SMPTE ST 2086 Mastering Display Color Volume static metadata isn\'t
    -- present in your video stream, or if that metadata is present but not
    -- accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct
    -- values in the input HDR 10 metadata (Hdr10Metadata) settings. For more
    -- information about MediaConvert HDR jobs, see
    -- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
    colorSpace :: Prelude.Maybe ColorSpace,
    -- | If the sample range metadata in your input video is accurate, or if you
    -- don\'t know about sample range, keep the default value, Follow (FOLLOW),
    -- for this setting. When you do, the service automatically detects your
    -- input sample range. If your input video has metadata indicating the
    -- wrong sample range, specify the accurate sample range here. When you do,
    -- MediaConvert ignores any sample range information in the input metadata.
    -- Regardless of whether MediaConvert uses the input sample range or the
    -- sample range that you specify, MediaConvert uses the sample range for
    -- transcoding and also writes it to the output metadata.
    sampleRange :: Prelude.Maybe InputSampleRange
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoSelector' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'programNumber', 'videoSelector_programNumber' - Selects a specific program from within a multi-program transport stream.
-- Note that Quad 4K is not currently supported.
--
-- 'alphaBehavior', 'videoSelector_alphaBehavior' - Ignore this setting unless this input is a QuickTime animation with an
-- alpha channel. Use this setting to create separate Key and Fill outputs.
-- In each output, specify which part of the input MediaConvert uses. Leave
-- this setting at the default value DISCARD to delete the alpha channel
-- and preserve the video. Set it to REMAP_TO_LUMA to delete the video and
-- map the alpha channel to the luma channel of your outputs.
--
-- 'colorSpaceUsage', 'videoSelector_colorSpaceUsage' - There are two sources for color metadata, the input file and the job
-- input settings Color space (ColorSpace) and HDR master display
-- information settings(Hdr10Metadata). The Color space usage setting
-- determines which takes precedence. Choose Force (FORCE) to use color
-- metadata from the input job settings. If you don\'t specify values for
-- those settings, the service defaults to using metadata from your input.
-- FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the
-- source when it is present. If there\'s no color metadata in your input
-- file, the service defaults to using values you specify in the input
-- settings.
--
-- 'hdr10Metadata', 'videoSelector_hdr10Metadata' - Use these settings to provide HDR 10 metadata that is missing or
-- inaccurate in your input video. Appropriate values vary depending on the
-- input video and must be provided by a color grader. The color grader
-- generates these values during the HDR 10 mastering process. The valid
-- range for each of these settings is 0 to 50,000. Each increment
-- represents 0.00002 in CIE1931 color coordinate. Related settings - When
-- you specify these values, you must also set Color space (ColorSpace) to
-- HDR 10 (HDR10). To specify whether the the values you specify here take
-- precedence over the values in the metadata of your input file, set Color
-- space usage (ColorSpaceUsage). To specify whether color metadata is
-- included in an output, set Color metadata (ColorMetadata). For more
-- information about MediaConvert HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
--
-- 'pid', 'videoSelector_pid' - Use PID (Pid) to select specific video data from an input file. Specify
-- this value as an integer; the system automatically converts it to the
-- hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet
-- identifier, is an identifier for a set of data in an MPEG-2 transport
-- stream container.
--
-- 'rotate', 'videoSelector_rotate' - Use Rotate (InputRotate) to specify how the service rotates your video.
-- You can choose automatic rotation or specify a rotation. You can specify
-- a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video
-- container is .mov or .mp4 and your input has rotation metadata, you can
-- choose Automatic to have the service rotate your video according to the
-- rotation specified in the metadata. The rotation must be within one
-- degree of 90, 180, or 270 degrees. If the rotation metadata specifies
-- any other rotation, the service will default to no rotation. By default,
-- the service does no rotation, even if your input video has rotation
-- metadata. The service doesn\'t pass through rotation metadata.
--
-- 'colorSpace', 'videoSelector_colorSpace' - If your input video has accurate color space metadata, or if you don\'t
-- know about color space, leave this set to the default value Follow
-- (FOLLOW). The service will automatically detect your input color space.
-- If your input video has metadata indicating the wrong color space,
-- specify the accurate color space here. If your input video is HDR 10 and
-- the SMPTE ST 2086 Mastering Display Color Volume static metadata isn\'t
-- present in your video stream, or if that metadata is present but not
-- accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct
-- values in the input HDR 10 metadata (Hdr10Metadata) settings. For more
-- information about MediaConvert HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
--
-- 'sampleRange', 'videoSelector_sampleRange' - If the sample range metadata in your input video is accurate, or if you
-- don\'t know about sample range, keep the default value, Follow (FOLLOW),
-- for this setting. When you do, the service automatically detects your
-- input sample range. If your input video has metadata indicating the
-- wrong sample range, specify the accurate sample range here. When you do,
-- MediaConvert ignores any sample range information in the input metadata.
-- Regardless of whether MediaConvert uses the input sample range or the
-- sample range that you specify, MediaConvert uses the sample range for
-- transcoding and also writes it to the output metadata.
newVideoSelector ::
  VideoSelector
newVideoSelector =
  VideoSelector'
    { programNumber = Prelude.Nothing,
      alphaBehavior = Prelude.Nothing,
      colorSpaceUsage = Prelude.Nothing,
      hdr10Metadata = Prelude.Nothing,
      pid = Prelude.Nothing,
      rotate = Prelude.Nothing,
      colorSpace = Prelude.Nothing,
      sampleRange = Prelude.Nothing
    }

-- | Selects a specific program from within a multi-program transport stream.
-- Note that Quad 4K is not currently supported.
videoSelector_programNumber :: Lens.Lens' VideoSelector (Prelude.Maybe Prelude.Int)
videoSelector_programNumber = Lens.lens (\VideoSelector' {programNumber} -> programNumber) (\s@VideoSelector' {} a -> s {programNumber = a} :: VideoSelector)

-- | Ignore this setting unless this input is a QuickTime animation with an
-- alpha channel. Use this setting to create separate Key and Fill outputs.
-- In each output, specify which part of the input MediaConvert uses. Leave
-- this setting at the default value DISCARD to delete the alpha channel
-- and preserve the video. Set it to REMAP_TO_LUMA to delete the video and
-- map the alpha channel to the luma channel of your outputs.
videoSelector_alphaBehavior :: Lens.Lens' VideoSelector (Prelude.Maybe AlphaBehavior)
videoSelector_alphaBehavior = Lens.lens (\VideoSelector' {alphaBehavior} -> alphaBehavior) (\s@VideoSelector' {} a -> s {alphaBehavior = a} :: VideoSelector)

-- | There are two sources for color metadata, the input file and the job
-- input settings Color space (ColorSpace) and HDR master display
-- information settings(Hdr10Metadata). The Color space usage setting
-- determines which takes precedence. Choose Force (FORCE) to use color
-- metadata from the input job settings. If you don\'t specify values for
-- those settings, the service defaults to using metadata from your input.
-- FALLBACK - Choose Fallback (FALLBACK) to use color metadata from the
-- source when it is present. If there\'s no color metadata in your input
-- file, the service defaults to using values you specify in the input
-- settings.
videoSelector_colorSpaceUsage :: Lens.Lens' VideoSelector (Prelude.Maybe ColorSpaceUsage)
videoSelector_colorSpaceUsage = Lens.lens (\VideoSelector' {colorSpaceUsage} -> colorSpaceUsage) (\s@VideoSelector' {} a -> s {colorSpaceUsage = a} :: VideoSelector)

-- | Use these settings to provide HDR 10 metadata that is missing or
-- inaccurate in your input video. Appropriate values vary depending on the
-- input video and must be provided by a color grader. The color grader
-- generates these values during the HDR 10 mastering process. The valid
-- range for each of these settings is 0 to 50,000. Each increment
-- represents 0.00002 in CIE1931 color coordinate. Related settings - When
-- you specify these values, you must also set Color space (ColorSpace) to
-- HDR 10 (HDR10). To specify whether the the values you specify here take
-- precedence over the values in the metadata of your input file, set Color
-- space usage (ColorSpaceUsage). To specify whether color metadata is
-- included in an output, set Color metadata (ColorMetadata). For more
-- information about MediaConvert HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
videoSelector_hdr10Metadata :: Lens.Lens' VideoSelector (Prelude.Maybe Hdr10Metadata)
videoSelector_hdr10Metadata = Lens.lens (\VideoSelector' {hdr10Metadata} -> hdr10Metadata) (\s@VideoSelector' {} a -> s {hdr10Metadata = a} :: VideoSelector)

-- | Use PID (Pid) to select specific video data from an input file. Specify
-- this value as an integer; the system automatically converts it to the
-- hexidecimal value. For example, 257 selects PID 0x101. A PID, or packet
-- identifier, is an identifier for a set of data in an MPEG-2 transport
-- stream container.
videoSelector_pid :: Lens.Lens' VideoSelector (Prelude.Maybe Prelude.Natural)
videoSelector_pid = Lens.lens (\VideoSelector' {pid} -> pid) (\s@VideoSelector' {} a -> s {pid = a} :: VideoSelector)

-- | Use Rotate (InputRotate) to specify how the service rotates your video.
-- You can choose automatic rotation or specify a rotation. You can specify
-- a clockwise rotation of 0, 90, 180, or 270 degrees. If your input video
-- container is .mov or .mp4 and your input has rotation metadata, you can
-- choose Automatic to have the service rotate your video according to the
-- rotation specified in the metadata. The rotation must be within one
-- degree of 90, 180, or 270 degrees. If the rotation metadata specifies
-- any other rotation, the service will default to no rotation. By default,
-- the service does no rotation, even if your input video has rotation
-- metadata. The service doesn\'t pass through rotation metadata.
videoSelector_rotate :: Lens.Lens' VideoSelector (Prelude.Maybe InputRotate)
videoSelector_rotate = Lens.lens (\VideoSelector' {rotate} -> rotate) (\s@VideoSelector' {} a -> s {rotate = a} :: VideoSelector)

-- | If your input video has accurate color space metadata, or if you don\'t
-- know about color space, leave this set to the default value Follow
-- (FOLLOW). The service will automatically detect your input color space.
-- If your input video has metadata indicating the wrong color space,
-- specify the accurate color space here. If your input video is HDR 10 and
-- the SMPTE ST 2086 Mastering Display Color Volume static metadata isn\'t
-- present in your video stream, or if that metadata is present but not
-- accurate, choose Force HDR 10 (FORCE_HDR10) here and specify correct
-- values in the input HDR 10 metadata (Hdr10Metadata) settings. For more
-- information about MediaConvert HDR jobs, see
-- https:\/\/docs.aws.amazon.com\/console\/mediaconvert\/hdr.
videoSelector_colorSpace :: Lens.Lens' VideoSelector (Prelude.Maybe ColorSpace)
videoSelector_colorSpace = Lens.lens (\VideoSelector' {colorSpace} -> colorSpace) (\s@VideoSelector' {} a -> s {colorSpace = a} :: VideoSelector)

-- | If the sample range metadata in your input video is accurate, or if you
-- don\'t know about sample range, keep the default value, Follow (FOLLOW),
-- for this setting. When you do, the service automatically detects your
-- input sample range. If your input video has metadata indicating the
-- wrong sample range, specify the accurate sample range here. When you do,
-- MediaConvert ignores any sample range information in the input metadata.
-- Regardless of whether MediaConvert uses the input sample range or the
-- sample range that you specify, MediaConvert uses the sample range for
-- transcoding and also writes it to the output metadata.
videoSelector_sampleRange :: Lens.Lens' VideoSelector (Prelude.Maybe InputSampleRange)
videoSelector_sampleRange = Lens.lens (\VideoSelector' {sampleRange} -> sampleRange) (\s@VideoSelector' {} a -> s {sampleRange = a} :: VideoSelector)

instance Core.FromJSON VideoSelector where
  parseJSON =
    Core.withObject
      "VideoSelector"
      ( \x ->
          VideoSelector'
            Prelude.<$> (x Core..:? "programNumber")
            Prelude.<*> (x Core..:? "alphaBehavior")
            Prelude.<*> (x Core..:? "colorSpaceUsage")
            Prelude.<*> (x Core..:? "hdr10Metadata")
            Prelude.<*> (x Core..:? "pid")
            Prelude.<*> (x Core..:? "rotate")
            Prelude.<*> (x Core..:? "colorSpace")
            Prelude.<*> (x Core..:? "sampleRange")
      )

instance Prelude.Hashable VideoSelector where
  hashWithSalt salt' VideoSelector' {..} =
    salt' `Prelude.hashWithSalt` sampleRange
      `Prelude.hashWithSalt` colorSpace
      `Prelude.hashWithSalt` rotate
      `Prelude.hashWithSalt` pid
      `Prelude.hashWithSalt` hdr10Metadata
      `Prelude.hashWithSalt` colorSpaceUsage
      `Prelude.hashWithSalt` alphaBehavior
      `Prelude.hashWithSalt` programNumber

instance Prelude.NFData VideoSelector where
  rnf VideoSelector' {..} =
    Prelude.rnf programNumber
      `Prelude.seq` Prelude.rnf sampleRange
      `Prelude.seq` Prelude.rnf colorSpace
      `Prelude.seq` Prelude.rnf rotate
      `Prelude.seq` Prelude.rnf pid
      `Prelude.seq` Prelude.rnf hdr10Metadata
      `Prelude.seq` Prelude.rnf colorSpaceUsage
      `Prelude.seq` Prelude.rnf alphaBehavior

instance Core.ToJSON VideoSelector where
  toJSON VideoSelector' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("programNumber" Core..=) Prelude.<$> programNumber,
            ("alphaBehavior" Core..=) Prelude.<$> alphaBehavior,
            ("colorSpaceUsage" Core..=)
              Prelude.<$> colorSpaceUsage,
            ("hdr10Metadata" Core..=) Prelude.<$> hdr10Metadata,
            ("pid" Core..=) Prelude.<$> pid,
            ("rotate" Core..=) Prelude.<$> rotate,
            ("colorSpace" Core..=) Prelude.<$> colorSpace,
            ("sampleRange" Core..=) Prelude.<$> sampleRange
          ]
      )
