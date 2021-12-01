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
-- Module      : Amazonka.MediaConvert.Types.VideoDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.VideoDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.AfdSignaling
import Amazonka.MediaConvert.Types.AntiAlias
import Amazonka.MediaConvert.Types.ColorMetadata
import Amazonka.MediaConvert.Types.DropFrameTimecode
import Amazonka.MediaConvert.Types.Rectangle
import Amazonka.MediaConvert.Types.RespondToAfd
import Amazonka.MediaConvert.Types.ScalingBehavior
import Amazonka.MediaConvert.Types.VideoCodecSettings
import Amazonka.MediaConvert.Types.VideoPreprocessor
import Amazonka.MediaConvert.Types.VideoTimecodeInsertion
import qualified Amazonka.Prelude as Prelude

-- | Settings related to video encoding of your output. The specific video
-- settings depend on the video codec that you choose. When you work
-- directly in your JSON job specification, include one instance of Video
-- description (VideoDescription) per output.
--
-- /See:/ 'newVideoDescription' smart constructor.
data VideoDescription = VideoDescription'
  { -- | Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable
    -- Timecode insertion when the input frame rate is identical to the output
    -- frame rate. To include timecodes in this output, set Timecode insertion
    -- (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to
    -- DISABLED. Default is DISABLED. When the service inserts timecodes in an
    -- output, by default, it uses any embedded timecodes from the input. If
    -- none are present, the service will set the timecode for the first output
    -- frame to zero. To change this default behavior, adjust the settings
    -- under Timecode configuration (TimecodeConfig). In the console, these
    -- settings are located under Job > Job settings > Timecode configuration.
    -- Note - Timecode source under input settings (InputTimecodeSource) does
    -- not affect the timecodes that are inserted in the output. Source under
    -- Job settings > Timecode configuration (TimecodeSource) does.
    timecodeInsertion :: Prelude.Maybe VideoTimecodeInsertion,
    -- | Use the Height (Height) setting to define the video resolution height
    -- for this output. Specify in pixels. If you don\'t provide a value here,
    -- the service will use the input height.
    height :: Prelude.Maybe Prelude.Natural,
    -- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert
    -- AFD signaling (AfdSignaling) to specify whether the service includes AFD
    -- values in the output video data and what those values are. * Choose None
    -- to remove all AFD values from this output. * Choose Fixed to ignore
    -- input AFD values and instead encode the value specified in the job. *
    -- Choose Auto to calculate output AFD values based on the input AFD scaler
    -- data.
    afdSignaling :: Prelude.Maybe AfdSignaling,
    -- | Use Sharpness (Sharpness) setting to specify the strength of
    -- anti-aliasing. This setting changes the width of the anti-alias filter
    -- kernel used for scaling. Sharpness only applies if your output
    -- resolution is different from your input resolution. 0 is the softest
    -- setting, 100 the sharpest, and 50 recommended for most content.
    sharpness :: Prelude.Maybe Prelude.Natural,
    -- | Use Cropping selection (crop) to specify the video area that the service
    -- will include in the output video frame.
    crop :: Prelude.Maybe Rectangle,
    -- | Use Width (Width) to define the video resolution width, in pixels, for
    -- this output. If you don\'t provide a value here, the service will use
    -- the input width.
    width :: Prelude.Maybe Prelude.Natural,
    -- | Specify how the service handles outputs that have a different aspect
    -- ratio from the input aspect ratio. Choose Stretch to output
    -- (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit.
    -- Keep the setting Default (DEFAULT) to have the service letterbox your
    -- video instead. This setting overrides any value that you specify for the
    -- setting Selection placement (position) in this output.
    scalingBehavior :: Prelude.Maybe ScalingBehavior,
    -- | Use Respond to AFD (RespondToAfd) to specify how the service changes the
    -- video itself in response to AFD values in the input. * Choose Respond to
    -- clip the input video frame according to the AFD value, input display
    -- aspect ratio, and output display aspect ratio. * Choose Passthrough to
    -- include the input AFD values. Do not choose this when AfdSignaling is
    -- set to (NONE). A preferred implementation of this workflow is to set
    -- RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to
    -- remove all input AFD values from this output.
    respondToAfd :: Prelude.Maybe RespondToAfd,
    -- | Applies only to 29.97 fps outputs. When this feature is enabled, the
    -- service will use drop-frame timecode on outputs. If it is not possible
    -- to use drop-frame timecode, the system will fall back to non-drop-frame.
    -- This setting is enabled by default when Timecode insertion
    -- (TimecodeInsertion) is enabled.
    dropFrameTimecode :: Prelude.Maybe DropFrameTimecode,
    -- | The anti-alias filter is automatically applied to all outputs. The
    -- service no longer accepts the value DISABLED for AntiAlias. If you
    -- specify that in your job, the service will ignore the setting.
    antiAlias :: Prelude.Maybe AntiAlias,
    -- | Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED).
    -- Use Fixed (FixedAfd) to specify a four-bit AFD value which the service
    -- will write on all frames of this video output.
    fixedAfd :: Prelude.Maybe Prelude.Natural,
    -- | Choose Insert (INSERT) for this setting to include color metadata in
    -- this output. Choose Ignore (IGNORE) to exclude color metadata from this
    -- output. If you don\'t specify a value, the service sets this to Insert
    -- by default.
    colorMetadata :: Prelude.Maybe ColorMetadata,
    -- | Video codec settings, (CodecSettings) under (VideoDescription), contains
    -- the group of settings related to video encoding. The settings in this
    -- group vary depending on the value that you choose for Video codec
    -- (Codec). For each codec enum that you choose, define the corresponding
    -- settings object. The following lists the codec enum, settings object
    -- pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE,
    -- FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings *
    -- MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8,
    -- Vp8Settings * VP9, Vp9Settings * XAVC, XavcSettings
    codecSettings :: Prelude.Maybe VideoCodecSettings,
    -- | Find additional transcoding features under Preprocessors
    -- (VideoPreprocessors). Enable the features at each output individually.
    -- These features are disabled by default.
    videoPreprocessors :: Prelude.Maybe VideoPreprocessor,
    -- | Use Selection placement (position) to define the video area in your
    -- output frame. The area outside of the rectangle that you specify here is
    -- black.
    position :: Prelude.Maybe Rectangle
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timecodeInsertion', 'videoDescription_timecodeInsertion' - Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable
-- Timecode insertion when the input frame rate is identical to the output
-- frame rate. To include timecodes in this output, set Timecode insertion
-- (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to
-- DISABLED. Default is DISABLED. When the service inserts timecodes in an
-- output, by default, it uses any embedded timecodes from the input. If
-- none are present, the service will set the timecode for the first output
-- frame to zero. To change this default behavior, adjust the settings
-- under Timecode configuration (TimecodeConfig). In the console, these
-- settings are located under Job > Job settings > Timecode configuration.
-- Note - Timecode source under input settings (InputTimecodeSource) does
-- not affect the timecodes that are inserted in the output. Source under
-- Job settings > Timecode configuration (TimecodeSource) does.
--
-- 'height', 'videoDescription_height' - Use the Height (Height) setting to define the video resolution height
-- for this output. Specify in pixels. If you don\'t provide a value here,
-- the service will use the input height.
--
-- 'afdSignaling', 'videoDescription_afdSignaling' - This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert
-- AFD signaling (AfdSignaling) to specify whether the service includes AFD
-- values in the output video data and what those values are. * Choose None
-- to remove all AFD values from this output. * Choose Fixed to ignore
-- input AFD values and instead encode the value specified in the job. *
-- Choose Auto to calculate output AFD values based on the input AFD scaler
-- data.
--
-- 'sharpness', 'videoDescription_sharpness' - Use Sharpness (Sharpness) setting to specify the strength of
-- anti-aliasing. This setting changes the width of the anti-alias filter
-- kernel used for scaling. Sharpness only applies if your output
-- resolution is different from your input resolution. 0 is the softest
-- setting, 100 the sharpest, and 50 recommended for most content.
--
-- 'crop', 'videoDescription_crop' - Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame.
--
-- 'width', 'videoDescription_width' - Use Width (Width) to define the video resolution width, in pixels, for
-- this output. If you don\'t provide a value here, the service will use
-- the input width.
--
-- 'scalingBehavior', 'videoDescription_scalingBehavior' - Specify how the service handles outputs that have a different aspect
-- ratio from the input aspect ratio. Choose Stretch to output
-- (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit.
-- Keep the setting Default (DEFAULT) to have the service letterbox your
-- video instead. This setting overrides any value that you specify for the
-- setting Selection placement (position) in this output.
--
-- 'respondToAfd', 'videoDescription_respondToAfd' - Use Respond to AFD (RespondToAfd) to specify how the service changes the
-- video itself in response to AFD values in the input. * Choose Respond to
-- clip the input video frame according to the AFD value, input display
-- aspect ratio, and output display aspect ratio. * Choose Passthrough to
-- include the input AFD values. Do not choose this when AfdSignaling is
-- set to (NONE). A preferred implementation of this workflow is to set
-- RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to
-- remove all input AFD values from this output.
--
-- 'dropFrameTimecode', 'videoDescription_dropFrameTimecode' - Applies only to 29.97 fps outputs. When this feature is enabled, the
-- service will use drop-frame timecode on outputs. If it is not possible
-- to use drop-frame timecode, the system will fall back to non-drop-frame.
-- This setting is enabled by default when Timecode insertion
-- (TimecodeInsertion) is enabled.
--
-- 'antiAlias', 'videoDescription_antiAlias' - The anti-alias filter is automatically applied to all outputs. The
-- service no longer accepts the value DISABLED for AntiAlias. If you
-- specify that in your job, the service will ignore the setting.
--
-- 'fixedAfd', 'videoDescription_fixedAfd' - Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED).
-- Use Fixed (FixedAfd) to specify a four-bit AFD value which the service
-- will write on all frames of this video output.
--
-- 'colorMetadata', 'videoDescription_colorMetadata' - Choose Insert (INSERT) for this setting to include color metadata in
-- this output. Choose Ignore (IGNORE) to exclude color metadata from this
-- output. If you don\'t specify a value, the service sets this to Insert
-- by default.
--
-- 'codecSettings', 'videoDescription_codecSettings' - Video codec settings, (CodecSettings) under (VideoDescription), contains
-- the group of settings related to video encoding. The settings in this
-- group vary depending on the value that you choose for Video codec
-- (Codec). For each codec enum that you choose, define the corresponding
-- settings object. The following lists the codec enum, settings object
-- pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE,
-- FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings *
-- MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8,
-- Vp8Settings * VP9, Vp9Settings * XAVC, XavcSettings
--
-- 'videoPreprocessors', 'videoDescription_videoPreprocessors' - Find additional transcoding features under Preprocessors
-- (VideoPreprocessors). Enable the features at each output individually.
-- These features are disabled by default.
--
-- 'position', 'videoDescription_position' - Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black.
newVideoDescription ::
  VideoDescription
newVideoDescription =
  VideoDescription'
    { timecodeInsertion =
        Prelude.Nothing,
      height = Prelude.Nothing,
      afdSignaling = Prelude.Nothing,
      sharpness = Prelude.Nothing,
      crop = Prelude.Nothing,
      width = Prelude.Nothing,
      scalingBehavior = Prelude.Nothing,
      respondToAfd = Prelude.Nothing,
      dropFrameTimecode = Prelude.Nothing,
      antiAlias = Prelude.Nothing,
      fixedAfd = Prelude.Nothing,
      colorMetadata = Prelude.Nothing,
      codecSettings = Prelude.Nothing,
      videoPreprocessors = Prelude.Nothing,
      position = Prelude.Nothing
    }

-- | Applies only to H.264, H.265, MPEG2, and ProRes outputs. Only enable
-- Timecode insertion when the input frame rate is identical to the output
-- frame rate. To include timecodes in this output, set Timecode insertion
-- (VideoTimecodeInsertion) to PIC_TIMING_SEI. To leave them out, set it to
-- DISABLED. Default is DISABLED. When the service inserts timecodes in an
-- output, by default, it uses any embedded timecodes from the input. If
-- none are present, the service will set the timecode for the first output
-- frame to zero. To change this default behavior, adjust the settings
-- under Timecode configuration (TimecodeConfig). In the console, these
-- settings are located under Job > Job settings > Timecode configuration.
-- Note - Timecode source under input settings (InputTimecodeSource) does
-- not affect the timecodes that are inserted in the output. Source under
-- Job settings > Timecode configuration (TimecodeSource) does.
videoDescription_timecodeInsertion :: Lens.Lens' VideoDescription (Prelude.Maybe VideoTimecodeInsertion)
videoDescription_timecodeInsertion = Lens.lens (\VideoDescription' {timecodeInsertion} -> timecodeInsertion) (\s@VideoDescription' {} a -> s {timecodeInsertion = a} :: VideoDescription)

-- | Use the Height (Height) setting to define the video resolution height
-- for this output. Specify in pixels. If you don\'t provide a value here,
-- the service will use the input height.
videoDescription_height :: Lens.Lens' VideoDescription (Prelude.Maybe Prelude.Natural)
videoDescription_height = Lens.lens (\VideoDescription' {height} -> height) (\s@VideoDescription' {} a -> s {height = a} :: VideoDescription)

-- | This setting only applies to H.264, H.265, and MPEG2 outputs. Use Insert
-- AFD signaling (AfdSignaling) to specify whether the service includes AFD
-- values in the output video data and what those values are. * Choose None
-- to remove all AFD values from this output. * Choose Fixed to ignore
-- input AFD values and instead encode the value specified in the job. *
-- Choose Auto to calculate output AFD values based on the input AFD scaler
-- data.
videoDescription_afdSignaling :: Lens.Lens' VideoDescription (Prelude.Maybe AfdSignaling)
videoDescription_afdSignaling = Lens.lens (\VideoDescription' {afdSignaling} -> afdSignaling) (\s@VideoDescription' {} a -> s {afdSignaling = a} :: VideoDescription)

-- | Use Sharpness (Sharpness) setting to specify the strength of
-- anti-aliasing. This setting changes the width of the anti-alias filter
-- kernel used for scaling. Sharpness only applies if your output
-- resolution is different from your input resolution. 0 is the softest
-- setting, 100 the sharpest, and 50 recommended for most content.
videoDescription_sharpness :: Lens.Lens' VideoDescription (Prelude.Maybe Prelude.Natural)
videoDescription_sharpness = Lens.lens (\VideoDescription' {sharpness} -> sharpness) (\s@VideoDescription' {} a -> s {sharpness = a} :: VideoDescription)

-- | Use Cropping selection (crop) to specify the video area that the service
-- will include in the output video frame.
videoDescription_crop :: Lens.Lens' VideoDescription (Prelude.Maybe Rectangle)
videoDescription_crop = Lens.lens (\VideoDescription' {crop} -> crop) (\s@VideoDescription' {} a -> s {crop = a} :: VideoDescription)

-- | Use Width (Width) to define the video resolution width, in pixels, for
-- this output. If you don\'t provide a value here, the service will use
-- the input width.
videoDescription_width :: Lens.Lens' VideoDescription (Prelude.Maybe Prelude.Natural)
videoDescription_width = Lens.lens (\VideoDescription' {width} -> width) (\s@VideoDescription' {} a -> s {width = a} :: VideoDescription)

-- | Specify how the service handles outputs that have a different aspect
-- ratio from the input aspect ratio. Choose Stretch to output
-- (STRETCH_TO_OUTPUT) to have the service stretch your video image to fit.
-- Keep the setting Default (DEFAULT) to have the service letterbox your
-- video instead. This setting overrides any value that you specify for the
-- setting Selection placement (position) in this output.
videoDescription_scalingBehavior :: Lens.Lens' VideoDescription (Prelude.Maybe ScalingBehavior)
videoDescription_scalingBehavior = Lens.lens (\VideoDescription' {scalingBehavior} -> scalingBehavior) (\s@VideoDescription' {} a -> s {scalingBehavior = a} :: VideoDescription)

-- | Use Respond to AFD (RespondToAfd) to specify how the service changes the
-- video itself in response to AFD values in the input. * Choose Respond to
-- clip the input video frame according to the AFD value, input display
-- aspect ratio, and output display aspect ratio. * Choose Passthrough to
-- include the input AFD values. Do not choose this when AfdSignaling is
-- set to (NONE). A preferred implementation of this workflow is to set
-- RespondToAfd to (NONE) and set AfdSignaling to (AUTO). * Choose None to
-- remove all input AFD values from this output.
videoDescription_respondToAfd :: Lens.Lens' VideoDescription (Prelude.Maybe RespondToAfd)
videoDescription_respondToAfd = Lens.lens (\VideoDescription' {respondToAfd} -> respondToAfd) (\s@VideoDescription' {} a -> s {respondToAfd = a} :: VideoDescription)

-- | Applies only to 29.97 fps outputs. When this feature is enabled, the
-- service will use drop-frame timecode on outputs. If it is not possible
-- to use drop-frame timecode, the system will fall back to non-drop-frame.
-- This setting is enabled by default when Timecode insertion
-- (TimecodeInsertion) is enabled.
videoDescription_dropFrameTimecode :: Lens.Lens' VideoDescription (Prelude.Maybe DropFrameTimecode)
videoDescription_dropFrameTimecode = Lens.lens (\VideoDescription' {dropFrameTimecode} -> dropFrameTimecode) (\s@VideoDescription' {} a -> s {dropFrameTimecode = a} :: VideoDescription)

-- | The anti-alias filter is automatically applied to all outputs. The
-- service no longer accepts the value DISABLED for AntiAlias. If you
-- specify that in your job, the service will ignore the setting.
videoDescription_antiAlias :: Lens.Lens' VideoDescription (Prelude.Maybe AntiAlias)
videoDescription_antiAlias = Lens.lens (\VideoDescription' {antiAlias} -> antiAlias) (\s@VideoDescription' {} a -> s {antiAlias = a} :: VideoDescription)

-- | Applies only if you set AFD Signaling(AfdSignaling) to Fixed (FIXED).
-- Use Fixed (FixedAfd) to specify a four-bit AFD value which the service
-- will write on all frames of this video output.
videoDescription_fixedAfd :: Lens.Lens' VideoDescription (Prelude.Maybe Prelude.Natural)
videoDescription_fixedAfd = Lens.lens (\VideoDescription' {fixedAfd} -> fixedAfd) (\s@VideoDescription' {} a -> s {fixedAfd = a} :: VideoDescription)

-- | Choose Insert (INSERT) for this setting to include color metadata in
-- this output. Choose Ignore (IGNORE) to exclude color metadata from this
-- output. If you don\'t specify a value, the service sets this to Insert
-- by default.
videoDescription_colorMetadata :: Lens.Lens' VideoDescription (Prelude.Maybe ColorMetadata)
videoDescription_colorMetadata = Lens.lens (\VideoDescription' {colorMetadata} -> colorMetadata) (\s@VideoDescription' {} a -> s {colorMetadata = a} :: VideoDescription)

-- | Video codec settings, (CodecSettings) under (VideoDescription), contains
-- the group of settings related to video encoding. The settings in this
-- group vary depending on the value that you choose for Video codec
-- (Codec). For each codec enum that you choose, define the corresponding
-- settings object. The following lists the codec enum, settings object
-- pairs. * AV1, Av1Settings * AVC_INTRA, AvcIntraSettings * FRAME_CAPTURE,
-- FrameCaptureSettings * H_264, H264Settings * H_265, H265Settings *
-- MPEG2, Mpeg2Settings * PRORES, ProresSettings * VC3, Vc3Settings * VP8,
-- Vp8Settings * VP9, Vp9Settings * XAVC, XavcSettings
videoDescription_codecSettings :: Lens.Lens' VideoDescription (Prelude.Maybe VideoCodecSettings)
videoDescription_codecSettings = Lens.lens (\VideoDescription' {codecSettings} -> codecSettings) (\s@VideoDescription' {} a -> s {codecSettings = a} :: VideoDescription)

-- | Find additional transcoding features under Preprocessors
-- (VideoPreprocessors). Enable the features at each output individually.
-- These features are disabled by default.
videoDescription_videoPreprocessors :: Lens.Lens' VideoDescription (Prelude.Maybe VideoPreprocessor)
videoDescription_videoPreprocessors = Lens.lens (\VideoDescription' {videoPreprocessors} -> videoPreprocessors) (\s@VideoDescription' {} a -> s {videoPreprocessors = a} :: VideoDescription)

-- | Use Selection placement (position) to define the video area in your
-- output frame. The area outside of the rectangle that you specify here is
-- black.
videoDescription_position :: Lens.Lens' VideoDescription (Prelude.Maybe Rectangle)
videoDescription_position = Lens.lens (\VideoDescription' {position} -> position) (\s@VideoDescription' {} a -> s {position = a} :: VideoDescription)

instance Core.FromJSON VideoDescription where
  parseJSON =
    Core.withObject
      "VideoDescription"
      ( \x ->
          VideoDescription'
            Prelude.<$> (x Core..:? "timecodeInsertion")
            Prelude.<*> (x Core..:? "height")
            Prelude.<*> (x Core..:? "afdSignaling")
            Prelude.<*> (x Core..:? "sharpness")
            Prelude.<*> (x Core..:? "crop")
            Prelude.<*> (x Core..:? "width")
            Prelude.<*> (x Core..:? "scalingBehavior")
            Prelude.<*> (x Core..:? "respondToAfd")
            Prelude.<*> (x Core..:? "dropFrameTimecode")
            Prelude.<*> (x Core..:? "antiAlias")
            Prelude.<*> (x Core..:? "fixedAfd")
            Prelude.<*> (x Core..:? "colorMetadata")
            Prelude.<*> (x Core..:? "codecSettings")
            Prelude.<*> (x Core..:? "videoPreprocessors")
            Prelude.<*> (x Core..:? "position")
      )

instance Prelude.Hashable VideoDescription where
  hashWithSalt salt' VideoDescription' {..} =
    salt' `Prelude.hashWithSalt` position
      `Prelude.hashWithSalt` videoPreprocessors
      `Prelude.hashWithSalt` codecSettings
      `Prelude.hashWithSalt` colorMetadata
      `Prelude.hashWithSalt` fixedAfd
      `Prelude.hashWithSalt` antiAlias
      `Prelude.hashWithSalt` dropFrameTimecode
      `Prelude.hashWithSalt` respondToAfd
      `Prelude.hashWithSalt` scalingBehavior
      `Prelude.hashWithSalt` width
      `Prelude.hashWithSalt` crop
      `Prelude.hashWithSalt` sharpness
      `Prelude.hashWithSalt` afdSignaling
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` timecodeInsertion

instance Prelude.NFData VideoDescription where
  rnf VideoDescription' {..} =
    Prelude.rnf timecodeInsertion
      `Prelude.seq` Prelude.rnf position
      `Prelude.seq` Prelude.rnf videoPreprocessors
      `Prelude.seq` Prelude.rnf codecSettings
      `Prelude.seq` Prelude.rnf colorMetadata
      `Prelude.seq` Prelude.rnf fixedAfd
      `Prelude.seq` Prelude.rnf antiAlias
      `Prelude.seq` Prelude.rnf dropFrameTimecode
      `Prelude.seq` Prelude.rnf respondToAfd
      `Prelude.seq` Prelude.rnf scalingBehavior
      `Prelude.seq` Prelude.rnf width
      `Prelude.seq` Prelude.rnf crop
      `Prelude.seq` Prelude.rnf sharpness
      `Prelude.seq` Prelude.rnf afdSignaling
      `Prelude.seq` Prelude.rnf height

instance Core.ToJSON VideoDescription where
  toJSON VideoDescription' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("timecodeInsertion" Core..=)
              Prelude.<$> timecodeInsertion,
            ("height" Core..=) Prelude.<$> height,
            ("afdSignaling" Core..=) Prelude.<$> afdSignaling,
            ("sharpness" Core..=) Prelude.<$> sharpness,
            ("crop" Core..=) Prelude.<$> crop,
            ("width" Core..=) Prelude.<$> width,
            ("scalingBehavior" Core..=)
              Prelude.<$> scalingBehavior,
            ("respondToAfd" Core..=) Prelude.<$> respondToAfd,
            ("dropFrameTimecode" Core..=)
              Prelude.<$> dropFrameTimecode,
            ("antiAlias" Core..=) Prelude.<$> antiAlias,
            ("fixedAfd" Core..=) Prelude.<$> fixedAfd,
            ("colorMetadata" Core..=) Prelude.<$> colorMetadata,
            ("codecSettings" Core..=) Prelude.<$> codecSettings,
            ("videoPreprocessors" Core..=)
              Prelude.<$> videoPreprocessors,
            ("position" Core..=) Prelude.<$> position
          ]
      )
