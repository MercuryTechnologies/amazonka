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
-- Module      : Amazonka.MediaConvert.Types.MxfSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MxfSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.MxfAfdSignaling
import Amazonka.MediaConvert.Types.MxfProfile
import Amazonka.MediaConvert.Types.MxfXavcProfileSettings
import qualified Amazonka.Prelude as Prelude

-- | These settings relate to your MXF output container.
--
-- /See:/ 'newMxfSettings' smart constructor.
data MxfSettings = MxfSettings'
  { -- | Specify the XAVC profile settings for MXF outputs when you set your MXF
    -- profile to XAVC.
    xavcProfileSettings :: Prelude.Maybe MxfXavcProfileSettings,
    -- | Optional. When you have AFD signaling set up in your output video
    -- stream, use this setting to choose whether to also include it in the MXF
    -- wrapper. Choose Don\'t copy (NO_COPY) to exclude AFD signaling from the
    -- MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the
    -- AFD values from the video stream for this output to the MXF wrapper.
    -- Regardless of which option you choose, the AFD values remain in the
    -- video stream. Related settings: To set up your output to include or
    -- exclude AFD values, see AfdSignaling, under VideoDescription. On the
    -- console, find AFD signaling under the output\'s video encoding settings.
    afdSignaling :: Prelude.Maybe MxfAfdSignaling,
    -- | Specify the MXF profile, also called shim, for this output. When you
    -- choose Auto, MediaConvert chooses a profile based on the video codec and
    -- resolution. For a list of codecs supported with each MXF profile, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/codecs-supported-with-each-mxf-profile.html.
    -- For more information about the automatic selection behavior, see
    -- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/default-automatic-selection-of-mxf-profiles.html.
    profile :: Prelude.Maybe MxfProfile
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MxfSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'xavcProfileSettings', 'mxfSettings_xavcProfileSettings' - Specify the XAVC profile settings for MXF outputs when you set your MXF
-- profile to XAVC.
--
-- 'afdSignaling', 'mxfSettings_afdSignaling' - Optional. When you have AFD signaling set up in your output video
-- stream, use this setting to choose whether to also include it in the MXF
-- wrapper. Choose Don\'t copy (NO_COPY) to exclude AFD signaling from the
-- MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the
-- AFD values from the video stream for this output to the MXF wrapper.
-- Regardless of which option you choose, the AFD values remain in the
-- video stream. Related settings: To set up your output to include or
-- exclude AFD values, see AfdSignaling, under VideoDescription. On the
-- console, find AFD signaling under the output\'s video encoding settings.
--
-- 'profile', 'mxfSettings_profile' - Specify the MXF profile, also called shim, for this output. When you
-- choose Auto, MediaConvert chooses a profile based on the video codec and
-- resolution. For a list of codecs supported with each MXF profile, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/codecs-supported-with-each-mxf-profile.html.
-- For more information about the automatic selection behavior, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/default-automatic-selection-of-mxf-profiles.html.
newMxfSettings ::
  MxfSettings
newMxfSettings =
  MxfSettings'
    { xavcProfileSettings = Prelude.Nothing,
      afdSignaling = Prelude.Nothing,
      profile = Prelude.Nothing
    }

-- | Specify the XAVC profile settings for MXF outputs when you set your MXF
-- profile to XAVC.
mxfSettings_xavcProfileSettings :: Lens.Lens' MxfSettings (Prelude.Maybe MxfXavcProfileSettings)
mxfSettings_xavcProfileSettings = Lens.lens (\MxfSettings' {xavcProfileSettings} -> xavcProfileSettings) (\s@MxfSettings' {} a -> s {xavcProfileSettings = a} :: MxfSettings)

-- | Optional. When you have AFD signaling set up in your output video
-- stream, use this setting to choose whether to also include it in the MXF
-- wrapper. Choose Don\'t copy (NO_COPY) to exclude AFD signaling from the
-- MXF wrapper. Choose Copy from video stream (COPY_FROM_VIDEO) to copy the
-- AFD values from the video stream for this output to the MXF wrapper.
-- Regardless of which option you choose, the AFD values remain in the
-- video stream. Related settings: To set up your output to include or
-- exclude AFD values, see AfdSignaling, under VideoDescription. On the
-- console, find AFD signaling under the output\'s video encoding settings.
mxfSettings_afdSignaling :: Lens.Lens' MxfSettings (Prelude.Maybe MxfAfdSignaling)
mxfSettings_afdSignaling = Lens.lens (\MxfSettings' {afdSignaling} -> afdSignaling) (\s@MxfSettings' {} a -> s {afdSignaling = a} :: MxfSettings)

-- | Specify the MXF profile, also called shim, for this output. When you
-- choose Auto, MediaConvert chooses a profile based on the video codec and
-- resolution. For a list of codecs supported with each MXF profile, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/codecs-supported-with-each-mxf-profile.html.
-- For more information about the automatic selection behavior, see
-- https:\/\/docs.aws.amazon.com\/mediaconvert\/latest\/ug\/default-automatic-selection-of-mxf-profiles.html.
mxfSettings_profile :: Lens.Lens' MxfSettings (Prelude.Maybe MxfProfile)
mxfSettings_profile = Lens.lens (\MxfSettings' {profile} -> profile) (\s@MxfSettings' {} a -> s {profile = a} :: MxfSettings)

instance Core.FromJSON MxfSettings where
  parseJSON =
    Core.withObject
      "MxfSettings"
      ( \x ->
          MxfSettings'
            Prelude.<$> (x Core..:? "xavcProfileSettings")
            Prelude.<*> (x Core..:? "afdSignaling")
            Prelude.<*> (x Core..:? "profile")
      )

instance Prelude.Hashable MxfSettings where
  hashWithSalt salt' MxfSettings' {..} =
    salt' `Prelude.hashWithSalt` profile
      `Prelude.hashWithSalt` afdSignaling
      `Prelude.hashWithSalt` xavcProfileSettings

instance Prelude.NFData MxfSettings where
  rnf MxfSettings' {..} =
    Prelude.rnf xavcProfileSettings
      `Prelude.seq` Prelude.rnf profile
      `Prelude.seq` Prelude.rnf afdSignaling

instance Core.ToJSON MxfSettings where
  toJSON MxfSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("xavcProfileSettings" Core..=)
              Prelude.<$> xavcProfileSettings,
            ("afdSignaling" Core..=) Prelude.<$> afdSignaling,
            ("profile" Core..=) Prelude.<$> profile
          ]
      )
