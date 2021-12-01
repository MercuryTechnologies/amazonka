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
-- Module      : Amazonka.MediaConvert.Types.MxfXavcProfileSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.MxfXavcProfileSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.MxfXavcDurationMode
import qualified Amazonka.Prelude as Prelude

-- | Specify the XAVC profile settings for MXF outputs when you set your MXF
-- profile to XAVC.
--
-- /See:/ 'newMxfXavcProfileSettings' smart constructor.
data MxfXavcProfileSettings = MxfXavcProfileSettings'
  { -- | Specify a value for this setting only for outputs that you set up with
    -- one of these two XAVC profiles: XAVC HD Intra CBG (XAVC_HD_INTRA_CBG) or
    -- XAVC 4K Intra CBG (XAVC_4K_INTRA_CBG). Specify the amount of space in
    -- each frame that the service reserves for ancillary data, such as
    -- teletext captions. The default value for this setting is 1492 bytes per
    -- frame. This should be sufficient to prevent overflow unless you have
    -- multiple pages of teletext captions data. If you have a large amount of
    -- teletext data, specify a larger number.
    maxAncDataSize :: Prelude.Maybe Prelude.Natural,
    -- | To create an output that complies with the XAVC file format guidelines
    -- for interoperability, keep the default value, Drop frames for compliance
    -- (DROP_FRAMES_FOR_COMPLIANCE). To include all frames from your input in
    -- this output, keep the default setting, Allow any duration
    -- (ALLOW_ANY_DURATION). The number of frames that MediaConvert excludes
    -- when you set this to Drop frames for compliance depends on the output
    -- frame rate and duration.
    durationMode :: Prelude.Maybe MxfXavcDurationMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MxfXavcProfileSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxAncDataSize', 'mxfXavcProfileSettings_maxAncDataSize' - Specify a value for this setting only for outputs that you set up with
-- one of these two XAVC profiles: XAVC HD Intra CBG (XAVC_HD_INTRA_CBG) or
-- XAVC 4K Intra CBG (XAVC_4K_INTRA_CBG). Specify the amount of space in
-- each frame that the service reserves for ancillary data, such as
-- teletext captions. The default value for this setting is 1492 bytes per
-- frame. This should be sufficient to prevent overflow unless you have
-- multiple pages of teletext captions data. If you have a large amount of
-- teletext data, specify a larger number.
--
-- 'durationMode', 'mxfXavcProfileSettings_durationMode' - To create an output that complies with the XAVC file format guidelines
-- for interoperability, keep the default value, Drop frames for compliance
-- (DROP_FRAMES_FOR_COMPLIANCE). To include all frames from your input in
-- this output, keep the default setting, Allow any duration
-- (ALLOW_ANY_DURATION). The number of frames that MediaConvert excludes
-- when you set this to Drop frames for compliance depends on the output
-- frame rate and duration.
newMxfXavcProfileSettings ::
  MxfXavcProfileSettings
newMxfXavcProfileSettings =
  MxfXavcProfileSettings'
    { maxAncDataSize =
        Prelude.Nothing,
      durationMode = Prelude.Nothing
    }

-- | Specify a value for this setting only for outputs that you set up with
-- one of these two XAVC profiles: XAVC HD Intra CBG (XAVC_HD_INTRA_CBG) or
-- XAVC 4K Intra CBG (XAVC_4K_INTRA_CBG). Specify the amount of space in
-- each frame that the service reserves for ancillary data, such as
-- teletext captions. The default value for this setting is 1492 bytes per
-- frame. This should be sufficient to prevent overflow unless you have
-- multiple pages of teletext captions data. If you have a large amount of
-- teletext data, specify a larger number.
mxfXavcProfileSettings_maxAncDataSize :: Lens.Lens' MxfXavcProfileSettings (Prelude.Maybe Prelude.Natural)
mxfXavcProfileSettings_maxAncDataSize = Lens.lens (\MxfXavcProfileSettings' {maxAncDataSize} -> maxAncDataSize) (\s@MxfXavcProfileSettings' {} a -> s {maxAncDataSize = a} :: MxfXavcProfileSettings)

-- | To create an output that complies with the XAVC file format guidelines
-- for interoperability, keep the default value, Drop frames for compliance
-- (DROP_FRAMES_FOR_COMPLIANCE). To include all frames from your input in
-- this output, keep the default setting, Allow any duration
-- (ALLOW_ANY_DURATION). The number of frames that MediaConvert excludes
-- when you set this to Drop frames for compliance depends on the output
-- frame rate and duration.
mxfXavcProfileSettings_durationMode :: Lens.Lens' MxfXavcProfileSettings (Prelude.Maybe MxfXavcDurationMode)
mxfXavcProfileSettings_durationMode = Lens.lens (\MxfXavcProfileSettings' {durationMode} -> durationMode) (\s@MxfXavcProfileSettings' {} a -> s {durationMode = a} :: MxfXavcProfileSettings)

instance Core.FromJSON MxfXavcProfileSettings where
  parseJSON =
    Core.withObject
      "MxfXavcProfileSettings"
      ( \x ->
          MxfXavcProfileSettings'
            Prelude.<$> (x Core..:? "maxAncDataSize")
            Prelude.<*> (x Core..:? "durationMode")
      )

instance Prelude.Hashable MxfXavcProfileSettings where
  hashWithSalt salt' MxfXavcProfileSettings' {..} =
    salt' `Prelude.hashWithSalt` durationMode
      `Prelude.hashWithSalt` maxAncDataSize

instance Prelude.NFData MxfXavcProfileSettings where
  rnf MxfXavcProfileSettings' {..} =
    Prelude.rnf maxAncDataSize
      `Prelude.seq` Prelude.rnf durationMode

instance Core.ToJSON MxfXavcProfileSettings where
  toJSON MxfXavcProfileSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("maxAncDataSize" Core..=)
              Prelude.<$> maxAncDataSize,
            ("durationMode" Core..=) Prelude.<$> durationMode
          ]
      )
