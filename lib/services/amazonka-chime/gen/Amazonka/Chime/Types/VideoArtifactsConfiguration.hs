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
-- Module      : Amazonka.Chime.Types.VideoArtifactsConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.VideoArtifactsConfiguration where

import Amazonka.Chime.Types.ArtifactsState
import Amazonka.Chime.Types.VideoMuxType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The video artifact configuration object.
--
-- /See:/ 'newVideoArtifactsConfiguration' smart constructor.
data VideoArtifactsConfiguration = VideoArtifactsConfiguration'
  { -- | The MUX type of the video artifact configuration object.
    muxType :: Prelude.Maybe VideoMuxType,
    -- | Indicates whether the video artifact is enabled or disabled.
    state :: ArtifactsState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VideoArtifactsConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'muxType', 'videoArtifactsConfiguration_muxType' - The MUX type of the video artifact configuration object.
--
-- 'state', 'videoArtifactsConfiguration_state' - Indicates whether the video artifact is enabled or disabled.
newVideoArtifactsConfiguration ::
  -- | 'state'
  ArtifactsState ->
  VideoArtifactsConfiguration
newVideoArtifactsConfiguration pState_ =
  VideoArtifactsConfiguration'
    { muxType =
        Prelude.Nothing,
      state = pState_
    }

-- | The MUX type of the video artifact configuration object.
videoArtifactsConfiguration_muxType :: Lens.Lens' VideoArtifactsConfiguration (Prelude.Maybe VideoMuxType)
videoArtifactsConfiguration_muxType = Lens.lens (\VideoArtifactsConfiguration' {muxType} -> muxType) (\s@VideoArtifactsConfiguration' {} a -> s {muxType = a} :: VideoArtifactsConfiguration)

-- | Indicates whether the video artifact is enabled or disabled.
videoArtifactsConfiguration_state :: Lens.Lens' VideoArtifactsConfiguration ArtifactsState
videoArtifactsConfiguration_state = Lens.lens (\VideoArtifactsConfiguration' {state} -> state) (\s@VideoArtifactsConfiguration' {} a -> s {state = a} :: VideoArtifactsConfiguration)

instance Core.FromJSON VideoArtifactsConfiguration where
  parseJSON =
    Core.withObject
      "VideoArtifactsConfiguration"
      ( \x ->
          VideoArtifactsConfiguration'
            Prelude.<$> (x Core..:? "MuxType")
            Prelude.<*> (x Core..: "State")
      )

instance Prelude.Hashable VideoArtifactsConfiguration where
  hashWithSalt salt' VideoArtifactsConfiguration' {..} =
    salt' `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` muxType

instance Prelude.NFData VideoArtifactsConfiguration where
  rnf VideoArtifactsConfiguration' {..} =
    Prelude.rnf muxType `Prelude.seq` Prelude.rnf state

instance Core.ToJSON VideoArtifactsConfiguration where
  toJSON VideoArtifactsConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MuxType" Core..=) Prelude.<$> muxType,
            Prelude.Just ("State" Core..= state)
          ]
      )
