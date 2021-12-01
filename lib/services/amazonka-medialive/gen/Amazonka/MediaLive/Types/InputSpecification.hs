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
-- Module      : Amazonka.MediaLive.Types.InputSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.InputCodec
import Amazonka.MediaLive.Types.InputMaximumBitrate
import Amazonka.MediaLive.Types.InputResolution
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for InputSpecification
--
-- /See:/ 'newInputSpecification' smart constructor.
data InputSpecification = InputSpecification'
  { -- | Input resolution, categorized coarsely
    resolution :: Prelude.Maybe InputResolution,
    -- | Input codec
    codec :: Prelude.Maybe InputCodec,
    -- | Maximum input bitrate, categorized coarsely
    maximumBitrate :: Prelude.Maybe InputMaximumBitrate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolution', 'inputSpecification_resolution' - Input resolution, categorized coarsely
--
-- 'codec', 'inputSpecification_codec' - Input codec
--
-- 'maximumBitrate', 'inputSpecification_maximumBitrate' - Maximum input bitrate, categorized coarsely
newInputSpecification ::
  InputSpecification
newInputSpecification =
  InputSpecification'
    { resolution = Prelude.Nothing,
      codec = Prelude.Nothing,
      maximumBitrate = Prelude.Nothing
    }

-- | Input resolution, categorized coarsely
inputSpecification_resolution :: Lens.Lens' InputSpecification (Prelude.Maybe InputResolution)
inputSpecification_resolution = Lens.lens (\InputSpecification' {resolution} -> resolution) (\s@InputSpecification' {} a -> s {resolution = a} :: InputSpecification)

-- | Input codec
inputSpecification_codec :: Lens.Lens' InputSpecification (Prelude.Maybe InputCodec)
inputSpecification_codec = Lens.lens (\InputSpecification' {codec} -> codec) (\s@InputSpecification' {} a -> s {codec = a} :: InputSpecification)

-- | Maximum input bitrate, categorized coarsely
inputSpecification_maximumBitrate :: Lens.Lens' InputSpecification (Prelude.Maybe InputMaximumBitrate)
inputSpecification_maximumBitrate = Lens.lens (\InputSpecification' {maximumBitrate} -> maximumBitrate) (\s@InputSpecification' {} a -> s {maximumBitrate = a} :: InputSpecification)

instance Core.FromJSON InputSpecification where
  parseJSON =
    Core.withObject
      "InputSpecification"
      ( \x ->
          InputSpecification'
            Prelude.<$> (x Core..:? "resolution")
            Prelude.<*> (x Core..:? "codec")
            Prelude.<*> (x Core..:? "maximumBitrate")
      )

instance Prelude.Hashable InputSpecification where
  hashWithSalt salt' InputSpecification' {..} =
    salt' `Prelude.hashWithSalt` maximumBitrate
      `Prelude.hashWithSalt` codec
      `Prelude.hashWithSalt` resolution

instance Prelude.NFData InputSpecification where
  rnf InputSpecification' {..} =
    Prelude.rnf resolution
      `Prelude.seq` Prelude.rnf maximumBitrate
      `Prelude.seq` Prelude.rnf codec

instance Core.ToJSON InputSpecification where
  toJSON InputSpecification' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("resolution" Core..=) Prelude.<$> resolution,
            ("codec" Core..=) Prelude.<$> codec,
            ("maximumBitrate" Core..=)
              Prelude.<$> maximumBitrate
          ]
      )
