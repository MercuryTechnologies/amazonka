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
-- Module      : Amazonka.MediaLive.Types.OutputDestination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.OutputDestination where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.MediaPackageOutputDestinationSettings
import Amazonka.MediaLive.Types.MultiplexProgramChannelDestinationSettings
import Amazonka.MediaLive.Types.OutputDestinationSettings
import qualified Amazonka.Prelude as Prelude

-- | Placeholder documentation for OutputDestination
--
-- /See:/ 'newOutputDestination' smart constructor.
data OutputDestination = OutputDestination'
  { -- | Destination settings for a standard output; one destination for each
    -- redundant encoder.
    settings :: Prelude.Maybe [OutputDestinationSettings],
    -- | Destination settings for a MediaPackage output; one destination for both
    -- encoders.
    mediaPackageSettings :: Prelude.Maybe [MediaPackageOutputDestinationSettings],
    -- | User-specified id. This is used in an output group or an output.
    id :: Prelude.Maybe Prelude.Text,
    -- | Destination settings for a Multiplex output; one destination for both
    -- encoders.
    multiplexSettings :: Prelude.Maybe MultiplexProgramChannelDestinationSettings
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OutputDestination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'settings', 'outputDestination_settings' - Destination settings for a standard output; one destination for each
-- redundant encoder.
--
-- 'mediaPackageSettings', 'outputDestination_mediaPackageSettings' - Destination settings for a MediaPackage output; one destination for both
-- encoders.
--
-- 'id', 'outputDestination_id' - User-specified id. This is used in an output group or an output.
--
-- 'multiplexSettings', 'outputDestination_multiplexSettings' - Destination settings for a Multiplex output; one destination for both
-- encoders.
newOutputDestination ::
  OutputDestination
newOutputDestination =
  OutputDestination'
    { settings = Prelude.Nothing,
      mediaPackageSettings = Prelude.Nothing,
      id = Prelude.Nothing,
      multiplexSettings = Prelude.Nothing
    }

-- | Destination settings for a standard output; one destination for each
-- redundant encoder.
outputDestination_settings :: Lens.Lens' OutputDestination (Prelude.Maybe [OutputDestinationSettings])
outputDestination_settings = Lens.lens (\OutputDestination' {settings} -> settings) (\s@OutputDestination' {} a -> s {settings = a} :: OutputDestination) Prelude.. Lens.mapping Lens.coerced

-- | Destination settings for a MediaPackage output; one destination for both
-- encoders.
outputDestination_mediaPackageSettings :: Lens.Lens' OutputDestination (Prelude.Maybe [MediaPackageOutputDestinationSettings])
outputDestination_mediaPackageSettings = Lens.lens (\OutputDestination' {mediaPackageSettings} -> mediaPackageSettings) (\s@OutputDestination' {} a -> s {mediaPackageSettings = a} :: OutputDestination) Prelude.. Lens.mapping Lens.coerced

-- | User-specified id. This is used in an output group or an output.
outputDestination_id :: Lens.Lens' OutputDestination (Prelude.Maybe Prelude.Text)
outputDestination_id = Lens.lens (\OutputDestination' {id} -> id) (\s@OutputDestination' {} a -> s {id = a} :: OutputDestination)

-- | Destination settings for a Multiplex output; one destination for both
-- encoders.
outputDestination_multiplexSettings :: Lens.Lens' OutputDestination (Prelude.Maybe MultiplexProgramChannelDestinationSettings)
outputDestination_multiplexSettings = Lens.lens (\OutputDestination' {multiplexSettings} -> multiplexSettings) (\s@OutputDestination' {} a -> s {multiplexSettings = a} :: OutputDestination)

instance Core.FromJSON OutputDestination where
  parseJSON =
    Core.withObject
      "OutputDestination"
      ( \x ->
          OutputDestination'
            Prelude.<$> (x Core..:? "settings" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "mediaPackageSettings"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "multiplexSettings")
      )

instance Prelude.Hashable OutputDestination where
  hashWithSalt salt' OutputDestination' {..} =
    salt' `Prelude.hashWithSalt` multiplexSettings
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` mediaPackageSettings
      `Prelude.hashWithSalt` settings

instance Prelude.NFData OutputDestination where
  rnf OutputDestination' {..} =
    Prelude.rnf settings
      `Prelude.seq` Prelude.rnf multiplexSettings
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf mediaPackageSettings

instance Core.ToJSON OutputDestination where
  toJSON OutputDestination' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("settings" Core..=) Prelude.<$> settings,
            ("mediaPackageSettings" Core..=)
              Prelude.<$> mediaPackageSettings,
            ("id" Core..=) Prelude.<$> id,
            ("multiplexSettings" Core..=)
              Prelude.<$> multiplexSettings
          ]
      )
