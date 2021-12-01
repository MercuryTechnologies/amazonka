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
-- Module      : Amazonka.MediaLive.Types.MultiplexProgramSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.MultiplexProgramSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaLive.Types.MultiplexProgramServiceDescriptor
import Amazonka.MediaLive.Types.MultiplexVideoSettings
import Amazonka.MediaLive.Types.PreferredChannelPipeline
import qualified Amazonka.Prelude as Prelude

-- | Multiplex Program settings configuration.
--
-- /See:/ 'newMultiplexProgramSettings' smart constructor.
data MultiplexProgramSettings = MultiplexProgramSettings'
  { -- | Indicates which pipeline is preferred by the multiplex for program
    -- ingest.
    preferredChannelPipeline :: Prelude.Maybe PreferredChannelPipeline,
    -- | Program video settings configuration.
    videoSettings :: Prelude.Maybe MultiplexVideoSettings,
    -- | Transport stream service descriptor configuration for the Multiplex
    -- program.
    serviceDescriptor :: Prelude.Maybe MultiplexProgramServiceDescriptor,
    -- | Unique program number.
    programNumber :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MultiplexProgramSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'preferredChannelPipeline', 'multiplexProgramSettings_preferredChannelPipeline' - Indicates which pipeline is preferred by the multiplex for program
-- ingest.
--
-- 'videoSettings', 'multiplexProgramSettings_videoSettings' - Program video settings configuration.
--
-- 'serviceDescriptor', 'multiplexProgramSettings_serviceDescriptor' - Transport stream service descriptor configuration for the Multiplex
-- program.
--
-- 'programNumber', 'multiplexProgramSettings_programNumber' - Unique program number.
newMultiplexProgramSettings ::
  -- | 'programNumber'
  Prelude.Natural ->
  MultiplexProgramSettings
newMultiplexProgramSettings pProgramNumber_ =
  MultiplexProgramSettings'
    { preferredChannelPipeline =
        Prelude.Nothing,
      videoSettings = Prelude.Nothing,
      serviceDescriptor = Prelude.Nothing,
      programNumber = pProgramNumber_
    }

-- | Indicates which pipeline is preferred by the multiplex for program
-- ingest.
multiplexProgramSettings_preferredChannelPipeline :: Lens.Lens' MultiplexProgramSettings (Prelude.Maybe PreferredChannelPipeline)
multiplexProgramSettings_preferredChannelPipeline = Lens.lens (\MultiplexProgramSettings' {preferredChannelPipeline} -> preferredChannelPipeline) (\s@MultiplexProgramSettings' {} a -> s {preferredChannelPipeline = a} :: MultiplexProgramSettings)

-- | Program video settings configuration.
multiplexProgramSettings_videoSettings :: Lens.Lens' MultiplexProgramSettings (Prelude.Maybe MultiplexVideoSettings)
multiplexProgramSettings_videoSettings = Lens.lens (\MultiplexProgramSettings' {videoSettings} -> videoSettings) (\s@MultiplexProgramSettings' {} a -> s {videoSettings = a} :: MultiplexProgramSettings)

-- | Transport stream service descriptor configuration for the Multiplex
-- program.
multiplexProgramSettings_serviceDescriptor :: Lens.Lens' MultiplexProgramSettings (Prelude.Maybe MultiplexProgramServiceDescriptor)
multiplexProgramSettings_serviceDescriptor = Lens.lens (\MultiplexProgramSettings' {serviceDescriptor} -> serviceDescriptor) (\s@MultiplexProgramSettings' {} a -> s {serviceDescriptor = a} :: MultiplexProgramSettings)

-- | Unique program number.
multiplexProgramSettings_programNumber :: Lens.Lens' MultiplexProgramSettings Prelude.Natural
multiplexProgramSettings_programNumber = Lens.lens (\MultiplexProgramSettings' {programNumber} -> programNumber) (\s@MultiplexProgramSettings' {} a -> s {programNumber = a} :: MultiplexProgramSettings)

instance Core.FromJSON MultiplexProgramSettings where
  parseJSON =
    Core.withObject
      "MultiplexProgramSettings"
      ( \x ->
          MultiplexProgramSettings'
            Prelude.<$> (x Core..:? "preferredChannelPipeline")
            Prelude.<*> (x Core..:? "videoSettings")
            Prelude.<*> (x Core..:? "serviceDescriptor")
            Prelude.<*> (x Core..: "programNumber")
      )

instance Prelude.Hashable MultiplexProgramSettings where
  hashWithSalt salt' MultiplexProgramSettings' {..} =
    salt' `Prelude.hashWithSalt` programNumber
      `Prelude.hashWithSalt` serviceDescriptor
      `Prelude.hashWithSalt` videoSettings
      `Prelude.hashWithSalt` preferredChannelPipeline

instance Prelude.NFData MultiplexProgramSettings where
  rnf MultiplexProgramSettings' {..} =
    Prelude.rnf preferredChannelPipeline
      `Prelude.seq` Prelude.rnf programNumber
      `Prelude.seq` Prelude.rnf serviceDescriptor
      `Prelude.seq` Prelude.rnf videoSettings

instance Core.ToJSON MultiplexProgramSettings where
  toJSON MultiplexProgramSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("preferredChannelPipeline" Core..=)
              Prelude.<$> preferredChannelPipeline,
            ("videoSettings" Core..=) Prelude.<$> videoSettings,
            ("serviceDescriptor" Core..=)
              Prelude.<$> serviceDescriptor,
            Prelude.Just
              ("programNumber" Core..= programNumber)
          ]
      )
