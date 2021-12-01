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
-- Module      : Amazonka.GroundStation.Types.AntennaDownlinkConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.AntennaDownlinkConfig where

import qualified Amazonka.Core as Core
import Amazonka.GroundStation.Types.SpectrumConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about how AWS Ground Station should configure an antenna for
-- downlink during a contact.
--
-- /See:/ 'newAntennaDownlinkConfig' smart constructor.
data AntennaDownlinkConfig = AntennaDownlinkConfig'
  { -- | Object that describes a spectral @Config@.
    spectrumConfig :: SpectrumConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AntennaDownlinkConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spectrumConfig', 'antennaDownlinkConfig_spectrumConfig' - Object that describes a spectral @Config@.
newAntennaDownlinkConfig ::
  -- | 'spectrumConfig'
  SpectrumConfig ->
  AntennaDownlinkConfig
newAntennaDownlinkConfig pSpectrumConfig_ =
  AntennaDownlinkConfig'
    { spectrumConfig =
        pSpectrumConfig_
    }

-- | Object that describes a spectral @Config@.
antennaDownlinkConfig_spectrumConfig :: Lens.Lens' AntennaDownlinkConfig SpectrumConfig
antennaDownlinkConfig_spectrumConfig = Lens.lens (\AntennaDownlinkConfig' {spectrumConfig} -> spectrumConfig) (\s@AntennaDownlinkConfig' {} a -> s {spectrumConfig = a} :: AntennaDownlinkConfig)

instance Core.FromJSON AntennaDownlinkConfig where
  parseJSON =
    Core.withObject
      "AntennaDownlinkConfig"
      ( \x ->
          AntennaDownlinkConfig'
            Prelude.<$> (x Core..: "spectrumConfig")
      )

instance Prelude.Hashable AntennaDownlinkConfig where
  hashWithSalt salt' AntennaDownlinkConfig' {..} =
    salt' `Prelude.hashWithSalt` spectrumConfig

instance Prelude.NFData AntennaDownlinkConfig where
  rnf AntennaDownlinkConfig' {..} =
    Prelude.rnf spectrumConfig

instance Core.ToJSON AntennaDownlinkConfig where
  toJSON AntennaDownlinkConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("spectrumConfig" Core..= spectrumConfig)
          ]
      )
