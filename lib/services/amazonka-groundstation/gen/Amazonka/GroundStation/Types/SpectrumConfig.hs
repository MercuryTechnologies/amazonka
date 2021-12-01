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
-- Module      : Amazonka.GroundStation.Types.SpectrumConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.SpectrumConfig where

import qualified Amazonka.Core as Core
import Amazonka.GroundStation.Types.Frequency
import Amazonka.GroundStation.Types.FrequencyBandwidth
import Amazonka.GroundStation.Types.Polarization
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Object that describes a spectral @Config@.
--
-- /See:/ 'newSpectrumConfig' smart constructor.
data SpectrumConfig = SpectrumConfig'
  { -- | Polarization of a spectral @Config@. Capturing both @\"RIGHT_HAND\"@ and
    -- @\"LEFT_HAND\"@ polarization requires two separate configs.
    polarization :: Prelude.Maybe Polarization,
    -- | Bandwidth of a spectral @Config@. AWS Ground Station currently has the
    -- following bandwidth limitations:
    --
    -- -   For @AntennaDownlinkDemodDecodeconfig@, valid values are between 125
    --     kHz to 650 MHz.
    --
    -- -   For @AntennaDownlinkconfig@ valid values are between 10 kHz to 54
    --     MHz.
    --
    -- -   For @AntennaUplinkConfig@, valid values are between 10 kHz to 54
    --     MHz.
    bandwidth :: FrequencyBandwidth,
    -- | Center frequency of a spectral @Config@. Valid values are between 2200
    -- to 2300 MHz and 7750 to 8400 MHz for downlink and 2025 to 2120 MHz for
    -- uplink.
    centerFrequency :: Frequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SpectrumConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'polarization', 'spectrumConfig_polarization' - Polarization of a spectral @Config@. Capturing both @\"RIGHT_HAND\"@ and
-- @\"LEFT_HAND\"@ polarization requires two separate configs.
--
-- 'bandwidth', 'spectrumConfig_bandwidth' - Bandwidth of a spectral @Config@. AWS Ground Station currently has the
-- following bandwidth limitations:
--
-- -   For @AntennaDownlinkDemodDecodeconfig@, valid values are between 125
--     kHz to 650 MHz.
--
-- -   For @AntennaDownlinkconfig@ valid values are between 10 kHz to 54
--     MHz.
--
-- -   For @AntennaUplinkConfig@, valid values are between 10 kHz to 54
--     MHz.
--
-- 'centerFrequency', 'spectrumConfig_centerFrequency' - Center frequency of a spectral @Config@. Valid values are between 2200
-- to 2300 MHz and 7750 to 8400 MHz for downlink and 2025 to 2120 MHz for
-- uplink.
newSpectrumConfig ::
  -- | 'bandwidth'
  FrequencyBandwidth ->
  -- | 'centerFrequency'
  Frequency ->
  SpectrumConfig
newSpectrumConfig pBandwidth_ pCenterFrequency_ =
  SpectrumConfig'
    { polarization = Prelude.Nothing,
      bandwidth = pBandwidth_,
      centerFrequency = pCenterFrequency_
    }

-- | Polarization of a spectral @Config@. Capturing both @\"RIGHT_HAND\"@ and
-- @\"LEFT_HAND\"@ polarization requires two separate configs.
spectrumConfig_polarization :: Lens.Lens' SpectrumConfig (Prelude.Maybe Polarization)
spectrumConfig_polarization = Lens.lens (\SpectrumConfig' {polarization} -> polarization) (\s@SpectrumConfig' {} a -> s {polarization = a} :: SpectrumConfig)

-- | Bandwidth of a spectral @Config@. AWS Ground Station currently has the
-- following bandwidth limitations:
--
-- -   For @AntennaDownlinkDemodDecodeconfig@, valid values are between 125
--     kHz to 650 MHz.
--
-- -   For @AntennaDownlinkconfig@ valid values are between 10 kHz to 54
--     MHz.
--
-- -   For @AntennaUplinkConfig@, valid values are between 10 kHz to 54
--     MHz.
spectrumConfig_bandwidth :: Lens.Lens' SpectrumConfig FrequencyBandwidth
spectrumConfig_bandwidth = Lens.lens (\SpectrumConfig' {bandwidth} -> bandwidth) (\s@SpectrumConfig' {} a -> s {bandwidth = a} :: SpectrumConfig)

-- | Center frequency of a spectral @Config@. Valid values are between 2200
-- to 2300 MHz and 7750 to 8400 MHz for downlink and 2025 to 2120 MHz for
-- uplink.
spectrumConfig_centerFrequency :: Lens.Lens' SpectrumConfig Frequency
spectrumConfig_centerFrequency = Lens.lens (\SpectrumConfig' {centerFrequency} -> centerFrequency) (\s@SpectrumConfig' {} a -> s {centerFrequency = a} :: SpectrumConfig)

instance Core.FromJSON SpectrumConfig where
  parseJSON =
    Core.withObject
      "SpectrumConfig"
      ( \x ->
          SpectrumConfig'
            Prelude.<$> (x Core..:? "polarization")
            Prelude.<*> (x Core..: "bandwidth")
            Prelude.<*> (x Core..: "centerFrequency")
      )

instance Prelude.Hashable SpectrumConfig where
  hashWithSalt salt' SpectrumConfig' {..} =
    salt' `Prelude.hashWithSalt` centerFrequency
      `Prelude.hashWithSalt` bandwidth
      `Prelude.hashWithSalt` polarization

instance Prelude.NFData SpectrumConfig where
  rnf SpectrumConfig' {..} =
    Prelude.rnf polarization
      `Prelude.seq` Prelude.rnf centerFrequency
      `Prelude.seq` Prelude.rnf bandwidth

instance Core.ToJSON SpectrumConfig where
  toJSON SpectrumConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("polarization" Core..=) Prelude.<$> polarization,
            Prelude.Just ("bandwidth" Core..= bandwidth),
            Prelude.Just
              ("centerFrequency" Core..= centerFrequency)
          ]
      )
