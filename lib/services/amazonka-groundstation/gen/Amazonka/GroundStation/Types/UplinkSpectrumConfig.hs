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
-- Module      : Amazonka.GroundStation.Types.UplinkSpectrumConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.UplinkSpectrumConfig where

import qualified Amazonka.Core as Core
import Amazonka.GroundStation.Types.Frequency
import Amazonka.GroundStation.Types.Polarization
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the uplink spectral @Config@.
--
-- /See:/ 'newUplinkSpectrumConfig' smart constructor.
data UplinkSpectrumConfig = UplinkSpectrumConfig'
  { -- | Polarization of an uplink spectral @Config@. Capturing both
    -- @\"RIGHT_HAND\"@ and @\"LEFT_HAND\"@ polarization requires two separate
    -- configs.
    polarization :: Prelude.Maybe Polarization,
    -- | Center frequency of an uplink spectral @Config@. Valid values are
    -- between 2025 to 2120 MHz.
    centerFrequency :: Frequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UplinkSpectrumConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'polarization', 'uplinkSpectrumConfig_polarization' - Polarization of an uplink spectral @Config@. Capturing both
-- @\"RIGHT_HAND\"@ and @\"LEFT_HAND\"@ polarization requires two separate
-- configs.
--
-- 'centerFrequency', 'uplinkSpectrumConfig_centerFrequency' - Center frequency of an uplink spectral @Config@. Valid values are
-- between 2025 to 2120 MHz.
newUplinkSpectrumConfig ::
  -- | 'centerFrequency'
  Frequency ->
  UplinkSpectrumConfig
newUplinkSpectrumConfig pCenterFrequency_ =
  UplinkSpectrumConfig'
    { polarization =
        Prelude.Nothing,
      centerFrequency = pCenterFrequency_
    }

-- | Polarization of an uplink spectral @Config@. Capturing both
-- @\"RIGHT_HAND\"@ and @\"LEFT_HAND\"@ polarization requires two separate
-- configs.
uplinkSpectrumConfig_polarization :: Lens.Lens' UplinkSpectrumConfig (Prelude.Maybe Polarization)
uplinkSpectrumConfig_polarization = Lens.lens (\UplinkSpectrumConfig' {polarization} -> polarization) (\s@UplinkSpectrumConfig' {} a -> s {polarization = a} :: UplinkSpectrumConfig)

-- | Center frequency of an uplink spectral @Config@. Valid values are
-- between 2025 to 2120 MHz.
uplinkSpectrumConfig_centerFrequency :: Lens.Lens' UplinkSpectrumConfig Frequency
uplinkSpectrumConfig_centerFrequency = Lens.lens (\UplinkSpectrumConfig' {centerFrequency} -> centerFrequency) (\s@UplinkSpectrumConfig' {} a -> s {centerFrequency = a} :: UplinkSpectrumConfig)

instance Core.FromJSON UplinkSpectrumConfig where
  parseJSON =
    Core.withObject
      "UplinkSpectrumConfig"
      ( \x ->
          UplinkSpectrumConfig'
            Prelude.<$> (x Core..:? "polarization")
            Prelude.<*> (x Core..: "centerFrequency")
      )

instance Prelude.Hashable UplinkSpectrumConfig where
  hashWithSalt salt' UplinkSpectrumConfig' {..} =
    salt' `Prelude.hashWithSalt` centerFrequency
      `Prelude.hashWithSalt` polarization

instance Prelude.NFData UplinkSpectrumConfig where
  rnf UplinkSpectrumConfig' {..} =
    Prelude.rnf polarization
      `Prelude.seq` Prelude.rnf centerFrequency

instance Core.ToJSON UplinkSpectrumConfig where
  toJSON UplinkSpectrumConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("polarization" Core..=) Prelude.<$> polarization,
            Prelude.Just
              ("centerFrequency" Core..= centerFrequency)
          ]
      )
