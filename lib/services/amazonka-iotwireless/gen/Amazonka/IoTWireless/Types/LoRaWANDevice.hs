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
-- Module      : Amazonka.IoTWireless.Types.LoRaWANDevice
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.LoRaWANDevice where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types.AbpV1_0_x
import Amazonka.IoTWireless.Types.AbpV1_1
import Amazonka.IoTWireless.Types.OtaaV1_0_x
import Amazonka.IoTWireless.Types.OtaaV1_1
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | LoRaWAN object for create functions.
--
-- /See:/ 'newLoRaWANDevice' smart constructor.
data LoRaWANDevice = LoRaWANDevice'
  { -- | LoRaWAN object for create APIs
    abpV1_0_x :: Prelude.Maybe AbpV1_0_x,
    -- | OTAA device object for v1.1 for create APIs
    otaaV1_1 :: Prelude.Maybe OtaaV1_1,
    -- | The ID of the service profile.
    serviceProfileId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the device profile for the new wireless device.
    deviceProfileId :: Prelude.Maybe Prelude.Text,
    -- | OTAA device object for create APIs for v1.0.x
    otaaV1_0_x :: Prelude.Maybe OtaaV1_0_x,
    -- | ABP device object for create APIs for v1.1
    abpV1_1 :: Prelude.Maybe AbpV1_1,
    -- | The DevEUI value.
    devEui :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoRaWANDevice' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'abpV1_0_x', 'loRaWANDevice_abpV1_0_x' - LoRaWAN object for create APIs
--
-- 'otaaV1_1', 'loRaWANDevice_otaaV1_1' - OTAA device object for v1.1 for create APIs
--
-- 'serviceProfileId', 'loRaWANDevice_serviceProfileId' - The ID of the service profile.
--
-- 'deviceProfileId', 'loRaWANDevice_deviceProfileId' - The ID of the device profile for the new wireless device.
--
-- 'otaaV1_0_x', 'loRaWANDevice_otaaV1_0_x' - OTAA device object for create APIs for v1.0.x
--
-- 'abpV1_1', 'loRaWANDevice_abpV1_1' - ABP device object for create APIs for v1.1
--
-- 'devEui', 'loRaWANDevice_devEui' - The DevEUI value.
newLoRaWANDevice ::
  LoRaWANDevice
newLoRaWANDevice =
  LoRaWANDevice'
    { abpV1_0_x = Prelude.Nothing,
      otaaV1_1 = Prelude.Nothing,
      serviceProfileId = Prelude.Nothing,
      deviceProfileId = Prelude.Nothing,
      otaaV1_0_x = Prelude.Nothing,
      abpV1_1 = Prelude.Nothing,
      devEui = Prelude.Nothing
    }

-- | LoRaWAN object for create APIs
loRaWANDevice_abpV1_0_x :: Lens.Lens' LoRaWANDevice (Prelude.Maybe AbpV1_0_x)
loRaWANDevice_abpV1_0_x = Lens.lens (\LoRaWANDevice' {abpV1_0_x} -> abpV1_0_x) (\s@LoRaWANDevice' {} a -> s {abpV1_0_x = a} :: LoRaWANDevice)

-- | OTAA device object for v1.1 for create APIs
loRaWANDevice_otaaV1_1 :: Lens.Lens' LoRaWANDevice (Prelude.Maybe OtaaV1_1)
loRaWANDevice_otaaV1_1 = Lens.lens (\LoRaWANDevice' {otaaV1_1} -> otaaV1_1) (\s@LoRaWANDevice' {} a -> s {otaaV1_1 = a} :: LoRaWANDevice)

-- | The ID of the service profile.
loRaWANDevice_serviceProfileId :: Lens.Lens' LoRaWANDevice (Prelude.Maybe Prelude.Text)
loRaWANDevice_serviceProfileId = Lens.lens (\LoRaWANDevice' {serviceProfileId} -> serviceProfileId) (\s@LoRaWANDevice' {} a -> s {serviceProfileId = a} :: LoRaWANDevice)

-- | The ID of the device profile for the new wireless device.
loRaWANDevice_deviceProfileId :: Lens.Lens' LoRaWANDevice (Prelude.Maybe Prelude.Text)
loRaWANDevice_deviceProfileId = Lens.lens (\LoRaWANDevice' {deviceProfileId} -> deviceProfileId) (\s@LoRaWANDevice' {} a -> s {deviceProfileId = a} :: LoRaWANDevice)

-- | OTAA device object for create APIs for v1.0.x
loRaWANDevice_otaaV1_0_x :: Lens.Lens' LoRaWANDevice (Prelude.Maybe OtaaV1_0_x)
loRaWANDevice_otaaV1_0_x = Lens.lens (\LoRaWANDevice' {otaaV1_0_x} -> otaaV1_0_x) (\s@LoRaWANDevice' {} a -> s {otaaV1_0_x = a} :: LoRaWANDevice)

-- | ABP device object for create APIs for v1.1
loRaWANDevice_abpV1_1 :: Lens.Lens' LoRaWANDevice (Prelude.Maybe AbpV1_1)
loRaWANDevice_abpV1_1 = Lens.lens (\LoRaWANDevice' {abpV1_1} -> abpV1_1) (\s@LoRaWANDevice' {} a -> s {abpV1_1 = a} :: LoRaWANDevice)

-- | The DevEUI value.
loRaWANDevice_devEui :: Lens.Lens' LoRaWANDevice (Prelude.Maybe Prelude.Text)
loRaWANDevice_devEui = Lens.lens (\LoRaWANDevice' {devEui} -> devEui) (\s@LoRaWANDevice' {} a -> s {devEui = a} :: LoRaWANDevice)

instance Core.FromJSON LoRaWANDevice where
  parseJSON =
    Core.withObject
      "LoRaWANDevice"
      ( \x ->
          LoRaWANDevice'
            Prelude.<$> (x Core..:? "AbpV1_0_x")
            Prelude.<*> (x Core..:? "OtaaV1_1")
            Prelude.<*> (x Core..:? "ServiceProfileId")
            Prelude.<*> (x Core..:? "DeviceProfileId")
            Prelude.<*> (x Core..:? "OtaaV1_0_x")
            Prelude.<*> (x Core..:? "AbpV1_1")
            Prelude.<*> (x Core..:? "DevEui")
      )

instance Prelude.Hashable LoRaWANDevice where
  hashWithSalt salt' LoRaWANDevice' {..} =
    salt' `Prelude.hashWithSalt` devEui
      `Prelude.hashWithSalt` abpV1_1
      `Prelude.hashWithSalt` otaaV1_0_x
      `Prelude.hashWithSalt` deviceProfileId
      `Prelude.hashWithSalt` serviceProfileId
      `Prelude.hashWithSalt` otaaV1_1
      `Prelude.hashWithSalt` abpV1_0_x

instance Prelude.NFData LoRaWANDevice where
  rnf LoRaWANDevice' {..} =
    Prelude.rnf abpV1_0_x
      `Prelude.seq` Prelude.rnf devEui
      `Prelude.seq` Prelude.rnf abpV1_1
      `Prelude.seq` Prelude.rnf otaaV1_0_x
      `Prelude.seq` Prelude.rnf deviceProfileId
      `Prelude.seq` Prelude.rnf serviceProfileId
      `Prelude.seq` Prelude.rnf otaaV1_1

instance Core.ToJSON LoRaWANDevice where
  toJSON LoRaWANDevice' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AbpV1_0_x" Core..=) Prelude.<$> abpV1_0_x,
            ("OtaaV1_1" Core..=) Prelude.<$> otaaV1_1,
            ("ServiceProfileId" Core..=)
              Prelude.<$> serviceProfileId,
            ("DeviceProfileId" Core..=)
              Prelude.<$> deviceProfileId,
            ("OtaaV1_0_x" Core..=) Prelude.<$> otaaV1_0_x,
            ("AbpV1_1" Core..=) Prelude.<$> abpV1_1,
            ("DevEui" Core..=) Prelude.<$> devEui
          ]
      )
