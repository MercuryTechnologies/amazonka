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
-- Module      : Amazonka.AlexaBusiness.Types.Device
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.Device where

import Amazonka.AlexaBusiness.Types.DeviceNetworkProfileInfo
import Amazonka.AlexaBusiness.Types.DeviceStatus
import Amazonka.AlexaBusiness.Types.DeviceStatusInfo
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A device with attributes.
--
-- /See:/ 'newDevice' smart constructor.
data Device = Device'
  { -- | The status of a device. If the status is not READY, check the
    -- DeviceStatusInfo value for details.
    deviceStatus :: Prelude.Maybe DeviceStatus,
    -- | Detailed information about a device\'s status.
    deviceStatusInfo :: Prelude.Maybe DeviceStatusInfo,
    -- | The ARN of a device.
    deviceArn :: Prelude.Maybe Prelude.Text,
    -- | The MAC address of a device.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The name of a device.
    deviceName :: Prelude.Maybe Prelude.Text,
    -- | The room ARN of a device.
    roomArn :: Prelude.Maybe Prelude.Text,
    -- | The software version of a device.
    softwareVersion :: Prelude.Maybe Prelude.Text,
    -- | The type of a device.
    deviceType :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about a device\'s network profile.
    networkProfileInfo :: Prelude.Maybe DeviceNetworkProfileInfo,
    -- | The serial number of a device.
    deviceSerialNumber :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Device' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deviceStatus', 'device_deviceStatus' - The status of a device. If the status is not READY, check the
-- DeviceStatusInfo value for details.
--
-- 'deviceStatusInfo', 'device_deviceStatusInfo' - Detailed information about a device\'s status.
--
-- 'deviceArn', 'device_deviceArn' - The ARN of a device.
--
-- 'macAddress', 'device_macAddress' - The MAC address of a device.
--
-- 'deviceName', 'device_deviceName' - The name of a device.
--
-- 'roomArn', 'device_roomArn' - The room ARN of a device.
--
-- 'softwareVersion', 'device_softwareVersion' - The software version of a device.
--
-- 'deviceType', 'device_deviceType' - The type of a device.
--
-- 'networkProfileInfo', 'device_networkProfileInfo' - Detailed information about a device\'s network profile.
--
-- 'deviceSerialNumber', 'device_deviceSerialNumber' - The serial number of a device.
newDevice ::
  Device
newDevice =
  Device'
    { deviceStatus = Prelude.Nothing,
      deviceStatusInfo = Prelude.Nothing,
      deviceArn = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      deviceName = Prelude.Nothing,
      roomArn = Prelude.Nothing,
      softwareVersion = Prelude.Nothing,
      deviceType = Prelude.Nothing,
      networkProfileInfo = Prelude.Nothing,
      deviceSerialNumber = Prelude.Nothing
    }

-- | The status of a device. If the status is not READY, check the
-- DeviceStatusInfo value for details.
device_deviceStatus :: Lens.Lens' Device (Prelude.Maybe DeviceStatus)
device_deviceStatus = Lens.lens (\Device' {deviceStatus} -> deviceStatus) (\s@Device' {} a -> s {deviceStatus = a} :: Device)

-- | Detailed information about a device\'s status.
device_deviceStatusInfo :: Lens.Lens' Device (Prelude.Maybe DeviceStatusInfo)
device_deviceStatusInfo = Lens.lens (\Device' {deviceStatusInfo} -> deviceStatusInfo) (\s@Device' {} a -> s {deviceStatusInfo = a} :: Device)

-- | The ARN of a device.
device_deviceArn :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceArn = Lens.lens (\Device' {deviceArn} -> deviceArn) (\s@Device' {} a -> s {deviceArn = a} :: Device)

-- | The MAC address of a device.
device_macAddress :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_macAddress = Lens.lens (\Device' {macAddress} -> macAddress) (\s@Device' {} a -> s {macAddress = a} :: Device)

-- | The name of a device.
device_deviceName :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceName = Lens.lens (\Device' {deviceName} -> deviceName) (\s@Device' {} a -> s {deviceName = a} :: Device)

-- | The room ARN of a device.
device_roomArn :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_roomArn = Lens.lens (\Device' {roomArn} -> roomArn) (\s@Device' {} a -> s {roomArn = a} :: Device)

-- | The software version of a device.
device_softwareVersion :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_softwareVersion = Lens.lens (\Device' {softwareVersion} -> softwareVersion) (\s@Device' {} a -> s {softwareVersion = a} :: Device)

-- | The type of a device.
device_deviceType :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceType = Lens.lens (\Device' {deviceType} -> deviceType) (\s@Device' {} a -> s {deviceType = a} :: Device)

-- | Detailed information about a device\'s network profile.
device_networkProfileInfo :: Lens.Lens' Device (Prelude.Maybe DeviceNetworkProfileInfo)
device_networkProfileInfo = Lens.lens (\Device' {networkProfileInfo} -> networkProfileInfo) (\s@Device' {} a -> s {networkProfileInfo = a} :: Device)

-- | The serial number of a device.
device_deviceSerialNumber :: Lens.Lens' Device (Prelude.Maybe Prelude.Text)
device_deviceSerialNumber = Lens.lens (\Device' {deviceSerialNumber} -> deviceSerialNumber) (\s@Device' {} a -> s {deviceSerialNumber = a} :: Device)

instance Core.FromJSON Device where
  parseJSON =
    Core.withObject
      "Device"
      ( \x ->
          Device'
            Prelude.<$> (x Core..:? "DeviceStatus")
            Prelude.<*> (x Core..:? "DeviceStatusInfo")
            Prelude.<*> (x Core..:? "DeviceArn")
            Prelude.<*> (x Core..:? "MacAddress")
            Prelude.<*> (x Core..:? "DeviceName")
            Prelude.<*> (x Core..:? "RoomArn")
            Prelude.<*> (x Core..:? "SoftwareVersion")
            Prelude.<*> (x Core..:? "DeviceType")
            Prelude.<*> (x Core..:? "NetworkProfileInfo")
            Prelude.<*> (x Core..:? "DeviceSerialNumber")
      )

instance Prelude.Hashable Device where
  hashWithSalt salt' Device' {..} =
    salt' `Prelude.hashWithSalt` deviceSerialNumber
      `Prelude.hashWithSalt` networkProfileInfo
      `Prelude.hashWithSalt` deviceType
      `Prelude.hashWithSalt` softwareVersion
      `Prelude.hashWithSalt` roomArn
      `Prelude.hashWithSalt` deviceName
      `Prelude.hashWithSalt` macAddress
      `Prelude.hashWithSalt` deviceArn
      `Prelude.hashWithSalt` deviceStatusInfo
      `Prelude.hashWithSalt` deviceStatus

instance Prelude.NFData Device where
  rnf Device' {..} =
    Prelude.rnf deviceStatus
      `Prelude.seq` Prelude.rnf deviceSerialNumber
      `Prelude.seq` Prelude.rnf networkProfileInfo
      `Prelude.seq` Prelude.rnf deviceType
      `Prelude.seq` Prelude.rnf softwareVersion
      `Prelude.seq` Prelude.rnf roomArn
      `Prelude.seq` Prelude.rnf deviceName
      `Prelude.seq` Prelude.rnf macAddress
      `Prelude.seq` Prelude.rnf deviceArn
      `Prelude.seq` Prelude.rnf deviceStatusInfo
