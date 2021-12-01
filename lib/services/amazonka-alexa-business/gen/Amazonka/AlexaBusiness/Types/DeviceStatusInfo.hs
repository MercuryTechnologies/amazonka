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
-- Module      : Amazonka.AlexaBusiness.Types.DeviceStatusInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.DeviceStatusInfo where

import Amazonka.AlexaBusiness.Types.ConnectionStatus
import Amazonka.AlexaBusiness.Types.DeviceStatusDetail
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about a device\'s status.
--
-- /See:/ 'newDeviceStatusInfo' smart constructor.
data DeviceStatusInfo = DeviceStatusInfo'
  { -- | The time (in epoch) when the device connection status changed.
    connectionStatusUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | One or more device status detail descriptions.
    deviceStatusDetails :: Prelude.Maybe [DeviceStatusDetail],
    -- | The latest available information about the connection status of a
    -- device.
    connectionStatus :: Prelude.Maybe ConnectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeviceStatusInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionStatusUpdatedTime', 'deviceStatusInfo_connectionStatusUpdatedTime' - The time (in epoch) when the device connection status changed.
--
-- 'deviceStatusDetails', 'deviceStatusInfo_deviceStatusDetails' - One or more device status detail descriptions.
--
-- 'connectionStatus', 'deviceStatusInfo_connectionStatus' - The latest available information about the connection status of a
-- device.
newDeviceStatusInfo ::
  DeviceStatusInfo
newDeviceStatusInfo =
  DeviceStatusInfo'
    { connectionStatusUpdatedTime =
        Prelude.Nothing,
      deviceStatusDetails = Prelude.Nothing,
      connectionStatus = Prelude.Nothing
    }

-- | The time (in epoch) when the device connection status changed.
deviceStatusInfo_connectionStatusUpdatedTime :: Lens.Lens' DeviceStatusInfo (Prelude.Maybe Prelude.UTCTime)
deviceStatusInfo_connectionStatusUpdatedTime = Lens.lens (\DeviceStatusInfo' {connectionStatusUpdatedTime} -> connectionStatusUpdatedTime) (\s@DeviceStatusInfo' {} a -> s {connectionStatusUpdatedTime = a} :: DeviceStatusInfo) Prelude.. Lens.mapping Core._Time

-- | One or more device status detail descriptions.
deviceStatusInfo_deviceStatusDetails :: Lens.Lens' DeviceStatusInfo (Prelude.Maybe [DeviceStatusDetail])
deviceStatusInfo_deviceStatusDetails = Lens.lens (\DeviceStatusInfo' {deviceStatusDetails} -> deviceStatusDetails) (\s@DeviceStatusInfo' {} a -> s {deviceStatusDetails = a} :: DeviceStatusInfo) Prelude.. Lens.mapping Lens.coerced

-- | The latest available information about the connection status of a
-- device.
deviceStatusInfo_connectionStatus :: Lens.Lens' DeviceStatusInfo (Prelude.Maybe ConnectionStatus)
deviceStatusInfo_connectionStatus = Lens.lens (\DeviceStatusInfo' {connectionStatus} -> connectionStatus) (\s@DeviceStatusInfo' {} a -> s {connectionStatus = a} :: DeviceStatusInfo)

instance Core.FromJSON DeviceStatusInfo where
  parseJSON =
    Core.withObject
      "DeviceStatusInfo"
      ( \x ->
          DeviceStatusInfo'
            Prelude.<$> (x Core..:? "ConnectionStatusUpdatedTime")
            Prelude.<*> ( x Core..:? "DeviceStatusDetails"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "ConnectionStatus")
      )

instance Prelude.Hashable DeviceStatusInfo where
  hashWithSalt salt' DeviceStatusInfo' {..} =
    salt' `Prelude.hashWithSalt` connectionStatus
      `Prelude.hashWithSalt` deviceStatusDetails
      `Prelude.hashWithSalt` connectionStatusUpdatedTime

instance Prelude.NFData DeviceStatusInfo where
  rnf DeviceStatusInfo' {..} =
    Prelude.rnf connectionStatusUpdatedTime
      `Prelude.seq` Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf deviceStatusDetails
