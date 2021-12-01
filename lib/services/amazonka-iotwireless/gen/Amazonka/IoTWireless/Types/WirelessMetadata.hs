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
-- Module      : Amazonka.IoTWireless.Types.WirelessMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessMetadata where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types.LoRaWANSendDataToDevice
import Amazonka.IoTWireless.Types.SidewalkSendDataToDevice
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | WirelessMetadata object.
--
-- /See:/ 'newWirelessMetadata' smart constructor.
data WirelessMetadata = WirelessMetadata'
  { -- | The Sidewalk account credentials.
    sidewalk :: Prelude.Maybe SidewalkSendDataToDevice,
    -- | LoRaWAN device info.
    loRaWAN :: Prelude.Maybe LoRaWANSendDataToDevice
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WirelessMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sidewalk', 'wirelessMetadata_sidewalk' - The Sidewalk account credentials.
--
-- 'loRaWAN', 'wirelessMetadata_loRaWAN' - LoRaWAN device info.
newWirelessMetadata ::
  WirelessMetadata
newWirelessMetadata =
  WirelessMetadata'
    { sidewalk = Prelude.Nothing,
      loRaWAN = Prelude.Nothing
    }

-- | The Sidewalk account credentials.
wirelessMetadata_sidewalk :: Lens.Lens' WirelessMetadata (Prelude.Maybe SidewalkSendDataToDevice)
wirelessMetadata_sidewalk = Lens.lens (\WirelessMetadata' {sidewalk} -> sidewalk) (\s@WirelessMetadata' {} a -> s {sidewalk = a} :: WirelessMetadata)

-- | LoRaWAN device info.
wirelessMetadata_loRaWAN :: Lens.Lens' WirelessMetadata (Prelude.Maybe LoRaWANSendDataToDevice)
wirelessMetadata_loRaWAN = Lens.lens (\WirelessMetadata' {loRaWAN} -> loRaWAN) (\s@WirelessMetadata' {} a -> s {loRaWAN = a} :: WirelessMetadata)

instance Prelude.Hashable WirelessMetadata where
  hashWithSalt salt' WirelessMetadata' {..} =
    salt' `Prelude.hashWithSalt` loRaWAN
      `Prelude.hashWithSalt` sidewalk

instance Prelude.NFData WirelessMetadata where
  rnf WirelessMetadata' {..} =
    Prelude.rnf sidewalk
      `Prelude.seq` Prelude.rnf loRaWAN

instance Core.ToJSON WirelessMetadata where
  toJSON WirelessMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Sidewalk" Core..=) Prelude.<$> sidewalk,
            ("LoRaWAN" Core..=) Prelude.<$> loRaWAN
          ]
      )
