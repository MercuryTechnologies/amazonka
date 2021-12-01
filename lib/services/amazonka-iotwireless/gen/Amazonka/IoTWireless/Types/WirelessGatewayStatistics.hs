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
-- Module      : Amazonka.IoTWireless.Types.WirelessGatewayStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.WirelessGatewayStatistics where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types.LoRaWANGateway
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a wireless gateway\'s operation.
--
-- /See:/ 'newWirelessGatewayStatistics' smart constructor.
data WirelessGatewayStatistics = WirelessGatewayStatistics'
  { -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | LoRaWAN gateway info.
    loRaWAN :: Prelude.Maybe LoRaWANGateway,
    -- | The name of the resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the wireless gateway reporting the data.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the most recent uplink was received.
    lastUplinkReceivedAt :: Prelude.Maybe Prelude.Text,
    -- | The description of the resource.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WirelessGatewayStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'wirelessGatewayStatistics_arn' - The Amazon Resource Name of the resource.
--
-- 'loRaWAN', 'wirelessGatewayStatistics_loRaWAN' - LoRaWAN gateway info.
--
-- 'name', 'wirelessGatewayStatistics_name' - The name of the resource.
--
-- 'id', 'wirelessGatewayStatistics_id' - The ID of the wireless gateway reporting the data.
--
-- 'lastUplinkReceivedAt', 'wirelessGatewayStatistics_lastUplinkReceivedAt' - The date and time when the most recent uplink was received.
--
-- 'description', 'wirelessGatewayStatistics_description' - The description of the resource.
newWirelessGatewayStatistics ::
  WirelessGatewayStatistics
newWirelessGatewayStatistics =
  WirelessGatewayStatistics'
    { arn = Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUplinkReceivedAt = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The Amazon Resource Name of the resource.
wirelessGatewayStatistics_arn :: Lens.Lens' WirelessGatewayStatistics (Prelude.Maybe Prelude.Text)
wirelessGatewayStatistics_arn = Lens.lens (\WirelessGatewayStatistics' {arn} -> arn) (\s@WirelessGatewayStatistics' {} a -> s {arn = a} :: WirelessGatewayStatistics)

-- | LoRaWAN gateway info.
wirelessGatewayStatistics_loRaWAN :: Lens.Lens' WirelessGatewayStatistics (Prelude.Maybe LoRaWANGateway)
wirelessGatewayStatistics_loRaWAN = Lens.lens (\WirelessGatewayStatistics' {loRaWAN} -> loRaWAN) (\s@WirelessGatewayStatistics' {} a -> s {loRaWAN = a} :: WirelessGatewayStatistics)

-- | The name of the resource.
wirelessGatewayStatistics_name :: Lens.Lens' WirelessGatewayStatistics (Prelude.Maybe Prelude.Text)
wirelessGatewayStatistics_name = Lens.lens (\WirelessGatewayStatistics' {name} -> name) (\s@WirelessGatewayStatistics' {} a -> s {name = a} :: WirelessGatewayStatistics)

-- | The ID of the wireless gateway reporting the data.
wirelessGatewayStatistics_id :: Lens.Lens' WirelessGatewayStatistics (Prelude.Maybe Prelude.Text)
wirelessGatewayStatistics_id = Lens.lens (\WirelessGatewayStatistics' {id} -> id) (\s@WirelessGatewayStatistics' {} a -> s {id = a} :: WirelessGatewayStatistics)

-- | The date and time when the most recent uplink was received.
wirelessGatewayStatistics_lastUplinkReceivedAt :: Lens.Lens' WirelessGatewayStatistics (Prelude.Maybe Prelude.Text)
wirelessGatewayStatistics_lastUplinkReceivedAt = Lens.lens (\WirelessGatewayStatistics' {lastUplinkReceivedAt} -> lastUplinkReceivedAt) (\s@WirelessGatewayStatistics' {} a -> s {lastUplinkReceivedAt = a} :: WirelessGatewayStatistics)

-- | The description of the resource.
wirelessGatewayStatistics_description :: Lens.Lens' WirelessGatewayStatistics (Prelude.Maybe Prelude.Text)
wirelessGatewayStatistics_description = Lens.lens (\WirelessGatewayStatistics' {description} -> description) (\s@WirelessGatewayStatistics' {} a -> s {description = a} :: WirelessGatewayStatistics)

instance Core.FromJSON WirelessGatewayStatistics where
  parseJSON =
    Core.withObject
      "WirelessGatewayStatistics"
      ( \x ->
          WirelessGatewayStatistics'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "LoRaWAN")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "LastUplinkReceivedAt")
            Prelude.<*> (x Core..:? "Description")
      )

instance Prelude.Hashable WirelessGatewayStatistics where
  hashWithSalt salt' WirelessGatewayStatistics' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUplinkReceivedAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` loRaWAN
      `Prelude.hashWithSalt` arn

instance Prelude.NFData WirelessGatewayStatistics where
  rnf WirelessGatewayStatistics' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUplinkReceivedAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf loRaWAN
