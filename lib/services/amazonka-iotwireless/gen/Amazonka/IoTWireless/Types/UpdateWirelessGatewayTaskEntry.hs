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
-- Module      : Amazonka.IoTWireless.Types.UpdateWirelessGatewayTaskEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.UpdateWirelessGatewayTaskEntry where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types.LoRaWANUpdateGatewayTaskEntry
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | UpdateWirelessGatewayTaskEntry object.
--
-- /See:/ 'newUpdateWirelessGatewayTaskEntry' smart constructor.
data UpdateWirelessGatewayTaskEntry = UpdateWirelessGatewayTaskEntry'
  { -- | The Amazon Resource Name of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The properties that relate to the LoRaWAN wireless gateway.
    loRaWAN :: Prelude.Maybe LoRaWANUpdateGatewayTaskEntry,
    -- | The ID of the new wireless gateway task entry.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateWirelessGatewayTaskEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateWirelessGatewayTaskEntry_arn' - The Amazon Resource Name of the resource.
--
-- 'loRaWAN', 'updateWirelessGatewayTaskEntry_loRaWAN' - The properties that relate to the LoRaWAN wireless gateway.
--
-- 'id', 'updateWirelessGatewayTaskEntry_id' - The ID of the new wireless gateway task entry.
newUpdateWirelessGatewayTaskEntry ::
  UpdateWirelessGatewayTaskEntry
newUpdateWirelessGatewayTaskEntry =
  UpdateWirelessGatewayTaskEntry'
    { arn =
        Prelude.Nothing,
      loRaWAN = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The Amazon Resource Name of the resource.
updateWirelessGatewayTaskEntry_arn :: Lens.Lens' UpdateWirelessGatewayTaskEntry (Prelude.Maybe Prelude.Text)
updateWirelessGatewayTaskEntry_arn = Lens.lens (\UpdateWirelessGatewayTaskEntry' {arn} -> arn) (\s@UpdateWirelessGatewayTaskEntry' {} a -> s {arn = a} :: UpdateWirelessGatewayTaskEntry)

-- | The properties that relate to the LoRaWAN wireless gateway.
updateWirelessGatewayTaskEntry_loRaWAN :: Lens.Lens' UpdateWirelessGatewayTaskEntry (Prelude.Maybe LoRaWANUpdateGatewayTaskEntry)
updateWirelessGatewayTaskEntry_loRaWAN = Lens.lens (\UpdateWirelessGatewayTaskEntry' {loRaWAN} -> loRaWAN) (\s@UpdateWirelessGatewayTaskEntry' {} a -> s {loRaWAN = a} :: UpdateWirelessGatewayTaskEntry)

-- | The ID of the new wireless gateway task entry.
updateWirelessGatewayTaskEntry_id :: Lens.Lens' UpdateWirelessGatewayTaskEntry (Prelude.Maybe Prelude.Text)
updateWirelessGatewayTaskEntry_id = Lens.lens (\UpdateWirelessGatewayTaskEntry' {id} -> id) (\s@UpdateWirelessGatewayTaskEntry' {} a -> s {id = a} :: UpdateWirelessGatewayTaskEntry)

instance Core.FromJSON UpdateWirelessGatewayTaskEntry where
  parseJSON =
    Core.withObject
      "UpdateWirelessGatewayTaskEntry"
      ( \x ->
          UpdateWirelessGatewayTaskEntry'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "LoRaWAN")
            Prelude.<*> (x Core..:? "Id")
      )

instance
  Prelude.Hashable
    UpdateWirelessGatewayTaskEntry
  where
  hashWithSalt
    salt'
    UpdateWirelessGatewayTaskEntry' {..} =
      salt' `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` loRaWAN
        `Prelude.hashWithSalt` arn

instance
  Prelude.NFData
    UpdateWirelessGatewayTaskEntry
  where
  rnf UpdateWirelessGatewayTaskEntry' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf loRaWAN
