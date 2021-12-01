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
-- Module      : Amazonka.Glacier.Types.ProvisionedCapacityDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.ProvisionedCapacityDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The definition for a provisioned capacity unit.
--
-- /See:/ 'newProvisionedCapacityDescription' smart constructor.
data ProvisionedCapacityDescription = ProvisionedCapacityDescription'
  { -- | The ID that identifies the provisioned capacity unit.
    capacityId :: Prelude.Maybe Prelude.Text,
    -- | The date that the provisioned capacity unit was purchased, in Universal
    -- Coordinated Time (UTC).
    startDate :: Prelude.Maybe Prelude.Text,
    -- | The date that the provisioned capacity unit expires, in Universal
    -- Coordinated Time (UTC).
    expirationDate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisionedCapacityDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityId', 'provisionedCapacityDescription_capacityId' - The ID that identifies the provisioned capacity unit.
--
-- 'startDate', 'provisionedCapacityDescription_startDate' - The date that the provisioned capacity unit was purchased, in Universal
-- Coordinated Time (UTC).
--
-- 'expirationDate', 'provisionedCapacityDescription_expirationDate' - The date that the provisioned capacity unit expires, in Universal
-- Coordinated Time (UTC).
newProvisionedCapacityDescription ::
  ProvisionedCapacityDescription
newProvisionedCapacityDescription =
  ProvisionedCapacityDescription'
    { capacityId =
        Prelude.Nothing,
      startDate = Prelude.Nothing,
      expirationDate = Prelude.Nothing
    }

-- | The ID that identifies the provisioned capacity unit.
provisionedCapacityDescription_capacityId :: Lens.Lens' ProvisionedCapacityDescription (Prelude.Maybe Prelude.Text)
provisionedCapacityDescription_capacityId = Lens.lens (\ProvisionedCapacityDescription' {capacityId} -> capacityId) (\s@ProvisionedCapacityDescription' {} a -> s {capacityId = a} :: ProvisionedCapacityDescription)

-- | The date that the provisioned capacity unit was purchased, in Universal
-- Coordinated Time (UTC).
provisionedCapacityDescription_startDate :: Lens.Lens' ProvisionedCapacityDescription (Prelude.Maybe Prelude.Text)
provisionedCapacityDescription_startDate = Lens.lens (\ProvisionedCapacityDescription' {startDate} -> startDate) (\s@ProvisionedCapacityDescription' {} a -> s {startDate = a} :: ProvisionedCapacityDescription)

-- | The date that the provisioned capacity unit expires, in Universal
-- Coordinated Time (UTC).
provisionedCapacityDescription_expirationDate :: Lens.Lens' ProvisionedCapacityDescription (Prelude.Maybe Prelude.Text)
provisionedCapacityDescription_expirationDate = Lens.lens (\ProvisionedCapacityDescription' {expirationDate} -> expirationDate) (\s@ProvisionedCapacityDescription' {} a -> s {expirationDate = a} :: ProvisionedCapacityDescription)

instance Core.FromJSON ProvisionedCapacityDescription where
  parseJSON =
    Core.withObject
      "ProvisionedCapacityDescription"
      ( \x ->
          ProvisionedCapacityDescription'
            Prelude.<$> (x Core..:? "CapacityId")
            Prelude.<*> (x Core..:? "StartDate")
            Prelude.<*> (x Core..:? "ExpirationDate")
      )

instance
  Prelude.Hashable
    ProvisionedCapacityDescription
  where
  hashWithSalt
    salt'
    ProvisionedCapacityDescription' {..} =
      salt' `Prelude.hashWithSalt` expirationDate
        `Prelude.hashWithSalt` startDate
        `Prelude.hashWithSalt` capacityId

instance
  Prelude.NFData
    ProvisionedCapacityDescription
  where
  rnf ProvisionedCapacityDescription' {..} =
    Prelude.rnf capacityId
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf startDate
