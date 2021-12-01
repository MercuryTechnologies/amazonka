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
-- Module      : Amazonka.DeviceFarm.Types.OfferingStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.OfferingStatus where

import qualified Amazonka.Core as Core
import Amazonka.DeviceFarm.Types.Offering
import Amazonka.DeviceFarm.Types.OfferingTransactionType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The status of the offering.
--
-- /See:/ 'newOfferingStatus' smart constructor.
data OfferingStatus = OfferingStatus'
  { -- | The date on which the offering is effective.
    effectiveOn :: Prelude.Maybe Core.POSIX,
    -- | Represents the metadata of an offering status.
    offering :: Prelude.Maybe Offering,
    -- | The number of available devices in the offering.
    quantity :: Prelude.Maybe Prelude.Int,
    -- | The type specified for the offering status.
    type' :: Prelude.Maybe OfferingTransactionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OfferingStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effectiveOn', 'offeringStatus_effectiveOn' - The date on which the offering is effective.
--
-- 'offering', 'offeringStatus_offering' - Represents the metadata of an offering status.
--
-- 'quantity', 'offeringStatus_quantity' - The number of available devices in the offering.
--
-- 'type'', 'offeringStatus_type' - The type specified for the offering status.
newOfferingStatus ::
  OfferingStatus
newOfferingStatus =
  OfferingStatus'
    { effectiveOn = Prelude.Nothing,
      offering = Prelude.Nothing,
      quantity = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The date on which the offering is effective.
offeringStatus_effectiveOn :: Lens.Lens' OfferingStatus (Prelude.Maybe Prelude.UTCTime)
offeringStatus_effectiveOn = Lens.lens (\OfferingStatus' {effectiveOn} -> effectiveOn) (\s@OfferingStatus' {} a -> s {effectiveOn = a} :: OfferingStatus) Prelude.. Lens.mapping Core._Time

-- | Represents the metadata of an offering status.
offeringStatus_offering :: Lens.Lens' OfferingStatus (Prelude.Maybe Offering)
offeringStatus_offering = Lens.lens (\OfferingStatus' {offering} -> offering) (\s@OfferingStatus' {} a -> s {offering = a} :: OfferingStatus)

-- | The number of available devices in the offering.
offeringStatus_quantity :: Lens.Lens' OfferingStatus (Prelude.Maybe Prelude.Int)
offeringStatus_quantity = Lens.lens (\OfferingStatus' {quantity} -> quantity) (\s@OfferingStatus' {} a -> s {quantity = a} :: OfferingStatus)

-- | The type specified for the offering status.
offeringStatus_type :: Lens.Lens' OfferingStatus (Prelude.Maybe OfferingTransactionType)
offeringStatus_type = Lens.lens (\OfferingStatus' {type'} -> type') (\s@OfferingStatus' {} a -> s {type' = a} :: OfferingStatus)

instance Core.FromJSON OfferingStatus where
  parseJSON =
    Core.withObject
      "OfferingStatus"
      ( \x ->
          OfferingStatus'
            Prelude.<$> (x Core..:? "effectiveOn")
            Prelude.<*> (x Core..:? "offering")
            Prelude.<*> (x Core..:? "quantity")
            Prelude.<*> (x Core..:? "type")
      )

instance Prelude.Hashable OfferingStatus where
  hashWithSalt salt' OfferingStatus' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` quantity
      `Prelude.hashWithSalt` offering
      `Prelude.hashWithSalt` effectiveOn

instance Prelude.NFData OfferingStatus where
  rnf OfferingStatus' {..} =
    Prelude.rnf effectiveOn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf quantity
      `Prelude.seq` Prelude.rnf offering
