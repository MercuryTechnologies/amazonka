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
-- Module      : Amazonka.Chime.Types.PhoneNumberOrder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.PhoneNumberOrder where

import Amazonka.Chime.Types.OrderedPhoneNumber
import Amazonka.Chime.Types.PhoneNumberOrderStatus
import Amazonka.Chime.Types.PhoneNumberProductType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The details of a phone number order created for Amazon Chime.
--
-- /See:/ 'newPhoneNumberOrder' smart constructor.
data PhoneNumberOrder = PhoneNumberOrder'
  { -- | The status of the phone number order.
    status :: Prelude.Maybe PhoneNumberOrderStatus,
    -- | The ordered phone number details, such as the phone number in E.164
    -- format and the phone number status.
    orderedPhoneNumbers :: Prelude.Maybe [OrderedPhoneNumber],
    -- | The updated phone number order time stamp, in ISO 8601 format.
    updatedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The phone number order product type.
    productType :: Prelude.Maybe PhoneNumberProductType,
    -- | The phone number order ID.
    phoneNumberOrderId :: Prelude.Maybe Prelude.Text,
    -- | The phone number order creation time stamp, in ISO 8601 format.
    createdTimestamp :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PhoneNumberOrder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'phoneNumberOrder_status' - The status of the phone number order.
--
-- 'orderedPhoneNumbers', 'phoneNumberOrder_orderedPhoneNumbers' - The ordered phone number details, such as the phone number in E.164
-- format and the phone number status.
--
-- 'updatedTimestamp', 'phoneNumberOrder_updatedTimestamp' - The updated phone number order time stamp, in ISO 8601 format.
--
-- 'productType', 'phoneNumberOrder_productType' - The phone number order product type.
--
-- 'phoneNumberOrderId', 'phoneNumberOrder_phoneNumberOrderId' - The phone number order ID.
--
-- 'createdTimestamp', 'phoneNumberOrder_createdTimestamp' - The phone number order creation time stamp, in ISO 8601 format.
newPhoneNumberOrder ::
  PhoneNumberOrder
newPhoneNumberOrder =
  PhoneNumberOrder'
    { status = Prelude.Nothing,
      orderedPhoneNumbers = Prelude.Nothing,
      updatedTimestamp = Prelude.Nothing,
      productType = Prelude.Nothing,
      phoneNumberOrderId = Prelude.Nothing,
      createdTimestamp = Prelude.Nothing
    }

-- | The status of the phone number order.
phoneNumberOrder_status :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe PhoneNumberOrderStatus)
phoneNumberOrder_status = Lens.lens (\PhoneNumberOrder' {status} -> status) (\s@PhoneNumberOrder' {} a -> s {status = a} :: PhoneNumberOrder)

-- | The ordered phone number details, such as the phone number in E.164
-- format and the phone number status.
phoneNumberOrder_orderedPhoneNumbers :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe [OrderedPhoneNumber])
phoneNumberOrder_orderedPhoneNumbers = Lens.lens (\PhoneNumberOrder' {orderedPhoneNumbers} -> orderedPhoneNumbers) (\s@PhoneNumberOrder' {} a -> s {orderedPhoneNumbers = a} :: PhoneNumberOrder) Prelude.. Lens.mapping Lens.coerced

-- | The updated phone number order time stamp, in ISO 8601 format.
phoneNumberOrder_updatedTimestamp :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe Prelude.UTCTime)
phoneNumberOrder_updatedTimestamp = Lens.lens (\PhoneNumberOrder' {updatedTimestamp} -> updatedTimestamp) (\s@PhoneNumberOrder' {} a -> s {updatedTimestamp = a} :: PhoneNumberOrder) Prelude.. Lens.mapping Core._Time

-- | The phone number order product type.
phoneNumberOrder_productType :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe PhoneNumberProductType)
phoneNumberOrder_productType = Lens.lens (\PhoneNumberOrder' {productType} -> productType) (\s@PhoneNumberOrder' {} a -> s {productType = a} :: PhoneNumberOrder)

-- | The phone number order ID.
phoneNumberOrder_phoneNumberOrderId :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe Prelude.Text)
phoneNumberOrder_phoneNumberOrderId = Lens.lens (\PhoneNumberOrder' {phoneNumberOrderId} -> phoneNumberOrderId) (\s@PhoneNumberOrder' {} a -> s {phoneNumberOrderId = a} :: PhoneNumberOrder)

-- | The phone number order creation time stamp, in ISO 8601 format.
phoneNumberOrder_createdTimestamp :: Lens.Lens' PhoneNumberOrder (Prelude.Maybe Prelude.UTCTime)
phoneNumberOrder_createdTimestamp = Lens.lens (\PhoneNumberOrder' {createdTimestamp} -> createdTimestamp) (\s@PhoneNumberOrder' {} a -> s {createdTimestamp = a} :: PhoneNumberOrder) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON PhoneNumberOrder where
  parseJSON =
    Core.withObject
      "PhoneNumberOrder"
      ( \x ->
          PhoneNumberOrder'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> ( x Core..:? "OrderedPhoneNumbers"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "UpdatedTimestamp")
            Prelude.<*> (x Core..:? "ProductType")
            Prelude.<*> (x Core..:? "PhoneNumberOrderId")
            Prelude.<*> (x Core..:? "CreatedTimestamp")
      )

instance Prelude.Hashable PhoneNumberOrder where
  hashWithSalt salt' PhoneNumberOrder' {..} =
    salt' `Prelude.hashWithSalt` createdTimestamp
      `Prelude.hashWithSalt` phoneNumberOrderId
      `Prelude.hashWithSalt` productType
      `Prelude.hashWithSalt` updatedTimestamp
      `Prelude.hashWithSalt` orderedPhoneNumbers
      `Prelude.hashWithSalt` status

instance Prelude.NFData PhoneNumberOrder where
  rnf PhoneNumberOrder' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf phoneNumberOrderId
      `Prelude.seq` Prelude.rnf productType
      `Prelude.seq` Prelude.rnf updatedTimestamp
      `Prelude.seq` Prelude.rnf orderedPhoneNumbers
