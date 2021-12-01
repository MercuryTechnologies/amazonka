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
-- Module      : Amazonka.OpenSearch.Types.ReservedInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.ReservedInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpenSearch.Types.OpenSearchPartitionInstanceType
import Amazonka.OpenSearch.Types.RecurringCharge
import Amazonka.OpenSearch.Types.ReservedInstancePaymentOption
import qualified Amazonka.Prelude as Prelude

-- | Details of a reserved OpenSearch instance.
--
-- /See:/ 'newReservedInstance' smart constructor.
data ReservedInstance = ReservedInstance'
  { -- | The state of the reserved OpenSearch instance.
    state :: Prelude.Maybe Prelude.Text,
    -- | The currency code for the reserved OpenSearch instance offering.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The number of OpenSearch instances that have been reserved.
    instanceCount :: Prelude.Maybe Prelude.Int,
    -- | The time the reservation started.
    startTime :: Prelude.Maybe Core.POSIX,
    -- | The OpenSearch instance type offered by the reserved instance offering.
    instanceType :: Prelude.Maybe OpenSearchPartitionInstanceType,
    -- | The customer-specified identifier to track this reservation.
    reservationName :: Prelude.Maybe Prelude.Text,
    billingSubscriptionId :: Prelude.Maybe Prelude.Integer,
    -- | The charge to your account regardless of whether you are creating any
    -- domains using the instance offering.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The rate you are charged for each hour for the domain that is using this
    -- reserved instance.
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | The unique identifier for the reservation.
    reservedInstanceId :: Prelude.Maybe Prelude.Text,
    -- | The offering identifier.
    reservedInstanceOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The upfront fixed charge you will paid to purchase the specific reserved
    -- OpenSearch instance offering.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The duration, in seconds, for which the OpenSearch instance is reserved.
    duration :: Prelude.Maybe Prelude.Int,
    -- | The payment option as defined in the reserved OpenSearch instance
    -- offering.
    paymentOption :: Prelude.Maybe ReservedInstancePaymentOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'reservedInstance_state' - The state of the reserved OpenSearch instance.
--
-- 'currencyCode', 'reservedInstance_currencyCode' - The currency code for the reserved OpenSearch instance offering.
--
-- 'instanceCount', 'reservedInstance_instanceCount' - The number of OpenSearch instances that have been reserved.
--
-- 'startTime', 'reservedInstance_startTime' - The time the reservation started.
--
-- 'instanceType', 'reservedInstance_instanceType' - The OpenSearch instance type offered by the reserved instance offering.
--
-- 'reservationName', 'reservedInstance_reservationName' - The customer-specified identifier to track this reservation.
--
-- 'billingSubscriptionId', 'reservedInstance_billingSubscriptionId' - Undocumented member.
--
-- 'recurringCharges', 'reservedInstance_recurringCharges' - The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
--
-- 'usagePrice', 'reservedInstance_usagePrice' - The rate you are charged for each hour for the domain that is using this
-- reserved instance.
--
-- 'reservedInstanceId', 'reservedInstance_reservedInstanceId' - The unique identifier for the reservation.
--
-- 'reservedInstanceOfferingId', 'reservedInstance_reservedInstanceOfferingId' - The offering identifier.
--
-- 'fixedPrice', 'reservedInstance_fixedPrice' - The upfront fixed charge you will paid to purchase the specific reserved
-- OpenSearch instance offering.
--
-- 'duration', 'reservedInstance_duration' - The duration, in seconds, for which the OpenSearch instance is reserved.
--
-- 'paymentOption', 'reservedInstance_paymentOption' - The payment option as defined in the reserved OpenSearch instance
-- offering.
newReservedInstance ::
  ReservedInstance
newReservedInstance =
  ReservedInstance'
    { state = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      instanceCount = Prelude.Nothing,
      startTime = Prelude.Nothing,
      instanceType = Prelude.Nothing,
      reservationName = Prelude.Nothing,
      billingSubscriptionId = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      reservedInstanceId = Prelude.Nothing,
      reservedInstanceOfferingId = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      duration = Prelude.Nothing,
      paymentOption = Prelude.Nothing
    }

-- | The state of the reserved OpenSearch instance.
reservedInstance_state :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Text)
reservedInstance_state = Lens.lens (\ReservedInstance' {state} -> state) (\s@ReservedInstance' {} a -> s {state = a} :: ReservedInstance)

-- | The currency code for the reserved OpenSearch instance offering.
reservedInstance_currencyCode :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Text)
reservedInstance_currencyCode = Lens.lens (\ReservedInstance' {currencyCode} -> currencyCode) (\s@ReservedInstance' {} a -> s {currencyCode = a} :: ReservedInstance)

-- | The number of OpenSearch instances that have been reserved.
reservedInstance_instanceCount :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Int)
reservedInstance_instanceCount = Lens.lens (\ReservedInstance' {instanceCount} -> instanceCount) (\s@ReservedInstance' {} a -> s {instanceCount = a} :: ReservedInstance)

-- | The time the reservation started.
reservedInstance_startTime :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.UTCTime)
reservedInstance_startTime = Lens.lens (\ReservedInstance' {startTime} -> startTime) (\s@ReservedInstance' {} a -> s {startTime = a} :: ReservedInstance) Prelude.. Lens.mapping Core._Time

-- | The OpenSearch instance type offered by the reserved instance offering.
reservedInstance_instanceType :: Lens.Lens' ReservedInstance (Prelude.Maybe OpenSearchPartitionInstanceType)
reservedInstance_instanceType = Lens.lens (\ReservedInstance' {instanceType} -> instanceType) (\s@ReservedInstance' {} a -> s {instanceType = a} :: ReservedInstance)

-- | The customer-specified identifier to track this reservation.
reservedInstance_reservationName :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Text)
reservedInstance_reservationName = Lens.lens (\ReservedInstance' {reservationName} -> reservationName) (\s@ReservedInstance' {} a -> s {reservationName = a} :: ReservedInstance)

-- | Undocumented member.
reservedInstance_billingSubscriptionId :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Integer)
reservedInstance_billingSubscriptionId = Lens.lens (\ReservedInstance' {billingSubscriptionId} -> billingSubscriptionId) (\s@ReservedInstance' {} a -> s {billingSubscriptionId = a} :: ReservedInstance)

-- | The charge to your account regardless of whether you are creating any
-- domains using the instance offering.
reservedInstance_recurringCharges :: Lens.Lens' ReservedInstance (Prelude.Maybe [RecurringCharge])
reservedInstance_recurringCharges = Lens.lens (\ReservedInstance' {recurringCharges} -> recurringCharges) (\s@ReservedInstance' {} a -> s {recurringCharges = a} :: ReservedInstance) Prelude.. Lens.mapping Lens.coerced

-- | The rate you are charged for each hour for the domain that is using this
-- reserved instance.
reservedInstance_usagePrice :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Double)
reservedInstance_usagePrice = Lens.lens (\ReservedInstance' {usagePrice} -> usagePrice) (\s@ReservedInstance' {} a -> s {usagePrice = a} :: ReservedInstance)

-- | The unique identifier for the reservation.
reservedInstance_reservedInstanceId :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Text)
reservedInstance_reservedInstanceId = Lens.lens (\ReservedInstance' {reservedInstanceId} -> reservedInstanceId) (\s@ReservedInstance' {} a -> s {reservedInstanceId = a} :: ReservedInstance)

-- | The offering identifier.
reservedInstance_reservedInstanceOfferingId :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Text)
reservedInstance_reservedInstanceOfferingId = Lens.lens (\ReservedInstance' {reservedInstanceOfferingId} -> reservedInstanceOfferingId) (\s@ReservedInstance' {} a -> s {reservedInstanceOfferingId = a} :: ReservedInstance)

-- | The upfront fixed charge you will paid to purchase the specific reserved
-- OpenSearch instance offering.
reservedInstance_fixedPrice :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Double)
reservedInstance_fixedPrice = Lens.lens (\ReservedInstance' {fixedPrice} -> fixedPrice) (\s@ReservedInstance' {} a -> s {fixedPrice = a} :: ReservedInstance)

-- | The duration, in seconds, for which the OpenSearch instance is reserved.
reservedInstance_duration :: Lens.Lens' ReservedInstance (Prelude.Maybe Prelude.Int)
reservedInstance_duration = Lens.lens (\ReservedInstance' {duration} -> duration) (\s@ReservedInstance' {} a -> s {duration = a} :: ReservedInstance)

-- | The payment option as defined in the reserved OpenSearch instance
-- offering.
reservedInstance_paymentOption :: Lens.Lens' ReservedInstance (Prelude.Maybe ReservedInstancePaymentOption)
reservedInstance_paymentOption = Lens.lens (\ReservedInstance' {paymentOption} -> paymentOption) (\s@ReservedInstance' {} a -> s {paymentOption = a} :: ReservedInstance)

instance Core.FromJSON ReservedInstance where
  parseJSON =
    Core.withObject
      "ReservedInstance"
      ( \x ->
          ReservedInstance'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "CurrencyCode")
            Prelude.<*> (x Core..:? "InstanceCount")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "InstanceType")
            Prelude.<*> (x Core..:? "ReservationName")
            Prelude.<*> (x Core..:? "BillingSubscriptionId")
            Prelude.<*> ( x Core..:? "RecurringCharges"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "UsagePrice")
            Prelude.<*> (x Core..:? "ReservedInstanceId")
            Prelude.<*> (x Core..:? "ReservedInstanceOfferingId")
            Prelude.<*> (x Core..:? "FixedPrice")
            Prelude.<*> (x Core..:? "Duration")
            Prelude.<*> (x Core..:? "PaymentOption")
      )

instance Prelude.Hashable ReservedInstance where
  hashWithSalt salt' ReservedInstance' {..} =
    salt' `Prelude.hashWithSalt` paymentOption
      `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` reservedInstanceOfferingId
      `Prelude.hashWithSalt` reservedInstanceId
      `Prelude.hashWithSalt` usagePrice
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` billingSubscriptionId
      `Prelude.hashWithSalt` reservationName
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` instanceCount
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` state

instance Prelude.NFData ReservedInstance where
  rnf ReservedInstance' {..} =
    Prelude.rnf state
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf reservedInstanceOfferingId
      `Prelude.seq` Prelude.rnf reservedInstanceId
      `Prelude.seq` Prelude.rnf usagePrice
      `Prelude.seq` Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf billingSubscriptionId
      `Prelude.seq` Prelude.rnf reservationName
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf instanceCount
      `Prelude.seq` Prelude.rnf currencyCode
