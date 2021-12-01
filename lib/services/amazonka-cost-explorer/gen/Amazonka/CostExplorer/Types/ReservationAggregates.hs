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
-- Module      : Amazonka.CostExplorer.Types.ReservationAggregates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.ReservationAggregates where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The aggregated numbers for your reservation usage.
--
-- /See:/ 'newReservationAggregates' smart constructor.
data ReservationAggregates = ReservationAggregates'
  { -- | How many reservation hours that you purchased.
    purchasedHours :: Prelude.Maybe Prelude.Text,
    -- | The total number of reservation hours that you used.
    totalActualHours :: Prelude.Maybe Prelude.Text,
    -- | The percentage of reservation time that you used.
    utilizationPercentage :: Prelude.Maybe Prelude.Text,
    -- | The total cost of your reservation. It\'s amortized over the reservation
    -- period.
    totalAmortizedFee :: Prelude.Maybe Prelude.Text,
    -- | The number of Amazon EC2 reservation hours that you didn\'t use. It\'s
    -- converted to normalized units. Normalized units are available only for
    -- Amazon EC2 usage after November 11, 2017.
    unusedUnits :: Prelude.Maybe Prelude.Text,
    -- | The unrealized savings because of purchasing and using a reservation.
    unrealizedSavings :: Prelude.Maybe Prelude.Text,
    -- | The cost of unused hours for your reservation.
    rICostForUnusedHours :: Prelude.Maybe Prelude.Text,
    -- | The number of reservation hours that you didn\'t use.
    unusedHours :: Prelude.Maybe Prelude.Text,
    -- | The realized savings because of purchasing and using a reservation.
    realizedSavings :: Prelude.Maybe Prelude.Text,
    -- | The number of Amazon EC2 reservation hours that you purchased. It\'s
    -- converted to normalized units. Normalized units are available only for
    -- Amazon EC2 usage after November 11, 2017.
    purchasedUnits :: Prelude.Maybe Prelude.Text,
    -- | The upfront cost of your reservation. It\'s amortized over the
    -- reservation period.
    amortizedUpfrontFee :: Prelude.Maybe Prelude.Text,
    -- | The monthly cost of your reservation. It\'s amortized over the
    -- reservation period.
    amortizedRecurringFee :: Prelude.Maybe Prelude.Text,
    -- | The percentage of Amazon EC2 reservation time that you used. It\'s
    -- converted to normalized units. Normalized units are available only for
    -- Amazon EC2 usage after November 11, 2017.
    utilizationPercentageInUnits :: Prelude.Maybe Prelude.Text,
    -- | How much you saved due to purchasing and utilizing reservation. Amazon
    -- Web Services calculates this by subtracting @TotalAmortizedFee@ from
    -- @OnDemandCostOfRIHoursUsed@.
    netRISavings :: Prelude.Maybe Prelude.Text,
    -- | How much your reservation costs if charged On-Demand rates.
    onDemandCostOfRIHoursUsed :: Prelude.Maybe Prelude.Text,
    -- | How much you might save if you use your entire reservation.
    totalPotentialRISavings :: Prelude.Maybe Prelude.Text,
    -- | The total number of Amazon EC2 reservation hours that you used. It\'s
    -- converted to normalized units. Normalized units are available only for
    -- Amazon EC2 usage after November 11, 2017.
    totalActualUnits :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservationAggregates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'purchasedHours', 'reservationAggregates_purchasedHours' - How many reservation hours that you purchased.
--
-- 'totalActualHours', 'reservationAggregates_totalActualHours' - The total number of reservation hours that you used.
--
-- 'utilizationPercentage', 'reservationAggregates_utilizationPercentage' - The percentage of reservation time that you used.
--
-- 'totalAmortizedFee', 'reservationAggregates_totalAmortizedFee' - The total cost of your reservation. It\'s amortized over the reservation
-- period.
--
-- 'unusedUnits', 'reservationAggregates_unusedUnits' - The number of Amazon EC2 reservation hours that you didn\'t use. It\'s
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
--
-- 'unrealizedSavings', 'reservationAggregates_unrealizedSavings' - The unrealized savings because of purchasing and using a reservation.
--
-- 'rICostForUnusedHours', 'reservationAggregates_rICostForUnusedHours' - The cost of unused hours for your reservation.
--
-- 'unusedHours', 'reservationAggregates_unusedHours' - The number of reservation hours that you didn\'t use.
--
-- 'realizedSavings', 'reservationAggregates_realizedSavings' - The realized savings because of purchasing and using a reservation.
--
-- 'purchasedUnits', 'reservationAggregates_purchasedUnits' - The number of Amazon EC2 reservation hours that you purchased. It\'s
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
--
-- 'amortizedUpfrontFee', 'reservationAggregates_amortizedUpfrontFee' - The upfront cost of your reservation. It\'s amortized over the
-- reservation period.
--
-- 'amortizedRecurringFee', 'reservationAggregates_amortizedRecurringFee' - The monthly cost of your reservation. It\'s amortized over the
-- reservation period.
--
-- 'utilizationPercentageInUnits', 'reservationAggregates_utilizationPercentageInUnits' - The percentage of Amazon EC2 reservation time that you used. It\'s
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
--
-- 'netRISavings', 'reservationAggregates_netRISavings' - How much you saved due to purchasing and utilizing reservation. Amazon
-- Web Services calculates this by subtracting @TotalAmortizedFee@ from
-- @OnDemandCostOfRIHoursUsed@.
--
-- 'onDemandCostOfRIHoursUsed', 'reservationAggregates_onDemandCostOfRIHoursUsed' - How much your reservation costs if charged On-Demand rates.
--
-- 'totalPotentialRISavings', 'reservationAggregates_totalPotentialRISavings' - How much you might save if you use your entire reservation.
--
-- 'totalActualUnits', 'reservationAggregates_totalActualUnits' - The total number of Amazon EC2 reservation hours that you used. It\'s
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
newReservationAggregates ::
  ReservationAggregates
newReservationAggregates =
  ReservationAggregates'
    { purchasedHours =
        Prelude.Nothing,
      totalActualHours = Prelude.Nothing,
      utilizationPercentage = Prelude.Nothing,
      totalAmortizedFee = Prelude.Nothing,
      unusedUnits = Prelude.Nothing,
      unrealizedSavings = Prelude.Nothing,
      rICostForUnusedHours = Prelude.Nothing,
      unusedHours = Prelude.Nothing,
      realizedSavings = Prelude.Nothing,
      purchasedUnits = Prelude.Nothing,
      amortizedUpfrontFee = Prelude.Nothing,
      amortizedRecurringFee = Prelude.Nothing,
      utilizationPercentageInUnits = Prelude.Nothing,
      netRISavings = Prelude.Nothing,
      onDemandCostOfRIHoursUsed = Prelude.Nothing,
      totalPotentialRISavings = Prelude.Nothing,
      totalActualUnits = Prelude.Nothing
    }

-- | How many reservation hours that you purchased.
reservationAggregates_purchasedHours :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_purchasedHours = Lens.lens (\ReservationAggregates' {purchasedHours} -> purchasedHours) (\s@ReservationAggregates' {} a -> s {purchasedHours = a} :: ReservationAggregates)

-- | The total number of reservation hours that you used.
reservationAggregates_totalActualHours :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_totalActualHours = Lens.lens (\ReservationAggregates' {totalActualHours} -> totalActualHours) (\s@ReservationAggregates' {} a -> s {totalActualHours = a} :: ReservationAggregates)

-- | The percentage of reservation time that you used.
reservationAggregates_utilizationPercentage :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_utilizationPercentage = Lens.lens (\ReservationAggregates' {utilizationPercentage} -> utilizationPercentage) (\s@ReservationAggregates' {} a -> s {utilizationPercentage = a} :: ReservationAggregates)

-- | The total cost of your reservation. It\'s amortized over the reservation
-- period.
reservationAggregates_totalAmortizedFee :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_totalAmortizedFee = Lens.lens (\ReservationAggregates' {totalAmortizedFee} -> totalAmortizedFee) (\s@ReservationAggregates' {} a -> s {totalAmortizedFee = a} :: ReservationAggregates)

-- | The number of Amazon EC2 reservation hours that you didn\'t use. It\'s
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
reservationAggregates_unusedUnits :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_unusedUnits = Lens.lens (\ReservationAggregates' {unusedUnits} -> unusedUnits) (\s@ReservationAggregates' {} a -> s {unusedUnits = a} :: ReservationAggregates)

-- | The unrealized savings because of purchasing and using a reservation.
reservationAggregates_unrealizedSavings :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_unrealizedSavings = Lens.lens (\ReservationAggregates' {unrealizedSavings} -> unrealizedSavings) (\s@ReservationAggregates' {} a -> s {unrealizedSavings = a} :: ReservationAggregates)

-- | The cost of unused hours for your reservation.
reservationAggregates_rICostForUnusedHours :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_rICostForUnusedHours = Lens.lens (\ReservationAggregates' {rICostForUnusedHours} -> rICostForUnusedHours) (\s@ReservationAggregates' {} a -> s {rICostForUnusedHours = a} :: ReservationAggregates)

-- | The number of reservation hours that you didn\'t use.
reservationAggregates_unusedHours :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_unusedHours = Lens.lens (\ReservationAggregates' {unusedHours} -> unusedHours) (\s@ReservationAggregates' {} a -> s {unusedHours = a} :: ReservationAggregates)

-- | The realized savings because of purchasing and using a reservation.
reservationAggregates_realizedSavings :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_realizedSavings = Lens.lens (\ReservationAggregates' {realizedSavings} -> realizedSavings) (\s@ReservationAggregates' {} a -> s {realizedSavings = a} :: ReservationAggregates)

-- | The number of Amazon EC2 reservation hours that you purchased. It\'s
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
reservationAggregates_purchasedUnits :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_purchasedUnits = Lens.lens (\ReservationAggregates' {purchasedUnits} -> purchasedUnits) (\s@ReservationAggregates' {} a -> s {purchasedUnits = a} :: ReservationAggregates)

-- | The upfront cost of your reservation. It\'s amortized over the
-- reservation period.
reservationAggregates_amortizedUpfrontFee :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_amortizedUpfrontFee = Lens.lens (\ReservationAggregates' {amortizedUpfrontFee} -> amortizedUpfrontFee) (\s@ReservationAggregates' {} a -> s {amortizedUpfrontFee = a} :: ReservationAggregates)

-- | The monthly cost of your reservation. It\'s amortized over the
-- reservation period.
reservationAggregates_amortizedRecurringFee :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_amortizedRecurringFee = Lens.lens (\ReservationAggregates' {amortizedRecurringFee} -> amortizedRecurringFee) (\s@ReservationAggregates' {} a -> s {amortizedRecurringFee = a} :: ReservationAggregates)

-- | The percentage of Amazon EC2 reservation time that you used. It\'s
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
reservationAggregates_utilizationPercentageInUnits :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_utilizationPercentageInUnits = Lens.lens (\ReservationAggregates' {utilizationPercentageInUnits} -> utilizationPercentageInUnits) (\s@ReservationAggregates' {} a -> s {utilizationPercentageInUnits = a} :: ReservationAggregates)

-- | How much you saved due to purchasing and utilizing reservation. Amazon
-- Web Services calculates this by subtracting @TotalAmortizedFee@ from
-- @OnDemandCostOfRIHoursUsed@.
reservationAggregates_netRISavings :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_netRISavings = Lens.lens (\ReservationAggregates' {netRISavings} -> netRISavings) (\s@ReservationAggregates' {} a -> s {netRISavings = a} :: ReservationAggregates)

-- | How much your reservation costs if charged On-Demand rates.
reservationAggregates_onDemandCostOfRIHoursUsed :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_onDemandCostOfRIHoursUsed = Lens.lens (\ReservationAggregates' {onDemandCostOfRIHoursUsed} -> onDemandCostOfRIHoursUsed) (\s@ReservationAggregates' {} a -> s {onDemandCostOfRIHoursUsed = a} :: ReservationAggregates)

-- | How much you might save if you use your entire reservation.
reservationAggregates_totalPotentialRISavings :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_totalPotentialRISavings = Lens.lens (\ReservationAggregates' {totalPotentialRISavings} -> totalPotentialRISavings) (\s@ReservationAggregates' {} a -> s {totalPotentialRISavings = a} :: ReservationAggregates)

-- | The total number of Amazon EC2 reservation hours that you used. It\'s
-- converted to normalized units. Normalized units are available only for
-- Amazon EC2 usage after November 11, 2017.
reservationAggregates_totalActualUnits :: Lens.Lens' ReservationAggregates (Prelude.Maybe Prelude.Text)
reservationAggregates_totalActualUnits = Lens.lens (\ReservationAggregates' {totalActualUnits} -> totalActualUnits) (\s@ReservationAggregates' {} a -> s {totalActualUnits = a} :: ReservationAggregates)

instance Core.FromJSON ReservationAggregates where
  parseJSON =
    Core.withObject
      "ReservationAggregates"
      ( \x ->
          ReservationAggregates'
            Prelude.<$> (x Core..:? "PurchasedHours")
            Prelude.<*> (x Core..:? "TotalActualHours")
            Prelude.<*> (x Core..:? "UtilizationPercentage")
            Prelude.<*> (x Core..:? "TotalAmortizedFee")
            Prelude.<*> (x Core..:? "UnusedUnits")
            Prelude.<*> (x Core..:? "UnrealizedSavings")
            Prelude.<*> (x Core..:? "RICostForUnusedHours")
            Prelude.<*> (x Core..:? "UnusedHours")
            Prelude.<*> (x Core..:? "RealizedSavings")
            Prelude.<*> (x Core..:? "PurchasedUnits")
            Prelude.<*> (x Core..:? "AmortizedUpfrontFee")
            Prelude.<*> (x Core..:? "AmortizedRecurringFee")
            Prelude.<*> (x Core..:? "UtilizationPercentageInUnits")
            Prelude.<*> (x Core..:? "NetRISavings")
            Prelude.<*> (x Core..:? "OnDemandCostOfRIHoursUsed")
            Prelude.<*> (x Core..:? "TotalPotentialRISavings")
            Prelude.<*> (x Core..:? "TotalActualUnits")
      )

instance Prelude.Hashable ReservationAggregates where
  hashWithSalt salt' ReservationAggregates' {..} =
    salt' `Prelude.hashWithSalt` totalActualUnits
      `Prelude.hashWithSalt` totalPotentialRISavings
      `Prelude.hashWithSalt` onDemandCostOfRIHoursUsed
      `Prelude.hashWithSalt` netRISavings
      `Prelude.hashWithSalt` utilizationPercentageInUnits
      `Prelude.hashWithSalt` amortizedRecurringFee
      `Prelude.hashWithSalt` amortizedUpfrontFee
      `Prelude.hashWithSalt` purchasedUnits
      `Prelude.hashWithSalt` realizedSavings
      `Prelude.hashWithSalt` unusedHours
      `Prelude.hashWithSalt` rICostForUnusedHours
      `Prelude.hashWithSalt` unrealizedSavings
      `Prelude.hashWithSalt` unusedUnits
      `Prelude.hashWithSalt` totalAmortizedFee
      `Prelude.hashWithSalt` utilizationPercentage
      `Prelude.hashWithSalt` totalActualHours
      `Prelude.hashWithSalt` purchasedHours

instance Prelude.NFData ReservationAggregates where
  rnf ReservationAggregates' {..} =
    Prelude.rnf purchasedHours
      `Prelude.seq` Prelude.rnf totalActualUnits
      `Prelude.seq` Prelude.rnf totalPotentialRISavings
      `Prelude.seq` Prelude.rnf onDemandCostOfRIHoursUsed
      `Prelude.seq` Prelude.rnf netRISavings
      `Prelude.seq` Prelude.rnf utilizationPercentageInUnits
      `Prelude.seq` Prelude.rnf amortizedRecurringFee
      `Prelude.seq` Prelude.rnf amortizedUpfrontFee
      `Prelude.seq` Prelude.rnf purchasedUnits
      `Prelude.seq` Prelude.rnf realizedSavings
      `Prelude.seq` Prelude.rnf unusedHours
      `Prelude.seq` Prelude.rnf rICostForUnusedHours
      `Prelude.seq` Prelude.rnf unrealizedSavings
      `Prelude.seq` Prelude.rnf unusedUnits
      `Prelude.seq` Prelude.rnf totalAmortizedFee
      `Prelude.seq` Prelude.rnf utilizationPercentage
      `Prelude.seq` Prelude.rnf totalActualHours
