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
-- Module      : Amazonka.EC2.Types.OnDemandOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.OnDemandOptions where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CapacityReservationOptions
import Amazonka.EC2.Types.FleetOnDemandAllocationStrategy
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /See:/ 'newOnDemandOptions' smart constructor.
data OnDemandOptions = OnDemandOptions'
  { -- | The strategy for using unused Capacity Reservations for fulfilling
    -- On-Demand capacity. Supported only for fleets of type @instant@.
    capacityReservationOptions :: Prelude.Maybe CapacityReservationOptions,
    -- | Indicates that the fleet launches all On-Demand Instances into a single
    -- Availability Zone. Supported only for fleets of type @instant@.
    singleAvailabilityZone :: Prelude.Maybe Prelude.Bool,
    -- | The maximum amount per hour for On-Demand Instances that you\'re willing
    -- to pay.
    maxTotalPrice :: Prelude.Maybe Prelude.Text,
    -- | The minimum target capacity for On-Demand Instances in the fleet. If the
    -- minimum target capacity is not reached, the fleet launches no instances.
    minTargetCapacity :: Prelude.Maybe Prelude.Int,
    -- | Indicates that the fleet uses a single instance type to launch all
    -- On-Demand Instances in the fleet. Supported only for fleets of type
    -- @instant@.
    singleInstanceType :: Prelude.Maybe Prelude.Bool,
    -- | The order of the launch template overrides to use in fulfilling
    -- On-Demand capacity. If you specify @lowest-price@, EC2 Fleet uses price
    -- to determine the order, launching the lowest price first. If you specify
    -- @prioritized@, EC2 Fleet uses the priority that you assigned to each
    -- launch template override, launching the highest priority first. If you
    -- do not specify a value, EC2 Fleet defaults to @lowest-price@.
    allocationStrategy :: Prelude.Maybe FleetOnDemandAllocationStrategy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OnDemandOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationOptions', 'onDemandOptions_capacityReservationOptions' - The strategy for using unused Capacity Reservations for fulfilling
-- On-Demand capacity. Supported only for fleets of type @instant@.
--
-- 'singleAvailabilityZone', 'onDemandOptions_singleAvailabilityZone' - Indicates that the fleet launches all On-Demand Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
--
-- 'maxTotalPrice', 'onDemandOptions_maxTotalPrice' - The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay.
--
-- 'minTargetCapacity', 'onDemandOptions_minTargetCapacity' - The minimum target capacity for On-Demand Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- 'singleInstanceType', 'onDemandOptions_singleInstanceType' - Indicates that the fleet uses a single instance type to launch all
-- On-Demand Instances in the fleet. Supported only for fleets of type
-- @instant@.
--
-- 'allocationStrategy', 'onDemandOptions_allocationStrategy' - The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowest-price@, EC2 Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, EC2 Fleet uses the priority that you assigned to each
-- launch template override, launching the highest priority first. If you
-- do not specify a value, EC2 Fleet defaults to @lowest-price@.
newOnDemandOptions ::
  OnDemandOptions
newOnDemandOptions =
  OnDemandOptions'
    { capacityReservationOptions =
        Prelude.Nothing,
      singleAvailabilityZone = Prelude.Nothing,
      maxTotalPrice = Prelude.Nothing,
      minTargetCapacity = Prelude.Nothing,
      singleInstanceType = Prelude.Nothing,
      allocationStrategy = Prelude.Nothing
    }

-- | The strategy for using unused Capacity Reservations for fulfilling
-- On-Demand capacity. Supported only for fleets of type @instant@.
onDemandOptions_capacityReservationOptions :: Lens.Lens' OnDemandOptions (Prelude.Maybe CapacityReservationOptions)
onDemandOptions_capacityReservationOptions = Lens.lens (\OnDemandOptions' {capacityReservationOptions} -> capacityReservationOptions) (\s@OnDemandOptions' {} a -> s {capacityReservationOptions = a} :: OnDemandOptions)

-- | Indicates that the fleet launches all On-Demand Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
onDemandOptions_singleAvailabilityZone :: Lens.Lens' OnDemandOptions (Prelude.Maybe Prelude.Bool)
onDemandOptions_singleAvailabilityZone = Lens.lens (\OnDemandOptions' {singleAvailabilityZone} -> singleAvailabilityZone) (\s@OnDemandOptions' {} a -> s {singleAvailabilityZone = a} :: OnDemandOptions)

-- | The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay.
onDemandOptions_maxTotalPrice :: Lens.Lens' OnDemandOptions (Prelude.Maybe Prelude.Text)
onDemandOptions_maxTotalPrice = Lens.lens (\OnDemandOptions' {maxTotalPrice} -> maxTotalPrice) (\s@OnDemandOptions' {} a -> s {maxTotalPrice = a} :: OnDemandOptions)

-- | The minimum target capacity for On-Demand Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
onDemandOptions_minTargetCapacity :: Lens.Lens' OnDemandOptions (Prelude.Maybe Prelude.Int)
onDemandOptions_minTargetCapacity = Lens.lens (\OnDemandOptions' {minTargetCapacity} -> minTargetCapacity) (\s@OnDemandOptions' {} a -> s {minTargetCapacity = a} :: OnDemandOptions)

-- | Indicates that the fleet uses a single instance type to launch all
-- On-Demand Instances in the fleet. Supported only for fleets of type
-- @instant@.
onDemandOptions_singleInstanceType :: Lens.Lens' OnDemandOptions (Prelude.Maybe Prelude.Bool)
onDemandOptions_singleInstanceType = Lens.lens (\OnDemandOptions' {singleInstanceType} -> singleInstanceType) (\s@OnDemandOptions' {} a -> s {singleInstanceType = a} :: OnDemandOptions)

-- | The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowest-price@, EC2 Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, EC2 Fleet uses the priority that you assigned to each
-- launch template override, launching the highest priority first. If you
-- do not specify a value, EC2 Fleet defaults to @lowest-price@.
onDemandOptions_allocationStrategy :: Lens.Lens' OnDemandOptions (Prelude.Maybe FleetOnDemandAllocationStrategy)
onDemandOptions_allocationStrategy = Lens.lens (\OnDemandOptions' {allocationStrategy} -> allocationStrategy) (\s@OnDemandOptions' {} a -> s {allocationStrategy = a} :: OnDemandOptions)

instance Core.FromXML OnDemandOptions where
  parseXML x =
    OnDemandOptions'
      Prelude.<$> (x Core..@? "capacityReservationOptions")
      Prelude.<*> (x Core..@? "singleAvailabilityZone")
      Prelude.<*> (x Core..@? "maxTotalPrice")
      Prelude.<*> (x Core..@? "minTargetCapacity")
      Prelude.<*> (x Core..@? "singleInstanceType")
      Prelude.<*> (x Core..@? "allocationStrategy")

instance Prelude.Hashable OnDemandOptions where
  hashWithSalt salt' OnDemandOptions' {..} =
    salt' `Prelude.hashWithSalt` allocationStrategy
      `Prelude.hashWithSalt` singleInstanceType
      `Prelude.hashWithSalt` minTargetCapacity
      `Prelude.hashWithSalt` maxTotalPrice
      `Prelude.hashWithSalt` singleAvailabilityZone
      `Prelude.hashWithSalt` capacityReservationOptions

instance Prelude.NFData OnDemandOptions where
  rnf OnDemandOptions' {..} =
    Prelude.rnf capacityReservationOptions
      `Prelude.seq` Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf singleInstanceType
      `Prelude.seq` Prelude.rnf minTargetCapacity
      `Prelude.seq` Prelude.rnf maxTotalPrice
      `Prelude.seq` Prelude.rnf singleAvailabilityZone
