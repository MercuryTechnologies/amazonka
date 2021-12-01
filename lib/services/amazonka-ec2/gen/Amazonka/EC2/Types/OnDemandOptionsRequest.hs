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
-- Module      : Amazonka.EC2.Types.OnDemandOptionsRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.OnDemandOptionsRequest where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CapacityReservationOptionsRequest
import Amazonka.EC2.Types.FleetOnDemandAllocationStrategy
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the configuration of On-Demand Instances in an EC2 Fleet.
--
-- /See:/ 'newOnDemandOptionsRequest' smart constructor.
data OnDemandOptionsRequest = OnDemandOptionsRequest'
  { -- | The strategy for using unused Capacity Reservations for fulfilling
    -- On-Demand capacity. Supported only for fleets of type @instant@.
    capacityReservationOptions :: Prelude.Maybe CapacityReservationOptionsRequest,
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
-- Create a value of 'OnDemandOptionsRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'capacityReservationOptions', 'onDemandOptionsRequest_capacityReservationOptions' - The strategy for using unused Capacity Reservations for fulfilling
-- On-Demand capacity. Supported only for fleets of type @instant@.
--
-- 'singleAvailabilityZone', 'onDemandOptionsRequest_singleAvailabilityZone' - Indicates that the fleet launches all On-Demand Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
--
-- 'maxTotalPrice', 'onDemandOptionsRequest_maxTotalPrice' - The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay.
--
-- 'minTargetCapacity', 'onDemandOptionsRequest_minTargetCapacity' - The minimum target capacity for On-Demand Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
--
-- 'singleInstanceType', 'onDemandOptionsRequest_singleInstanceType' - Indicates that the fleet uses a single instance type to launch all
-- On-Demand Instances in the fleet. Supported only for fleets of type
-- @instant@.
--
-- 'allocationStrategy', 'onDemandOptionsRequest_allocationStrategy' - The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowest-price@, EC2 Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, EC2 Fleet uses the priority that you assigned to each
-- launch template override, launching the highest priority first. If you
-- do not specify a value, EC2 Fleet defaults to @lowest-price@.
newOnDemandOptionsRequest ::
  OnDemandOptionsRequest
newOnDemandOptionsRequest =
  OnDemandOptionsRequest'
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
onDemandOptionsRequest_capacityReservationOptions :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe CapacityReservationOptionsRequest)
onDemandOptionsRequest_capacityReservationOptions = Lens.lens (\OnDemandOptionsRequest' {capacityReservationOptions} -> capacityReservationOptions) (\s@OnDemandOptionsRequest' {} a -> s {capacityReservationOptions = a} :: OnDemandOptionsRequest)

-- | Indicates that the fleet launches all On-Demand Instances into a single
-- Availability Zone. Supported only for fleets of type @instant@.
onDemandOptionsRequest_singleAvailabilityZone :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe Prelude.Bool)
onDemandOptionsRequest_singleAvailabilityZone = Lens.lens (\OnDemandOptionsRequest' {singleAvailabilityZone} -> singleAvailabilityZone) (\s@OnDemandOptionsRequest' {} a -> s {singleAvailabilityZone = a} :: OnDemandOptionsRequest)

-- | The maximum amount per hour for On-Demand Instances that you\'re willing
-- to pay.
onDemandOptionsRequest_maxTotalPrice :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe Prelude.Text)
onDemandOptionsRequest_maxTotalPrice = Lens.lens (\OnDemandOptionsRequest' {maxTotalPrice} -> maxTotalPrice) (\s@OnDemandOptionsRequest' {} a -> s {maxTotalPrice = a} :: OnDemandOptionsRequest)

-- | The minimum target capacity for On-Demand Instances in the fleet. If the
-- minimum target capacity is not reached, the fleet launches no instances.
onDemandOptionsRequest_minTargetCapacity :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe Prelude.Int)
onDemandOptionsRequest_minTargetCapacity = Lens.lens (\OnDemandOptionsRequest' {minTargetCapacity} -> minTargetCapacity) (\s@OnDemandOptionsRequest' {} a -> s {minTargetCapacity = a} :: OnDemandOptionsRequest)

-- | Indicates that the fleet uses a single instance type to launch all
-- On-Demand Instances in the fleet. Supported only for fleets of type
-- @instant@.
onDemandOptionsRequest_singleInstanceType :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe Prelude.Bool)
onDemandOptionsRequest_singleInstanceType = Lens.lens (\OnDemandOptionsRequest' {singleInstanceType} -> singleInstanceType) (\s@OnDemandOptionsRequest' {} a -> s {singleInstanceType = a} :: OnDemandOptionsRequest)

-- | The order of the launch template overrides to use in fulfilling
-- On-Demand capacity. If you specify @lowest-price@, EC2 Fleet uses price
-- to determine the order, launching the lowest price first. If you specify
-- @prioritized@, EC2 Fleet uses the priority that you assigned to each
-- launch template override, launching the highest priority first. If you
-- do not specify a value, EC2 Fleet defaults to @lowest-price@.
onDemandOptionsRequest_allocationStrategy :: Lens.Lens' OnDemandOptionsRequest (Prelude.Maybe FleetOnDemandAllocationStrategy)
onDemandOptionsRequest_allocationStrategy = Lens.lens (\OnDemandOptionsRequest' {allocationStrategy} -> allocationStrategy) (\s@OnDemandOptionsRequest' {} a -> s {allocationStrategy = a} :: OnDemandOptionsRequest)

instance Prelude.Hashable OnDemandOptionsRequest where
  hashWithSalt salt' OnDemandOptionsRequest' {..} =
    salt' `Prelude.hashWithSalt` allocationStrategy
      `Prelude.hashWithSalt` singleInstanceType
      `Prelude.hashWithSalt` minTargetCapacity
      `Prelude.hashWithSalt` maxTotalPrice
      `Prelude.hashWithSalt` singleAvailabilityZone
      `Prelude.hashWithSalt` capacityReservationOptions

instance Prelude.NFData OnDemandOptionsRequest where
  rnf OnDemandOptionsRequest' {..} =
    Prelude.rnf capacityReservationOptions
      `Prelude.seq` Prelude.rnf allocationStrategy
      `Prelude.seq` Prelude.rnf singleInstanceType
      `Prelude.seq` Prelude.rnf minTargetCapacity
      `Prelude.seq` Prelude.rnf maxTotalPrice
      `Prelude.seq` Prelude.rnf singleAvailabilityZone

instance Core.ToQuery OnDemandOptionsRequest where
  toQuery OnDemandOptionsRequest' {..} =
    Prelude.mconcat
      [ "CapacityReservationOptions"
          Core.=: capacityReservationOptions,
        "SingleAvailabilityZone"
          Core.=: singleAvailabilityZone,
        "MaxTotalPrice" Core.=: maxTotalPrice,
        "MinTargetCapacity" Core.=: minTargetCapacity,
        "SingleInstanceType" Core.=: singleInstanceType,
        "AllocationStrategy" Core.=: allocationStrategy
      ]
