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
-- Module      : Amazonka.Redshift.Types.ReservedNode
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ReservedNode where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.RecurringCharge
import Amazonka.Redshift.Types.ReservedNodeOfferingType

-- | Describes a reserved node. You can call the
-- DescribeReservedNodeOfferings API to obtain the available reserved node
-- offerings.
--
-- /See:/ 'newReservedNode' smart constructor.
data ReservedNode = ReservedNode'
  { reservedNodeOfferingType :: Prelude.Maybe ReservedNodeOfferingType,
    -- | The state of the reserved compute node.
    --
    -- Possible Values:
    --
    -- -   pending-payment-This reserved node has recently been purchased, and
    --     the sale has been approved, but payment has not yet been confirmed.
    --
    -- -   active-This reserved node is owned by the caller and is available
    --     for use.
    --
    -- -   payment-failed-Payment failed for the purchase attempt.
    --
    -- -   retired-The reserved node is no longer available.
    --
    -- -   exchanging-The owner is exchanging the reserved node for another
    --     reserved node.
    state :: Prelude.Maybe Prelude.Text,
    -- | The currency code for the reserved cluster.
    currencyCode :: Prelude.Maybe Prelude.Text,
    -- | The time the reservation started. You purchase a reserved node offering
    -- for a duration. This is the start time of that duration.
    startTime :: Prelude.Maybe Core.ISO8601,
    -- | The number of reserved compute nodes.
    nodeCount :: Prelude.Maybe Prelude.Int,
    -- | The unique identifier for the reservation.
    reservedNodeId :: Prelude.Maybe Prelude.Text,
    -- | The identifier for the reserved node offering.
    reservedNodeOfferingId :: Prelude.Maybe Prelude.Text,
    -- | The recurring charges for the reserved node.
    recurringCharges :: Prelude.Maybe [RecurringCharge],
    -- | The anticipated utilization of the reserved node, as defined in the
    -- reserved node offering.
    offeringType :: Prelude.Maybe Prelude.Text,
    -- | The hourly rate Amazon Redshift charges you for this reserved node.
    usagePrice :: Prelude.Maybe Prelude.Double,
    -- | The node type of the reserved node.
    nodeType :: Prelude.Maybe Prelude.Text,
    -- | The fixed cost Amazon Redshift charges you for this reserved node.
    fixedPrice :: Prelude.Maybe Prelude.Double,
    -- | The duration of the node reservation in seconds.
    duration :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReservedNode' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reservedNodeOfferingType', 'reservedNode_reservedNodeOfferingType' -
--
-- 'state', 'reservedNode_state' - The state of the reserved compute node.
--
-- Possible Values:
--
-- -   pending-payment-This reserved node has recently been purchased, and
--     the sale has been approved, but payment has not yet been confirmed.
--
-- -   active-This reserved node is owned by the caller and is available
--     for use.
--
-- -   payment-failed-Payment failed for the purchase attempt.
--
-- -   retired-The reserved node is no longer available.
--
-- -   exchanging-The owner is exchanging the reserved node for another
--     reserved node.
--
-- 'currencyCode', 'reservedNode_currencyCode' - The currency code for the reserved cluster.
--
-- 'startTime', 'reservedNode_startTime' - The time the reservation started. You purchase a reserved node offering
-- for a duration. This is the start time of that duration.
--
-- 'nodeCount', 'reservedNode_nodeCount' - The number of reserved compute nodes.
--
-- 'reservedNodeId', 'reservedNode_reservedNodeId' - The unique identifier for the reservation.
--
-- 'reservedNodeOfferingId', 'reservedNode_reservedNodeOfferingId' - The identifier for the reserved node offering.
--
-- 'recurringCharges', 'reservedNode_recurringCharges' - The recurring charges for the reserved node.
--
-- 'offeringType', 'reservedNode_offeringType' - The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
--
-- 'usagePrice', 'reservedNode_usagePrice' - The hourly rate Amazon Redshift charges you for this reserved node.
--
-- 'nodeType', 'reservedNode_nodeType' - The node type of the reserved node.
--
-- 'fixedPrice', 'reservedNode_fixedPrice' - The fixed cost Amazon Redshift charges you for this reserved node.
--
-- 'duration', 'reservedNode_duration' - The duration of the node reservation in seconds.
newReservedNode ::
  ReservedNode
newReservedNode =
  ReservedNode'
    { reservedNodeOfferingType =
        Prelude.Nothing,
      state = Prelude.Nothing,
      currencyCode = Prelude.Nothing,
      startTime = Prelude.Nothing,
      nodeCount = Prelude.Nothing,
      reservedNodeId = Prelude.Nothing,
      reservedNodeOfferingId = Prelude.Nothing,
      recurringCharges = Prelude.Nothing,
      offeringType = Prelude.Nothing,
      usagePrice = Prelude.Nothing,
      nodeType = Prelude.Nothing,
      fixedPrice = Prelude.Nothing,
      duration = Prelude.Nothing
    }

-- |
reservedNode_reservedNodeOfferingType :: Lens.Lens' ReservedNode (Prelude.Maybe ReservedNodeOfferingType)
reservedNode_reservedNodeOfferingType = Lens.lens (\ReservedNode' {reservedNodeOfferingType} -> reservedNodeOfferingType) (\s@ReservedNode' {} a -> s {reservedNodeOfferingType = a} :: ReservedNode)

-- | The state of the reserved compute node.
--
-- Possible Values:
--
-- -   pending-payment-This reserved node has recently been purchased, and
--     the sale has been approved, but payment has not yet been confirmed.
--
-- -   active-This reserved node is owned by the caller and is available
--     for use.
--
-- -   payment-failed-Payment failed for the purchase attempt.
--
-- -   retired-The reserved node is no longer available.
--
-- -   exchanging-The owner is exchanging the reserved node for another
--     reserved node.
reservedNode_state :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_state = Lens.lens (\ReservedNode' {state} -> state) (\s@ReservedNode' {} a -> s {state = a} :: ReservedNode)

-- | The currency code for the reserved cluster.
reservedNode_currencyCode :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_currencyCode = Lens.lens (\ReservedNode' {currencyCode} -> currencyCode) (\s@ReservedNode' {} a -> s {currencyCode = a} :: ReservedNode)

-- | The time the reservation started. You purchase a reserved node offering
-- for a duration. This is the start time of that duration.
reservedNode_startTime :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.UTCTime)
reservedNode_startTime = Lens.lens (\ReservedNode' {startTime} -> startTime) (\s@ReservedNode' {} a -> s {startTime = a} :: ReservedNode) Prelude.. Lens.mapping Core._Time

-- | The number of reserved compute nodes.
reservedNode_nodeCount :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Int)
reservedNode_nodeCount = Lens.lens (\ReservedNode' {nodeCount} -> nodeCount) (\s@ReservedNode' {} a -> s {nodeCount = a} :: ReservedNode)

-- | The unique identifier for the reservation.
reservedNode_reservedNodeId :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_reservedNodeId = Lens.lens (\ReservedNode' {reservedNodeId} -> reservedNodeId) (\s@ReservedNode' {} a -> s {reservedNodeId = a} :: ReservedNode)

-- | The identifier for the reserved node offering.
reservedNode_reservedNodeOfferingId :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_reservedNodeOfferingId = Lens.lens (\ReservedNode' {reservedNodeOfferingId} -> reservedNodeOfferingId) (\s@ReservedNode' {} a -> s {reservedNodeOfferingId = a} :: ReservedNode)

-- | The recurring charges for the reserved node.
reservedNode_recurringCharges :: Lens.Lens' ReservedNode (Prelude.Maybe [RecurringCharge])
reservedNode_recurringCharges = Lens.lens (\ReservedNode' {recurringCharges} -> recurringCharges) (\s@ReservedNode' {} a -> s {recurringCharges = a} :: ReservedNode) Prelude.. Lens.mapping Lens.coerced

-- | The anticipated utilization of the reserved node, as defined in the
-- reserved node offering.
reservedNode_offeringType :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_offeringType = Lens.lens (\ReservedNode' {offeringType} -> offeringType) (\s@ReservedNode' {} a -> s {offeringType = a} :: ReservedNode)

-- | The hourly rate Amazon Redshift charges you for this reserved node.
reservedNode_usagePrice :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Double)
reservedNode_usagePrice = Lens.lens (\ReservedNode' {usagePrice} -> usagePrice) (\s@ReservedNode' {} a -> s {usagePrice = a} :: ReservedNode)

-- | The node type of the reserved node.
reservedNode_nodeType :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Text)
reservedNode_nodeType = Lens.lens (\ReservedNode' {nodeType} -> nodeType) (\s@ReservedNode' {} a -> s {nodeType = a} :: ReservedNode)

-- | The fixed cost Amazon Redshift charges you for this reserved node.
reservedNode_fixedPrice :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Double)
reservedNode_fixedPrice = Lens.lens (\ReservedNode' {fixedPrice} -> fixedPrice) (\s@ReservedNode' {} a -> s {fixedPrice = a} :: ReservedNode)

-- | The duration of the node reservation in seconds.
reservedNode_duration :: Lens.Lens' ReservedNode (Prelude.Maybe Prelude.Int)
reservedNode_duration = Lens.lens (\ReservedNode' {duration} -> duration) (\s@ReservedNode' {} a -> s {duration = a} :: ReservedNode)

instance Core.FromXML ReservedNode where
  parseXML x =
    ReservedNode'
      Prelude.<$> (x Core..@? "ReservedNodeOfferingType")
      Prelude.<*> (x Core..@? "State")
      Prelude.<*> (x Core..@? "CurrencyCode")
      Prelude.<*> (x Core..@? "StartTime")
      Prelude.<*> (x Core..@? "NodeCount")
      Prelude.<*> (x Core..@? "ReservedNodeId")
      Prelude.<*> (x Core..@? "ReservedNodeOfferingId")
      Prelude.<*> ( x Core..@? "RecurringCharges"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "RecurringCharge")
                  )
      Prelude.<*> (x Core..@? "OfferingType")
      Prelude.<*> (x Core..@? "UsagePrice")
      Prelude.<*> (x Core..@? "NodeType")
      Prelude.<*> (x Core..@? "FixedPrice")
      Prelude.<*> (x Core..@? "Duration")

instance Prelude.Hashable ReservedNode where
  hashWithSalt salt' ReservedNode' {..} =
    salt' `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` fixedPrice
      `Prelude.hashWithSalt` nodeType
      `Prelude.hashWithSalt` usagePrice
      `Prelude.hashWithSalt` offeringType
      `Prelude.hashWithSalt` recurringCharges
      `Prelude.hashWithSalt` reservedNodeOfferingId
      `Prelude.hashWithSalt` reservedNodeId
      `Prelude.hashWithSalt` nodeCount
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` currencyCode
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` reservedNodeOfferingType

instance Prelude.NFData ReservedNode where
  rnf ReservedNode' {..} =
    Prelude.rnf reservedNodeOfferingType
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf fixedPrice
      `Prelude.seq` Prelude.rnf nodeType
      `Prelude.seq` Prelude.rnf usagePrice
      `Prelude.seq` Prelude.rnf offeringType
      `Prelude.seq` Prelude.rnf recurringCharges
      `Prelude.seq` Prelude.rnf reservedNodeOfferingId
      `Prelude.seq` Prelude.rnf reservedNodeId
      `Prelude.seq` Prelude.rnf nodeCount
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf currencyCode
      `Prelude.seq` Prelude.rnf state
