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
-- Module      : Amazonka.Outposts.Types.Order
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.Order where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Outposts.Types.LineItem
import Amazonka.Outposts.Types.OrderStatus
import Amazonka.Outposts.Types.PaymentOption
import qualified Amazonka.Prelude as Prelude

-- | Information about an order.
--
-- /See:/ 'newOrder' smart constructor.
data Order = Order'
  { -- | The status of the order
    status :: Prelude.Maybe OrderStatus,
    -- | The submission date for the order.
    orderSubmissionDate :: Prelude.Maybe Core.POSIX,
    -- | The line items for the order
    lineItems :: Prelude.Maybe [LineItem],
    -- | The fulfillment date of the order.
    orderFulfilledDate :: Prelude.Maybe Core.POSIX,
    -- | The ID of the order.
    orderId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Outpost.
    outpostId :: Prelude.Maybe Prelude.Text,
    -- | The payment option for the order.
    paymentOption :: Prelude.Maybe PaymentOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Order' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'order_status' - The status of the order
--
-- 'orderSubmissionDate', 'order_orderSubmissionDate' - The submission date for the order.
--
-- 'lineItems', 'order_lineItems' - The line items for the order
--
-- 'orderFulfilledDate', 'order_orderFulfilledDate' - The fulfillment date of the order.
--
-- 'orderId', 'order_orderId' - The ID of the order.
--
-- 'outpostId', 'order_outpostId' - The ID of the Outpost.
--
-- 'paymentOption', 'order_paymentOption' - The payment option for the order.
newOrder ::
  Order
newOrder =
  Order'
    { status = Prelude.Nothing,
      orderSubmissionDate = Prelude.Nothing,
      lineItems = Prelude.Nothing,
      orderFulfilledDate = Prelude.Nothing,
      orderId = Prelude.Nothing,
      outpostId = Prelude.Nothing,
      paymentOption = Prelude.Nothing
    }

-- | The status of the order
order_status :: Lens.Lens' Order (Prelude.Maybe OrderStatus)
order_status = Lens.lens (\Order' {status} -> status) (\s@Order' {} a -> s {status = a} :: Order)

-- | The submission date for the order.
order_orderSubmissionDate :: Lens.Lens' Order (Prelude.Maybe Prelude.UTCTime)
order_orderSubmissionDate = Lens.lens (\Order' {orderSubmissionDate} -> orderSubmissionDate) (\s@Order' {} a -> s {orderSubmissionDate = a} :: Order) Prelude.. Lens.mapping Core._Time

-- | The line items for the order
order_lineItems :: Lens.Lens' Order (Prelude.Maybe [LineItem])
order_lineItems = Lens.lens (\Order' {lineItems} -> lineItems) (\s@Order' {} a -> s {lineItems = a} :: Order) Prelude.. Lens.mapping Lens.coerced

-- | The fulfillment date of the order.
order_orderFulfilledDate :: Lens.Lens' Order (Prelude.Maybe Prelude.UTCTime)
order_orderFulfilledDate = Lens.lens (\Order' {orderFulfilledDate} -> orderFulfilledDate) (\s@Order' {} a -> s {orderFulfilledDate = a} :: Order) Prelude.. Lens.mapping Core._Time

-- | The ID of the order.
order_orderId :: Lens.Lens' Order (Prelude.Maybe Prelude.Text)
order_orderId = Lens.lens (\Order' {orderId} -> orderId) (\s@Order' {} a -> s {orderId = a} :: Order)

-- | The ID of the Outpost.
order_outpostId :: Lens.Lens' Order (Prelude.Maybe Prelude.Text)
order_outpostId = Lens.lens (\Order' {outpostId} -> outpostId) (\s@Order' {} a -> s {outpostId = a} :: Order)

-- | The payment option for the order.
order_paymentOption :: Lens.Lens' Order (Prelude.Maybe PaymentOption)
order_paymentOption = Lens.lens (\Order' {paymentOption} -> paymentOption) (\s@Order' {} a -> s {paymentOption = a} :: Order)

instance Core.FromJSON Order where
  parseJSON =
    Core.withObject
      "Order"
      ( \x ->
          Order'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "OrderSubmissionDate")
            Prelude.<*> (x Core..:? "LineItems" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "OrderFulfilledDate")
            Prelude.<*> (x Core..:? "OrderId")
            Prelude.<*> (x Core..:? "OutpostId")
            Prelude.<*> (x Core..:? "PaymentOption")
      )

instance Prelude.Hashable Order where
  hashWithSalt salt' Order' {..} =
    salt' `Prelude.hashWithSalt` paymentOption
      `Prelude.hashWithSalt` outpostId
      `Prelude.hashWithSalt` orderId
      `Prelude.hashWithSalt` orderFulfilledDate
      `Prelude.hashWithSalt` lineItems
      `Prelude.hashWithSalt` orderSubmissionDate
      `Prelude.hashWithSalt` status

instance Prelude.NFData Order where
  rnf Order' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf paymentOption
      `Prelude.seq` Prelude.rnf outpostId
      `Prelude.seq` Prelude.rnf orderId
      `Prelude.seq` Prelude.rnf orderFulfilledDate
      `Prelude.seq` Prelude.rnf lineItems
      `Prelude.seq` Prelude.rnf orderSubmissionDate
