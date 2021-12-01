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
-- Module      : Amazonka.MarketplaceEntitlement.Types.Entitlement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MarketplaceEntitlement.Types.Entitlement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MarketplaceEntitlement.Types.EntitlementValue
import qualified Amazonka.Prelude as Prelude

-- | An entitlement represents capacity in a product owned by the customer.
-- For example, a customer might own some number of users or seats in an
-- SaaS application or some amount of data capacity in a multi-tenant
-- database.
--
-- /See:/ 'newEntitlement' smart constructor.
data Entitlement = Entitlement'
  { -- | The dimension for which the given entitlement applies. Dimensions
    -- represent categories of capacity in a product and are specified when the
    -- product is listed in AWS Marketplace.
    dimension :: Prelude.Maybe Prelude.Text,
    -- | The EntitlementValue represents the amount of capacity that the customer
    -- is entitled to for the product.
    value :: Prelude.Maybe EntitlementValue,
    -- | The expiration date represents the minimum date through which this
    -- entitlement is expected to remain valid. For contractual products listed
    -- on AWS Marketplace, the expiration date is the date at which the
    -- customer will renew or cancel their contract. Customers who are opting
    -- to renew their contract will still have entitlements with an expiration
    -- date.
    expirationDate :: Prelude.Maybe Core.POSIX,
    -- | The customer identifier is a handle to each unique customer in an
    -- application. Customer identifiers are obtained through the
    -- ResolveCustomer operation in AWS Marketplace Metering Service.
    customerIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The product code for which the given entitlement applies. Product codes
    -- are provided by AWS Marketplace when the product listing is created.
    productCode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Entitlement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimension', 'entitlement_dimension' - The dimension for which the given entitlement applies. Dimensions
-- represent categories of capacity in a product and are specified when the
-- product is listed in AWS Marketplace.
--
-- 'value', 'entitlement_value' - The EntitlementValue represents the amount of capacity that the customer
-- is entitled to for the product.
--
-- 'expirationDate', 'entitlement_expirationDate' - The expiration date represents the minimum date through which this
-- entitlement is expected to remain valid. For contractual products listed
-- on AWS Marketplace, the expiration date is the date at which the
-- customer will renew or cancel their contract. Customers who are opting
-- to renew their contract will still have entitlements with an expiration
-- date.
--
-- 'customerIdentifier', 'entitlement_customerIdentifier' - The customer identifier is a handle to each unique customer in an
-- application. Customer identifiers are obtained through the
-- ResolveCustomer operation in AWS Marketplace Metering Service.
--
-- 'productCode', 'entitlement_productCode' - The product code for which the given entitlement applies. Product codes
-- are provided by AWS Marketplace when the product listing is created.
newEntitlement ::
  Entitlement
newEntitlement =
  Entitlement'
    { dimension = Prelude.Nothing,
      value = Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      customerIdentifier = Prelude.Nothing,
      productCode = Prelude.Nothing
    }

-- | The dimension for which the given entitlement applies. Dimensions
-- represent categories of capacity in a product and are specified when the
-- product is listed in AWS Marketplace.
entitlement_dimension :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.Text)
entitlement_dimension = Lens.lens (\Entitlement' {dimension} -> dimension) (\s@Entitlement' {} a -> s {dimension = a} :: Entitlement)

-- | The EntitlementValue represents the amount of capacity that the customer
-- is entitled to for the product.
entitlement_value :: Lens.Lens' Entitlement (Prelude.Maybe EntitlementValue)
entitlement_value = Lens.lens (\Entitlement' {value} -> value) (\s@Entitlement' {} a -> s {value = a} :: Entitlement)

-- | The expiration date represents the minimum date through which this
-- entitlement is expected to remain valid. For contractual products listed
-- on AWS Marketplace, the expiration date is the date at which the
-- customer will renew or cancel their contract. Customers who are opting
-- to renew their contract will still have entitlements with an expiration
-- date.
entitlement_expirationDate :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.UTCTime)
entitlement_expirationDate = Lens.lens (\Entitlement' {expirationDate} -> expirationDate) (\s@Entitlement' {} a -> s {expirationDate = a} :: Entitlement) Prelude.. Lens.mapping Core._Time

-- | The customer identifier is a handle to each unique customer in an
-- application. Customer identifiers are obtained through the
-- ResolveCustomer operation in AWS Marketplace Metering Service.
entitlement_customerIdentifier :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.Text)
entitlement_customerIdentifier = Lens.lens (\Entitlement' {customerIdentifier} -> customerIdentifier) (\s@Entitlement' {} a -> s {customerIdentifier = a} :: Entitlement)

-- | The product code for which the given entitlement applies. Product codes
-- are provided by AWS Marketplace when the product listing is created.
entitlement_productCode :: Lens.Lens' Entitlement (Prelude.Maybe Prelude.Text)
entitlement_productCode = Lens.lens (\Entitlement' {productCode} -> productCode) (\s@Entitlement' {} a -> s {productCode = a} :: Entitlement)

instance Core.FromJSON Entitlement where
  parseJSON =
    Core.withObject
      "Entitlement"
      ( \x ->
          Entitlement'
            Prelude.<$> (x Core..:? "Dimension")
            Prelude.<*> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "ExpirationDate")
            Prelude.<*> (x Core..:? "CustomerIdentifier")
            Prelude.<*> (x Core..:? "ProductCode")
      )

instance Prelude.Hashable Entitlement where
  hashWithSalt salt' Entitlement' {..} =
    salt' `Prelude.hashWithSalt` productCode
      `Prelude.hashWithSalt` customerIdentifier
      `Prelude.hashWithSalt` expirationDate
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` dimension

instance Prelude.NFData Entitlement where
  rnf Entitlement' {..} =
    Prelude.rnf dimension
      `Prelude.seq` Prelude.rnf productCode
      `Prelude.seq` Prelude.rnf customerIdentifier
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf value
