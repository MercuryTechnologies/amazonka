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
-- Module      : Amazonka.CloudFront.Types.TrustedSigners
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.TrustedSigners where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A list of accounts whose public keys CloudFront can use to verify the
-- signatures of signed URLs and signed cookies.
--
-- /See:/ 'newTrustedSigners' smart constructor.
data TrustedSigners = TrustedSigners'
  { -- | A list of account identifiers.
    items :: Prelude.Maybe [Prelude.Text],
    -- | This field is @true@ if any of the accounts have public keys that
    -- CloudFront can use to verify the signatures of signed URLs and signed
    -- cookies. If not, this field is @false@.
    enabled :: Prelude.Bool,
    -- | The number of accounts in the list.
    quantity :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustedSigners' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'trustedSigners_items' - A list of account identifiers.
--
-- 'enabled', 'trustedSigners_enabled' - This field is @true@ if any of the accounts have public keys that
-- CloudFront can use to verify the signatures of signed URLs and signed
-- cookies. If not, this field is @false@.
--
-- 'quantity', 'trustedSigners_quantity' - The number of accounts in the list.
newTrustedSigners ::
  -- | 'enabled'
  Prelude.Bool ->
  -- | 'quantity'
  Prelude.Int ->
  TrustedSigners
newTrustedSigners pEnabled_ pQuantity_ =
  TrustedSigners'
    { items = Prelude.Nothing,
      enabled = pEnabled_,
      quantity = pQuantity_
    }

-- | A list of account identifiers.
trustedSigners_items :: Lens.Lens' TrustedSigners (Prelude.Maybe [Prelude.Text])
trustedSigners_items = Lens.lens (\TrustedSigners' {items} -> items) (\s@TrustedSigners' {} a -> s {items = a} :: TrustedSigners) Prelude.. Lens.mapping Lens.coerced

-- | This field is @true@ if any of the accounts have public keys that
-- CloudFront can use to verify the signatures of signed URLs and signed
-- cookies. If not, this field is @false@.
trustedSigners_enabled :: Lens.Lens' TrustedSigners Prelude.Bool
trustedSigners_enabled = Lens.lens (\TrustedSigners' {enabled} -> enabled) (\s@TrustedSigners' {} a -> s {enabled = a} :: TrustedSigners)

-- | The number of accounts in the list.
trustedSigners_quantity :: Lens.Lens' TrustedSigners Prelude.Int
trustedSigners_quantity = Lens.lens (\TrustedSigners' {quantity} -> quantity) (\s@TrustedSigners' {} a -> s {quantity = a} :: TrustedSigners)

instance Core.FromXML TrustedSigners where
  parseXML x =
    TrustedSigners'
      Prelude.<$> ( x Core..@? "Items" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AwsAccountNumber")
                  )
      Prelude.<*> (x Core..@ "Enabled")
      Prelude.<*> (x Core..@ "Quantity")

instance Prelude.Hashable TrustedSigners where
  hashWithSalt salt' TrustedSigners' {..} =
    salt' `Prelude.hashWithSalt` quantity
      `Prelude.hashWithSalt` enabled
      `Prelude.hashWithSalt` items

instance Prelude.NFData TrustedSigners where
  rnf TrustedSigners' {..} =
    Prelude.rnf items
      `Prelude.seq` Prelude.rnf quantity
      `Prelude.seq` Prelude.rnf enabled

instance Core.ToXML TrustedSigners where
  toXML TrustedSigners' {..} =
    Prelude.mconcat
      [ "Items"
          Core.@= Core.toXML
            ( Core.toXMLList "AwsAccountNumber"
                Prelude.<$> items
            ),
        "Enabled" Core.@= enabled,
        "Quantity" Core.@= quantity
      ]
