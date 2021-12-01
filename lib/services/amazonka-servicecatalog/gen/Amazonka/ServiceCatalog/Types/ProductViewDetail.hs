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
-- Module      : Amazonka.ServiceCatalog.Types.ProductViewDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProductViewDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ProductViewSummary
import Amazonka.ServiceCatalog.Types.RequestStatus

-- | Information about a product view.
--
-- /See:/ 'newProductViewDetail' smart constructor.
data ProductViewDetail = ProductViewDetail'
  { -- | The status of the product.
    --
    -- -   @AVAILABLE@ - The product is ready for use.
    --
    -- -   @CREATING@ - Product creation has started; the product is not ready
    --     for use.
    --
    -- -   @FAILED@ - An action failed.
    status :: Prelude.Maybe RequestStatus,
    -- | Summary information about the product view.
    productViewSummary :: Prelude.Maybe ProductViewSummary,
    -- | The UTC time stamp of the creation time.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the product.
    productARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductViewDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'productViewDetail_status' - The status of the product.
--
-- -   @AVAILABLE@ - The product is ready for use.
--
-- -   @CREATING@ - Product creation has started; the product is not ready
--     for use.
--
-- -   @FAILED@ - An action failed.
--
-- 'productViewSummary', 'productViewDetail_productViewSummary' - Summary information about the product view.
--
-- 'createdTime', 'productViewDetail_createdTime' - The UTC time stamp of the creation time.
--
-- 'productARN', 'productViewDetail_productARN' - The ARN of the product.
newProductViewDetail ::
  ProductViewDetail
newProductViewDetail =
  ProductViewDetail'
    { status = Prelude.Nothing,
      productViewSummary = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      productARN = Prelude.Nothing
    }

-- | The status of the product.
--
-- -   @AVAILABLE@ - The product is ready for use.
--
-- -   @CREATING@ - Product creation has started; the product is not ready
--     for use.
--
-- -   @FAILED@ - An action failed.
productViewDetail_status :: Lens.Lens' ProductViewDetail (Prelude.Maybe RequestStatus)
productViewDetail_status = Lens.lens (\ProductViewDetail' {status} -> status) (\s@ProductViewDetail' {} a -> s {status = a} :: ProductViewDetail)

-- | Summary information about the product view.
productViewDetail_productViewSummary :: Lens.Lens' ProductViewDetail (Prelude.Maybe ProductViewSummary)
productViewDetail_productViewSummary = Lens.lens (\ProductViewDetail' {productViewSummary} -> productViewSummary) (\s@ProductViewDetail' {} a -> s {productViewSummary = a} :: ProductViewDetail)

-- | The UTC time stamp of the creation time.
productViewDetail_createdTime :: Lens.Lens' ProductViewDetail (Prelude.Maybe Prelude.UTCTime)
productViewDetail_createdTime = Lens.lens (\ProductViewDetail' {createdTime} -> createdTime) (\s@ProductViewDetail' {} a -> s {createdTime = a} :: ProductViewDetail) Prelude.. Lens.mapping Core._Time

-- | The ARN of the product.
productViewDetail_productARN :: Lens.Lens' ProductViewDetail (Prelude.Maybe Prelude.Text)
productViewDetail_productARN = Lens.lens (\ProductViewDetail' {productARN} -> productARN) (\s@ProductViewDetail' {} a -> s {productARN = a} :: ProductViewDetail)

instance Core.FromJSON ProductViewDetail where
  parseJSON =
    Core.withObject
      "ProductViewDetail"
      ( \x ->
          ProductViewDetail'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ProductViewSummary")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "ProductARN")
      )

instance Prelude.Hashable ProductViewDetail where
  hashWithSalt salt' ProductViewDetail' {..} =
    salt' `Prelude.hashWithSalt` productARN
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` productViewSummary
      `Prelude.hashWithSalt` status

instance Prelude.NFData ProductViewDetail where
  rnf ProductViewDetail' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf productARN
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf productViewSummary
