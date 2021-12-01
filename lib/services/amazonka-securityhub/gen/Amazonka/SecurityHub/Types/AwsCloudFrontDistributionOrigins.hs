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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOrigins
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOrigins where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionOriginItem

-- | A complex type that contains information about origins and origin groups
-- for this distribution.
--
-- /See:/ 'newAwsCloudFrontDistributionOrigins' smart constructor.
data AwsCloudFrontDistributionOrigins = AwsCloudFrontDistributionOrigins'
  { -- | A complex type that contains origins or origin groups for this
    -- distribution.
    items :: Prelude.Maybe [AwsCloudFrontDistributionOriginItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionOrigins' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'awsCloudFrontDistributionOrigins_items' - A complex type that contains origins or origin groups for this
-- distribution.
newAwsCloudFrontDistributionOrigins ::
  AwsCloudFrontDistributionOrigins
newAwsCloudFrontDistributionOrigins =
  AwsCloudFrontDistributionOrigins'
    { items =
        Prelude.Nothing
    }

-- | A complex type that contains origins or origin groups for this
-- distribution.
awsCloudFrontDistributionOrigins_items :: Lens.Lens' AwsCloudFrontDistributionOrigins (Prelude.Maybe [AwsCloudFrontDistributionOriginItem])
awsCloudFrontDistributionOrigins_items = Lens.lens (\AwsCloudFrontDistributionOrigins' {items} -> items) (\s@AwsCloudFrontDistributionOrigins' {} a -> s {items = a} :: AwsCloudFrontDistributionOrigins) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AwsCloudFrontDistributionOrigins
  where
  parseJSON =
    Core.withObject
      "AwsCloudFrontDistributionOrigins"
      ( \x ->
          AwsCloudFrontDistributionOrigins'
            Prelude.<$> (x Core..:? "Items" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionOrigins
  where
  hashWithSalt
    salt'
    AwsCloudFrontDistributionOrigins' {..} =
      salt' `Prelude.hashWithSalt` items

instance
  Prelude.NFData
    AwsCloudFrontDistributionOrigins
  where
  rnf AwsCloudFrontDistributionOrigins' {..} =
    Prelude.rnf items

instance Core.ToJSON AwsCloudFrontDistributionOrigins where
  toJSON AwsCloudFrontDistributionOrigins' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Items" Core..=) Prelude.<$> items]
      )
