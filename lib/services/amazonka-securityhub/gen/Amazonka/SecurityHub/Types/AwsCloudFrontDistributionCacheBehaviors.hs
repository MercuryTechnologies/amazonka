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
-- Module      : Amazonka.SecurityHub.Types.AwsCloudFrontDistributionCacheBehaviors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsCloudFrontDistributionCacheBehaviors where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsCloudFrontDistributionCacheBehavior

-- | Provides information about caching for the distribution.
--
-- /See:/ 'newAwsCloudFrontDistributionCacheBehaviors' smart constructor.
data AwsCloudFrontDistributionCacheBehaviors = AwsCloudFrontDistributionCacheBehaviors'
  { -- | The cache behaviors for the distribution.
    items :: Prelude.Maybe [AwsCloudFrontDistributionCacheBehavior]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsCloudFrontDistributionCacheBehaviors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'items', 'awsCloudFrontDistributionCacheBehaviors_items' - The cache behaviors for the distribution.
newAwsCloudFrontDistributionCacheBehaviors ::
  AwsCloudFrontDistributionCacheBehaviors
newAwsCloudFrontDistributionCacheBehaviors =
  AwsCloudFrontDistributionCacheBehaviors'
    { items =
        Prelude.Nothing
    }

-- | The cache behaviors for the distribution.
awsCloudFrontDistributionCacheBehaviors_items :: Lens.Lens' AwsCloudFrontDistributionCacheBehaviors (Prelude.Maybe [AwsCloudFrontDistributionCacheBehavior])
awsCloudFrontDistributionCacheBehaviors_items = Lens.lens (\AwsCloudFrontDistributionCacheBehaviors' {items} -> items) (\s@AwsCloudFrontDistributionCacheBehaviors' {} a -> s {items = a} :: AwsCloudFrontDistributionCacheBehaviors) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    AwsCloudFrontDistributionCacheBehaviors
  where
  parseJSON =
    Core.withObject
      "AwsCloudFrontDistributionCacheBehaviors"
      ( \x ->
          AwsCloudFrontDistributionCacheBehaviors'
            Prelude.<$> (x Core..:? "Items" Core..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    AwsCloudFrontDistributionCacheBehaviors
  where
  hashWithSalt
    salt'
    AwsCloudFrontDistributionCacheBehaviors' {..} =
      salt' `Prelude.hashWithSalt` items

instance
  Prelude.NFData
    AwsCloudFrontDistributionCacheBehaviors
  where
  rnf AwsCloudFrontDistributionCacheBehaviors' {..} =
    Prelude.rnf items

instance
  Core.ToJSON
    AwsCloudFrontDistributionCacheBehaviors
  where
  toJSON AwsCloudFrontDistributionCacheBehaviors' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Items" Core..=) Prelude.<$> items]
      )
