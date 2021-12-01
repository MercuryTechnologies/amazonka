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
-- Module      : Amazonka.AccessAnalyzer.Types.Trail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AccessAnalyzer.Types.Trail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the CloudTrail trail being analyzed to generate a
-- policy.
--
-- /See:/ 'newTrail' smart constructor.
data Trail = Trail'
  { -- | A list of regions to get CloudTrail data from and analyze to generate a
    -- policy.
    regions :: Prelude.Maybe [Prelude.Text],
    -- | Possible values are @true@ or @false@. If set to @true@, IAM Access
    -- Analyzer retrieves CloudTrail data from all regions to analyze and
    -- generate a policy.
    allRegions :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the ARN of the trail. The format of a trail ARN is
    -- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@.
    cloudTrailArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Trail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regions', 'trail_regions' - A list of regions to get CloudTrail data from and analyze to generate a
-- policy.
--
-- 'allRegions', 'trail_allRegions' - Possible values are @true@ or @false@. If set to @true@, IAM Access
-- Analyzer retrieves CloudTrail data from all regions to analyze and
-- generate a policy.
--
-- 'cloudTrailArn', 'trail_cloudTrailArn' - Specifies the ARN of the trail. The format of a trail ARN is
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@.
newTrail ::
  -- | 'cloudTrailArn'
  Prelude.Text ->
  Trail
newTrail pCloudTrailArn_ =
  Trail'
    { regions = Prelude.Nothing,
      allRegions = Prelude.Nothing,
      cloudTrailArn = pCloudTrailArn_
    }

-- | A list of regions to get CloudTrail data from and analyze to generate a
-- policy.
trail_regions :: Lens.Lens' Trail (Prelude.Maybe [Prelude.Text])
trail_regions = Lens.lens (\Trail' {regions} -> regions) (\s@Trail' {} a -> s {regions = a} :: Trail) Prelude.. Lens.mapping Lens.coerced

-- | Possible values are @true@ or @false@. If set to @true@, IAM Access
-- Analyzer retrieves CloudTrail data from all regions to analyze and
-- generate a policy.
trail_allRegions :: Lens.Lens' Trail (Prelude.Maybe Prelude.Bool)
trail_allRegions = Lens.lens (\Trail' {allRegions} -> allRegions) (\s@Trail' {} a -> s {allRegions = a} :: Trail)

-- | Specifies the ARN of the trail. The format of a trail ARN is
-- @arn:aws:cloudtrail:us-east-2:123456789012:trail\/MyTrail@.
trail_cloudTrailArn :: Lens.Lens' Trail Prelude.Text
trail_cloudTrailArn = Lens.lens (\Trail' {cloudTrailArn} -> cloudTrailArn) (\s@Trail' {} a -> s {cloudTrailArn = a} :: Trail)

instance Prelude.Hashable Trail where
  hashWithSalt salt' Trail' {..} =
    salt' `Prelude.hashWithSalt` cloudTrailArn
      `Prelude.hashWithSalt` allRegions
      `Prelude.hashWithSalt` regions

instance Prelude.NFData Trail where
  rnf Trail' {..} =
    Prelude.rnf regions
      `Prelude.seq` Prelude.rnf cloudTrailArn
      `Prelude.seq` Prelude.rnf allRegions

instance Core.ToJSON Trail where
  toJSON Trail' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("regions" Core..=) Prelude.<$> regions,
            ("allRegions" Core..=) Prelude.<$> allRegions,
            Prelude.Just
              ("cloudTrailArn" Core..= cloudTrailArn)
          ]
      )
