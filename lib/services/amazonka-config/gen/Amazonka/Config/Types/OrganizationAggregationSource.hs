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
-- Module      : Amazonka.Config.Types.OrganizationAggregationSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.OrganizationAggregationSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | This object contains regions to set up the aggregator and an IAM role to
-- retrieve organization details.
--
-- /See:/ 'newOrganizationAggregationSource' smart constructor.
data OrganizationAggregationSource = OrganizationAggregationSource'
  { -- | The source regions being aggregated.
    awsRegions :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | If true, aggregate existing Config regions and future regions.
    allAwsRegions :: Prelude.Maybe Prelude.Bool,
    -- | ARN of the IAM role used to retrieve Amazon Web Services Organization
    -- details associated with the aggregator account.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OrganizationAggregationSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'awsRegions', 'organizationAggregationSource_awsRegions' - The source regions being aggregated.
--
-- 'allAwsRegions', 'organizationAggregationSource_allAwsRegions' - If true, aggregate existing Config regions and future regions.
--
-- 'roleArn', 'organizationAggregationSource_roleArn' - ARN of the IAM role used to retrieve Amazon Web Services Organization
-- details associated with the aggregator account.
newOrganizationAggregationSource ::
  -- | 'roleArn'
  Prelude.Text ->
  OrganizationAggregationSource
newOrganizationAggregationSource pRoleArn_ =
  OrganizationAggregationSource'
    { awsRegions =
        Prelude.Nothing,
      allAwsRegions = Prelude.Nothing,
      roleArn = pRoleArn_
    }

-- | The source regions being aggregated.
organizationAggregationSource_awsRegions :: Lens.Lens' OrganizationAggregationSource (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
organizationAggregationSource_awsRegions = Lens.lens (\OrganizationAggregationSource' {awsRegions} -> awsRegions) (\s@OrganizationAggregationSource' {} a -> s {awsRegions = a} :: OrganizationAggregationSource) Prelude.. Lens.mapping Lens.coerced

-- | If true, aggregate existing Config regions and future regions.
organizationAggregationSource_allAwsRegions :: Lens.Lens' OrganizationAggregationSource (Prelude.Maybe Prelude.Bool)
organizationAggregationSource_allAwsRegions = Lens.lens (\OrganizationAggregationSource' {allAwsRegions} -> allAwsRegions) (\s@OrganizationAggregationSource' {} a -> s {allAwsRegions = a} :: OrganizationAggregationSource)

-- | ARN of the IAM role used to retrieve Amazon Web Services Organization
-- details associated with the aggregator account.
organizationAggregationSource_roleArn :: Lens.Lens' OrganizationAggregationSource Prelude.Text
organizationAggregationSource_roleArn = Lens.lens (\OrganizationAggregationSource' {roleArn} -> roleArn) (\s@OrganizationAggregationSource' {} a -> s {roleArn = a} :: OrganizationAggregationSource)

instance Core.FromJSON OrganizationAggregationSource where
  parseJSON =
    Core.withObject
      "OrganizationAggregationSource"
      ( \x ->
          OrganizationAggregationSource'
            Prelude.<$> (x Core..:? "AwsRegions")
            Prelude.<*> (x Core..:? "AllAwsRegions")
            Prelude.<*> (x Core..: "RoleArn")
      )

instance
  Prelude.Hashable
    OrganizationAggregationSource
  where
  hashWithSalt salt' OrganizationAggregationSource' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` allAwsRegions
      `Prelude.hashWithSalt` awsRegions

instance Prelude.NFData OrganizationAggregationSource where
  rnf OrganizationAggregationSource' {..} =
    Prelude.rnf awsRegions
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf allAwsRegions

instance Core.ToJSON OrganizationAggregationSource where
  toJSON OrganizationAggregationSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AwsRegions" Core..=) Prelude.<$> awsRegions,
            ("AllAwsRegions" Core..=) Prelude.<$> allAwsRegions,
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )
