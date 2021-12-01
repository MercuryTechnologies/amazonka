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
-- Module      : Amazonka.ServiceCatalog.Types.ProvisioningPreferences
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProvisioningPreferences where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The user-defined preferences that will be applied when updating a
-- provisioned product. Not all preferences are applicable to all
-- provisioned product type
--
-- One or more AWS accounts that will have access to the provisioned
-- product.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- The AWS accounts specified should be within the list of accounts in the
-- @STACKSET@ constraint. To get the list of accounts in the @STACKSET@
-- constraint, use the @DescribeProvisioningParameters@ operation.
--
-- If no values are specified, the default value is all accounts from the
-- @STACKSET@ constraint.
--
-- /See:/ 'newProvisioningPreferences' smart constructor.
data ProvisioningPreferences = ProvisioningPreferences'
  { -- | One or more AWS Regions where the provisioned product will be available.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- The specified regions should be within the list of regions from the
    -- @STACKSET@ constraint. To get the list of regions in the @STACKSET@
    -- constraint, use the @DescribeProvisioningParameters@ operation.
    --
    -- If no values are specified, the default value is all regions from the
    -- @STACKSET@ constraint.
    stackSetRegions :: Prelude.Maybe [Prelude.Text],
    -- | The maximum percentage of accounts in which to perform this operation at
    -- one time.
    --
    -- When calculating the number of accounts based on the specified
    -- percentage, AWS Service Catalog rounds down to the next whole number.
    -- This is true except in cases where rounding down would result is zero.
    -- In this case, AWS Service Catalog sets the number as @1@ instead.
    --
    -- Note that this setting lets you specify the maximum for operations. For
    -- large deployments, under certain circumstances the actual number of
    -- accounts acted upon concurrently may be lower due to service throttling.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
    -- @StackSetMaxConcurrentPercentage@, but not both.
    stackSetMaxConcurrencyPercentage :: Prelude.Maybe Prelude.Natural,
    -- | The number of accounts, per region, for which this operation can fail
    -- before AWS Service Catalog stops the operation in that region. If the
    -- operation is stopped in a region, AWS Service Catalog doesn\'t attempt
    -- the operation in any subsequent regions.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- Conditional: You must specify either @StackSetFailureToleranceCount@ or
    -- @StackSetFailureTolerancePercentage@, but not both.
    --
    -- The default value is @0@ if no value is specified.
    stackSetFailureToleranceCount :: Prelude.Maybe Prelude.Natural,
    -- | The percentage of accounts, per region, for which this stack operation
    -- can fail before AWS Service Catalog stops the operation in that region.
    -- If the operation is stopped in a region, AWS Service Catalog doesn\'t
    -- attempt the operation in any subsequent regions.
    --
    -- When calculating the number of accounts based on the specified
    -- percentage, AWS Service Catalog rounds down to the next whole number.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- Conditional: You must specify either @StackSetFailureToleranceCount@ or
    -- @StackSetFailureTolerancePercentage@, but not both.
    stackSetFailureTolerancePercentage :: Prelude.Maybe Prelude.Natural,
    -- | One or more AWS accounts where the provisioned product will be
    -- available.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- The specified accounts should be within the list of accounts from the
    -- @STACKSET@ constraint. To get the list of accounts in the @STACKSET@
    -- constraint, use the @DescribeProvisioningParameters@ operation.
    --
    -- If no values are specified, the default value is all acounts from the
    -- @STACKSET@ constraint.
    stackSetAccounts :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of accounts in which to perform this operation at one
    -- time. This is dependent on the value of @StackSetFailureToleranceCount@.
    -- @StackSetMaxConcurrentCount@ is at most one more than the
    -- @StackSetFailureToleranceCount@.
    --
    -- Note that this setting lets you specify the maximum for operations. For
    -- large deployments, under certain circumstances the actual number of
    -- accounts acted upon concurrently may be lower due to service throttling.
    --
    -- Applicable only to a @CFN_STACKSET@ provisioned product type.
    --
    -- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
    -- @StackSetMaxConcurrentPercentage@, but not both.
    stackSetMaxConcurrencyCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProvisioningPreferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackSetRegions', 'provisioningPreferences_stackSetRegions' - One or more AWS Regions where the provisioned product will be available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- The specified regions should be within the list of regions from the
-- @STACKSET@ constraint. To get the list of regions in the @STACKSET@
-- constraint, use the @DescribeProvisioningParameters@ operation.
--
-- If no values are specified, the default value is all regions from the
-- @STACKSET@ constraint.
--
-- 'stackSetMaxConcurrencyPercentage', 'provisioningPreferences_stackSetMaxConcurrencyPercentage' - The maximum percentage of accounts in which to perform this operation at
-- one time.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS Service Catalog rounds down to the next whole number.
-- This is true except in cases where rounding down would result is zero.
-- In this case, AWS Service Catalog sets the number as @1@ instead.
--
-- Note that this setting lets you specify the maximum for operations. For
-- large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
-- @StackSetMaxConcurrentPercentage@, but not both.
--
-- 'stackSetFailureToleranceCount', 'provisioningPreferences_stackSetFailureToleranceCount' - The number of accounts, per region, for which this operation can fail
-- before AWS Service Catalog stops the operation in that region. If the
-- operation is stopped in a region, AWS Service Catalog doesn\'t attempt
-- the operation in any subsequent regions.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or
-- @StackSetFailureTolerancePercentage@, but not both.
--
-- The default value is @0@ if no value is specified.
--
-- 'stackSetFailureTolerancePercentage', 'provisioningPreferences_stackSetFailureTolerancePercentage' - The percentage of accounts, per region, for which this stack operation
-- can fail before AWS Service Catalog stops the operation in that region.
-- If the operation is stopped in a region, AWS Service Catalog doesn\'t
-- attempt the operation in any subsequent regions.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS Service Catalog rounds down to the next whole number.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or
-- @StackSetFailureTolerancePercentage@, but not both.
--
-- 'stackSetAccounts', 'provisioningPreferences_stackSetAccounts' - One or more AWS accounts where the provisioned product will be
-- available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- The specified accounts should be within the list of accounts from the
-- @STACKSET@ constraint. To get the list of accounts in the @STACKSET@
-- constraint, use the @DescribeProvisioningParameters@ operation.
--
-- If no values are specified, the default value is all acounts from the
-- @STACKSET@ constraint.
--
-- 'stackSetMaxConcurrencyCount', 'provisioningPreferences_stackSetMaxConcurrencyCount' - The maximum number of accounts in which to perform this operation at one
-- time. This is dependent on the value of @StackSetFailureToleranceCount@.
-- @StackSetMaxConcurrentCount@ is at most one more than the
-- @StackSetFailureToleranceCount@.
--
-- Note that this setting lets you specify the maximum for operations. For
-- large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
-- @StackSetMaxConcurrentPercentage@, but not both.
newProvisioningPreferences ::
  ProvisioningPreferences
newProvisioningPreferences =
  ProvisioningPreferences'
    { stackSetRegions =
        Prelude.Nothing,
      stackSetMaxConcurrencyPercentage = Prelude.Nothing,
      stackSetFailureToleranceCount = Prelude.Nothing,
      stackSetFailureTolerancePercentage =
        Prelude.Nothing,
      stackSetAccounts = Prelude.Nothing,
      stackSetMaxConcurrencyCount = Prelude.Nothing
    }

-- | One or more AWS Regions where the provisioned product will be available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- The specified regions should be within the list of regions from the
-- @STACKSET@ constraint. To get the list of regions in the @STACKSET@
-- constraint, use the @DescribeProvisioningParameters@ operation.
--
-- If no values are specified, the default value is all regions from the
-- @STACKSET@ constraint.
provisioningPreferences_stackSetRegions :: Lens.Lens' ProvisioningPreferences (Prelude.Maybe [Prelude.Text])
provisioningPreferences_stackSetRegions = Lens.lens (\ProvisioningPreferences' {stackSetRegions} -> stackSetRegions) (\s@ProvisioningPreferences' {} a -> s {stackSetRegions = a} :: ProvisioningPreferences) Prelude.. Lens.mapping Lens.coerced

-- | The maximum percentage of accounts in which to perform this operation at
-- one time.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS Service Catalog rounds down to the next whole number.
-- This is true except in cases where rounding down would result is zero.
-- In this case, AWS Service Catalog sets the number as @1@ instead.
--
-- Note that this setting lets you specify the maximum for operations. For
-- large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
-- @StackSetMaxConcurrentPercentage@, but not both.
provisioningPreferences_stackSetMaxConcurrencyPercentage :: Lens.Lens' ProvisioningPreferences (Prelude.Maybe Prelude.Natural)
provisioningPreferences_stackSetMaxConcurrencyPercentage = Lens.lens (\ProvisioningPreferences' {stackSetMaxConcurrencyPercentage} -> stackSetMaxConcurrencyPercentage) (\s@ProvisioningPreferences' {} a -> s {stackSetMaxConcurrencyPercentage = a} :: ProvisioningPreferences)

-- | The number of accounts, per region, for which this operation can fail
-- before AWS Service Catalog stops the operation in that region. If the
-- operation is stopped in a region, AWS Service Catalog doesn\'t attempt
-- the operation in any subsequent regions.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or
-- @StackSetFailureTolerancePercentage@, but not both.
--
-- The default value is @0@ if no value is specified.
provisioningPreferences_stackSetFailureToleranceCount :: Lens.Lens' ProvisioningPreferences (Prelude.Maybe Prelude.Natural)
provisioningPreferences_stackSetFailureToleranceCount = Lens.lens (\ProvisioningPreferences' {stackSetFailureToleranceCount} -> stackSetFailureToleranceCount) (\s@ProvisioningPreferences' {} a -> s {stackSetFailureToleranceCount = a} :: ProvisioningPreferences)

-- | The percentage of accounts, per region, for which this stack operation
-- can fail before AWS Service Catalog stops the operation in that region.
-- If the operation is stopped in a region, AWS Service Catalog doesn\'t
-- attempt the operation in any subsequent regions.
--
-- When calculating the number of accounts based on the specified
-- percentage, AWS Service Catalog rounds down to the next whole number.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetFailureToleranceCount@ or
-- @StackSetFailureTolerancePercentage@, but not both.
provisioningPreferences_stackSetFailureTolerancePercentage :: Lens.Lens' ProvisioningPreferences (Prelude.Maybe Prelude.Natural)
provisioningPreferences_stackSetFailureTolerancePercentage = Lens.lens (\ProvisioningPreferences' {stackSetFailureTolerancePercentage} -> stackSetFailureTolerancePercentage) (\s@ProvisioningPreferences' {} a -> s {stackSetFailureTolerancePercentage = a} :: ProvisioningPreferences)

-- | One or more AWS accounts where the provisioned product will be
-- available.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- The specified accounts should be within the list of accounts from the
-- @STACKSET@ constraint. To get the list of accounts in the @STACKSET@
-- constraint, use the @DescribeProvisioningParameters@ operation.
--
-- If no values are specified, the default value is all acounts from the
-- @STACKSET@ constraint.
provisioningPreferences_stackSetAccounts :: Lens.Lens' ProvisioningPreferences (Prelude.Maybe [Prelude.Text])
provisioningPreferences_stackSetAccounts = Lens.lens (\ProvisioningPreferences' {stackSetAccounts} -> stackSetAccounts) (\s@ProvisioningPreferences' {} a -> s {stackSetAccounts = a} :: ProvisioningPreferences) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of accounts in which to perform this operation at one
-- time. This is dependent on the value of @StackSetFailureToleranceCount@.
-- @StackSetMaxConcurrentCount@ is at most one more than the
-- @StackSetFailureToleranceCount@.
--
-- Note that this setting lets you specify the maximum for operations. For
-- large deployments, under certain circumstances the actual number of
-- accounts acted upon concurrently may be lower due to service throttling.
--
-- Applicable only to a @CFN_STACKSET@ provisioned product type.
--
-- Conditional: You must specify either @StackSetMaxConcurrentCount@ or
-- @StackSetMaxConcurrentPercentage@, but not both.
provisioningPreferences_stackSetMaxConcurrencyCount :: Lens.Lens' ProvisioningPreferences (Prelude.Maybe Prelude.Natural)
provisioningPreferences_stackSetMaxConcurrencyCount = Lens.lens (\ProvisioningPreferences' {stackSetMaxConcurrencyCount} -> stackSetMaxConcurrencyCount) (\s@ProvisioningPreferences' {} a -> s {stackSetMaxConcurrencyCount = a} :: ProvisioningPreferences)

instance Prelude.Hashable ProvisioningPreferences where
  hashWithSalt salt' ProvisioningPreferences' {..} =
    salt'
      `Prelude.hashWithSalt` stackSetMaxConcurrencyCount
      `Prelude.hashWithSalt` stackSetAccounts
      `Prelude.hashWithSalt` stackSetFailureTolerancePercentage
      `Prelude.hashWithSalt` stackSetFailureToleranceCount
      `Prelude.hashWithSalt` stackSetMaxConcurrencyPercentage
      `Prelude.hashWithSalt` stackSetRegions

instance Prelude.NFData ProvisioningPreferences where
  rnf ProvisioningPreferences' {..} =
    Prelude.rnf stackSetRegions
      `Prelude.seq` Prelude.rnf stackSetMaxConcurrencyCount
      `Prelude.seq` Prelude.rnf stackSetAccounts
      `Prelude.seq` Prelude.rnf stackSetFailureTolerancePercentage
      `Prelude.seq` Prelude.rnf stackSetFailureToleranceCount
      `Prelude.seq` Prelude.rnf stackSetMaxConcurrencyPercentage

instance Core.ToJSON ProvisioningPreferences where
  toJSON ProvisioningPreferences' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StackSetRegions" Core..=)
              Prelude.<$> stackSetRegions,
            ("StackSetMaxConcurrencyPercentage" Core..=)
              Prelude.<$> stackSetMaxConcurrencyPercentage,
            ("StackSetFailureToleranceCount" Core..=)
              Prelude.<$> stackSetFailureToleranceCount,
            ("StackSetFailureTolerancePercentage" Core..=)
              Prelude.<$> stackSetFailureTolerancePercentage,
            ("StackSetAccounts" Core..=)
              Prelude.<$> stackSetAccounts,
            ("StackSetMaxConcurrencyCount" Core..=)
              Prelude.<$> stackSetMaxConcurrencyCount
          ]
      )
