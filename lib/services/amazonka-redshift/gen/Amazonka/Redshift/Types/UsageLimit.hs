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
-- Module      : Amazonka.Redshift.Types.UsageLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.UsageLimit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.Tag
import Amazonka.Redshift.Types.UsageLimitBreachAction
import Amazonka.Redshift.Types.UsageLimitFeatureType
import Amazonka.Redshift.Types.UsageLimitLimitType
import Amazonka.Redshift.Types.UsageLimitPeriod

-- | Describes a usage limit object for a cluster.
--
-- /See:/ 'newUsageLimit' smart constructor.
data UsageLimit = UsageLimit'
  { -- | The limit amount. If time-based, this amount is in minutes. If
    -- data-based, this amount is in terabytes (TB).
    amount :: Prelude.Maybe Prelude.Integer,
    -- | The type of limit. Depending on the feature type, this can be based on a
    -- time duration or data size.
    limitType :: Prelude.Maybe UsageLimitLimitType,
    -- | The identifier of the usage limit.
    usageLimitId :: Prelude.Maybe Prelude.Text,
    -- | The time period that the amount applies to. A @weekly@ period begins on
    -- Sunday. The default is @monthly@.
    period :: Prelude.Maybe UsageLimitPeriod,
    -- | The identifier of the cluster with a usage limit.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The action that Amazon Redshift takes when the limit is reached.
    -- Possible values are:
    --
    -- -   __log__ - To log an event in a system table. The default is log.
    --
    -- -   __emit-metric__ - To emit CloudWatch metrics.
    --
    -- -   __disable__ - To disable the feature until the next usage period
    --     begins.
    breachAction :: Prelude.Maybe UsageLimitBreachAction,
    -- | The Amazon Redshift feature to which the limit applies.
    featureType :: Prelude.Maybe UsageLimitFeatureType,
    -- | A list of tag instances.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UsageLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'amount', 'usageLimit_amount' - The limit amount. If time-based, this amount is in minutes. If
-- data-based, this amount is in terabytes (TB).
--
-- 'limitType', 'usageLimit_limitType' - The type of limit. Depending on the feature type, this can be based on a
-- time duration or data size.
--
-- 'usageLimitId', 'usageLimit_usageLimitId' - The identifier of the usage limit.
--
-- 'period', 'usageLimit_period' - The time period that the amount applies to. A @weekly@ period begins on
-- Sunday. The default is @monthly@.
--
-- 'clusterIdentifier', 'usageLimit_clusterIdentifier' - The identifier of the cluster with a usage limit.
--
-- 'breachAction', 'usageLimit_breachAction' - The action that Amazon Redshift takes when the limit is reached.
-- Possible values are:
--
-- -   __log__ - To log an event in a system table. The default is log.
--
-- -   __emit-metric__ - To emit CloudWatch metrics.
--
-- -   __disable__ - To disable the feature until the next usage period
--     begins.
--
-- 'featureType', 'usageLimit_featureType' - The Amazon Redshift feature to which the limit applies.
--
-- 'tags', 'usageLimit_tags' - A list of tag instances.
newUsageLimit ::
  UsageLimit
newUsageLimit =
  UsageLimit'
    { amount = Prelude.Nothing,
      limitType = Prelude.Nothing,
      usageLimitId = Prelude.Nothing,
      period = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      breachAction = Prelude.Nothing,
      featureType = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The limit amount. If time-based, this amount is in minutes. If
-- data-based, this amount is in terabytes (TB).
usageLimit_amount :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Integer)
usageLimit_amount = Lens.lens (\UsageLimit' {amount} -> amount) (\s@UsageLimit' {} a -> s {amount = a} :: UsageLimit)

-- | The type of limit. Depending on the feature type, this can be based on a
-- time duration or data size.
usageLimit_limitType :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitLimitType)
usageLimit_limitType = Lens.lens (\UsageLimit' {limitType} -> limitType) (\s@UsageLimit' {} a -> s {limitType = a} :: UsageLimit)

-- | The identifier of the usage limit.
usageLimit_usageLimitId :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Text)
usageLimit_usageLimitId = Lens.lens (\UsageLimit' {usageLimitId} -> usageLimitId) (\s@UsageLimit' {} a -> s {usageLimitId = a} :: UsageLimit)

-- | The time period that the amount applies to. A @weekly@ period begins on
-- Sunday. The default is @monthly@.
usageLimit_period :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitPeriod)
usageLimit_period = Lens.lens (\UsageLimit' {period} -> period) (\s@UsageLimit' {} a -> s {period = a} :: UsageLimit)

-- | The identifier of the cluster with a usage limit.
usageLimit_clusterIdentifier :: Lens.Lens' UsageLimit (Prelude.Maybe Prelude.Text)
usageLimit_clusterIdentifier = Lens.lens (\UsageLimit' {clusterIdentifier} -> clusterIdentifier) (\s@UsageLimit' {} a -> s {clusterIdentifier = a} :: UsageLimit)

-- | The action that Amazon Redshift takes when the limit is reached.
-- Possible values are:
--
-- -   __log__ - To log an event in a system table. The default is log.
--
-- -   __emit-metric__ - To emit CloudWatch metrics.
--
-- -   __disable__ - To disable the feature until the next usage period
--     begins.
usageLimit_breachAction :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitBreachAction)
usageLimit_breachAction = Lens.lens (\UsageLimit' {breachAction} -> breachAction) (\s@UsageLimit' {} a -> s {breachAction = a} :: UsageLimit)

-- | The Amazon Redshift feature to which the limit applies.
usageLimit_featureType :: Lens.Lens' UsageLimit (Prelude.Maybe UsageLimitFeatureType)
usageLimit_featureType = Lens.lens (\UsageLimit' {featureType} -> featureType) (\s@UsageLimit' {} a -> s {featureType = a} :: UsageLimit)

-- | A list of tag instances.
usageLimit_tags :: Lens.Lens' UsageLimit (Prelude.Maybe [Tag])
usageLimit_tags = Lens.lens (\UsageLimit' {tags} -> tags) (\s@UsageLimit' {} a -> s {tags = a} :: UsageLimit) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML UsageLimit where
  parseXML x =
    UsageLimit'
      Prelude.<$> (x Core..@? "Amount")
      Prelude.<*> (x Core..@? "LimitType")
      Prelude.<*> (x Core..@? "UsageLimitId")
      Prelude.<*> (x Core..@? "Period")
      Prelude.<*> (x Core..@? "ClusterIdentifier")
      Prelude.<*> (x Core..@? "BreachAction")
      Prelude.<*> (x Core..@? "FeatureType")
      Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Tag")
                  )

instance Prelude.Hashable UsageLimit where
  hashWithSalt salt' UsageLimit' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` featureType
      `Prelude.hashWithSalt` breachAction
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` period
      `Prelude.hashWithSalt` usageLimitId
      `Prelude.hashWithSalt` limitType
      `Prelude.hashWithSalt` amount

instance Prelude.NFData UsageLimit where
  rnf UsageLimit' {..} =
    Prelude.rnf amount `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf featureType
      `Prelude.seq` Prelude.rnf breachAction
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf period
      `Prelude.seq` Prelude.rnf usageLimitId
      `Prelude.seq` Prelude.rnf limitType
