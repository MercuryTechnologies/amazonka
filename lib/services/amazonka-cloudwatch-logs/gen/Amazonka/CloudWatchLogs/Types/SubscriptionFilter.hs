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
-- Module      : Amazonka.CloudWatchLogs.Types.SubscriptionFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.SubscriptionFilter where

import Amazonka.CloudWatchLogs.Types.Distribution
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents a subscription filter.
--
-- /See:/ 'newSubscriptionFilter' smart constructor.
data SubscriptionFilter = SubscriptionFilter'
  { -- | The creation time of the subscription filter, expressed as the number of
    -- milliseconds after Jan 1, 1970 00:00:00 UTC.
    creationTime :: Prelude.Maybe Prelude.Natural,
    -- | The name of the subscription filter.
    filterName :: Prelude.Maybe Prelude.Text,
    distribution :: Prelude.Maybe Distribution,
    -- | The Amazon Resource Name (ARN) of the destination.
    destinationArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the log group.
    logGroupName :: Prelude.Maybe Prelude.Text,
    filterPattern :: Prelude.Maybe Prelude.Text,
    roleArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubscriptionFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'subscriptionFilter_creationTime' - The creation time of the subscription filter, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
--
-- 'filterName', 'subscriptionFilter_filterName' - The name of the subscription filter.
--
-- 'distribution', 'subscriptionFilter_distribution' - Undocumented member.
--
-- 'destinationArn', 'subscriptionFilter_destinationArn' - The Amazon Resource Name (ARN) of the destination.
--
-- 'logGroupName', 'subscriptionFilter_logGroupName' - The name of the log group.
--
-- 'filterPattern', 'subscriptionFilter_filterPattern' - Undocumented member.
--
-- 'roleArn', 'subscriptionFilter_roleArn' -
newSubscriptionFilter ::
  SubscriptionFilter
newSubscriptionFilter =
  SubscriptionFilter'
    { creationTime = Prelude.Nothing,
      filterName = Prelude.Nothing,
      distribution = Prelude.Nothing,
      destinationArn = Prelude.Nothing,
      logGroupName = Prelude.Nothing,
      filterPattern = Prelude.Nothing,
      roleArn = Prelude.Nothing
    }

-- | The creation time of the subscription filter, expressed as the number of
-- milliseconds after Jan 1, 1970 00:00:00 UTC.
subscriptionFilter_creationTime :: Lens.Lens' SubscriptionFilter (Prelude.Maybe Prelude.Natural)
subscriptionFilter_creationTime = Lens.lens (\SubscriptionFilter' {creationTime} -> creationTime) (\s@SubscriptionFilter' {} a -> s {creationTime = a} :: SubscriptionFilter)

-- | The name of the subscription filter.
subscriptionFilter_filterName :: Lens.Lens' SubscriptionFilter (Prelude.Maybe Prelude.Text)
subscriptionFilter_filterName = Lens.lens (\SubscriptionFilter' {filterName} -> filterName) (\s@SubscriptionFilter' {} a -> s {filterName = a} :: SubscriptionFilter)

-- | Undocumented member.
subscriptionFilter_distribution :: Lens.Lens' SubscriptionFilter (Prelude.Maybe Distribution)
subscriptionFilter_distribution = Lens.lens (\SubscriptionFilter' {distribution} -> distribution) (\s@SubscriptionFilter' {} a -> s {distribution = a} :: SubscriptionFilter)

-- | The Amazon Resource Name (ARN) of the destination.
subscriptionFilter_destinationArn :: Lens.Lens' SubscriptionFilter (Prelude.Maybe Prelude.Text)
subscriptionFilter_destinationArn = Lens.lens (\SubscriptionFilter' {destinationArn} -> destinationArn) (\s@SubscriptionFilter' {} a -> s {destinationArn = a} :: SubscriptionFilter)

-- | The name of the log group.
subscriptionFilter_logGroupName :: Lens.Lens' SubscriptionFilter (Prelude.Maybe Prelude.Text)
subscriptionFilter_logGroupName = Lens.lens (\SubscriptionFilter' {logGroupName} -> logGroupName) (\s@SubscriptionFilter' {} a -> s {logGroupName = a} :: SubscriptionFilter)

-- | Undocumented member.
subscriptionFilter_filterPattern :: Lens.Lens' SubscriptionFilter (Prelude.Maybe Prelude.Text)
subscriptionFilter_filterPattern = Lens.lens (\SubscriptionFilter' {filterPattern} -> filterPattern) (\s@SubscriptionFilter' {} a -> s {filterPattern = a} :: SubscriptionFilter)

-- |
subscriptionFilter_roleArn :: Lens.Lens' SubscriptionFilter (Prelude.Maybe Prelude.Text)
subscriptionFilter_roleArn = Lens.lens (\SubscriptionFilter' {roleArn} -> roleArn) (\s@SubscriptionFilter' {} a -> s {roleArn = a} :: SubscriptionFilter)

instance Core.FromJSON SubscriptionFilter where
  parseJSON =
    Core.withObject
      "SubscriptionFilter"
      ( \x ->
          SubscriptionFilter'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "filterName")
            Prelude.<*> (x Core..:? "distribution")
            Prelude.<*> (x Core..:? "destinationArn")
            Prelude.<*> (x Core..:? "logGroupName")
            Prelude.<*> (x Core..:? "filterPattern")
            Prelude.<*> (x Core..:? "roleArn")
      )

instance Prelude.Hashable SubscriptionFilter where
  hashWithSalt salt' SubscriptionFilter' {..} =
    salt' `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` filterPattern
      `Prelude.hashWithSalt` logGroupName
      `Prelude.hashWithSalt` destinationArn
      `Prelude.hashWithSalt` distribution
      `Prelude.hashWithSalt` filterName
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData SubscriptionFilter where
  rnf SubscriptionFilter' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf filterPattern
      `Prelude.seq` Prelude.rnf logGroupName
      `Prelude.seq` Prelude.rnf destinationArn
      `Prelude.seq` Prelude.rnf distribution
      `Prelude.seq` Prelude.rnf filterName
