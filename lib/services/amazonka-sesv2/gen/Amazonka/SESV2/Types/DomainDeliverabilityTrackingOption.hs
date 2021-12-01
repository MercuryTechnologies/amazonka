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
-- Module      : Amazonka.SESV2.Types.DomainDeliverabilityTrackingOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.DomainDeliverabilityTrackingOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.InboxPlacementTrackingOption

-- | An object that contains information about the Deliverability dashboard
-- subscription for a verified domain that you use to send email and
-- currently has an active Deliverability dashboard subscription. If a
-- Deliverability dashboard subscription is active for a domain, you gain
-- access to reputation, inbox placement, and other metrics for the domain.
--
-- /See:/ 'newDomainDeliverabilityTrackingOption' smart constructor.
data DomainDeliverabilityTrackingOption = DomainDeliverabilityTrackingOption'
  { -- | A verified domain that’s associated with your Amazon Web Services
    -- account and currently has an active Deliverability dashboard
    -- subscription.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The date, in Unix time format, when you enabled the Deliverability
    -- dashboard for the domain.
    subscriptionStartDate :: Prelude.Maybe Core.POSIX,
    -- | An object that contains information about the inbox placement data
    -- settings for the domain.
    inboxPlacementTrackingOption :: Prelude.Maybe InboxPlacementTrackingOption
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainDeliverabilityTrackingOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'domainDeliverabilityTrackingOption_domain' - A verified domain that’s associated with your Amazon Web Services
-- account and currently has an active Deliverability dashboard
-- subscription.
--
-- 'subscriptionStartDate', 'domainDeliverabilityTrackingOption_subscriptionStartDate' - The date, in Unix time format, when you enabled the Deliverability
-- dashboard for the domain.
--
-- 'inboxPlacementTrackingOption', 'domainDeliverabilityTrackingOption_inboxPlacementTrackingOption' - An object that contains information about the inbox placement data
-- settings for the domain.
newDomainDeliverabilityTrackingOption ::
  DomainDeliverabilityTrackingOption
newDomainDeliverabilityTrackingOption =
  DomainDeliverabilityTrackingOption'
    { domain =
        Prelude.Nothing,
      subscriptionStartDate = Prelude.Nothing,
      inboxPlacementTrackingOption =
        Prelude.Nothing
    }

-- | A verified domain that’s associated with your Amazon Web Services
-- account and currently has an active Deliverability dashboard
-- subscription.
domainDeliverabilityTrackingOption_domain :: Lens.Lens' DomainDeliverabilityTrackingOption (Prelude.Maybe Prelude.Text)
domainDeliverabilityTrackingOption_domain = Lens.lens (\DomainDeliverabilityTrackingOption' {domain} -> domain) (\s@DomainDeliverabilityTrackingOption' {} a -> s {domain = a} :: DomainDeliverabilityTrackingOption)

-- | The date, in Unix time format, when you enabled the Deliverability
-- dashboard for the domain.
domainDeliverabilityTrackingOption_subscriptionStartDate :: Lens.Lens' DomainDeliverabilityTrackingOption (Prelude.Maybe Prelude.UTCTime)
domainDeliverabilityTrackingOption_subscriptionStartDate = Lens.lens (\DomainDeliverabilityTrackingOption' {subscriptionStartDate} -> subscriptionStartDate) (\s@DomainDeliverabilityTrackingOption' {} a -> s {subscriptionStartDate = a} :: DomainDeliverabilityTrackingOption) Prelude.. Lens.mapping Core._Time

-- | An object that contains information about the inbox placement data
-- settings for the domain.
domainDeliverabilityTrackingOption_inboxPlacementTrackingOption :: Lens.Lens' DomainDeliverabilityTrackingOption (Prelude.Maybe InboxPlacementTrackingOption)
domainDeliverabilityTrackingOption_inboxPlacementTrackingOption = Lens.lens (\DomainDeliverabilityTrackingOption' {inboxPlacementTrackingOption} -> inboxPlacementTrackingOption) (\s@DomainDeliverabilityTrackingOption' {} a -> s {inboxPlacementTrackingOption = a} :: DomainDeliverabilityTrackingOption)

instance
  Core.FromJSON
    DomainDeliverabilityTrackingOption
  where
  parseJSON =
    Core.withObject
      "DomainDeliverabilityTrackingOption"
      ( \x ->
          DomainDeliverabilityTrackingOption'
            Prelude.<$> (x Core..:? "Domain")
            Prelude.<*> (x Core..:? "SubscriptionStartDate")
            Prelude.<*> (x Core..:? "InboxPlacementTrackingOption")
      )

instance
  Prelude.Hashable
    DomainDeliverabilityTrackingOption
  where
  hashWithSalt
    salt'
    DomainDeliverabilityTrackingOption' {..} =
      salt'
        `Prelude.hashWithSalt` inboxPlacementTrackingOption
        `Prelude.hashWithSalt` subscriptionStartDate
        `Prelude.hashWithSalt` domain

instance
  Prelude.NFData
    DomainDeliverabilityTrackingOption
  where
  rnf DomainDeliverabilityTrackingOption' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf inboxPlacementTrackingOption
      `Prelude.seq` Prelude.rnf subscriptionStartDate

instance
  Core.ToJSON
    DomainDeliverabilityTrackingOption
  where
  toJSON DomainDeliverabilityTrackingOption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Domain" Core..=) Prelude.<$> domain,
            ("SubscriptionStartDate" Core..=)
              Prelude.<$> subscriptionStartDate,
            ("InboxPlacementTrackingOption" Core..=)
              Prelude.<$> inboxPlacementTrackingOption
          ]
      )
