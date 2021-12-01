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
-- Module      : Amazonka.FMS.Types.DnsRuleGroupLimitExceededViolation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.DnsRuleGroupLimitExceededViolation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The VPC that Firewall Manager was applying a DNS Fireall policy to
-- reached the limit for associated DNS Firewall rule groups. Firewall
-- Manager tried to associate another rule group with the VPC and failed
-- due to the limit.
--
-- /See:/ 'newDnsRuleGroupLimitExceededViolation' smart constructor.
data DnsRuleGroupLimitExceededViolation = DnsRuleGroupLimitExceededViolation'
  { -- | A description of the violation that specifies the rule group and VPC.
    violationTargetDescription :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPC ID.
    violationTarget :: Prelude.Maybe Prelude.Text,
    -- | The number of rule groups currently associated with the VPC.
    numberOfRuleGroupsAlreadyAssociated :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DnsRuleGroupLimitExceededViolation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'violationTargetDescription', 'dnsRuleGroupLimitExceededViolation_violationTargetDescription' - A description of the violation that specifies the rule group and VPC.
--
-- 'violationTarget', 'dnsRuleGroupLimitExceededViolation_violationTarget' - Information about the VPC ID.
--
-- 'numberOfRuleGroupsAlreadyAssociated', 'dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated' - The number of rule groups currently associated with the VPC.
newDnsRuleGroupLimitExceededViolation ::
  DnsRuleGroupLimitExceededViolation
newDnsRuleGroupLimitExceededViolation =
  DnsRuleGroupLimitExceededViolation'
    { violationTargetDescription =
        Prelude.Nothing,
      violationTarget = Prelude.Nothing,
      numberOfRuleGroupsAlreadyAssociated =
        Prelude.Nothing
    }

-- | A description of the violation that specifies the rule group and VPC.
dnsRuleGroupLimitExceededViolation_violationTargetDescription :: Lens.Lens' DnsRuleGroupLimitExceededViolation (Prelude.Maybe Prelude.Text)
dnsRuleGroupLimitExceededViolation_violationTargetDescription = Lens.lens (\DnsRuleGroupLimitExceededViolation' {violationTargetDescription} -> violationTargetDescription) (\s@DnsRuleGroupLimitExceededViolation' {} a -> s {violationTargetDescription = a} :: DnsRuleGroupLimitExceededViolation)

-- | Information about the VPC ID.
dnsRuleGroupLimitExceededViolation_violationTarget :: Lens.Lens' DnsRuleGroupLimitExceededViolation (Prelude.Maybe Prelude.Text)
dnsRuleGroupLimitExceededViolation_violationTarget = Lens.lens (\DnsRuleGroupLimitExceededViolation' {violationTarget} -> violationTarget) (\s@DnsRuleGroupLimitExceededViolation' {} a -> s {violationTarget = a} :: DnsRuleGroupLimitExceededViolation)

-- | The number of rule groups currently associated with the VPC.
dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated :: Lens.Lens' DnsRuleGroupLimitExceededViolation (Prelude.Maybe Prelude.Int)
dnsRuleGroupLimitExceededViolation_numberOfRuleGroupsAlreadyAssociated = Lens.lens (\DnsRuleGroupLimitExceededViolation' {numberOfRuleGroupsAlreadyAssociated} -> numberOfRuleGroupsAlreadyAssociated) (\s@DnsRuleGroupLimitExceededViolation' {} a -> s {numberOfRuleGroupsAlreadyAssociated = a} :: DnsRuleGroupLimitExceededViolation)

instance
  Core.FromJSON
    DnsRuleGroupLimitExceededViolation
  where
  parseJSON =
    Core.withObject
      "DnsRuleGroupLimitExceededViolation"
      ( \x ->
          DnsRuleGroupLimitExceededViolation'
            Prelude.<$> (x Core..:? "ViolationTargetDescription")
            Prelude.<*> (x Core..:? "ViolationTarget")
            Prelude.<*> (x Core..:? "NumberOfRuleGroupsAlreadyAssociated")
      )

instance
  Prelude.Hashable
    DnsRuleGroupLimitExceededViolation
  where
  hashWithSalt
    salt'
    DnsRuleGroupLimitExceededViolation' {..} =
      salt'
        `Prelude.hashWithSalt` numberOfRuleGroupsAlreadyAssociated
        `Prelude.hashWithSalt` violationTarget
        `Prelude.hashWithSalt` violationTargetDescription

instance
  Prelude.NFData
    DnsRuleGroupLimitExceededViolation
  where
  rnf DnsRuleGroupLimitExceededViolation' {..} =
    Prelude.rnf violationTargetDescription
      `Prelude.seq` Prelude.rnf numberOfRuleGroupsAlreadyAssociated
      `Prelude.seq` Prelude.rnf violationTarget
