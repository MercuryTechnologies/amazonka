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
-- Module      : Amazonka.FMS.Types.PolicyComplianceDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.PolicyComplianceDetail where

import qualified Amazonka.Core as Core
import Amazonka.FMS.Types.ComplianceViolator
import Amazonka.FMS.Types.DependentServiceName
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the noncompliant resources in a member account for a specific
-- Firewall Manager policy. A maximum of 100 entries are displayed. If more
-- than 100 resources are noncompliant, @EvaluationLimitExceeded@ is set to
-- @True@.
--
-- /See:/ 'newPolicyComplianceDetail' smart constructor.
data PolicyComplianceDetail = PolicyComplianceDetail'
  { -- | A timestamp that indicates when the returned information should be
    -- considered out of date.
    expiredAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the Firewall Manager policy.
    policyId :: Prelude.Maybe Prelude.Text,
    -- | An array of resources that aren\'t protected by the WAF or Shield
    -- Advanced policy or that aren\'t in compliance with the security group
    -- policy.
    violators :: Prelude.Maybe [ComplianceViolator],
    -- | Indicates if over 100 resources are noncompliant with the Firewall
    -- Manager policy.
    evaluationLimitExceeded :: Prelude.Maybe Prelude.Bool,
    -- | Details about problems with dependent services, such as WAF or Config,
    -- and the error message received that indicates the problem with the
    -- service.
    issueInfoMap :: Prelude.Maybe (Prelude.HashMap DependentServiceName Prelude.Text),
    -- | The Amazon Web Services account that created the Firewall Manager
    -- policy.
    policyOwner :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID.
    memberAccount :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyComplianceDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expiredAt', 'policyComplianceDetail_expiredAt' - A timestamp that indicates when the returned information should be
-- considered out of date.
--
-- 'policyId', 'policyComplianceDetail_policyId' - The ID of the Firewall Manager policy.
--
-- 'violators', 'policyComplianceDetail_violators' - An array of resources that aren\'t protected by the WAF or Shield
-- Advanced policy or that aren\'t in compliance with the security group
-- policy.
--
-- 'evaluationLimitExceeded', 'policyComplianceDetail_evaluationLimitExceeded' - Indicates if over 100 resources are noncompliant with the Firewall
-- Manager policy.
--
-- 'issueInfoMap', 'policyComplianceDetail_issueInfoMap' - Details about problems with dependent services, such as WAF or Config,
-- and the error message received that indicates the problem with the
-- service.
--
-- 'policyOwner', 'policyComplianceDetail_policyOwner' - The Amazon Web Services account that created the Firewall Manager
-- policy.
--
-- 'memberAccount', 'policyComplianceDetail_memberAccount' - The Amazon Web Services account ID.
newPolicyComplianceDetail ::
  PolicyComplianceDetail
newPolicyComplianceDetail =
  PolicyComplianceDetail'
    { expiredAt =
        Prelude.Nothing,
      policyId = Prelude.Nothing,
      violators = Prelude.Nothing,
      evaluationLimitExceeded = Prelude.Nothing,
      issueInfoMap = Prelude.Nothing,
      policyOwner = Prelude.Nothing,
      memberAccount = Prelude.Nothing
    }

-- | A timestamp that indicates when the returned information should be
-- considered out of date.
policyComplianceDetail_expiredAt :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.UTCTime)
policyComplianceDetail_expiredAt = Lens.lens (\PolicyComplianceDetail' {expiredAt} -> expiredAt) (\s@PolicyComplianceDetail' {} a -> s {expiredAt = a} :: PolicyComplianceDetail) Prelude.. Lens.mapping Core._Time

-- | The ID of the Firewall Manager policy.
policyComplianceDetail_policyId :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Text)
policyComplianceDetail_policyId = Lens.lens (\PolicyComplianceDetail' {policyId} -> policyId) (\s@PolicyComplianceDetail' {} a -> s {policyId = a} :: PolicyComplianceDetail)

-- | An array of resources that aren\'t protected by the WAF or Shield
-- Advanced policy or that aren\'t in compliance with the security group
-- policy.
policyComplianceDetail_violators :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe [ComplianceViolator])
policyComplianceDetail_violators = Lens.lens (\PolicyComplianceDetail' {violators} -> violators) (\s@PolicyComplianceDetail' {} a -> s {violators = a} :: PolicyComplianceDetail) Prelude.. Lens.mapping Lens.coerced

-- | Indicates if over 100 resources are noncompliant with the Firewall
-- Manager policy.
policyComplianceDetail_evaluationLimitExceeded :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Bool)
policyComplianceDetail_evaluationLimitExceeded = Lens.lens (\PolicyComplianceDetail' {evaluationLimitExceeded} -> evaluationLimitExceeded) (\s@PolicyComplianceDetail' {} a -> s {evaluationLimitExceeded = a} :: PolicyComplianceDetail)

-- | Details about problems with dependent services, such as WAF or Config,
-- and the error message received that indicates the problem with the
-- service.
policyComplianceDetail_issueInfoMap :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe (Prelude.HashMap DependentServiceName Prelude.Text))
policyComplianceDetail_issueInfoMap = Lens.lens (\PolicyComplianceDetail' {issueInfoMap} -> issueInfoMap) (\s@PolicyComplianceDetail' {} a -> s {issueInfoMap = a} :: PolicyComplianceDetail) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account that created the Firewall Manager
-- policy.
policyComplianceDetail_policyOwner :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Text)
policyComplianceDetail_policyOwner = Lens.lens (\PolicyComplianceDetail' {policyOwner} -> policyOwner) (\s@PolicyComplianceDetail' {} a -> s {policyOwner = a} :: PolicyComplianceDetail)

-- | The Amazon Web Services account ID.
policyComplianceDetail_memberAccount :: Lens.Lens' PolicyComplianceDetail (Prelude.Maybe Prelude.Text)
policyComplianceDetail_memberAccount = Lens.lens (\PolicyComplianceDetail' {memberAccount} -> memberAccount) (\s@PolicyComplianceDetail' {} a -> s {memberAccount = a} :: PolicyComplianceDetail)

instance Core.FromJSON PolicyComplianceDetail where
  parseJSON =
    Core.withObject
      "PolicyComplianceDetail"
      ( \x ->
          PolicyComplianceDetail'
            Prelude.<$> (x Core..:? "ExpiredAt")
            Prelude.<*> (x Core..:? "PolicyId")
            Prelude.<*> (x Core..:? "Violators" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "EvaluationLimitExceeded")
            Prelude.<*> (x Core..:? "IssueInfoMap" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "PolicyOwner")
            Prelude.<*> (x Core..:? "MemberAccount")
      )

instance Prelude.Hashable PolicyComplianceDetail where
  hashWithSalt salt' PolicyComplianceDetail' {..} =
    salt' `Prelude.hashWithSalt` memberAccount
      `Prelude.hashWithSalt` policyOwner
      `Prelude.hashWithSalt` issueInfoMap
      `Prelude.hashWithSalt` evaluationLimitExceeded
      `Prelude.hashWithSalt` violators
      `Prelude.hashWithSalt` policyId
      `Prelude.hashWithSalt` expiredAt

instance Prelude.NFData PolicyComplianceDetail where
  rnf PolicyComplianceDetail' {..} =
    Prelude.rnf expiredAt
      `Prelude.seq` Prelude.rnf memberAccount
      `Prelude.seq` Prelude.rnf policyOwner
      `Prelude.seq` Prelude.rnf issueInfoMap
      `Prelude.seq` Prelude.rnf evaluationLimitExceeded
      `Prelude.seq` Prelude.rnf violators
      `Prelude.seq` Prelude.rnf policyId
