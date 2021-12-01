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
-- Module      : Amazonka.SSM.Types.ParameterInlinePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.ParameterInlinePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | One or more policies assigned to a parameter.
--
-- /See:/ 'newParameterInlinePolicy' smart constructor.
data ParameterInlinePolicy = ParameterInlinePolicy'
  { -- | The type of policy. Parameter Store, a capablility of Amazon Web
    -- Services Systems Manager, supports the following policy types:
    -- Expiration, ExpirationNotification, and NoChangeNotification.
    policyType :: Prelude.Maybe Prelude.Text,
    -- | The status of the policy. Policies report the following statuses:
    -- Pending (the policy hasn\'t been enforced or applied yet), Finished (the
    -- policy was applied), Failed (the policy wasn\'t applied), or InProgress
    -- (the policy is being applied now).
    policyStatus :: Prelude.Maybe Prelude.Text,
    -- | The JSON text of the policy.
    policyText :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterInlinePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyType', 'parameterInlinePolicy_policyType' - The type of policy. Parameter Store, a capablility of Amazon Web
-- Services Systems Manager, supports the following policy types:
-- Expiration, ExpirationNotification, and NoChangeNotification.
--
-- 'policyStatus', 'parameterInlinePolicy_policyStatus' - The status of the policy. Policies report the following statuses:
-- Pending (the policy hasn\'t been enforced or applied yet), Finished (the
-- policy was applied), Failed (the policy wasn\'t applied), or InProgress
-- (the policy is being applied now).
--
-- 'policyText', 'parameterInlinePolicy_policyText' - The JSON text of the policy.
newParameterInlinePolicy ::
  ParameterInlinePolicy
newParameterInlinePolicy =
  ParameterInlinePolicy'
    { policyType =
        Prelude.Nothing,
      policyStatus = Prelude.Nothing,
      policyText = Prelude.Nothing
    }

-- | The type of policy. Parameter Store, a capablility of Amazon Web
-- Services Systems Manager, supports the following policy types:
-- Expiration, ExpirationNotification, and NoChangeNotification.
parameterInlinePolicy_policyType :: Lens.Lens' ParameterInlinePolicy (Prelude.Maybe Prelude.Text)
parameterInlinePolicy_policyType = Lens.lens (\ParameterInlinePolicy' {policyType} -> policyType) (\s@ParameterInlinePolicy' {} a -> s {policyType = a} :: ParameterInlinePolicy)

-- | The status of the policy. Policies report the following statuses:
-- Pending (the policy hasn\'t been enforced or applied yet), Finished (the
-- policy was applied), Failed (the policy wasn\'t applied), or InProgress
-- (the policy is being applied now).
parameterInlinePolicy_policyStatus :: Lens.Lens' ParameterInlinePolicy (Prelude.Maybe Prelude.Text)
parameterInlinePolicy_policyStatus = Lens.lens (\ParameterInlinePolicy' {policyStatus} -> policyStatus) (\s@ParameterInlinePolicy' {} a -> s {policyStatus = a} :: ParameterInlinePolicy)

-- | The JSON text of the policy.
parameterInlinePolicy_policyText :: Lens.Lens' ParameterInlinePolicy (Prelude.Maybe Prelude.Text)
parameterInlinePolicy_policyText = Lens.lens (\ParameterInlinePolicy' {policyText} -> policyText) (\s@ParameterInlinePolicy' {} a -> s {policyText = a} :: ParameterInlinePolicy)

instance Core.FromJSON ParameterInlinePolicy where
  parseJSON =
    Core.withObject
      "ParameterInlinePolicy"
      ( \x ->
          ParameterInlinePolicy'
            Prelude.<$> (x Core..:? "PolicyType")
            Prelude.<*> (x Core..:? "PolicyStatus")
            Prelude.<*> (x Core..:? "PolicyText")
      )

instance Prelude.Hashable ParameterInlinePolicy where
  hashWithSalt salt' ParameterInlinePolicy' {..} =
    salt' `Prelude.hashWithSalt` policyText
      `Prelude.hashWithSalt` policyStatus
      `Prelude.hashWithSalt` policyType

instance Prelude.NFData ParameterInlinePolicy where
  rnf ParameterInlinePolicy' {..} =
    Prelude.rnf policyType
      `Prelude.seq` Prelude.rnf policyText
      `Prelude.seq` Prelude.rnf policyStatus
