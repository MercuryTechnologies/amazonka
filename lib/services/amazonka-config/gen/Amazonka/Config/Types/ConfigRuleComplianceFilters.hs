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
-- Module      : Amazonka.Config.Types.ConfigRuleComplianceFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ConfigRuleComplianceFilters where

import Amazonka.Config.Types.ComplianceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Filters the compliance results based on account ID, region, compliance
-- type, and rule name.
--
-- /See:/ 'newConfigRuleComplianceFilters' smart constructor.
data ConfigRuleComplianceFilters = ConfigRuleComplianceFilters'
  { -- | The name of the Config rule.
    configRuleName :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account ID of the source account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The rule compliance status.
    --
    -- For the @ConfigRuleComplianceFilters@ data type, Config supports only
    -- @COMPLIANT@ and @NON_COMPLIANT@. Config does not support the
    -- @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
    complianceType :: Prelude.Maybe ComplianceType,
    -- | The source region where the data is aggregated.
    awsRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConfigRuleComplianceFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configRuleName', 'configRuleComplianceFilters_configRuleName' - The name of the Config rule.
--
-- 'accountId', 'configRuleComplianceFilters_accountId' - The 12-digit account ID of the source account.
--
-- 'complianceType', 'configRuleComplianceFilters_complianceType' - The rule compliance status.
--
-- For the @ConfigRuleComplianceFilters@ data type, Config supports only
-- @COMPLIANT@ and @NON_COMPLIANT@. Config does not support the
-- @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
--
-- 'awsRegion', 'configRuleComplianceFilters_awsRegion' - The source region where the data is aggregated.
newConfigRuleComplianceFilters ::
  ConfigRuleComplianceFilters
newConfigRuleComplianceFilters =
  ConfigRuleComplianceFilters'
    { configRuleName =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      awsRegion = Prelude.Nothing
    }

-- | The name of the Config rule.
configRuleComplianceFilters_configRuleName :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe Prelude.Text)
configRuleComplianceFilters_configRuleName = Lens.lens (\ConfigRuleComplianceFilters' {configRuleName} -> configRuleName) (\s@ConfigRuleComplianceFilters' {} a -> s {configRuleName = a} :: ConfigRuleComplianceFilters)

-- | The 12-digit account ID of the source account.
configRuleComplianceFilters_accountId :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe Prelude.Text)
configRuleComplianceFilters_accountId = Lens.lens (\ConfigRuleComplianceFilters' {accountId} -> accountId) (\s@ConfigRuleComplianceFilters' {} a -> s {accountId = a} :: ConfigRuleComplianceFilters)

-- | The rule compliance status.
--
-- For the @ConfigRuleComplianceFilters@ data type, Config supports only
-- @COMPLIANT@ and @NON_COMPLIANT@. Config does not support the
-- @NOT_APPLICABLE@ and the @INSUFFICIENT_DATA@ values.
configRuleComplianceFilters_complianceType :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe ComplianceType)
configRuleComplianceFilters_complianceType = Lens.lens (\ConfigRuleComplianceFilters' {complianceType} -> complianceType) (\s@ConfigRuleComplianceFilters' {} a -> s {complianceType = a} :: ConfigRuleComplianceFilters)

-- | The source region where the data is aggregated.
configRuleComplianceFilters_awsRegion :: Lens.Lens' ConfigRuleComplianceFilters (Prelude.Maybe Prelude.Text)
configRuleComplianceFilters_awsRegion = Lens.lens (\ConfigRuleComplianceFilters' {awsRegion} -> awsRegion) (\s@ConfigRuleComplianceFilters' {} a -> s {awsRegion = a} :: ConfigRuleComplianceFilters)

instance Prelude.Hashable ConfigRuleComplianceFilters where
  hashWithSalt salt' ConfigRuleComplianceFilters' {..} =
    salt' `Prelude.hashWithSalt` awsRegion
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` configRuleName

instance Prelude.NFData ConfigRuleComplianceFilters where
  rnf ConfigRuleComplianceFilters' {..} =
    Prelude.rnf configRuleName
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf accountId

instance Core.ToJSON ConfigRuleComplianceFilters where
  toJSON ConfigRuleComplianceFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ConfigRuleName" Core..=)
              Prelude.<$> configRuleName,
            ("AccountId" Core..=) Prelude.<$> accountId,
            ("ComplianceType" Core..=)
              Prelude.<$> complianceType,
            ("AwsRegion" Core..=) Prelude.<$> awsRegion
          ]
      )
