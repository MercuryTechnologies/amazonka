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
-- Module      : Amazonka.NetworkFirewall.Types.StatefulRuleOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.StatefulRuleOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkFirewall.Types.RuleOrder
import qualified Amazonka.Prelude as Prelude

-- | Additional options governing how Network Firewall handles the rule
-- group. You can only use these for stateful rule groups.
--
-- /See:/ 'newStatefulRuleOptions' smart constructor.
data StatefulRuleOptions = StatefulRuleOptions'
  { -- | Indicates how to manage the order of the rule evaluation for the rule
    -- group. By default, Network Firewall leaves the rule evaluation order up
    -- to the Suricata rule processing engine. If you set this to
    -- @STRICT_ORDER@, your rules are evaluated in the exact order that
    -- they\'re listed in your Suricata rules string.
    ruleOrder :: Prelude.Maybe RuleOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StatefulRuleOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ruleOrder', 'statefulRuleOptions_ruleOrder' - Indicates how to manage the order of the rule evaluation for the rule
-- group. By default, Network Firewall leaves the rule evaluation order up
-- to the Suricata rule processing engine. If you set this to
-- @STRICT_ORDER@, your rules are evaluated in the exact order that
-- they\'re listed in your Suricata rules string.
newStatefulRuleOptions ::
  StatefulRuleOptions
newStatefulRuleOptions =
  StatefulRuleOptions' {ruleOrder = Prelude.Nothing}

-- | Indicates how to manage the order of the rule evaluation for the rule
-- group. By default, Network Firewall leaves the rule evaluation order up
-- to the Suricata rule processing engine. If you set this to
-- @STRICT_ORDER@, your rules are evaluated in the exact order that
-- they\'re listed in your Suricata rules string.
statefulRuleOptions_ruleOrder :: Lens.Lens' StatefulRuleOptions (Prelude.Maybe RuleOrder)
statefulRuleOptions_ruleOrder = Lens.lens (\StatefulRuleOptions' {ruleOrder} -> ruleOrder) (\s@StatefulRuleOptions' {} a -> s {ruleOrder = a} :: StatefulRuleOptions)

instance Core.FromJSON StatefulRuleOptions where
  parseJSON =
    Core.withObject
      "StatefulRuleOptions"
      ( \x ->
          StatefulRuleOptions'
            Prelude.<$> (x Core..:? "RuleOrder")
      )

instance Prelude.Hashable StatefulRuleOptions where
  hashWithSalt salt' StatefulRuleOptions' {..} =
    salt' `Prelude.hashWithSalt` ruleOrder

instance Prelude.NFData StatefulRuleOptions where
  rnf StatefulRuleOptions' {..} = Prelude.rnf ruleOrder

instance Core.ToJSON StatefulRuleOptions where
  toJSON StatefulRuleOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [("RuleOrder" Core..=) Prelude.<$> ruleOrder]
      )
