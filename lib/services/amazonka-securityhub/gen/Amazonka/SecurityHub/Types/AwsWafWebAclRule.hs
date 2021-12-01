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
-- Module      : Amazonka.SecurityHub.Types.AwsWafWebAclRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsWafWebAclRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.WafAction
import Amazonka.SecurityHub.Types.WafExcludedRule
import Amazonka.SecurityHub.Types.WafOverrideAction

-- | Details for a rule in an WAF WebACL.
--
-- /See:/ 'newAwsWafWebAclRule' smart constructor.
data AwsWafWebAclRule = AwsWafWebAclRule'
  { -- | Specifies the order in which the rules in a WebACL are evaluated. Rules
    -- with a lower value for @Priority@ are evaluated before rules with a
    -- higher value. The value must be a unique integer. If you add multiple
    -- rules to a WebACL, the values do not need to be consecutive.
    priority :: Prelude.Maybe Prelude.Int,
    -- | Use the @OverrideAction@ to test your RuleGroup.
    --
    -- Any rule in a RuleGroup can potentially block a request. If you set the
    -- @OverrideAction@ to @None@, the RuleGroup blocks a request if any
    -- individual rule in the RuleGroup matches the request and is configured
    -- to block that request.
    --
    -- However, if you first want to test the RuleGroup, set the
    -- @OverrideAction@ to @Count@. The RuleGroup then overrides any block
    -- action specified by individual rules contained within the group. Instead
    -- of blocking matching requests, those requests are counted.
    --
    -- @ActivatedRule@|@OverrideAction@ applies only when updating or adding a
    -- RuleGroup to a WebACL. In this case you do not use
    -- @ActivatedRule@|@Action@. For all other update requests,
    -- @ActivatedRule@|@Action@ is used instead of
    -- @ActivatedRule@|@OverrideAction@.
    overrideAction :: Prelude.Maybe WafOverrideAction,
    -- | The identifier for a rule.
    ruleId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the action that CloudFront or WAF takes when a web request
    -- matches the conditions in the rule.
    action :: Prelude.Maybe WafAction,
    -- | Rules to exclude from a rule group.
    excludedRules :: Prelude.Maybe [WafExcludedRule],
    -- | The rule type.
    --
    -- Valid values: @REGULAR@ | @RATE_BASED@ | @GROUP@
    --
    -- The default is @REGULAR@.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsWafWebAclRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priority', 'awsWafWebAclRule_priority' - Specifies the order in which the rules in a WebACL are evaluated. Rules
-- with a lower value for @Priority@ are evaluated before rules with a
-- higher value. The value must be a unique integer. If you add multiple
-- rules to a WebACL, the values do not need to be consecutive.
--
-- 'overrideAction', 'awsWafWebAclRule_overrideAction' - Use the @OverrideAction@ to test your RuleGroup.
--
-- Any rule in a RuleGroup can potentially block a request. If you set the
-- @OverrideAction@ to @None@, the RuleGroup blocks a request if any
-- individual rule in the RuleGroup matches the request and is configured
-- to block that request.
--
-- However, if you first want to test the RuleGroup, set the
-- @OverrideAction@ to @Count@. The RuleGroup then overrides any block
-- action specified by individual rules contained within the group. Instead
-- of blocking matching requests, those requests are counted.
--
-- @ActivatedRule@|@OverrideAction@ applies only when updating or adding a
-- RuleGroup to a WebACL. In this case you do not use
-- @ActivatedRule@|@Action@. For all other update requests,
-- @ActivatedRule@|@Action@ is used instead of
-- @ActivatedRule@|@OverrideAction@.
--
-- 'ruleId', 'awsWafWebAclRule_ruleId' - The identifier for a rule.
--
-- 'action', 'awsWafWebAclRule_action' - Specifies the action that CloudFront or WAF takes when a web request
-- matches the conditions in the rule.
--
-- 'excludedRules', 'awsWafWebAclRule_excludedRules' - Rules to exclude from a rule group.
--
-- 'type'', 'awsWafWebAclRule_type' - The rule type.
--
-- Valid values: @REGULAR@ | @RATE_BASED@ | @GROUP@
--
-- The default is @REGULAR@.
newAwsWafWebAclRule ::
  AwsWafWebAclRule
newAwsWafWebAclRule =
  AwsWafWebAclRule'
    { priority = Prelude.Nothing,
      overrideAction = Prelude.Nothing,
      ruleId = Prelude.Nothing,
      action = Prelude.Nothing,
      excludedRules = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Specifies the order in which the rules in a WebACL are evaluated. Rules
-- with a lower value for @Priority@ are evaluated before rules with a
-- higher value. The value must be a unique integer. If you add multiple
-- rules to a WebACL, the values do not need to be consecutive.
awsWafWebAclRule_priority :: Lens.Lens' AwsWafWebAclRule (Prelude.Maybe Prelude.Int)
awsWafWebAclRule_priority = Lens.lens (\AwsWafWebAclRule' {priority} -> priority) (\s@AwsWafWebAclRule' {} a -> s {priority = a} :: AwsWafWebAclRule)

-- | Use the @OverrideAction@ to test your RuleGroup.
--
-- Any rule in a RuleGroup can potentially block a request. If you set the
-- @OverrideAction@ to @None@, the RuleGroup blocks a request if any
-- individual rule in the RuleGroup matches the request and is configured
-- to block that request.
--
-- However, if you first want to test the RuleGroup, set the
-- @OverrideAction@ to @Count@. The RuleGroup then overrides any block
-- action specified by individual rules contained within the group. Instead
-- of blocking matching requests, those requests are counted.
--
-- @ActivatedRule@|@OverrideAction@ applies only when updating or adding a
-- RuleGroup to a WebACL. In this case you do not use
-- @ActivatedRule@|@Action@. For all other update requests,
-- @ActivatedRule@|@Action@ is used instead of
-- @ActivatedRule@|@OverrideAction@.
awsWafWebAclRule_overrideAction :: Lens.Lens' AwsWafWebAclRule (Prelude.Maybe WafOverrideAction)
awsWafWebAclRule_overrideAction = Lens.lens (\AwsWafWebAclRule' {overrideAction} -> overrideAction) (\s@AwsWafWebAclRule' {} a -> s {overrideAction = a} :: AwsWafWebAclRule)

-- | The identifier for a rule.
awsWafWebAclRule_ruleId :: Lens.Lens' AwsWafWebAclRule (Prelude.Maybe Prelude.Text)
awsWafWebAclRule_ruleId = Lens.lens (\AwsWafWebAclRule' {ruleId} -> ruleId) (\s@AwsWafWebAclRule' {} a -> s {ruleId = a} :: AwsWafWebAclRule)

-- | Specifies the action that CloudFront or WAF takes when a web request
-- matches the conditions in the rule.
awsWafWebAclRule_action :: Lens.Lens' AwsWafWebAclRule (Prelude.Maybe WafAction)
awsWafWebAclRule_action = Lens.lens (\AwsWafWebAclRule' {action} -> action) (\s@AwsWafWebAclRule' {} a -> s {action = a} :: AwsWafWebAclRule)

-- | Rules to exclude from a rule group.
awsWafWebAclRule_excludedRules :: Lens.Lens' AwsWafWebAclRule (Prelude.Maybe [WafExcludedRule])
awsWafWebAclRule_excludedRules = Lens.lens (\AwsWafWebAclRule' {excludedRules} -> excludedRules) (\s@AwsWafWebAclRule' {} a -> s {excludedRules = a} :: AwsWafWebAclRule) Prelude.. Lens.mapping Lens.coerced

-- | The rule type.
--
-- Valid values: @REGULAR@ | @RATE_BASED@ | @GROUP@
--
-- The default is @REGULAR@.
awsWafWebAclRule_type :: Lens.Lens' AwsWafWebAclRule (Prelude.Maybe Prelude.Text)
awsWafWebAclRule_type = Lens.lens (\AwsWafWebAclRule' {type'} -> type') (\s@AwsWafWebAclRule' {} a -> s {type' = a} :: AwsWafWebAclRule)

instance Core.FromJSON AwsWafWebAclRule where
  parseJSON =
    Core.withObject
      "AwsWafWebAclRule"
      ( \x ->
          AwsWafWebAclRule'
            Prelude.<$> (x Core..:? "Priority")
            Prelude.<*> (x Core..:? "OverrideAction")
            Prelude.<*> (x Core..:? "RuleId")
            Prelude.<*> (x Core..:? "Action")
            Prelude.<*> (x Core..:? "ExcludedRules" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable AwsWafWebAclRule where
  hashWithSalt salt' AwsWafWebAclRule' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` excludedRules
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` ruleId
      `Prelude.hashWithSalt` overrideAction
      `Prelude.hashWithSalt` priority

instance Prelude.NFData AwsWafWebAclRule where
  rnf AwsWafWebAclRule' {..} =
    Prelude.rnf priority
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf excludedRules
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf ruleId
      `Prelude.seq` Prelude.rnf overrideAction

instance Core.ToJSON AwsWafWebAclRule where
  toJSON AwsWafWebAclRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Priority" Core..=) Prelude.<$> priority,
            ("OverrideAction" Core..=)
              Prelude.<$> overrideAction,
            ("RuleId" Core..=) Prelude.<$> ruleId,
            ("Action" Core..=) Prelude.<$> action,
            ("ExcludedRules" Core..=) Prelude.<$> excludedRules,
            ("Type" Core..=) Prelude.<$> type'
          ]
      )
