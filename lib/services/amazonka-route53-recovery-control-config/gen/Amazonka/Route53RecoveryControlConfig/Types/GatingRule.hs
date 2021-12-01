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
-- Module      : Amazonka.Route53RecoveryControlConfig.Types.GatingRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryControlConfig.Types.GatingRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53RecoveryControlConfig.Types.RuleConfig
import Amazonka.Route53RecoveryControlConfig.Types.Status

-- | A gating rule verifies that a set of gating controls evaluates as true,
-- based on a rule configuration that you specify. If the gating rule
-- evaluates to true, Amazon Route 53 Application Recovery Controller
-- allows a set of routing control state changes to run and complete
-- against the set of target controls.
--
-- /See:/ 'newGatingRule' smart constructor.
data GatingRule = GatingRule'
  { -- | The deployment status of a gating rule. Status can be one of the
    -- following: PENDING, DEPLOYED, PENDING_DELETION.
    status :: Status,
    -- | Routing controls that can only be set or unset if the specified
    -- RuleConfig evaluates to true for the specified GatingControls. For
    -- example, say you have three gating controls, one for each of three
    -- Amazon Web Services Regions. Now you specify ATLEAST 2 as your
    -- RuleConfig. With these settings, you can only change (set or unset) the
    -- routing controls that you have specified as TargetControls if that rule
    -- evaluates to true.
    --
    -- In other words, your ability to change the routing controls that you
    -- have specified as TargetControls is gated by the rule that you set for
    -- the routing controls in GatingControls.
    targetControls :: [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the control panel.
    controlPanelArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the gating rule.
    safetyRuleArn :: Prelude.Text,
    -- | The gating controls for the gating rule. That is, routing controls that
    -- are evaluated by the rule configuration that you specify.
    gatingControls :: [Prelude.Text],
    -- | The criteria that you set for specific gating controls (routing
    -- controls) that designates how many controls must be enabled to allow you
    -- to change (set or unset) the target controls.
    ruleConfig :: RuleConfig,
    -- | An evaluation period, in milliseconds (ms), during which any request
    -- against the target routing controls will fail. This helps prevent
    -- \"flapping\" of state. The wait period is 5000 ms by default, but you
    -- can choose a custom value.
    waitPeriodMs :: Prelude.Int,
    -- | The name for the gating rule.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'gatingRule_status' - The deployment status of a gating rule. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
--
-- 'targetControls', 'gatingRule_targetControls' - Routing controls that can only be set or unset if the specified
-- RuleConfig evaluates to true for the specified GatingControls. For
-- example, say you have three gating controls, one for each of three
-- Amazon Web Services Regions. Now you specify ATLEAST 2 as your
-- RuleConfig. With these settings, you can only change (set or unset) the
-- routing controls that you have specified as TargetControls if that rule
-- evaluates to true.
--
-- In other words, your ability to change the routing controls that you
-- have specified as TargetControls is gated by the rule that you set for
-- the routing controls in GatingControls.
--
-- 'controlPanelArn', 'gatingRule_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel.
--
-- 'safetyRuleArn', 'gatingRule_safetyRuleArn' - The Amazon Resource Name (ARN) of the gating rule.
--
-- 'gatingControls', 'gatingRule_gatingControls' - The gating controls for the gating rule. That is, routing controls that
-- are evaluated by the rule configuration that you specify.
--
-- 'ruleConfig', 'gatingRule_ruleConfig' - The criteria that you set for specific gating controls (routing
-- controls) that designates how many controls must be enabled to allow you
-- to change (set or unset) the target controls.
--
-- 'waitPeriodMs', 'gatingRule_waitPeriodMs' - An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
--
-- 'name', 'gatingRule_name' - The name for the gating rule.
newGatingRule ::
  -- | 'status'
  Status ->
  -- | 'controlPanelArn'
  Prelude.Text ->
  -- | 'safetyRuleArn'
  Prelude.Text ->
  -- | 'ruleConfig'
  RuleConfig ->
  -- | 'waitPeriodMs'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  GatingRule
newGatingRule
  pStatus_
  pControlPanelArn_
  pSafetyRuleArn_
  pRuleConfig_
  pWaitPeriodMs_
  pName_ =
    GatingRule'
      { status = pStatus_,
        targetControls = Prelude.mempty,
        controlPanelArn = pControlPanelArn_,
        safetyRuleArn = pSafetyRuleArn_,
        gatingControls = Prelude.mempty,
        ruleConfig = pRuleConfig_,
        waitPeriodMs = pWaitPeriodMs_,
        name = pName_
      }

-- | The deployment status of a gating rule. Status can be one of the
-- following: PENDING, DEPLOYED, PENDING_DELETION.
gatingRule_status :: Lens.Lens' GatingRule Status
gatingRule_status = Lens.lens (\GatingRule' {status} -> status) (\s@GatingRule' {} a -> s {status = a} :: GatingRule)

-- | Routing controls that can only be set or unset if the specified
-- RuleConfig evaluates to true for the specified GatingControls. For
-- example, say you have three gating controls, one for each of three
-- Amazon Web Services Regions. Now you specify ATLEAST 2 as your
-- RuleConfig. With these settings, you can only change (set or unset) the
-- routing controls that you have specified as TargetControls if that rule
-- evaluates to true.
--
-- In other words, your ability to change the routing controls that you
-- have specified as TargetControls is gated by the rule that you set for
-- the routing controls in GatingControls.
gatingRule_targetControls :: Lens.Lens' GatingRule [Prelude.Text]
gatingRule_targetControls = Lens.lens (\GatingRule' {targetControls} -> targetControls) (\s@GatingRule' {} a -> s {targetControls = a} :: GatingRule) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the control panel.
gatingRule_controlPanelArn :: Lens.Lens' GatingRule Prelude.Text
gatingRule_controlPanelArn = Lens.lens (\GatingRule' {controlPanelArn} -> controlPanelArn) (\s@GatingRule' {} a -> s {controlPanelArn = a} :: GatingRule)

-- | The Amazon Resource Name (ARN) of the gating rule.
gatingRule_safetyRuleArn :: Lens.Lens' GatingRule Prelude.Text
gatingRule_safetyRuleArn = Lens.lens (\GatingRule' {safetyRuleArn} -> safetyRuleArn) (\s@GatingRule' {} a -> s {safetyRuleArn = a} :: GatingRule)

-- | The gating controls for the gating rule. That is, routing controls that
-- are evaluated by the rule configuration that you specify.
gatingRule_gatingControls :: Lens.Lens' GatingRule [Prelude.Text]
gatingRule_gatingControls = Lens.lens (\GatingRule' {gatingControls} -> gatingControls) (\s@GatingRule' {} a -> s {gatingControls = a} :: GatingRule) Prelude.. Lens.coerced

-- | The criteria that you set for specific gating controls (routing
-- controls) that designates how many controls must be enabled to allow you
-- to change (set or unset) the target controls.
gatingRule_ruleConfig :: Lens.Lens' GatingRule RuleConfig
gatingRule_ruleConfig = Lens.lens (\GatingRule' {ruleConfig} -> ruleConfig) (\s@GatingRule' {} a -> s {ruleConfig = a} :: GatingRule)

-- | An evaluation period, in milliseconds (ms), during which any request
-- against the target routing controls will fail. This helps prevent
-- \"flapping\" of state. The wait period is 5000 ms by default, but you
-- can choose a custom value.
gatingRule_waitPeriodMs :: Lens.Lens' GatingRule Prelude.Int
gatingRule_waitPeriodMs = Lens.lens (\GatingRule' {waitPeriodMs} -> waitPeriodMs) (\s@GatingRule' {} a -> s {waitPeriodMs = a} :: GatingRule)

-- | The name for the gating rule.
gatingRule_name :: Lens.Lens' GatingRule Prelude.Text
gatingRule_name = Lens.lens (\GatingRule' {name} -> name) (\s@GatingRule' {} a -> s {name = a} :: GatingRule)

instance Core.FromJSON GatingRule where
  parseJSON =
    Core.withObject
      "GatingRule"
      ( \x ->
          GatingRule'
            Prelude.<$> (x Core..: "Status")
            Prelude.<*> (x Core..:? "TargetControls" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "ControlPanelArn")
            Prelude.<*> (x Core..: "SafetyRuleArn")
            Prelude.<*> (x Core..:? "GatingControls" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "RuleConfig")
            Prelude.<*> (x Core..: "WaitPeriodMs")
            Prelude.<*> (x Core..: "Name")
      )

instance Prelude.Hashable GatingRule where
  hashWithSalt salt' GatingRule' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` waitPeriodMs
      `Prelude.hashWithSalt` ruleConfig
      `Prelude.hashWithSalt` gatingControls
      `Prelude.hashWithSalt` safetyRuleArn
      `Prelude.hashWithSalt` controlPanelArn
      `Prelude.hashWithSalt` targetControls
      `Prelude.hashWithSalt` status

instance Prelude.NFData GatingRule where
  rnf GatingRule' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf waitPeriodMs
      `Prelude.seq` Prelude.rnf ruleConfig
      `Prelude.seq` Prelude.rnf gatingControls
      `Prelude.seq` Prelude.rnf safetyRuleArn
      `Prelude.seq` Prelude.rnf controlPanelArn
      `Prelude.seq` Prelude.rnf targetControls
