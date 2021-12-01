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
-- Module      : Amazonka.ELBV2.Types.ForwardActionConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.ForwardActionConfig where

import qualified Amazonka.Core as Core
import Amazonka.ELBV2.Types.TargetGroupStickinessConfig
import Amazonka.ELBV2.Types.TargetGroupTuple
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a forward action.
--
-- /See:/ 'newForwardActionConfig' smart constructor.
data ForwardActionConfig = ForwardActionConfig'
  { -- | One or more target groups. For Network Load Balancers, you can specify a
    -- single target group.
    targetGroups :: Prelude.Maybe [TargetGroupTuple],
    -- | The target group stickiness for the rule.
    targetGroupStickinessConfig :: Prelude.Maybe TargetGroupStickinessConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ForwardActionConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetGroups', 'forwardActionConfig_targetGroups' - One or more target groups. For Network Load Balancers, you can specify a
-- single target group.
--
-- 'targetGroupStickinessConfig', 'forwardActionConfig_targetGroupStickinessConfig' - The target group stickiness for the rule.
newForwardActionConfig ::
  ForwardActionConfig
newForwardActionConfig =
  ForwardActionConfig'
    { targetGroups =
        Prelude.Nothing,
      targetGroupStickinessConfig = Prelude.Nothing
    }

-- | One or more target groups. For Network Load Balancers, you can specify a
-- single target group.
forwardActionConfig_targetGroups :: Lens.Lens' ForwardActionConfig (Prelude.Maybe [TargetGroupTuple])
forwardActionConfig_targetGroups = Lens.lens (\ForwardActionConfig' {targetGroups} -> targetGroups) (\s@ForwardActionConfig' {} a -> s {targetGroups = a} :: ForwardActionConfig) Prelude.. Lens.mapping Lens.coerced

-- | The target group stickiness for the rule.
forwardActionConfig_targetGroupStickinessConfig :: Lens.Lens' ForwardActionConfig (Prelude.Maybe TargetGroupStickinessConfig)
forwardActionConfig_targetGroupStickinessConfig = Lens.lens (\ForwardActionConfig' {targetGroupStickinessConfig} -> targetGroupStickinessConfig) (\s@ForwardActionConfig' {} a -> s {targetGroupStickinessConfig = a} :: ForwardActionConfig)

instance Core.FromXML ForwardActionConfig where
  parseXML x =
    ForwardActionConfig'
      Prelude.<$> ( x Core..@? "TargetGroups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "TargetGroupStickinessConfig")

instance Prelude.Hashable ForwardActionConfig where
  hashWithSalt salt' ForwardActionConfig' {..} =
    salt'
      `Prelude.hashWithSalt` targetGroupStickinessConfig
      `Prelude.hashWithSalt` targetGroups

instance Prelude.NFData ForwardActionConfig where
  rnf ForwardActionConfig' {..} =
    Prelude.rnf targetGroups
      `Prelude.seq` Prelude.rnf targetGroupStickinessConfig

instance Core.ToQuery ForwardActionConfig where
  toQuery ForwardActionConfig' {..} =
    Prelude.mconcat
      [ "TargetGroups"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> targetGroups),
        "TargetGroupStickinessConfig"
          Core.=: targetGroupStickinessConfig
      ]
