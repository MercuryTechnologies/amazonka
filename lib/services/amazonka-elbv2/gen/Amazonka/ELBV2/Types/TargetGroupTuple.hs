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
-- Module      : Amazonka.ELBV2.Types.TargetGroupTuple
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.TargetGroupTuple where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about how traffic will be distributed between multiple
-- target groups in a forward rule.
--
-- /See:/ 'newTargetGroupTuple' smart constructor.
data TargetGroupTuple = TargetGroupTuple'
  { -- | The weight. The range is 0 to 999.
    weight :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the target group.
    targetGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetGroupTuple' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'weight', 'targetGroupTuple_weight' - The weight. The range is 0 to 999.
--
-- 'targetGroupArn', 'targetGroupTuple_targetGroupArn' - The Amazon Resource Name (ARN) of the target group.
newTargetGroupTuple ::
  TargetGroupTuple
newTargetGroupTuple =
  TargetGroupTuple'
    { weight = Prelude.Nothing,
      targetGroupArn = Prelude.Nothing
    }

-- | The weight. The range is 0 to 999.
targetGroupTuple_weight :: Lens.Lens' TargetGroupTuple (Prelude.Maybe Prelude.Int)
targetGroupTuple_weight = Lens.lens (\TargetGroupTuple' {weight} -> weight) (\s@TargetGroupTuple' {} a -> s {weight = a} :: TargetGroupTuple)

-- | The Amazon Resource Name (ARN) of the target group.
targetGroupTuple_targetGroupArn :: Lens.Lens' TargetGroupTuple (Prelude.Maybe Prelude.Text)
targetGroupTuple_targetGroupArn = Lens.lens (\TargetGroupTuple' {targetGroupArn} -> targetGroupArn) (\s@TargetGroupTuple' {} a -> s {targetGroupArn = a} :: TargetGroupTuple)

instance Core.FromXML TargetGroupTuple where
  parseXML x =
    TargetGroupTuple'
      Prelude.<$> (x Core..@? "Weight")
      Prelude.<*> (x Core..@? "TargetGroupArn")

instance Prelude.Hashable TargetGroupTuple where
  hashWithSalt salt' TargetGroupTuple' {..} =
    salt' `Prelude.hashWithSalt` targetGroupArn
      `Prelude.hashWithSalt` weight

instance Prelude.NFData TargetGroupTuple where
  rnf TargetGroupTuple' {..} =
    Prelude.rnf weight
      `Prelude.seq` Prelude.rnf targetGroupArn

instance Core.ToQuery TargetGroupTuple where
  toQuery TargetGroupTuple' {..} =
    Prelude.mconcat
      [ "Weight" Core.=: weight,
        "TargetGroupArn" Core.=: targetGroupArn
      ]
