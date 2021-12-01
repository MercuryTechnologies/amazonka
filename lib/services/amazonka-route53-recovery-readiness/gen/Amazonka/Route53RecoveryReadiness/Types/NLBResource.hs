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
-- Module      : Amazonka.Route53RecoveryReadiness.Types.NLBResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53RecoveryReadiness.Types.NLBResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The NLB resource a DNS Target Resource points to
--
-- /See:/ 'newNLBResource' smart constructor.
data NLBResource = NLBResource'
  { -- | An NLB resource arn
    arn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NLBResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'nLBResource_arn' - An NLB resource arn
newNLBResource ::
  NLBResource
newNLBResource = NLBResource' {arn = Prelude.Nothing}

-- | An NLB resource arn
nLBResource_arn :: Lens.Lens' NLBResource (Prelude.Maybe Prelude.Text)
nLBResource_arn = Lens.lens (\NLBResource' {arn} -> arn) (\s@NLBResource' {} a -> s {arn = a} :: NLBResource)

instance Core.FromJSON NLBResource where
  parseJSON =
    Core.withObject
      "NLBResource"
      (\x -> NLBResource' Prelude.<$> (x Core..:? "arn"))

instance Prelude.Hashable NLBResource where
  hashWithSalt salt' NLBResource' {..} =
    salt' `Prelude.hashWithSalt` arn

instance Prelude.NFData NLBResource where
  rnf NLBResource' {..} = Prelude.rnf arn

instance Core.ToJSON NLBResource where
  toJSON NLBResource' {..} =
    Core.object
      (Prelude.catMaybes [("arn" Core..=) Prelude.<$> arn])
