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
-- Module      : Amazonka.IoT.Types.ExplicitDeny
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ExplicitDeny where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.Policy
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information that explicitly denies authorization.
--
-- /See:/ 'newExplicitDeny' smart constructor.
data ExplicitDeny = ExplicitDeny'
  { -- | The policies that denied the authorization.
    policies :: Prelude.Maybe [Policy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExplicitDeny' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'explicitDeny_policies' - The policies that denied the authorization.
newExplicitDeny ::
  ExplicitDeny
newExplicitDeny =
  ExplicitDeny' {policies = Prelude.Nothing}

-- | The policies that denied the authorization.
explicitDeny_policies :: Lens.Lens' ExplicitDeny (Prelude.Maybe [Policy])
explicitDeny_policies = Lens.lens (\ExplicitDeny' {policies} -> policies) (\s@ExplicitDeny' {} a -> s {policies = a} :: ExplicitDeny) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ExplicitDeny where
  parseJSON =
    Core.withObject
      "ExplicitDeny"
      ( \x ->
          ExplicitDeny'
            Prelude.<$> (x Core..:? "policies" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ExplicitDeny where
  hashWithSalt salt' ExplicitDeny' {..} =
    salt' `Prelude.hashWithSalt` policies

instance Prelude.NFData ExplicitDeny where
  rnf ExplicitDeny' {..} = Prelude.rnf policies
