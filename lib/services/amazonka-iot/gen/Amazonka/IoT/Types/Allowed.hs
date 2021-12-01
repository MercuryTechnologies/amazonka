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
-- Module      : Amazonka.IoT.Types.Allowed
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Allowed where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.Policy
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information that allowed the authorization.
--
-- /See:/ 'newAllowed' smart constructor.
data Allowed = Allowed'
  { -- | A list of policies that allowed the authentication.
    policies :: Prelude.Maybe [Policy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Allowed' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'allowed_policies' - A list of policies that allowed the authentication.
newAllowed ::
  Allowed
newAllowed = Allowed' {policies = Prelude.Nothing}

-- | A list of policies that allowed the authentication.
allowed_policies :: Lens.Lens' Allowed (Prelude.Maybe [Policy])
allowed_policies = Lens.lens (\Allowed' {policies} -> policies) (\s@Allowed' {} a -> s {policies = a} :: Allowed) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Allowed where
  parseJSON =
    Core.withObject
      "Allowed"
      ( \x ->
          Allowed'
            Prelude.<$> (x Core..:? "policies" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Allowed where
  hashWithSalt salt' Allowed' {..} =
    salt' `Prelude.hashWithSalt` policies

instance Prelude.NFData Allowed where
  rnf Allowed' {..} = Prelude.rnf policies
