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
-- Module      : Amazonka.ELB.Types.Policies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.Policies where

import qualified Amazonka.Core as Core
import Amazonka.ELB.Internal
import Amazonka.ELB.Types.AppCookieStickinessPolicy
import Amazonka.ELB.Types.LBCookieStickinessPolicy
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The policies for a load balancer.
--
-- /See:/ 'newPolicies' smart constructor.
data Policies = Policies'
  { -- | The policies other than the stickiness policies.
    otherPolicies :: Prelude.Maybe [Prelude.Text],
    -- | The stickiness policies created using CreateLBCookieStickinessPolicy.
    lBCookieStickinessPolicies :: Prelude.Maybe [LBCookieStickinessPolicy],
    -- | The stickiness policies created using CreateAppCookieStickinessPolicy.
    appCookieStickinessPolicies :: Prelude.Maybe [AppCookieStickinessPolicy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Policies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'otherPolicies', 'policies_otherPolicies' - The policies other than the stickiness policies.
--
-- 'lBCookieStickinessPolicies', 'policies_lBCookieStickinessPolicies' - The stickiness policies created using CreateLBCookieStickinessPolicy.
--
-- 'appCookieStickinessPolicies', 'policies_appCookieStickinessPolicies' - The stickiness policies created using CreateAppCookieStickinessPolicy.
newPolicies ::
  Policies
newPolicies =
  Policies'
    { otherPolicies = Prelude.Nothing,
      lBCookieStickinessPolicies = Prelude.Nothing,
      appCookieStickinessPolicies = Prelude.Nothing
    }

-- | The policies other than the stickiness policies.
policies_otherPolicies :: Lens.Lens' Policies (Prelude.Maybe [Prelude.Text])
policies_otherPolicies = Lens.lens (\Policies' {otherPolicies} -> otherPolicies) (\s@Policies' {} a -> s {otherPolicies = a} :: Policies) Prelude.. Lens.mapping Lens.coerced

-- | The stickiness policies created using CreateLBCookieStickinessPolicy.
policies_lBCookieStickinessPolicies :: Lens.Lens' Policies (Prelude.Maybe [LBCookieStickinessPolicy])
policies_lBCookieStickinessPolicies = Lens.lens (\Policies' {lBCookieStickinessPolicies} -> lBCookieStickinessPolicies) (\s@Policies' {} a -> s {lBCookieStickinessPolicies = a} :: Policies) Prelude.. Lens.mapping Lens.coerced

-- | The stickiness policies created using CreateAppCookieStickinessPolicy.
policies_appCookieStickinessPolicies :: Lens.Lens' Policies (Prelude.Maybe [AppCookieStickinessPolicy])
policies_appCookieStickinessPolicies = Lens.lens (\Policies' {appCookieStickinessPolicies} -> appCookieStickinessPolicies) (\s@Policies' {} a -> s {appCookieStickinessPolicies = a} :: Policies) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML Policies where
  parseXML x =
    Policies'
      Prelude.<$> ( x Core..@? "OtherPolicies" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "LBCookieStickinessPolicies"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "AppCookieStickinessPolicies"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable Policies where
  hashWithSalt salt' Policies' {..} =
    salt'
      `Prelude.hashWithSalt` appCookieStickinessPolicies
      `Prelude.hashWithSalt` lBCookieStickinessPolicies
      `Prelude.hashWithSalt` otherPolicies

instance Prelude.NFData Policies where
  rnf Policies' {..} =
    Prelude.rnf otherPolicies
      `Prelude.seq` Prelude.rnf appCookieStickinessPolicies
      `Prelude.seq` Prelude.rnf lBCookieStickinessPolicies
