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
-- Module      : Amazonka.Route53Resolver.Types.ResolverRuleConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverRuleConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.TargetAddress

-- | In an
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_UpdateResolverRule.html UpdateResolverRule>
-- request, information about the changes that you want to make.
--
-- /See:/ 'newResolverRuleConfig' smart constructor.
data ResolverRuleConfig = ResolverRuleConfig'
  { -- | The ID of the new outbound Resolver endpoint that you want to use to
    -- route DNS queries to the IP addresses that you specify in @TargetIps@.
    resolverEndpointId :: Prelude.Maybe Prelude.Text,
    -- | For DNS queries that originate in your VPC, the new IP addresses that
    -- you want to route outbound DNS queries to.
    targetIps :: Prelude.Maybe (Prelude.NonEmpty TargetAddress),
    -- | The new name for the Resolver rule. The name that you specify appears in
    -- the Resolver dashboard in the Route 53 console.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolverRuleConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverEndpointId', 'resolverRuleConfig_resolverEndpointId' - The ID of the new outbound Resolver endpoint that you want to use to
-- route DNS queries to the IP addresses that you specify in @TargetIps@.
--
-- 'targetIps', 'resolverRuleConfig_targetIps' - For DNS queries that originate in your VPC, the new IP addresses that
-- you want to route outbound DNS queries to.
--
-- 'name', 'resolverRuleConfig_name' - The new name for the Resolver rule. The name that you specify appears in
-- the Resolver dashboard in the Route 53 console.
newResolverRuleConfig ::
  ResolverRuleConfig
newResolverRuleConfig =
  ResolverRuleConfig'
    { resolverEndpointId =
        Prelude.Nothing,
      targetIps = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The ID of the new outbound Resolver endpoint that you want to use to
-- route DNS queries to the IP addresses that you specify in @TargetIps@.
resolverRuleConfig_resolverEndpointId :: Lens.Lens' ResolverRuleConfig (Prelude.Maybe Prelude.Text)
resolverRuleConfig_resolverEndpointId = Lens.lens (\ResolverRuleConfig' {resolverEndpointId} -> resolverEndpointId) (\s@ResolverRuleConfig' {} a -> s {resolverEndpointId = a} :: ResolverRuleConfig)

-- | For DNS queries that originate in your VPC, the new IP addresses that
-- you want to route outbound DNS queries to.
resolverRuleConfig_targetIps :: Lens.Lens' ResolverRuleConfig (Prelude.Maybe (Prelude.NonEmpty TargetAddress))
resolverRuleConfig_targetIps = Lens.lens (\ResolverRuleConfig' {targetIps} -> targetIps) (\s@ResolverRuleConfig' {} a -> s {targetIps = a} :: ResolverRuleConfig) Prelude.. Lens.mapping Lens.coerced

-- | The new name for the Resolver rule. The name that you specify appears in
-- the Resolver dashboard in the Route 53 console.
resolverRuleConfig_name :: Lens.Lens' ResolverRuleConfig (Prelude.Maybe Prelude.Text)
resolverRuleConfig_name = Lens.lens (\ResolverRuleConfig' {name} -> name) (\s@ResolverRuleConfig' {} a -> s {name = a} :: ResolverRuleConfig)

instance Prelude.Hashable ResolverRuleConfig where
  hashWithSalt salt' ResolverRuleConfig' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` targetIps
      `Prelude.hashWithSalt` resolverEndpointId

instance Prelude.NFData ResolverRuleConfig where
  rnf ResolverRuleConfig' {..} =
    Prelude.rnf resolverEndpointId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf targetIps

instance Core.ToJSON ResolverRuleConfig where
  toJSON ResolverRuleConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ResolverEndpointId" Core..=)
              Prelude.<$> resolverEndpointId,
            ("TargetIps" Core..=) Prelude.<$> targetIps,
            ("Name" Core..=) Prelude.<$> name
          ]
      )
