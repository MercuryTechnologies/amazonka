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
-- Module      : Amazonka.NetworkFirewall.Types.Firewall
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.Firewall where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkFirewall.Types.SubnetMapping
import Amazonka.NetworkFirewall.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The firewall defines the configuration settings for an AWS Network
-- Firewall firewall. These settings include the firewall policy, the
-- subnets in your VPC to use for the firewall endpoints, and any tags that
-- are attached to the firewall AWS resource.
--
-- The status of the firewall, for example whether it\'s ready to filter
-- network traffic, is provided in the corresponding FirewallStatus. You
-- can retrieve both objects by calling DescribeFirewall.
--
-- /See:/ 'newFirewall' smart constructor.
data Firewall = Firewall'
  { -- | The Amazon Resource Name (ARN) of the firewall.
    firewallArn :: Prelude.Maybe Prelude.Text,
    -- | A setting indicating whether the firewall is protected against a change
    -- to the firewall policy association. Use this setting to protect against
    -- accidentally modifying the firewall policy for a firewall that is in
    -- use. When you create a firewall, the operation initializes this setting
    -- to @TRUE@.
    firewallPolicyChangeProtection :: Prelude.Maybe Prelude.Bool,
    -- | A setting indicating whether the firewall is protected against changes
    -- to the subnet associations. Use this setting to protect against
    -- accidentally modifying the subnet associations for a firewall that is in
    -- use. When you create a firewall, the operation initializes this setting
    -- to @TRUE@.
    subnetChangeProtection :: Prelude.Maybe Prelude.Bool,
    -- | A flag indicating whether it is possible to delete the firewall. A
    -- setting of @TRUE@ indicates that the firewall is protected against
    -- deletion. Use this setting to protect against accidentally deleting a
    -- firewall that is in use. When you create a firewall, the operation
    -- initializes this flag to @TRUE@.
    deleteProtection :: Prelude.Maybe Prelude.Bool,
    -- | A description of the firewall.
    description :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The descriptive name of the firewall. You can\'t change the name of a
    -- firewall after you create it.
    firewallName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the firewall policy.
    --
    -- The relationship of firewall to firewall policy is many to one. Each
    -- firewall requires one firewall policy association, and you can use the
    -- same firewall policy for multiple firewalls.
    firewallPolicyArn :: Prelude.Text,
    -- | The unique identifier of the VPC where the firewall is in use.
    vpcId :: Prelude.Text,
    -- | The public subnets that Network Firewall is using for the firewall. Each
    -- subnet must belong to a different Availability Zone.
    subnetMappings :: [SubnetMapping],
    -- | The unique identifier for the firewall.
    firewallId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Firewall' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'firewallArn', 'firewall_firewallArn' - The Amazon Resource Name (ARN) of the firewall.
--
-- 'firewallPolicyChangeProtection', 'firewall_firewallPolicyChangeProtection' - A setting indicating whether the firewall is protected against a change
-- to the firewall policy association. Use this setting to protect against
-- accidentally modifying the firewall policy for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
--
-- 'subnetChangeProtection', 'firewall_subnetChangeProtection' - A setting indicating whether the firewall is protected against changes
-- to the subnet associations. Use this setting to protect against
-- accidentally modifying the subnet associations for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
--
-- 'deleteProtection', 'firewall_deleteProtection' - A flag indicating whether it is possible to delete the firewall. A
-- setting of @TRUE@ indicates that the firewall is protected against
-- deletion. Use this setting to protect against accidentally deleting a
-- firewall that is in use. When you create a firewall, the operation
-- initializes this flag to @TRUE@.
--
-- 'description', 'firewall_description' - A description of the firewall.
--
-- 'tags', 'firewall_tags' -
--
-- 'firewallName', 'firewall_firewallName' - The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
--
-- 'firewallPolicyArn', 'firewall_firewallPolicyArn' - The Amazon Resource Name (ARN) of the firewall policy.
--
-- The relationship of firewall to firewall policy is many to one. Each
-- firewall requires one firewall policy association, and you can use the
-- same firewall policy for multiple firewalls.
--
-- 'vpcId', 'firewall_vpcId' - The unique identifier of the VPC where the firewall is in use.
--
-- 'subnetMappings', 'firewall_subnetMappings' - The public subnets that Network Firewall is using for the firewall. Each
-- subnet must belong to a different Availability Zone.
--
-- 'firewallId', 'firewall_firewallId' - The unique identifier for the firewall.
newFirewall ::
  -- | 'firewallPolicyArn'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  -- | 'firewallId'
  Prelude.Text ->
  Firewall
newFirewall pFirewallPolicyArn_ pVpcId_ pFirewallId_ =
  Firewall'
    { firewallArn = Prelude.Nothing,
      firewallPolicyChangeProtection = Prelude.Nothing,
      subnetChangeProtection = Prelude.Nothing,
      deleteProtection = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      firewallName = Prelude.Nothing,
      firewallPolicyArn = pFirewallPolicyArn_,
      vpcId = pVpcId_,
      subnetMappings = Prelude.mempty,
      firewallId = pFirewallId_
    }

-- | The Amazon Resource Name (ARN) of the firewall.
firewall_firewallArn :: Lens.Lens' Firewall (Prelude.Maybe Prelude.Text)
firewall_firewallArn = Lens.lens (\Firewall' {firewallArn} -> firewallArn) (\s@Firewall' {} a -> s {firewallArn = a} :: Firewall)

-- | A setting indicating whether the firewall is protected against a change
-- to the firewall policy association. Use this setting to protect against
-- accidentally modifying the firewall policy for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
firewall_firewallPolicyChangeProtection :: Lens.Lens' Firewall (Prelude.Maybe Prelude.Bool)
firewall_firewallPolicyChangeProtection = Lens.lens (\Firewall' {firewallPolicyChangeProtection} -> firewallPolicyChangeProtection) (\s@Firewall' {} a -> s {firewallPolicyChangeProtection = a} :: Firewall)

-- | A setting indicating whether the firewall is protected against changes
-- to the subnet associations. Use this setting to protect against
-- accidentally modifying the subnet associations for a firewall that is in
-- use. When you create a firewall, the operation initializes this setting
-- to @TRUE@.
firewall_subnetChangeProtection :: Lens.Lens' Firewall (Prelude.Maybe Prelude.Bool)
firewall_subnetChangeProtection = Lens.lens (\Firewall' {subnetChangeProtection} -> subnetChangeProtection) (\s@Firewall' {} a -> s {subnetChangeProtection = a} :: Firewall)

-- | A flag indicating whether it is possible to delete the firewall. A
-- setting of @TRUE@ indicates that the firewall is protected against
-- deletion. Use this setting to protect against accidentally deleting a
-- firewall that is in use. When you create a firewall, the operation
-- initializes this flag to @TRUE@.
firewall_deleteProtection :: Lens.Lens' Firewall (Prelude.Maybe Prelude.Bool)
firewall_deleteProtection = Lens.lens (\Firewall' {deleteProtection} -> deleteProtection) (\s@Firewall' {} a -> s {deleteProtection = a} :: Firewall)

-- | A description of the firewall.
firewall_description :: Lens.Lens' Firewall (Prelude.Maybe Prelude.Text)
firewall_description = Lens.lens (\Firewall' {description} -> description) (\s@Firewall' {} a -> s {description = a} :: Firewall)

-- |
firewall_tags :: Lens.Lens' Firewall (Prelude.Maybe (Prelude.NonEmpty Tag))
firewall_tags = Lens.lens (\Firewall' {tags} -> tags) (\s@Firewall' {} a -> s {tags = a} :: Firewall) Prelude.. Lens.mapping Lens.coerced

-- | The descriptive name of the firewall. You can\'t change the name of a
-- firewall after you create it.
firewall_firewallName :: Lens.Lens' Firewall (Prelude.Maybe Prelude.Text)
firewall_firewallName = Lens.lens (\Firewall' {firewallName} -> firewallName) (\s@Firewall' {} a -> s {firewallName = a} :: Firewall)

-- | The Amazon Resource Name (ARN) of the firewall policy.
--
-- The relationship of firewall to firewall policy is many to one. Each
-- firewall requires one firewall policy association, and you can use the
-- same firewall policy for multiple firewalls.
firewall_firewallPolicyArn :: Lens.Lens' Firewall Prelude.Text
firewall_firewallPolicyArn = Lens.lens (\Firewall' {firewallPolicyArn} -> firewallPolicyArn) (\s@Firewall' {} a -> s {firewallPolicyArn = a} :: Firewall)

-- | The unique identifier of the VPC where the firewall is in use.
firewall_vpcId :: Lens.Lens' Firewall Prelude.Text
firewall_vpcId = Lens.lens (\Firewall' {vpcId} -> vpcId) (\s@Firewall' {} a -> s {vpcId = a} :: Firewall)

-- | The public subnets that Network Firewall is using for the firewall. Each
-- subnet must belong to a different Availability Zone.
firewall_subnetMappings :: Lens.Lens' Firewall [SubnetMapping]
firewall_subnetMappings = Lens.lens (\Firewall' {subnetMappings} -> subnetMappings) (\s@Firewall' {} a -> s {subnetMappings = a} :: Firewall) Prelude.. Lens.coerced

-- | The unique identifier for the firewall.
firewall_firewallId :: Lens.Lens' Firewall Prelude.Text
firewall_firewallId = Lens.lens (\Firewall' {firewallId} -> firewallId) (\s@Firewall' {} a -> s {firewallId = a} :: Firewall)

instance Core.FromJSON Firewall where
  parseJSON =
    Core.withObject
      "Firewall"
      ( \x ->
          Firewall'
            Prelude.<$> (x Core..:? "FirewallArn")
            Prelude.<*> (x Core..:? "FirewallPolicyChangeProtection")
            Prelude.<*> (x Core..:? "SubnetChangeProtection")
            Prelude.<*> (x Core..:? "DeleteProtection")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags")
            Prelude.<*> (x Core..:? "FirewallName")
            Prelude.<*> (x Core..: "FirewallPolicyArn")
            Prelude.<*> (x Core..: "VpcId")
            Prelude.<*> (x Core..:? "SubnetMappings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "FirewallId")
      )

instance Prelude.Hashable Firewall where
  hashWithSalt salt' Firewall' {..} =
    salt' `Prelude.hashWithSalt` firewallId
      `Prelude.hashWithSalt` subnetMappings
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` firewallPolicyArn
      `Prelude.hashWithSalt` firewallName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` deleteProtection
      `Prelude.hashWithSalt` subnetChangeProtection
      `Prelude.hashWithSalt` firewallPolicyChangeProtection
      `Prelude.hashWithSalt` firewallArn

instance Prelude.NFData Firewall where
  rnf Firewall' {..} =
    Prelude.rnf firewallArn
      `Prelude.seq` Prelude.rnf firewallId
      `Prelude.seq` Prelude.rnf subnetMappings
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf firewallPolicyArn
      `Prelude.seq` Prelude.rnf firewallName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf deleteProtection
      `Prelude.seq` Prelude.rnf subnetChangeProtection
      `Prelude.seq` Prelude.rnf firewallPolicyChangeProtection
