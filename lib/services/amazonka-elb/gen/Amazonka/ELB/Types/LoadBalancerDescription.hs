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
-- Module      : Amazonka.ELB.Types.LoadBalancerDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.LoadBalancerDescription where

import qualified Amazonka.Core as Core
import Amazonka.ELB.Internal
import Amazonka.ELB.Types.BackendServerDescription
import Amazonka.ELB.Types.HealthCheck
import Amazonka.ELB.Types.Instance
import Amazonka.ELB.Types.ListenerDescription
import Amazonka.ELB.Types.Policies
import Amazonka.ELB.Types.SourceSecurityGroup
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a load balancer.
--
-- /See:/ 'newLoadBalancerDescription' smart constructor.
data LoadBalancerDescription = LoadBalancerDescription'
  { -- | The security group for the load balancer, which you can use as part of
    -- your inbound rules for your registered instances. To only allow traffic
    -- from load balancers, add a security group rule that specifies this
    -- source security group as the inbound source.
    sourceSecurityGroup :: Prelude.Maybe SourceSecurityGroup,
    -- | The DNS name of the load balancer.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/using-domain-names-with-elb.html Configure a Custom Domain Name>
    -- in the /Classic Load Balancers Guide/.
    canonicalHostedZoneName :: Prelude.Maybe Prelude.Text,
    -- | The security groups for the load balancer. Valid only for load balancers
    -- in a VPC.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | Information about the health checks conducted on the load balancer.
    healthCheck :: Prelude.Maybe HealthCheck,
    -- | The name of the load balancer.
    loadBalancerName :: Prelude.Maybe Prelude.Text,
    -- | The date and time the load balancer was created.
    createdTime :: Prelude.Maybe Core.ISO8601,
    -- | The ID of the VPC for the load balancer.
    vPCId :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the subnets for the load balancer.
    subnets :: Prelude.Maybe [Prelude.Text],
    -- | The Availability Zones for the load balancer.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | Information about your EC2 instances.
    backendServerDescriptions :: Prelude.Maybe [BackendServerDescription],
    -- | The ID of the Amazon Route 53 hosted zone for the load balancer.
    canonicalHostedZoneNameID :: Prelude.Maybe Prelude.Text,
    -- | The IDs of the instances for the load balancer.
    instances :: Prelude.Maybe [Instance],
    -- | The type of load balancer. Valid only for load balancers in a VPC.
    --
    -- If @Scheme@ is @internet-facing@, the load balancer has a public DNS
    -- name that resolves to a public IP address.
    --
    -- If @Scheme@ is @internal@, the load balancer has a public DNS name that
    -- resolves to a private IP address.
    scheme :: Prelude.Maybe Prelude.Text,
    -- | The listeners for the load balancer.
    listenerDescriptions :: Prelude.Maybe [ListenerDescription],
    -- | The DNS name of the load balancer.
    dNSName :: Prelude.Maybe Prelude.Text,
    -- | The policies defined for the load balancer.
    policies :: Prelude.Maybe Policies
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceSecurityGroup', 'loadBalancerDescription_sourceSecurityGroup' - The security group for the load balancer, which you can use as part of
-- your inbound rules for your registered instances. To only allow traffic
-- from load balancers, add a security group rule that specifies this
-- source security group as the inbound source.
--
-- 'canonicalHostedZoneName', 'loadBalancerDescription_canonicalHostedZoneName' - The DNS name of the load balancer.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/using-domain-names-with-elb.html Configure a Custom Domain Name>
-- in the /Classic Load Balancers Guide/.
--
-- 'securityGroups', 'loadBalancerDescription_securityGroups' - The security groups for the load balancer. Valid only for load balancers
-- in a VPC.
--
-- 'healthCheck', 'loadBalancerDescription_healthCheck' - Information about the health checks conducted on the load balancer.
--
-- 'loadBalancerName', 'loadBalancerDescription_loadBalancerName' - The name of the load balancer.
--
-- 'createdTime', 'loadBalancerDescription_createdTime' - The date and time the load balancer was created.
--
-- 'vPCId', 'loadBalancerDescription_vPCId' - The ID of the VPC for the load balancer.
--
-- 'subnets', 'loadBalancerDescription_subnets' - The IDs of the subnets for the load balancer.
--
-- 'availabilityZones', 'loadBalancerDescription_availabilityZones' - The Availability Zones for the load balancer.
--
-- 'backendServerDescriptions', 'loadBalancerDescription_backendServerDescriptions' - Information about your EC2 instances.
--
-- 'canonicalHostedZoneNameID', 'loadBalancerDescription_canonicalHostedZoneNameID' - The ID of the Amazon Route 53 hosted zone for the load balancer.
--
-- 'instances', 'loadBalancerDescription_instances' - The IDs of the instances for the load balancer.
--
-- 'scheme', 'loadBalancerDescription_scheme' - The type of load balancer. Valid only for load balancers in a VPC.
--
-- If @Scheme@ is @internet-facing@, the load balancer has a public DNS
-- name that resolves to a public IP address.
--
-- If @Scheme@ is @internal@, the load balancer has a public DNS name that
-- resolves to a private IP address.
--
-- 'listenerDescriptions', 'loadBalancerDescription_listenerDescriptions' - The listeners for the load balancer.
--
-- 'dNSName', 'loadBalancerDescription_dNSName' - The DNS name of the load balancer.
--
-- 'policies', 'loadBalancerDescription_policies' - The policies defined for the load balancer.
newLoadBalancerDescription ::
  LoadBalancerDescription
newLoadBalancerDescription =
  LoadBalancerDescription'
    { sourceSecurityGroup =
        Prelude.Nothing,
      canonicalHostedZoneName = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      healthCheck = Prelude.Nothing,
      loadBalancerName = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      vPCId = Prelude.Nothing,
      subnets = Prelude.Nothing,
      availabilityZones = Prelude.Nothing,
      backendServerDescriptions = Prelude.Nothing,
      canonicalHostedZoneNameID = Prelude.Nothing,
      instances = Prelude.Nothing,
      scheme = Prelude.Nothing,
      listenerDescriptions = Prelude.Nothing,
      dNSName = Prelude.Nothing,
      policies = Prelude.Nothing
    }

-- | The security group for the load balancer, which you can use as part of
-- your inbound rules for your registered instances. To only allow traffic
-- from load balancers, add a security group rule that specifies this
-- source security group as the inbound source.
loadBalancerDescription_sourceSecurityGroup :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe SourceSecurityGroup)
loadBalancerDescription_sourceSecurityGroup = Lens.lens (\LoadBalancerDescription' {sourceSecurityGroup} -> sourceSecurityGroup) (\s@LoadBalancerDescription' {} a -> s {sourceSecurityGroup = a} :: LoadBalancerDescription)

-- | The DNS name of the load balancer.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/using-domain-names-with-elb.html Configure a Custom Domain Name>
-- in the /Classic Load Balancers Guide/.
loadBalancerDescription_canonicalHostedZoneName :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe Prelude.Text)
loadBalancerDescription_canonicalHostedZoneName = Lens.lens (\LoadBalancerDescription' {canonicalHostedZoneName} -> canonicalHostedZoneName) (\s@LoadBalancerDescription' {} a -> s {canonicalHostedZoneName = a} :: LoadBalancerDescription)

-- | The security groups for the load balancer. Valid only for load balancers
-- in a VPC.
loadBalancerDescription_securityGroups :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe [Prelude.Text])
loadBalancerDescription_securityGroups = Lens.lens (\LoadBalancerDescription' {securityGroups} -> securityGroups) (\s@LoadBalancerDescription' {} a -> s {securityGroups = a} :: LoadBalancerDescription) Prelude.. Lens.mapping Lens.coerced

-- | Information about the health checks conducted on the load balancer.
loadBalancerDescription_healthCheck :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe HealthCheck)
loadBalancerDescription_healthCheck = Lens.lens (\LoadBalancerDescription' {healthCheck} -> healthCheck) (\s@LoadBalancerDescription' {} a -> s {healthCheck = a} :: LoadBalancerDescription)

-- | The name of the load balancer.
loadBalancerDescription_loadBalancerName :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe Prelude.Text)
loadBalancerDescription_loadBalancerName = Lens.lens (\LoadBalancerDescription' {loadBalancerName} -> loadBalancerName) (\s@LoadBalancerDescription' {} a -> s {loadBalancerName = a} :: LoadBalancerDescription)

-- | The date and time the load balancer was created.
loadBalancerDescription_createdTime :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe Prelude.UTCTime)
loadBalancerDescription_createdTime = Lens.lens (\LoadBalancerDescription' {createdTime} -> createdTime) (\s@LoadBalancerDescription' {} a -> s {createdTime = a} :: LoadBalancerDescription) Prelude.. Lens.mapping Core._Time

-- | The ID of the VPC for the load balancer.
loadBalancerDescription_vPCId :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe Prelude.Text)
loadBalancerDescription_vPCId = Lens.lens (\LoadBalancerDescription' {vPCId} -> vPCId) (\s@LoadBalancerDescription' {} a -> s {vPCId = a} :: LoadBalancerDescription)

-- | The IDs of the subnets for the load balancer.
loadBalancerDescription_subnets :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe [Prelude.Text])
loadBalancerDescription_subnets = Lens.lens (\LoadBalancerDescription' {subnets} -> subnets) (\s@LoadBalancerDescription' {} a -> s {subnets = a} :: LoadBalancerDescription) Prelude.. Lens.mapping Lens.coerced

-- | The Availability Zones for the load balancer.
loadBalancerDescription_availabilityZones :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe [Prelude.Text])
loadBalancerDescription_availabilityZones = Lens.lens (\LoadBalancerDescription' {availabilityZones} -> availabilityZones) (\s@LoadBalancerDescription' {} a -> s {availabilityZones = a} :: LoadBalancerDescription) Prelude.. Lens.mapping Lens.coerced

-- | Information about your EC2 instances.
loadBalancerDescription_backendServerDescriptions :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe [BackendServerDescription])
loadBalancerDescription_backendServerDescriptions = Lens.lens (\LoadBalancerDescription' {backendServerDescriptions} -> backendServerDescriptions) (\s@LoadBalancerDescription' {} a -> s {backendServerDescriptions = a} :: LoadBalancerDescription) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Route 53 hosted zone for the load balancer.
loadBalancerDescription_canonicalHostedZoneNameID :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe Prelude.Text)
loadBalancerDescription_canonicalHostedZoneNameID = Lens.lens (\LoadBalancerDescription' {canonicalHostedZoneNameID} -> canonicalHostedZoneNameID) (\s@LoadBalancerDescription' {} a -> s {canonicalHostedZoneNameID = a} :: LoadBalancerDescription)

-- | The IDs of the instances for the load balancer.
loadBalancerDescription_instances :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe [Instance])
loadBalancerDescription_instances = Lens.lens (\LoadBalancerDescription' {instances} -> instances) (\s@LoadBalancerDescription' {} a -> s {instances = a} :: LoadBalancerDescription) Prelude.. Lens.mapping Lens.coerced

-- | The type of load balancer. Valid only for load balancers in a VPC.
--
-- If @Scheme@ is @internet-facing@, the load balancer has a public DNS
-- name that resolves to a public IP address.
--
-- If @Scheme@ is @internal@, the load balancer has a public DNS name that
-- resolves to a private IP address.
loadBalancerDescription_scheme :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe Prelude.Text)
loadBalancerDescription_scheme = Lens.lens (\LoadBalancerDescription' {scheme} -> scheme) (\s@LoadBalancerDescription' {} a -> s {scheme = a} :: LoadBalancerDescription)

-- | The listeners for the load balancer.
loadBalancerDescription_listenerDescriptions :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe [ListenerDescription])
loadBalancerDescription_listenerDescriptions = Lens.lens (\LoadBalancerDescription' {listenerDescriptions} -> listenerDescriptions) (\s@LoadBalancerDescription' {} a -> s {listenerDescriptions = a} :: LoadBalancerDescription) Prelude.. Lens.mapping Lens.coerced

-- | The DNS name of the load balancer.
loadBalancerDescription_dNSName :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe Prelude.Text)
loadBalancerDescription_dNSName = Lens.lens (\LoadBalancerDescription' {dNSName} -> dNSName) (\s@LoadBalancerDescription' {} a -> s {dNSName = a} :: LoadBalancerDescription)

-- | The policies defined for the load balancer.
loadBalancerDescription_policies :: Lens.Lens' LoadBalancerDescription (Prelude.Maybe Policies)
loadBalancerDescription_policies = Lens.lens (\LoadBalancerDescription' {policies} -> policies) (\s@LoadBalancerDescription' {} a -> s {policies = a} :: LoadBalancerDescription)

instance Core.FromXML LoadBalancerDescription where
  parseXML x =
    LoadBalancerDescription'
      Prelude.<$> (x Core..@? "SourceSecurityGroup")
      Prelude.<*> (x Core..@? "CanonicalHostedZoneName")
      Prelude.<*> ( x Core..@? "SecurityGroups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "HealthCheck")
      Prelude.<*> (x Core..@? "LoadBalancerName")
      Prelude.<*> (x Core..@? "CreatedTime")
      Prelude.<*> (x Core..@? "VPCId")
      Prelude.<*> ( x Core..@? "Subnets" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "AvailabilityZones"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "BackendServerDescriptions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "CanonicalHostedZoneNameID")
      Prelude.<*> ( x Core..@? "Instances" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Scheme")
      Prelude.<*> ( x Core..@? "ListenerDescriptions"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "DNSName")
      Prelude.<*> (x Core..@? "Policies")

instance Prelude.Hashable LoadBalancerDescription where
  hashWithSalt salt' LoadBalancerDescription' {..} =
    salt' `Prelude.hashWithSalt` policies
      `Prelude.hashWithSalt` dNSName
      `Prelude.hashWithSalt` listenerDescriptions
      `Prelude.hashWithSalt` scheme
      `Prelude.hashWithSalt` instances
      `Prelude.hashWithSalt` canonicalHostedZoneNameID
      `Prelude.hashWithSalt` backendServerDescriptions
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` subnets
      `Prelude.hashWithSalt` vPCId
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` healthCheck
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` canonicalHostedZoneName
      `Prelude.hashWithSalt` sourceSecurityGroup

instance Prelude.NFData LoadBalancerDescription where
  rnf LoadBalancerDescription' {..} =
    Prelude.rnf sourceSecurityGroup
      `Prelude.seq` Prelude.rnf policies
      `Prelude.seq` Prelude.rnf dNSName
      `Prelude.seq` Prelude.rnf listenerDescriptions
      `Prelude.seq` Prelude.rnf scheme
      `Prelude.seq` Prelude.rnf instances
      `Prelude.seq` Prelude.rnf canonicalHostedZoneNameID
      `Prelude.seq` Prelude.rnf backendServerDescriptions
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf subnets
      `Prelude.seq` Prelude.rnf vPCId
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf loadBalancerName
      `Prelude.seq` Prelude.rnf healthCheck
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf canonicalHostedZoneName
