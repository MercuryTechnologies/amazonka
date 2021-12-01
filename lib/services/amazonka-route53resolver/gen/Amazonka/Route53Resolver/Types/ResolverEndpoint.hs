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
-- Module      : Amazonka.Route53Resolver.Types.ResolverEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53Resolver.Types.ResolverEndpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53Resolver.Types.ResolverEndpointDirection
import Amazonka.Route53Resolver.Types.ResolverEndpointStatus

-- | In the response to a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_CreateResolverEndpoint.html CreateResolverEndpoint>,
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_DeleteResolverEndpoint.html DeleteResolverEndpoint>,
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_GetResolverEndpoint.html GetResolverEndpoint>,
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_ListResolverEndpoints.html ListResolverEndpoints>,
-- or
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_UpdateResolverEndpoint.html UpdateResolverEndpoint>
-- request, a complex type that contains settings for an existing inbound
-- or outbound Resolver endpoint.
--
-- /See:/ 'newResolverEndpoint' smart constructor.
data ResolverEndpoint = ResolverEndpoint'
  { -- | The date and time that the endpoint was created, in Unix time format and
    -- Coordinated Universal Time (UTC).
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | A code that specifies the current status of the Resolver endpoint. Valid
    -- values include the following:
    --
    -- -   @CREATING@: Resolver is creating and configuring one or more Amazon
    --     VPC network interfaces for this endpoint.
    --
    -- -   @OPERATIONAL@: The Amazon VPC network interfaces for this endpoint
    --     are correctly configured and able to pass inbound or outbound DNS
    --     queries between your network and Resolver.
    --
    -- -   @UPDATING@: Resolver is associating or disassociating one or more
    --     network interfaces with this endpoint.
    --
    -- -   @AUTO_RECOVERING@: Resolver is trying to recover one or more of the
    --     network interfaces that are associated with this endpoint. During
    --     the recovery process, the endpoint functions with limited capacity
    --     because of the limit on the number of DNS queries per IP address
    --     (per network interface). For the current limit, see
    --     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html#limits-api-entities-resolver Limits on Route 53 Resolver>.
    --
    -- -   @ACTION_NEEDED@: This endpoint is unhealthy, and Resolver can\'t
    --     automatically recover it. To resolve the problem, we recommend that
    --     you check each IP address that you associated with the endpoint. For
    --     each IP address that isn\'t available, add another IP address and
    --     then delete the IP address that isn\'t available. (An endpoint must
    --     always include at least two IP addresses.) A status of
    --     @ACTION_NEEDED@ can have a variety of causes. Here are two common
    --     causes:
    --
    --     -   One or more of the network interfaces that are associated with
    --         the endpoint were deleted using Amazon VPC.
    --
    --     -   The network interface couldn\'t be created for some reason
    --         that\'s outside the control of Resolver.
    --
    -- -   @DELETING@: Resolver is deleting this endpoint and the associated
    --     network interfaces.
    status :: Prelude.Maybe ResolverEndpointStatus,
    -- | The ID of one or more security groups that control access to this VPC.
    -- The security group must include one or more inbound rules (for inbound
    -- endpoints) or outbound rules (for outbound endpoints). Inbound and
    -- outbound rules must allow TCP and UDP access. For inbound access, open
    -- port 53. For outbound access, open the port that you\'re using for DNS
    -- queries on your network.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether the Resolver endpoint allows inbound or outbound DNS
    -- queries:
    --
    -- -   @INBOUND@: allows DNS queries to your VPC from your network
    --
    -- -   @OUTBOUND@: allows DNS queries from your VPC to your network
    direction :: Prelude.Maybe ResolverEndpointDirection,
    -- | The ARN (Amazon Resource Name) for the Resolver endpoint.
    arn :: Prelude.Maybe Prelude.Text,
    -- | A unique string that identifies the request that created the Resolver
    -- endpoint. The @CreatorRequestId@ allows failed requests to be retried
    -- without the risk of running the operation twice.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the endpoint was last modified, in Unix time
    -- format and Coordinated Universal Time (UTC).
    modificationTime :: Prelude.Maybe Prelude.Text,
    -- | The number of IP addresses that the Resolver endpoint can use for DNS
    -- queries.
    ipAddressCount :: Prelude.Maybe Prelude.Int,
    -- | A detailed description of the status of the Resolver endpoint.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The name that you assigned to the Resolver endpoint when you submitted a
    -- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_CreateResolverEndpoint.html CreateResolverEndpoint>
    -- request.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Resolver endpoint.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC that you want to create the Resolver endpoint in.
    hostVPCId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResolverEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'resolverEndpoint_creationTime' - The date and time that the endpoint was created, in Unix time format and
-- Coordinated Universal Time (UTC).
--
-- 'status', 'resolverEndpoint_status' - A code that specifies the current status of the Resolver endpoint. Valid
-- values include the following:
--
-- -   @CREATING@: Resolver is creating and configuring one or more Amazon
--     VPC network interfaces for this endpoint.
--
-- -   @OPERATIONAL@: The Amazon VPC network interfaces for this endpoint
--     are correctly configured and able to pass inbound or outbound DNS
--     queries between your network and Resolver.
--
-- -   @UPDATING@: Resolver is associating or disassociating one or more
--     network interfaces with this endpoint.
--
-- -   @AUTO_RECOVERING@: Resolver is trying to recover one or more of the
--     network interfaces that are associated with this endpoint. During
--     the recovery process, the endpoint functions with limited capacity
--     because of the limit on the number of DNS queries per IP address
--     (per network interface). For the current limit, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html#limits-api-entities-resolver Limits on Route 53 Resolver>.
--
-- -   @ACTION_NEEDED@: This endpoint is unhealthy, and Resolver can\'t
--     automatically recover it. To resolve the problem, we recommend that
--     you check each IP address that you associated with the endpoint. For
--     each IP address that isn\'t available, add another IP address and
--     then delete the IP address that isn\'t available. (An endpoint must
--     always include at least two IP addresses.) A status of
--     @ACTION_NEEDED@ can have a variety of causes. Here are two common
--     causes:
--
--     -   One or more of the network interfaces that are associated with
--         the endpoint were deleted using Amazon VPC.
--
--     -   The network interface couldn\'t be created for some reason
--         that\'s outside the control of Resolver.
--
-- -   @DELETING@: Resolver is deleting this endpoint and the associated
--     network interfaces.
--
-- 'securityGroupIds', 'resolverEndpoint_securityGroupIds' - The ID of one or more security groups that control access to this VPC.
-- The security group must include one or more inbound rules (for inbound
-- endpoints) or outbound rules (for outbound endpoints). Inbound and
-- outbound rules must allow TCP and UDP access. For inbound access, open
-- port 53. For outbound access, open the port that you\'re using for DNS
-- queries on your network.
--
-- 'direction', 'resolverEndpoint_direction' - Indicates whether the Resolver endpoint allows inbound or outbound DNS
-- queries:
--
-- -   @INBOUND@: allows DNS queries to your VPC from your network
--
-- -   @OUTBOUND@: allows DNS queries from your VPC to your network
--
-- 'arn', 'resolverEndpoint_arn' - The ARN (Amazon Resource Name) for the Resolver endpoint.
--
-- 'creatorRequestId', 'resolverEndpoint_creatorRequestId' - A unique string that identifies the request that created the Resolver
-- endpoint. The @CreatorRequestId@ allows failed requests to be retried
-- without the risk of running the operation twice.
--
-- 'modificationTime', 'resolverEndpoint_modificationTime' - The date and time that the endpoint was last modified, in Unix time
-- format and Coordinated Universal Time (UTC).
--
-- 'ipAddressCount', 'resolverEndpoint_ipAddressCount' - The number of IP addresses that the Resolver endpoint can use for DNS
-- queries.
--
-- 'statusMessage', 'resolverEndpoint_statusMessage' - A detailed description of the status of the Resolver endpoint.
--
-- 'name', 'resolverEndpoint_name' - The name that you assigned to the Resolver endpoint when you submitted a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_CreateResolverEndpoint.html CreateResolverEndpoint>
-- request.
--
-- 'id', 'resolverEndpoint_id' - The ID of the Resolver endpoint.
--
-- 'hostVPCId', 'resolverEndpoint_hostVPCId' - The ID of the VPC that you want to create the Resolver endpoint in.
newResolverEndpoint ::
  ResolverEndpoint
newResolverEndpoint =
  ResolverEndpoint'
    { creationTime = Prelude.Nothing,
      status = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      direction = Prelude.Nothing,
      arn = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      modificationTime = Prelude.Nothing,
      ipAddressCount = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      hostVPCId = Prelude.Nothing
    }

-- | The date and time that the endpoint was created, in Unix time format and
-- Coordinated Universal Time (UTC).
resolverEndpoint_creationTime :: Lens.Lens' ResolverEndpoint (Prelude.Maybe Prelude.Text)
resolverEndpoint_creationTime = Lens.lens (\ResolverEndpoint' {creationTime} -> creationTime) (\s@ResolverEndpoint' {} a -> s {creationTime = a} :: ResolverEndpoint)

-- | A code that specifies the current status of the Resolver endpoint. Valid
-- values include the following:
--
-- -   @CREATING@: Resolver is creating and configuring one or more Amazon
--     VPC network interfaces for this endpoint.
--
-- -   @OPERATIONAL@: The Amazon VPC network interfaces for this endpoint
--     are correctly configured and able to pass inbound or outbound DNS
--     queries between your network and Resolver.
--
-- -   @UPDATING@: Resolver is associating or disassociating one or more
--     network interfaces with this endpoint.
--
-- -   @AUTO_RECOVERING@: Resolver is trying to recover one or more of the
--     network interfaces that are associated with this endpoint. During
--     the recovery process, the endpoint functions with limited capacity
--     because of the limit on the number of DNS queries per IP address
--     (per network interface). For the current limit, see
--     <https://docs.aws.amazon.com/Route53/latest/DeveloperGuide/DNSLimitations.html#limits-api-entities-resolver Limits on Route 53 Resolver>.
--
-- -   @ACTION_NEEDED@: This endpoint is unhealthy, and Resolver can\'t
--     automatically recover it. To resolve the problem, we recommend that
--     you check each IP address that you associated with the endpoint. For
--     each IP address that isn\'t available, add another IP address and
--     then delete the IP address that isn\'t available. (An endpoint must
--     always include at least two IP addresses.) A status of
--     @ACTION_NEEDED@ can have a variety of causes. Here are two common
--     causes:
--
--     -   One or more of the network interfaces that are associated with
--         the endpoint were deleted using Amazon VPC.
--
--     -   The network interface couldn\'t be created for some reason
--         that\'s outside the control of Resolver.
--
-- -   @DELETING@: Resolver is deleting this endpoint and the associated
--     network interfaces.
resolverEndpoint_status :: Lens.Lens' ResolverEndpoint (Prelude.Maybe ResolverEndpointStatus)
resolverEndpoint_status = Lens.lens (\ResolverEndpoint' {status} -> status) (\s@ResolverEndpoint' {} a -> s {status = a} :: ResolverEndpoint)

-- | The ID of one or more security groups that control access to this VPC.
-- The security group must include one or more inbound rules (for inbound
-- endpoints) or outbound rules (for outbound endpoints). Inbound and
-- outbound rules must allow TCP and UDP access. For inbound access, open
-- port 53. For outbound access, open the port that you\'re using for DNS
-- queries on your network.
resolverEndpoint_securityGroupIds :: Lens.Lens' ResolverEndpoint (Prelude.Maybe [Prelude.Text])
resolverEndpoint_securityGroupIds = Lens.lens (\ResolverEndpoint' {securityGroupIds} -> securityGroupIds) (\s@ResolverEndpoint' {} a -> s {securityGroupIds = a} :: ResolverEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether the Resolver endpoint allows inbound or outbound DNS
-- queries:
--
-- -   @INBOUND@: allows DNS queries to your VPC from your network
--
-- -   @OUTBOUND@: allows DNS queries from your VPC to your network
resolverEndpoint_direction :: Lens.Lens' ResolverEndpoint (Prelude.Maybe ResolverEndpointDirection)
resolverEndpoint_direction = Lens.lens (\ResolverEndpoint' {direction} -> direction) (\s@ResolverEndpoint' {} a -> s {direction = a} :: ResolverEndpoint)

-- | The ARN (Amazon Resource Name) for the Resolver endpoint.
resolverEndpoint_arn :: Lens.Lens' ResolverEndpoint (Prelude.Maybe Prelude.Text)
resolverEndpoint_arn = Lens.lens (\ResolverEndpoint' {arn} -> arn) (\s@ResolverEndpoint' {} a -> s {arn = a} :: ResolverEndpoint)

-- | A unique string that identifies the request that created the Resolver
-- endpoint. The @CreatorRequestId@ allows failed requests to be retried
-- without the risk of running the operation twice.
resolverEndpoint_creatorRequestId :: Lens.Lens' ResolverEndpoint (Prelude.Maybe Prelude.Text)
resolverEndpoint_creatorRequestId = Lens.lens (\ResolverEndpoint' {creatorRequestId} -> creatorRequestId) (\s@ResolverEndpoint' {} a -> s {creatorRequestId = a} :: ResolverEndpoint)

-- | The date and time that the endpoint was last modified, in Unix time
-- format and Coordinated Universal Time (UTC).
resolverEndpoint_modificationTime :: Lens.Lens' ResolverEndpoint (Prelude.Maybe Prelude.Text)
resolverEndpoint_modificationTime = Lens.lens (\ResolverEndpoint' {modificationTime} -> modificationTime) (\s@ResolverEndpoint' {} a -> s {modificationTime = a} :: ResolverEndpoint)

-- | The number of IP addresses that the Resolver endpoint can use for DNS
-- queries.
resolverEndpoint_ipAddressCount :: Lens.Lens' ResolverEndpoint (Prelude.Maybe Prelude.Int)
resolverEndpoint_ipAddressCount = Lens.lens (\ResolverEndpoint' {ipAddressCount} -> ipAddressCount) (\s@ResolverEndpoint' {} a -> s {ipAddressCount = a} :: ResolverEndpoint)

-- | A detailed description of the status of the Resolver endpoint.
resolverEndpoint_statusMessage :: Lens.Lens' ResolverEndpoint (Prelude.Maybe Prelude.Text)
resolverEndpoint_statusMessage = Lens.lens (\ResolverEndpoint' {statusMessage} -> statusMessage) (\s@ResolverEndpoint' {} a -> s {statusMessage = a} :: ResolverEndpoint)

-- | The name that you assigned to the Resolver endpoint when you submitted a
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_CreateResolverEndpoint.html CreateResolverEndpoint>
-- request.
resolverEndpoint_name :: Lens.Lens' ResolverEndpoint (Prelude.Maybe Prelude.Text)
resolverEndpoint_name = Lens.lens (\ResolverEndpoint' {name} -> name) (\s@ResolverEndpoint' {} a -> s {name = a} :: ResolverEndpoint)

-- | The ID of the Resolver endpoint.
resolverEndpoint_id :: Lens.Lens' ResolverEndpoint (Prelude.Maybe Prelude.Text)
resolverEndpoint_id = Lens.lens (\ResolverEndpoint' {id} -> id) (\s@ResolverEndpoint' {} a -> s {id = a} :: ResolverEndpoint)

-- | The ID of the VPC that you want to create the Resolver endpoint in.
resolverEndpoint_hostVPCId :: Lens.Lens' ResolverEndpoint (Prelude.Maybe Prelude.Text)
resolverEndpoint_hostVPCId = Lens.lens (\ResolverEndpoint' {hostVPCId} -> hostVPCId) (\s@ResolverEndpoint' {} a -> s {hostVPCId = a} :: ResolverEndpoint)

instance Core.FromJSON ResolverEndpoint where
  parseJSON =
    Core.withObject
      "ResolverEndpoint"
      ( \x ->
          ResolverEndpoint'
            Prelude.<$> (x Core..:? "CreationTime")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> ( x Core..:? "SecurityGroupIds"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Direction")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatorRequestId")
            Prelude.<*> (x Core..:? "ModificationTime")
            Prelude.<*> (x Core..:? "IpAddressCount")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "HostVPCId")
      )

instance Prelude.Hashable ResolverEndpoint where
  hashWithSalt salt' ResolverEndpoint' {..} =
    salt' `Prelude.hashWithSalt` hostVPCId
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` ipAddressCount
      `Prelude.hashWithSalt` modificationTime
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` direction
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ResolverEndpoint where
  rnf ResolverEndpoint' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf hostVPCId
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf ipAddressCount
      `Prelude.seq` Prelude.rnf modificationTime
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf direction
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf status
