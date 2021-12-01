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
-- Module      : Amazonka.EC2.Types.VpcPeeringConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpcPeeringConnection where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.VpcPeeringConnectionStateReason
import Amazonka.EC2.Types.VpcPeeringConnectionVpcInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a VPC peering connection.
--
-- /See:/ 'newVpcPeeringConnection' smart constructor.
data VpcPeeringConnection = VpcPeeringConnection'
  { -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The status of the VPC peering connection.
    status :: Prelude.Maybe VpcPeeringConnectionStateReason,
    -- | Information about the accepter VPC. CIDR block information is only
    -- returned when describing an active VPC peering connection.
    accepterVpcInfo :: Prelude.Maybe VpcPeeringConnectionVpcInfo,
    -- | Information about the requester VPC. CIDR block information is only
    -- returned when describing an active VPC peering connection.
    requesterVpcInfo :: Prelude.Maybe VpcPeeringConnectionVpcInfo,
    -- | The time that an unaccepted VPC peering connection will expire.
    expirationTime :: Prelude.Maybe Core.ISO8601,
    -- | Any tags assigned to the resource.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcPeeringConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcPeeringConnectionId', 'vpcPeeringConnection_vpcPeeringConnectionId' - The ID of the VPC peering connection.
--
-- 'status', 'vpcPeeringConnection_status' - The status of the VPC peering connection.
--
-- 'accepterVpcInfo', 'vpcPeeringConnection_accepterVpcInfo' - Information about the accepter VPC. CIDR block information is only
-- returned when describing an active VPC peering connection.
--
-- 'requesterVpcInfo', 'vpcPeeringConnection_requesterVpcInfo' - Information about the requester VPC. CIDR block information is only
-- returned when describing an active VPC peering connection.
--
-- 'expirationTime', 'vpcPeeringConnection_expirationTime' - The time that an unaccepted VPC peering connection will expire.
--
-- 'tags', 'vpcPeeringConnection_tags' - Any tags assigned to the resource.
newVpcPeeringConnection ::
  VpcPeeringConnection
newVpcPeeringConnection =
  VpcPeeringConnection'
    { vpcPeeringConnectionId =
        Prelude.Nothing,
      status = Prelude.Nothing,
      accepterVpcInfo = Prelude.Nothing,
      requesterVpcInfo = Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the VPC peering connection.
vpcPeeringConnection_vpcPeeringConnectionId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_vpcPeeringConnectionId = Lens.lens (\VpcPeeringConnection' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@VpcPeeringConnection' {} a -> s {vpcPeeringConnectionId = a} :: VpcPeeringConnection)

-- | The status of the VPC peering connection.
vpcPeeringConnection_status :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe VpcPeeringConnectionStateReason)
vpcPeeringConnection_status = Lens.lens (\VpcPeeringConnection' {status} -> status) (\s@VpcPeeringConnection' {} a -> s {status = a} :: VpcPeeringConnection)

-- | Information about the accepter VPC. CIDR block information is only
-- returned when describing an active VPC peering connection.
vpcPeeringConnection_accepterVpcInfo :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe VpcPeeringConnectionVpcInfo)
vpcPeeringConnection_accepterVpcInfo = Lens.lens (\VpcPeeringConnection' {accepterVpcInfo} -> accepterVpcInfo) (\s@VpcPeeringConnection' {} a -> s {accepterVpcInfo = a} :: VpcPeeringConnection)

-- | Information about the requester VPC. CIDR block information is only
-- returned when describing an active VPC peering connection.
vpcPeeringConnection_requesterVpcInfo :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe VpcPeeringConnectionVpcInfo)
vpcPeeringConnection_requesterVpcInfo = Lens.lens (\VpcPeeringConnection' {requesterVpcInfo} -> requesterVpcInfo) (\s@VpcPeeringConnection' {} a -> s {requesterVpcInfo = a} :: VpcPeeringConnection)

-- | The time that an unaccepted VPC peering connection will expire.
vpcPeeringConnection_expirationTime :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.UTCTime)
vpcPeeringConnection_expirationTime = Lens.lens (\VpcPeeringConnection' {expirationTime} -> expirationTime) (\s@VpcPeeringConnection' {} a -> s {expirationTime = a} :: VpcPeeringConnection) Prelude.. Lens.mapping Core._Time

-- | Any tags assigned to the resource.
vpcPeeringConnection_tags :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe [Tag])
vpcPeeringConnection_tags = Lens.lens (\VpcPeeringConnection' {tags} -> tags) (\s@VpcPeeringConnection' {} a -> s {tags = a} :: VpcPeeringConnection) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML VpcPeeringConnection where
  parseXML x =
    VpcPeeringConnection'
      Prelude.<$> (x Core..@? "vpcPeeringConnectionId")
      Prelude.<*> (x Core..@? "status")
      Prelude.<*> (x Core..@? "accepterVpcInfo")
      Prelude.<*> (x Core..@? "requesterVpcInfo")
      Prelude.<*> (x Core..@? "expirationTime")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable VpcPeeringConnection where
  hashWithSalt salt' VpcPeeringConnection' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` expirationTime
      `Prelude.hashWithSalt` requesterVpcInfo
      `Prelude.hashWithSalt` accepterVpcInfo
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` vpcPeeringConnectionId

instance Prelude.NFData VpcPeeringConnection where
  rnf VpcPeeringConnection' {..} =
    Prelude.rnf vpcPeeringConnectionId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf requesterVpcInfo
      `Prelude.seq` Prelude.rnf accepterVpcInfo
      `Prelude.seq` Prelude.rnf status
