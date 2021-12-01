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
-- Module      : Amazonka.EC2.Types.TargetNetwork
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TargetNetwork where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AssociationStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a target network associated with a Client VPN endpoint.
--
-- /See:/ 'newTargetNetwork' smart constructor.
data TargetNetwork = TargetNetwork'
  { -- | The ID of the association.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The current state of the target network association.
    status :: Prelude.Maybe AssociationStatus,
    -- | The IDs of the security groups applied to the target network
    -- association.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the subnet specified as the target network.
    targetNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC in which the target network (subnet) is located.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Client VPN endpoint with which the target network is
    -- associated.
    clientVpnEndpointId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TargetNetwork' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'targetNetwork_associationId' - The ID of the association.
--
-- 'status', 'targetNetwork_status' - The current state of the target network association.
--
-- 'securityGroups', 'targetNetwork_securityGroups' - The IDs of the security groups applied to the target network
-- association.
--
-- 'targetNetworkId', 'targetNetwork_targetNetworkId' - The ID of the subnet specified as the target network.
--
-- 'vpcId', 'targetNetwork_vpcId' - The ID of the VPC in which the target network (subnet) is located.
--
-- 'clientVpnEndpointId', 'targetNetwork_clientVpnEndpointId' - The ID of the Client VPN endpoint with which the target network is
-- associated.
newTargetNetwork ::
  TargetNetwork
newTargetNetwork =
  TargetNetwork'
    { associationId = Prelude.Nothing,
      status = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      targetNetworkId = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      clientVpnEndpointId = Prelude.Nothing
    }

-- | The ID of the association.
targetNetwork_associationId :: Lens.Lens' TargetNetwork (Prelude.Maybe Prelude.Text)
targetNetwork_associationId = Lens.lens (\TargetNetwork' {associationId} -> associationId) (\s@TargetNetwork' {} a -> s {associationId = a} :: TargetNetwork)

-- | The current state of the target network association.
targetNetwork_status :: Lens.Lens' TargetNetwork (Prelude.Maybe AssociationStatus)
targetNetwork_status = Lens.lens (\TargetNetwork' {status} -> status) (\s@TargetNetwork' {} a -> s {status = a} :: TargetNetwork)

-- | The IDs of the security groups applied to the target network
-- association.
targetNetwork_securityGroups :: Lens.Lens' TargetNetwork (Prelude.Maybe [Prelude.Text])
targetNetwork_securityGroups = Lens.lens (\TargetNetwork' {securityGroups} -> securityGroups) (\s@TargetNetwork' {} a -> s {securityGroups = a} :: TargetNetwork) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the subnet specified as the target network.
targetNetwork_targetNetworkId :: Lens.Lens' TargetNetwork (Prelude.Maybe Prelude.Text)
targetNetwork_targetNetworkId = Lens.lens (\TargetNetwork' {targetNetworkId} -> targetNetworkId) (\s@TargetNetwork' {} a -> s {targetNetworkId = a} :: TargetNetwork)

-- | The ID of the VPC in which the target network (subnet) is located.
targetNetwork_vpcId :: Lens.Lens' TargetNetwork (Prelude.Maybe Prelude.Text)
targetNetwork_vpcId = Lens.lens (\TargetNetwork' {vpcId} -> vpcId) (\s@TargetNetwork' {} a -> s {vpcId = a} :: TargetNetwork)

-- | The ID of the Client VPN endpoint with which the target network is
-- associated.
targetNetwork_clientVpnEndpointId :: Lens.Lens' TargetNetwork (Prelude.Maybe Prelude.Text)
targetNetwork_clientVpnEndpointId = Lens.lens (\TargetNetwork' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@TargetNetwork' {} a -> s {clientVpnEndpointId = a} :: TargetNetwork)

instance Core.FromXML TargetNetwork where
  parseXML x =
    TargetNetwork'
      Prelude.<$> (x Core..@? "associationId")
      Prelude.<*> (x Core..@? "status")
      Prelude.<*> ( x Core..@? "securityGroups" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "targetNetworkId")
      Prelude.<*> (x Core..@? "vpcId")
      Prelude.<*> (x Core..@? "clientVpnEndpointId")

instance Prelude.Hashable TargetNetwork where
  hashWithSalt salt' TargetNetwork' {..} =
    salt' `Prelude.hashWithSalt` clientVpnEndpointId
      `Prelude.hashWithSalt` vpcId
      `Prelude.hashWithSalt` targetNetworkId
      `Prelude.hashWithSalt` securityGroups
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` associationId

instance Prelude.NFData TargetNetwork where
  rnf TargetNetwork' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf clientVpnEndpointId
      `Prelude.seq` Prelude.rnf vpcId
      `Prelude.seq` Prelude.rnf targetNetworkId
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf status
