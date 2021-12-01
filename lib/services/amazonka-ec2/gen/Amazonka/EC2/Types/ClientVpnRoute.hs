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
-- Module      : Amazonka.EC2.Types.ClientVpnRoute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientVpnRoute where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ClientVpnRouteStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a Client VPN endpoint route.
--
-- /See:/ 'newClientVpnRoute' smart constructor.
data ClientVpnRoute = ClientVpnRoute'
  { -- | The current state of the route.
    status :: Prelude.Maybe ClientVpnRouteStatus,
    -- | Indicates how the route was associated with the Client VPN endpoint.
    -- @associate@ indicates that the route was automatically added when the
    -- target network was associated with the Client VPN endpoint. @add-route@
    -- indicates that the route was manually added using the
    -- __CreateClientVpnRoute__ action.
    origin :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Client VPN endpoint with which the route is associated.
    clientVpnEndpointId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet through which traffic is routed.
    targetSubnet :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 address range, in CIDR notation, of the route destination.
    destinationCidr :: Prelude.Maybe Prelude.Text,
    -- | The route type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | A brief description of the route.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientVpnRoute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'clientVpnRoute_status' - The current state of the route.
--
-- 'origin', 'clientVpnRoute_origin' - Indicates how the route was associated with the Client VPN endpoint.
-- @associate@ indicates that the route was automatically added when the
-- target network was associated with the Client VPN endpoint. @add-route@
-- indicates that the route was manually added using the
-- __CreateClientVpnRoute__ action.
--
-- 'clientVpnEndpointId', 'clientVpnRoute_clientVpnEndpointId' - The ID of the Client VPN endpoint with which the route is associated.
--
-- 'targetSubnet', 'clientVpnRoute_targetSubnet' - The ID of the subnet through which traffic is routed.
--
-- 'destinationCidr', 'clientVpnRoute_destinationCidr' - The IPv4 address range, in CIDR notation, of the route destination.
--
-- 'type'', 'clientVpnRoute_type' - The route type.
--
-- 'description', 'clientVpnRoute_description' - A brief description of the route.
newClientVpnRoute ::
  ClientVpnRoute
newClientVpnRoute =
  ClientVpnRoute'
    { status = Prelude.Nothing,
      origin = Prelude.Nothing,
      clientVpnEndpointId = Prelude.Nothing,
      targetSubnet = Prelude.Nothing,
      destinationCidr = Prelude.Nothing,
      type' = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The current state of the route.
clientVpnRoute_status :: Lens.Lens' ClientVpnRoute (Prelude.Maybe ClientVpnRouteStatus)
clientVpnRoute_status = Lens.lens (\ClientVpnRoute' {status} -> status) (\s@ClientVpnRoute' {} a -> s {status = a} :: ClientVpnRoute)

-- | Indicates how the route was associated with the Client VPN endpoint.
-- @associate@ indicates that the route was automatically added when the
-- target network was associated with the Client VPN endpoint. @add-route@
-- indicates that the route was manually added using the
-- __CreateClientVpnRoute__ action.
clientVpnRoute_origin :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_origin = Lens.lens (\ClientVpnRoute' {origin} -> origin) (\s@ClientVpnRoute' {} a -> s {origin = a} :: ClientVpnRoute)

-- | The ID of the Client VPN endpoint with which the route is associated.
clientVpnRoute_clientVpnEndpointId :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_clientVpnEndpointId = Lens.lens (\ClientVpnRoute' {clientVpnEndpointId} -> clientVpnEndpointId) (\s@ClientVpnRoute' {} a -> s {clientVpnEndpointId = a} :: ClientVpnRoute)

-- | The ID of the subnet through which traffic is routed.
clientVpnRoute_targetSubnet :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_targetSubnet = Lens.lens (\ClientVpnRoute' {targetSubnet} -> targetSubnet) (\s@ClientVpnRoute' {} a -> s {targetSubnet = a} :: ClientVpnRoute)

-- | The IPv4 address range, in CIDR notation, of the route destination.
clientVpnRoute_destinationCidr :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_destinationCidr = Lens.lens (\ClientVpnRoute' {destinationCidr} -> destinationCidr) (\s@ClientVpnRoute' {} a -> s {destinationCidr = a} :: ClientVpnRoute)

-- | The route type.
clientVpnRoute_type :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_type = Lens.lens (\ClientVpnRoute' {type'} -> type') (\s@ClientVpnRoute' {} a -> s {type' = a} :: ClientVpnRoute)

-- | A brief description of the route.
clientVpnRoute_description :: Lens.Lens' ClientVpnRoute (Prelude.Maybe Prelude.Text)
clientVpnRoute_description = Lens.lens (\ClientVpnRoute' {description} -> description) (\s@ClientVpnRoute' {} a -> s {description = a} :: ClientVpnRoute)

instance Core.FromXML ClientVpnRoute where
  parseXML x =
    ClientVpnRoute'
      Prelude.<$> (x Core..@? "status")
      Prelude.<*> (x Core..@? "origin")
      Prelude.<*> (x Core..@? "clientVpnEndpointId")
      Prelude.<*> (x Core..@? "targetSubnet")
      Prelude.<*> (x Core..@? "destinationCidr")
      Prelude.<*> (x Core..@? "type")
      Prelude.<*> (x Core..@? "description")

instance Prelude.Hashable ClientVpnRoute where
  hashWithSalt salt' ClientVpnRoute' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` destinationCidr
      `Prelude.hashWithSalt` targetSubnet
      `Prelude.hashWithSalt` clientVpnEndpointId
      `Prelude.hashWithSalt` origin
      `Prelude.hashWithSalt` status

instance Prelude.NFData ClientVpnRoute where
  rnf ClientVpnRoute' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf destinationCidr
      `Prelude.seq` Prelude.rnf targetSubnet
      `Prelude.seq` Prelude.rnf clientVpnEndpointId
      `Prelude.seq` Prelude.rnf origin
