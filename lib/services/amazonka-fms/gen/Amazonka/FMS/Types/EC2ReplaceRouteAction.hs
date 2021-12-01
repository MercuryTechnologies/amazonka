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
-- Module      : Amazonka.FMS.Types.EC2ReplaceRouteAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.EC2ReplaceRouteAction where

import qualified Amazonka.Core as Core
import Amazonka.FMS.Types.ActionTarget
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the ReplaceRoute action in Amazon EC2.
--
-- /See:/ 'newEC2ReplaceRouteAction' smart constructor.
data EC2ReplaceRouteAction = EC2ReplaceRouteAction'
  { -- | Information about the IPv6 CIDR address block used for the destination
    -- match. The value that you provide must match the CIDR of an existing
    -- route in the table.
    destinationIpv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Information about the ID of an internet gateway or virtual private
    -- gateway.
    gatewayId :: Prelude.Maybe ActionTarget,
    -- | Information about the ID of the prefix list for the route.
    destinationPrefixListId :: Prelude.Maybe Prelude.Text,
    -- | A description of the ReplaceRoute action in Amazon EC2.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv4 CIDR address block used for the destination
    -- match. The value that you provide must match the CIDR of an existing
    -- route in the table.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Information about the ID of the route table.
    routeTableId :: ActionTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2ReplaceRouteAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationIpv6CidrBlock', 'eC2ReplaceRouteAction_destinationIpv6CidrBlock' - Information about the IPv6 CIDR address block used for the destination
-- match. The value that you provide must match the CIDR of an existing
-- route in the table.
--
-- 'gatewayId', 'eC2ReplaceRouteAction_gatewayId' - Information about the ID of an internet gateway or virtual private
-- gateway.
--
-- 'destinationPrefixListId', 'eC2ReplaceRouteAction_destinationPrefixListId' - Information about the ID of the prefix list for the route.
--
-- 'description', 'eC2ReplaceRouteAction_description' - A description of the ReplaceRoute action in Amazon EC2.
--
-- 'destinationCidrBlock', 'eC2ReplaceRouteAction_destinationCidrBlock' - Information about the IPv4 CIDR address block used for the destination
-- match. The value that you provide must match the CIDR of an existing
-- route in the table.
--
-- 'routeTableId', 'eC2ReplaceRouteAction_routeTableId' - Information about the ID of the route table.
newEC2ReplaceRouteAction ::
  -- | 'routeTableId'
  ActionTarget ->
  EC2ReplaceRouteAction
newEC2ReplaceRouteAction pRouteTableId_ =
  EC2ReplaceRouteAction'
    { destinationIpv6CidrBlock =
        Prelude.Nothing,
      gatewayId = Prelude.Nothing,
      destinationPrefixListId = Prelude.Nothing,
      description = Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | Information about the IPv6 CIDR address block used for the destination
-- match. The value that you provide must match the CIDR of an existing
-- route in the table.
eC2ReplaceRouteAction_destinationIpv6CidrBlock :: Lens.Lens' EC2ReplaceRouteAction (Prelude.Maybe Prelude.Text)
eC2ReplaceRouteAction_destinationIpv6CidrBlock = Lens.lens (\EC2ReplaceRouteAction' {destinationIpv6CidrBlock} -> destinationIpv6CidrBlock) (\s@EC2ReplaceRouteAction' {} a -> s {destinationIpv6CidrBlock = a} :: EC2ReplaceRouteAction)

-- | Information about the ID of an internet gateway or virtual private
-- gateway.
eC2ReplaceRouteAction_gatewayId :: Lens.Lens' EC2ReplaceRouteAction (Prelude.Maybe ActionTarget)
eC2ReplaceRouteAction_gatewayId = Lens.lens (\EC2ReplaceRouteAction' {gatewayId} -> gatewayId) (\s@EC2ReplaceRouteAction' {} a -> s {gatewayId = a} :: EC2ReplaceRouteAction)

-- | Information about the ID of the prefix list for the route.
eC2ReplaceRouteAction_destinationPrefixListId :: Lens.Lens' EC2ReplaceRouteAction (Prelude.Maybe Prelude.Text)
eC2ReplaceRouteAction_destinationPrefixListId = Lens.lens (\EC2ReplaceRouteAction' {destinationPrefixListId} -> destinationPrefixListId) (\s@EC2ReplaceRouteAction' {} a -> s {destinationPrefixListId = a} :: EC2ReplaceRouteAction)

-- | A description of the ReplaceRoute action in Amazon EC2.
eC2ReplaceRouteAction_description :: Lens.Lens' EC2ReplaceRouteAction (Prelude.Maybe Prelude.Text)
eC2ReplaceRouteAction_description = Lens.lens (\EC2ReplaceRouteAction' {description} -> description) (\s@EC2ReplaceRouteAction' {} a -> s {description = a} :: EC2ReplaceRouteAction)

-- | Information about the IPv4 CIDR address block used for the destination
-- match. The value that you provide must match the CIDR of an existing
-- route in the table.
eC2ReplaceRouteAction_destinationCidrBlock :: Lens.Lens' EC2ReplaceRouteAction (Prelude.Maybe Prelude.Text)
eC2ReplaceRouteAction_destinationCidrBlock = Lens.lens (\EC2ReplaceRouteAction' {destinationCidrBlock} -> destinationCidrBlock) (\s@EC2ReplaceRouteAction' {} a -> s {destinationCidrBlock = a} :: EC2ReplaceRouteAction)

-- | Information about the ID of the route table.
eC2ReplaceRouteAction_routeTableId :: Lens.Lens' EC2ReplaceRouteAction ActionTarget
eC2ReplaceRouteAction_routeTableId = Lens.lens (\EC2ReplaceRouteAction' {routeTableId} -> routeTableId) (\s@EC2ReplaceRouteAction' {} a -> s {routeTableId = a} :: EC2ReplaceRouteAction)

instance Core.FromJSON EC2ReplaceRouteAction where
  parseJSON =
    Core.withObject
      "EC2ReplaceRouteAction"
      ( \x ->
          EC2ReplaceRouteAction'
            Prelude.<$> (x Core..:? "DestinationIpv6CidrBlock")
            Prelude.<*> (x Core..:? "GatewayId")
            Prelude.<*> (x Core..:? "DestinationPrefixListId")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "DestinationCidrBlock")
            Prelude.<*> (x Core..: "RouteTableId")
      )

instance Prelude.Hashable EC2ReplaceRouteAction where
  hashWithSalt salt' EC2ReplaceRouteAction' {..} =
    salt' `Prelude.hashWithSalt` routeTableId
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationPrefixListId
      `Prelude.hashWithSalt` gatewayId
      `Prelude.hashWithSalt` destinationIpv6CidrBlock

instance Prelude.NFData EC2ReplaceRouteAction where
  rnf EC2ReplaceRouteAction' {..} =
    Prelude.rnf destinationIpv6CidrBlock
      `Prelude.seq` Prelude.rnf routeTableId
      `Prelude.seq` Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf destinationPrefixListId
      `Prelude.seq` Prelude.rnf gatewayId
