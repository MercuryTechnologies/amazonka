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
-- Module      : Amazonka.FMS.Types.EC2DeleteRouteAction
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FMS.Types.EC2DeleteRouteAction where

import qualified Amazonka.Core as Core
import Amazonka.FMS.Types.ActionTarget
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the DeleteRoute action in Amazon EC2.
--
-- /See:/ 'newEC2DeleteRouteAction' smart constructor.
data EC2DeleteRouteAction = EC2DeleteRouteAction'
  { -- | Information about the IPv6 CIDR range for the route. The value you
    -- specify must match the CIDR for the route exactly.
    destinationIpv6CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Information about the ID of the prefix list for the route.
    destinationPrefixListId :: Prelude.Maybe Prelude.Text,
    -- | A description of the DeleteRoute action.
    description :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv4 CIDR range for the route. The value you
    -- specify must match the CIDR for the route exactly.
    destinationCidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Information about the ID of the route table.
    routeTableId :: ActionTarget
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EC2DeleteRouteAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationIpv6CidrBlock', 'eC2DeleteRouteAction_destinationIpv6CidrBlock' - Information about the IPv6 CIDR range for the route. The value you
-- specify must match the CIDR for the route exactly.
--
-- 'destinationPrefixListId', 'eC2DeleteRouteAction_destinationPrefixListId' - Information about the ID of the prefix list for the route.
--
-- 'description', 'eC2DeleteRouteAction_description' - A description of the DeleteRoute action.
--
-- 'destinationCidrBlock', 'eC2DeleteRouteAction_destinationCidrBlock' - Information about the IPv4 CIDR range for the route. The value you
-- specify must match the CIDR for the route exactly.
--
-- 'routeTableId', 'eC2DeleteRouteAction_routeTableId' - Information about the ID of the route table.
newEC2DeleteRouteAction ::
  -- | 'routeTableId'
  ActionTarget ->
  EC2DeleteRouteAction
newEC2DeleteRouteAction pRouteTableId_ =
  EC2DeleteRouteAction'
    { destinationIpv6CidrBlock =
        Prelude.Nothing,
      destinationPrefixListId = Prelude.Nothing,
      description = Prelude.Nothing,
      destinationCidrBlock = Prelude.Nothing,
      routeTableId = pRouteTableId_
    }

-- | Information about the IPv6 CIDR range for the route. The value you
-- specify must match the CIDR for the route exactly.
eC2DeleteRouteAction_destinationIpv6CidrBlock :: Lens.Lens' EC2DeleteRouteAction (Prelude.Maybe Prelude.Text)
eC2DeleteRouteAction_destinationIpv6CidrBlock = Lens.lens (\EC2DeleteRouteAction' {destinationIpv6CidrBlock} -> destinationIpv6CidrBlock) (\s@EC2DeleteRouteAction' {} a -> s {destinationIpv6CidrBlock = a} :: EC2DeleteRouteAction)

-- | Information about the ID of the prefix list for the route.
eC2DeleteRouteAction_destinationPrefixListId :: Lens.Lens' EC2DeleteRouteAction (Prelude.Maybe Prelude.Text)
eC2DeleteRouteAction_destinationPrefixListId = Lens.lens (\EC2DeleteRouteAction' {destinationPrefixListId} -> destinationPrefixListId) (\s@EC2DeleteRouteAction' {} a -> s {destinationPrefixListId = a} :: EC2DeleteRouteAction)

-- | A description of the DeleteRoute action.
eC2DeleteRouteAction_description :: Lens.Lens' EC2DeleteRouteAction (Prelude.Maybe Prelude.Text)
eC2DeleteRouteAction_description = Lens.lens (\EC2DeleteRouteAction' {description} -> description) (\s@EC2DeleteRouteAction' {} a -> s {description = a} :: EC2DeleteRouteAction)

-- | Information about the IPv4 CIDR range for the route. The value you
-- specify must match the CIDR for the route exactly.
eC2DeleteRouteAction_destinationCidrBlock :: Lens.Lens' EC2DeleteRouteAction (Prelude.Maybe Prelude.Text)
eC2DeleteRouteAction_destinationCidrBlock = Lens.lens (\EC2DeleteRouteAction' {destinationCidrBlock} -> destinationCidrBlock) (\s@EC2DeleteRouteAction' {} a -> s {destinationCidrBlock = a} :: EC2DeleteRouteAction)

-- | Information about the ID of the route table.
eC2DeleteRouteAction_routeTableId :: Lens.Lens' EC2DeleteRouteAction ActionTarget
eC2DeleteRouteAction_routeTableId = Lens.lens (\EC2DeleteRouteAction' {routeTableId} -> routeTableId) (\s@EC2DeleteRouteAction' {} a -> s {routeTableId = a} :: EC2DeleteRouteAction)

instance Core.FromJSON EC2DeleteRouteAction where
  parseJSON =
    Core.withObject
      "EC2DeleteRouteAction"
      ( \x ->
          EC2DeleteRouteAction'
            Prelude.<$> (x Core..:? "DestinationIpv6CidrBlock")
            Prelude.<*> (x Core..:? "DestinationPrefixListId")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "DestinationCidrBlock")
            Prelude.<*> (x Core..: "RouteTableId")
      )

instance Prelude.Hashable EC2DeleteRouteAction where
  hashWithSalt salt' EC2DeleteRouteAction' {..} =
    salt' `Prelude.hashWithSalt` routeTableId
      `Prelude.hashWithSalt` destinationCidrBlock
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` destinationPrefixListId
      `Prelude.hashWithSalt` destinationIpv6CidrBlock

instance Prelude.NFData EC2DeleteRouteAction where
  rnf EC2DeleteRouteAction' {..} =
    Prelude.rnf destinationIpv6CidrBlock
      `Prelude.seq` Prelude.rnf routeTableId
      `Prelude.seq` Prelude.rnf destinationCidrBlock
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf destinationPrefixListId
