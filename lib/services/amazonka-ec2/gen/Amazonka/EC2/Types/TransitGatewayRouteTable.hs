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
-- Module      : Amazonka.EC2.Types.TransitGatewayRouteTable
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayRouteTable where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TransitGatewayRouteTableState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a transit gateway route table.
--
-- /See:/ 'newTransitGatewayRouteTable' smart constructor.
data TransitGatewayRouteTable = TransitGatewayRouteTable'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Core.ISO8601,
    -- | The state of the transit gateway route table.
    state :: Prelude.Maybe TransitGatewayRouteTableState,
    -- | Indicates whether this is the default propagation route table for the
    -- transit gateway.
    defaultPropagationRouteTable :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the transit gateway route table.
    transitGatewayRouteTableId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this is the default association route table for the
    -- transit gateway.
    defaultAssociationRouteTable :: Prelude.Maybe Prelude.Bool,
    -- | Any tags assigned to the route table.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayRouteTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'transitGatewayRouteTable_creationTime' - The creation time.
--
-- 'state', 'transitGatewayRouteTable_state' - The state of the transit gateway route table.
--
-- 'defaultPropagationRouteTable', 'transitGatewayRouteTable_defaultPropagationRouteTable' - Indicates whether this is the default propagation route table for the
-- transit gateway.
--
-- 'transitGatewayRouteTableId', 'transitGatewayRouteTable_transitGatewayRouteTableId' - The ID of the transit gateway route table.
--
-- 'transitGatewayId', 'transitGatewayRouteTable_transitGatewayId' - The ID of the transit gateway.
--
-- 'defaultAssociationRouteTable', 'transitGatewayRouteTable_defaultAssociationRouteTable' - Indicates whether this is the default association route table for the
-- transit gateway.
--
-- 'tags', 'transitGatewayRouteTable_tags' - Any tags assigned to the route table.
newTransitGatewayRouteTable ::
  TransitGatewayRouteTable
newTransitGatewayRouteTable =
  TransitGatewayRouteTable'
    { creationTime =
        Prelude.Nothing,
      state = Prelude.Nothing,
      defaultPropagationRouteTable = Prelude.Nothing,
      transitGatewayRouteTableId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      defaultAssociationRouteTable = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The creation time.
transitGatewayRouteTable_creationTime :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.UTCTime)
transitGatewayRouteTable_creationTime = Lens.lens (\TransitGatewayRouteTable' {creationTime} -> creationTime) (\s@TransitGatewayRouteTable' {} a -> s {creationTime = a} :: TransitGatewayRouteTable) Prelude.. Lens.mapping Core._Time

-- | The state of the transit gateway route table.
transitGatewayRouteTable_state :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe TransitGatewayRouteTableState)
transitGatewayRouteTable_state = Lens.lens (\TransitGatewayRouteTable' {state} -> state) (\s@TransitGatewayRouteTable' {} a -> s {state = a} :: TransitGatewayRouteTable)

-- | Indicates whether this is the default propagation route table for the
-- transit gateway.
transitGatewayRouteTable_defaultPropagationRouteTable :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.Bool)
transitGatewayRouteTable_defaultPropagationRouteTable = Lens.lens (\TransitGatewayRouteTable' {defaultPropagationRouteTable} -> defaultPropagationRouteTable) (\s@TransitGatewayRouteTable' {} a -> s {defaultPropagationRouteTable = a} :: TransitGatewayRouteTable)

-- | The ID of the transit gateway route table.
transitGatewayRouteTable_transitGatewayRouteTableId :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.Text)
transitGatewayRouteTable_transitGatewayRouteTableId = Lens.lens (\TransitGatewayRouteTable' {transitGatewayRouteTableId} -> transitGatewayRouteTableId) (\s@TransitGatewayRouteTable' {} a -> s {transitGatewayRouteTableId = a} :: TransitGatewayRouteTable)

-- | The ID of the transit gateway.
transitGatewayRouteTable_transitGatewayId :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.Text)
transitGatewayRouteTable_transitGatewayId = Lens.lens (\TransitGatewayRouteTable' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayRouteTable' {} a -> s {transitGatewayId = a} :: TransitGatewayRouteTable)

-- | Indicates whether this is the default association route table for the
-- transit gateway.
transitGatewayRouteTable_defaultAssociationRouteTable :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe Prelude.Bool)
transitGatewayRouteTable_defaultAssociationRouteTable = Lens.lens (\TransitGatewayRouteTable' {defaultAssociationRouteTable} -> defaultAssociationRouteTable) (\s@TransitGatewayRouteTable' {} a -> s {defaultAssociationRouteTable = a} :: TransitGatewayRouteTable)

-- | Any tags assigned to the route table.
transitGatewayRouteTable_tags :: Lens.Lens' TransitGatewayRouteTable (Prelude.Maybe [Tag])
transitGatewayRouteTable_tags = Lens.lens (\TransitGatewayRouteTable' {tags} -> tags) (\s@TransitGatewayRouteTable' {} a -> s {tags = a} :: TransitGatewayRouteTable) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML TransitGatewayRouteTable where
  parseXML x =
    TransitGatewayRouteTable'
      Prelude.<$> (x Core..@? "creationTime")
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> (x Core..@? "defaultPropagationRouteTable")
      Prelude.<*> (x Core..@? "transitGatewayRouteTableId")
      Prelude.<*> (x Core..@? "transitGatewayId")
      Prelude.<*> (x Core..@? "defaultAssociationRouteTable")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance Prelude.Hashable TransitGatewayRouteTable where
  hashWithSalt salt' TransitGatewayRouteTable' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` defaultAssociationRouteTable
      `Prelude.hashWithSalt` transitGatewayId
      `Prelude.hashWithSalt` transitGatewayRouteTableId
      `Prelude.hashWithSalt` defaultPropagationRouteTable
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData TransitGatewayRouteTable where
  rnf TransitGatewayRouteTable' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf defaultAssociationRouteTable
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf transitGatewayRouteTableId
      `Prelude.seq` Prelude.rnf defaultPropagationRouteTable
      `Prelude.seq` Prelude.rnf state
