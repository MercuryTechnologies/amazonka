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
-- Module      : Amazonka.EC2.Types.TransitGatewayMulticastDomain
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayMulticastDomain where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.Tag
import Amazonka.EC2.Types.TransitGatewayMulticastDomainOptions
import Amazonka.EC2.Types.TransitGatewayMulticastDomainState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the transit gateway multicast domain.
--
-- /See:/ 'newTransitGatewayMulticastDomain' smart constructor.
data TransitGatewayMulticastDomain = TransitGatewayMulticastDomain'
  { -- | The time the transit gateway multicast domain was created.
    creationTime :: Prelude.Maybe Core.ISO8601,
    -- | The state of the transit gateway multicast domain.
    state :: Prelude.Maybe TransitGatewayMulticastDomainState,
    -- | The ID of the transit gateway multicast domain.
    transitGatewayMulticastDomainId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the transit gateway multicast domain.
    transitGatewayMulticastDomainArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the transit gateway
    -- multicast domain.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The options for the transit gateway multicast domain.
    options :: Prelude.Maybe TransitGatewayMulticastDomainOptions,
    -- | The tags for the transit gateway multicast domain.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayMulticastDomain' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'transitGatewayMulticastDomain_creationTime' - The time the transit gateway multicast domain was created.
--
-- 'state', 'transitGatewayMulticastDomain_state' - The state of the transit gateway multicast domain.
--
-- 'transitGatewayMulticastDomainId', 'transitGatewayMulticastDomain_transitGatewayMulticastDomainId' - The ID of the transit gateway multicast domain.
--
-- 'transitGatewayMulticastDomainArn', 'transitGatewayMulticastDomain_transitGatewayMulticastDomainArn' - The Amazon Resource Name (ARN) of the transit gateway multicast domain.
--
-- 'ownerId', 'transitGatewayMulticastDomain_ownerId' - The ID of the Amazon Web Services account that owns the transit gateway
-- multicast domain.
--
-- 'transitGatewayId', 'transitGatewayMulticastDomain_transitGatewayId' - The ID of the transit gateway.
--
-- 'options', 'transitGatewayMulticastDomain_options' - The options for the transit gateway multicast domain.
--
-- 'tags', 'transitGatewayMulticastDomain_tags' - The tags for the transit gateway multicast domain.
newTransitGatewayMulticastDomain ::
  TransitGatewayMulticastDomain
newTransitGatewayMulticastDomain =
  TransitGatewayMulticastDomain'
    { creationTime =
        Prelude.Nothing,
      state = Prelude.Nothing,
      transitGatewayMulticastDomainId =
        Prelude.Nothing,
      transitGatewayMulticastDomainArn =
        Prelude.Nothing,
      ownerId = Prelude.Nothing,
      transitGatewayId = Prelude.Nothing,
      options = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The time the transit gateway multicast domain was created.
transitGatewayMulticastDomain_creationTime :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe Prelude.UTCTime)
transitGatewayMulticastDomain_creationTime = Lens.lens (\TransitGatewayMulticastDomain' {creationTime} -> creationTime) (\s@TransitGatewayMulticastDomain' {} a -> s {creationTime = a} :: TransitGatewayMulticastDomain) Prelude.. Lens.mapping Core._Time

-- | The state of the transit gateway multicast domain.
transitGatewayMulticastDomain_state :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe TransitGatewayMulticastDomainState)
transitGatewayMulticastDomain_state = Lens.lens (\TransitGatewayMulticastDomain' {state} -> state) (\s@TransitGatewayMulticastDomain' {} a -> s {state = a} :: TransitGatewayMulticastDomain)

-- | The ID of the transit gateway multicast domain.
transitGatewayMulticastDomain_transitGatewayMulticastDomainId :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomain_transitGatewayMulticastDomainId = Lens.lens (\TransitGatewayMulticastDomain' {transitGatewayMulticastDomainId} -> transitGatewayMulticastDomainId) (\s@TransitGatewayMulticastDomain' {} a -> s {transitGatewayMulticastDomainId = a} :: TransitGatewayMulticastDomain)

-- | The Amazon Resource Name (ARN) of the transit gateway multicast domain.
transitGatewayMulticastDomain_transitGatewayMulticastDomainArn :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomain_transitGatewayMulticastDomainArn = Lens.lens (\TransitGatewayMulticastDomain' {transitGatewayMulticastDomainArn} -> transitGatewayMulticastDomainArn) (\s@TransitGatewayMulticastDomain' {} a -> s {transitGatewayMulticastDomainArn = a} :: TransitGatewayMulticastDomain)

-- | The ID of the Amazon Web Services account that owns the transit gateway
-- multicast domain.
transitGatewayMulticastDomain_ownerId :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomain_ownerId = Lens.lens (\TransitGatewayMulticastDomain' {ownerId} -> ownerId) (\s@TransitGatewayMulticastDomain' {} a -> s {ownerId = a} :: TransitGatewayMulticastDomain)

-- | The ID of the transit gateway.
transitGatewayMulticastDomain_transitGatewayId :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe Prelude.Text)
transitGatewayMulticastDomain_transitGatewayId = Lens.lens (\TransitGatewayMulticastDomain' {transitGatewayId} -> transitGatewayId) (\s@TransitGatewayMulticastDomain' {} a -> s {transitGatewayId = a} :: TransitGatewayMulticastDomain)

-- | The options for the transit gateway multicast domain.
transitGatewayMulticastDomain_options :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe TransitGatewayMulticastDomainOptions)
transitGatewayMulticastDomain_options = Lens.lens (\TransitGatewayMulticastDomain' {options} -> options) (\s@TransitGatewayMulticastDomain' {} a -> s {options = a} :: TransitGatewayMulticastDomain)

-- | The tags for the transit gateway multicast domain.
transitGatewayMulticastDomain_tags :: Lens.Lens' TransitGatewayMulticastDomain (Prelude.Maybe [Tag])
transitGatewayMulticastDomain_tags = Lens.lens (\TransitGatewayMulticastDomain' {tags} -> tags) (\s@TransitGatewayMulticastDomain' {} a -> s {tags = a} :: TransitGatewayMulticastDomain) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML TransitGatewayMulticastDomain where
  parseXML x =
    TransitGatewayMulticastDomain'
      Prelude.<$> (x Core..@? "creationTime")
      Prelude.<*> (x Core..@? "state")
      Prelude.<*> (x Core..@? "transitGatewayMulticastDomainId")
      Prelude.<*> (x Core..@? "transitGatewayMulticastDomainArn")
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> (x Core..@? "transitGatewayId")
      Prelude.<*> (x Core..@? "options")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )

instance
  Prelude.Hashable
    TransitGatewayMulticastDomain
  where
  hashWithSalt salt' TransitGatewayMulticastDomain' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` options
      `Prelude.hashWithSalt` transitGatewayId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` transitGatewayMulticastDomainArn
      `Prelude.hashWithSalt` transitGatewayMulticastDomainId
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData TransitGatewayMulticastDomain where
  rnf TransitGatewayMulticastDomain' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf options
      `Prelude.seq` Prelude.rnf transitGatewayId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf transitGatewayMulticastDomainArn
      `Prelude.seq` Prelude.rnf transitGatewayMulticastDomainId
      `Prelude.seq` Prelude.rnf state
