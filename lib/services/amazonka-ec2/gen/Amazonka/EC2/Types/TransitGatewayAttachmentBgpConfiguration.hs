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
-- Module      : Amazonka.EC2.Types.TransitGatewayAttachmentBgpConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TransitGatewayAttachmentBgpConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.BgpStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The BGP configuration information.
--
-- /See:/ 'newTransitGatewayAttachmentBgpConfiguration' smart constructor.
data TransitGatewayAttachmentBgpConfiguration = TransitGatewayAttachmentBgpConfiguration'
  { -- | The transit gateway Autonomous System Number (ASN).
    transitGatewayAsn :: Prelude.Maybe Prelude.Integer,
    -- | The peer Autonomous System Number (ASN).
    peerAsn :: Prelude.Maybe Prelude.Integer,
    -- | The interior BGP peer IP address for the transit gateway.
    transitGatewayAddress :: Prelude.Maybe Prelude.Text,
    -- | The BGP status.
    bgpStatus :: Prelude.Maybe BgpStatus,
    -- | The interior BGP peer IP address for the appliance.
    peerAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TransitGatewayAttachmentBgpConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayAsn', 'transitGatewayAttachmentBgpConfiguration_transitGatewayAsn' - The transit gateway Autonomous System Number (ASN).
--
-- 'peerAsn', 'transitGatewayAttachmentBgpConfiguration_peerAsn' - The peer Autonomous System Number (ASN).
--
-- 'transitGatewayAddress', 'transitGatewayAttachmentBgpConfiguration_transitGatewayAddress' - The interior BGP peer IP address for the transit gateway.
--
-- 'bgpStatus', 'transitGatewayAttachmentBgpConfiguration_bgpStatus' - The BGP status.
--
-- 'peerAddress', 'transitGatewayAttachmentBgpConfiguration_peerAddress' - The interior BGP peer IP address for the appliance.
newTransitGatewayAttachmentBgpConfiguration ::
  TransitGatewayAttachmentBgpConfiguration
newTransitGatewayAttachmentBgpConfiguration =
  TransitGatewayAttachmentBgpConfiguration'
    { transitGatewayAsn =
        Prelude.Nothing,
      peerAsn = Prelude.Nothing,
      transitGatewayAddress =
        Prelude.Nothing,
      bgpStatus = Prelude.Nothing,
      peerAddress = Prelude.Nothing
    }

-- | The transit gateway Autonomous System Number (ASN).
transitGatewayAttachmentBgpConfiguration_transitGatewayAsn :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Prelude.Maybe Prelude.Integer)
transitGatewayAttachmentBgpConfiguration_transitGatewayAsn = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {transitGatewayAsn} -> transitGatewayAsn) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {transitGatewayAsn = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The peer Autonomous System Number (ASN).
transitGatewayAttachmentBgpConfiguration_peerAsn :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Prelude.Maybe Prelude.Integer)
transitGatewayAttachmentBgpConfiguration_peerAsn = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {peerAsn} -> peerAsn) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {peerAsn = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The interior BGP peer IP address for the transit gateway.
transitGatewayAttachmentBgpConfiguration_transitGatewayAddress :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Prelude.Maybe Prelude.Text)
transitGatewayAttachmentBgpConfiguration_transitGatewayAddress = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {transitGatewayAddress} -> transitGatewayAddress) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {transitGatewayAddress = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The BGP status.
transitGatewayAttachmentBgpConfiguration_bgpStatus :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Prelude.Maybe BgpStatus)
transitGatewayAttachmentBgpConfiguration_bgpStatus = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {bgpStatus} -> bgpStatus) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {bgpStatus = a} :: TransitGatewayAttachmentBgpConfiguration)

-- | The interior BGP peer IP address for the appliance.
transitGatewayAttachmentBgpConfiguration_peerAddress :: Lens.Lens' TransitGatewayAttachmentBgpConfiguration (Prelude.Maybe Prelude.Text)
transitGatewayAttachmentBgpConfiguration_peerAddress = Lens.lens (\TransitGatewayAttachmentBgpConfiguration' {peerAddress} -> peerAddress) (\s@TransitGatewayAttachmentBgpConfiguration' {} a -> s {peerAddress = a} :: TransitGatewayAttachmentBgpConfiguration)

instance
  Core.FromXML
    TransitGatewayAttachmentBgpConfiguration
  where
  parseXML x =
    TransitGatewayAttachmentBgpConfiguration'
      Prelude.<$> (x Core..@? "transitGatewayAsn")
        Prelude.<*> (x Core..@? "peerAsn")
        Prelude.<*> (x Core..@? "transitGatewayAddress")
        Prelude.<*> (x Core..@? "bgpStatus")
        Prelude.<*> (x Core..@? "peerAddress")

instance
  Prelude.Hashable
    TransitGatewayAttachmentBgpConfiguration
  where
  hashWithSalt
    salt'
    TransitGatewayAttachmentBgpConfiguration' {..} =
      salt' `Prelude.hashWithSalt` peerAddress
        `Prelude.hashWithSalt` bgpStatus
        `Prelude.hashWithSalt` transitGatewayAddress
        `Prelude.hashWithSalt` peerAsn
        `Prelude.hashWithSalt` transitGatewayAsn

instance
  Prelude.NFData
    TransitGatewayAttachmentBgpConfiguration
  where
  rnf TransitGatewayAttachmentBgpConfiguration' {..} =
    Prelude.rnf transitGatewayAsn
      `Prelude.seq` Prelude.rnf peerAddress
      `Prelude.seq` Prelude.rnf bgpStatus
      `Prelude.seq` Prelude.rnf transitGatewayAddress
      `Prelude.seq` Prelude.rnf peerAsn
