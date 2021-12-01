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
-- Module      : Amazonka.EC2.Types.NetworkInterfaceAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterfaceAssociation where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes association information for an Elastic IP address (IPv4 only),
-- or a Carrier IP address (for a network interface which resides in a
-- subnet in a Wavelength Zone).
--
-- /See:/ 'newNetworkInterfaceAssociation' smart constructor.
data NetworkInterfaceAssociation = NetworkInterfaceAssociation'
  { -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The public DNS name.
    publicDnsName :: Prelude.Maybe Prelude.Text,
    -- | The allocation ID.
    allocationId :: Prelude.Maybe Prelude.Text,
    -- | The carrier IP address associated with the network interface.
    --
    -- This option is only available when the network interface is in a subnet
    -- which is associated with a Wavelength Zone.
    carrierIp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Elastic IP address owner.
    ipOwnerId :: Prelude.Maybe Prelude.Text,
    -- | The customer-owned IP address associated with the network interface.
    customerOwnedIp :: Prelude.Maybe Prelude.Text,
    -- | The address of the Elastic IP address bound to the network interface.
    publicIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterfaceAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'networkInterfaceAssociation_associationId' - The association ID.
--
-- 'publicDnsName', 'networkInterfaceAssociation_publicDnsName' - The public DNS name.
--
-- 'allocationId', 'networkInterfaceAssociation_allocationId' - The allocation ID.
--
-- 'carrierIp', 'networkInterfaceAssociation_carrierIp' - The carrier IP address associated with the network interface.
--
-- This option is only available when the network interface is in a subnet
-- which is associated with a Wavelength Zone.
--
-- 'ipOwnerId', 'networkInterfaceAssociation_ipOwnerId' - The ID of the Elastic IP address owner.
--
-- 'customerOwnedIp', 'networkInterfaceAssociation_customerOwnedIp' - The customer-owned IP address associated with the network interface.
--
-- 'publicIp', 'networkInterfaceAssociation_publicIp' - The address of the Elastic IP address bound to the network interface.
newNetworkInterfaceAssociation ::
  NetworkInterfaceAssociation
newNetworkInterfaceAssociation =
  NetworkInterfaceAssociation'
    { associationId =
        Prelude.Nothing,
      publicDnsName = Prelude.Nothing,
      allocationId = Prelude.Nothing,
      carrierIp = Prelude.Nothing,
      ipOwnerId = Prelude.Nothing,
      customerOwnedIp = Prelude.Nothing,
      publicIp = Prelude.Nothing
    }

-- | The association ID.
networkInterfaceAssociation_associationId :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_associationId = Lens.lens (\NetworkInterfaceAssociation' {associationId} -> associationId) (\s@NetworkInterfaceAssociation' {} a -> s {associationId = a} :: NetworkInterfaceAssociation)

-- | The public DNS name.
networkInterfaceAssociation_publicDnsName :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_publicDnsName = Lens.lens (\NetworkInterfaceAssociation' {publicDnsName} -> publicDnsName) (\s@NetworkInterfaceAssociation' {} a -> s {publicDnsName = a} :: NetworkInterfaceAssociation)

-- | The allocation ID.
networkInterfaceAssociation_allocationId :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_allocationId = Lens.lens (\NetworkInterfaceAssociation' {allocationId} -> allocationId) (\s@NetworkInterfaceAssociation' {} a -> s {allocationId = a} :: NetworkInterfaceAssociation)

-- | The carrier IP address associated with the network interface.
--
-- This option is only available when the network interface is in a subnet
-- which is associated with a Wavelength Zone.
networkInterfaceAssociation_carrierIp :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_carrierIp = Lens.lens (\NetworkInterfaceAssociation' {carrierIp} -> carrierIp) (\s@NetworkInterfaceAssociation' {} a -> s {carrierIp = a} :: NetworkInterfaceAssociation)

-- | The ID of the Elastic IP address owner.
networkInterfaceAssociation_ipOwnerId :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_ipOwnerId = Lens.lens (\NetworkInterfaceAssociation' {ipOwnerId} -> ipOwnerId) (\s@NetworkInterfaceAssociation' {} a -> s {ipOwnerId = a} :: NetworkInterfaceAssociation)

-- | The customer-owned IP address associated with the network interface.
networkInterfaceAssociation_customerOwnedIp :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_customerOwnedIp = Lens.lens (\NetworkInterfaceAssociation' {customerOwnedIp} -> customerOwnedIp) (\s@NetworkInterfaceAssociation' {} a -> s {customerOwnedIp = a} :: NetworkInterfaceAssociation)

-- | The address of the Elastic IP address bound to the network interface.
networkInterfaceAssociation_publicIp :: Lens.Lens' NetworkInterfaceAssociation (Prelude.Maybe Prelude.Text)
networkInterfaceAssociation_publicIp = Lens.lens (\NetworkInterfaceAssociation' {publicIp} -> publicIp) (\s@NetworkInterfaceAssociation' {} a -> s {publicIp = a} :: NetworkInterfaceAssociation)

instance Core.FromXML NetworkInterfaceAssociation where
  parseXML x =
    NetworkInterfaceAssociation'
      Prelude.<$> (x Core..@? "associationId")
      Prelude.<*> (x Core..@? "publicDnsName")
      Prelude.<*> (x Core..@? "allocationId")
      Prelude.<*> (x Core..@? "carrierIp")
      Prelude.<*> (x Core..@? "ipOwnerId")
      Prelude.<*> (x Core..@? "customerOwnedIp")
      Prelude.<*> (x Core..@? "publicIp")

instance Prelude.Hashable NetworkInterfaceAssociation where
  hashWithSalt salt' NetworkInterfaceAssociation' {..} =
    salt' `Prelude.hashWithSalt` publicIp
      `Prelude.hashWithSalt` customerOwnedIp
      `Prelude.hashWithSalt` ipOwnerId
      `Prelude.hashWithSalt` carrierIp
      `Prelude.hashWithSalt` allocationId
      `Prelude.hashWithSalt` publicDnsName
      `Prelude.hashWithSalt` associationId

instance Prelude.NFData NetworkInterfaceAssociation where
  rnf NetworkInterfaceAssociation' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf publicIp
      `Prelude.seq` Prelude.rnf customerOwnedIp
      `Prelude.seq` Prelude.rnf ipOwnerId
      `Prelude.seq` Prelude.rnf carrierIp
      `Prelude.seq` Prelude.rnf allocationId
      `Prelude.seq` Prelude.rnf publicDnsName
