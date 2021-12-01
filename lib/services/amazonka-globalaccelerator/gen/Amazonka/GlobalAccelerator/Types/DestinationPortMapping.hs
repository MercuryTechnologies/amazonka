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
-- Module      : Amazonka.GlobalAccelerator.Types.DestinationPortMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.DestinationPortMapping where

import qualified Amazonka.Core as Core
import Amazonka.GlobalAccelerator.Types.CustomRoutingDestinationTrafficState
import Amazonka.GlobalAccelerator.Types.IpAddressType
import Amazonka.GlobalAccelerator.Types.SocketAddress
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The port mappings for a specified endpoint IP address (destination).
--
-- /See:/ 'newDestinationPortMapping' smart constructor.
data DestinationPortMapping = DestinationPortMapping'
  { -- | The endpoint IP address\/port combination for traffic received on the
    -- accelerator socket address.
    destinationSocketAddress :: Prelude.Maybe SocketAddress,
    -- | The Amazon Resource Name (ARN) of the custom routing accelerator that
    -- you have port mappings for.
    acceleratorArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint group.
    endpointGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region for the endpoint group.
    endpointGroupRegion :: Prelude.Maybe Prelude.Text,
    -- | The IP address type, which must be IPv4.
    ipAddressType :: Prelude.Maybe IpAddressType,
    -- | The IP address\/port combinations (sockets) that map to a given
    -- destination socket address.
    acceleratorSocketAddresses :: Prelude.Maybe [SocketAddress],
    -- | The ID for the virtual private cloud (VPC) subnet.
    endpointId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether or not a port mapping destination can receive traffic.
    -- The value is either ALLOW, if traffic is allowed to the destination, or
    -- DENY, if traffic is not allowed to the destination.
    destinationTrafficState :: Prelude.Maybe CustomRoutingDestinationTrafficState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DestinationPortMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'destinationSocketAddress', 'destinationPortMapping_destinationSocketAddress' - The endpoint IP address\/port combination for traffic received on the
-- accelerator socket address.
--
-- 'acceleratorArn', 'destinationPortMapping_acceleratorArn' - The Amazon Resource Name (ARN) of the custom routing accelerator that
-- you have port mappings for.
--
-- 'endpointGroupArn', 'destinationPortMapping_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group.
--
-- 'endpointGroupRegion', 'destinationPortMapping_endpointGroupRegion' - The AWS Region for the endpoint group.
--
-- 'ipAddressType', 'destinationPortMapping_ipAddressType' - The IP address type, which must be IPv4.
--
-- 'acceleratorSocketAddresses', 'destinationPortMapping_acceleratorSocketAddresses' - The IP address\/port combinations (sockets) that map to a given
-- destination socket address.
--
-- 'endpointId', 'destinationPortMapping_endpointId' - The ID for the virtual private cloud (VPC) subnet.
--
-- 'destinationTrafficState', 'destinationPortMapping_destinationTrafficState' - Indicates whether or not a port mapping destination can receive traffic.
-- The value is either ALLOW, if traffic is allowed to the destination, or
-- DENY, if traffic is not allowed to the destination.
newDestinationPortMapping ::
  DestinationPortMapping
newDestinationPortMapping =
  DestinationPortMapping'
    { destinationSocketAddress =
        Prelude.Nothing,
      acceleratorArn = Prelude.Nothing,
      endpointGroupArn = Prelude.Nothing,
      endpointGroupRegion = Prelude.Nothing,
      ipAddressType = Prelude.Nothing,
      acceleratorSocketAddresses = Prelude.Nothing,
      endpointId = Prelude.Nothing,
      destinationTrafficState = Prelude.Nothing
    }

-- | The endpoint IP address\/port combination for traffic received on the
-- accelerator socket address.
destinationPortMapping_destinationSocketAddress :: Lens.Lens' DestinationPortMapping (Prelude.Maybe SocketAddress)
destinationPortMapping_destinationSocketAddress = Lens.lens (\DestinationPortMapping' {destinationSocketAddress} -> destinationSocketAddress) (\s@DestinationPortMapping' {} a -> s {destinationSocketAddress = a} :: DestinationPortMapping)

-- | The Amazon Resource Name (ARN) of the custom routing accelerator that
-- you have port mappings for.
destinationPortMapping_acceleratorArn :: Lens.Lens' DestinationPortMapping (Prelude.Maybe Prelude.Text)
destinationPortMapping_acceleratorArn = Lens.lens (\DestinationPortMapping' {acceleratorArn} -> acceleratorArn) (\s@DestinationPortMapping' {} a -> s {acceleratorArn = a} :: DestinationPortMapping)

-- | The Amazon Resource Name (ARN) of the endpoint group.
destinationPortMapping_endpointGroupArn :: Lens.Lens' DestinationPortMapping (Prelude.Maybe Prelude.Text)
destinationPortMapping_endpointGroupArn = Lens.lens (\DestinationPortMapping' {endpointGroupArn} -> endpointGroupArn) (\s@DestinationPortMapping' {} a -> s {endpointGroupArn = a} :: DestinationPortMapping)

-- | The AWS Region for the endpoint group.
destinationPortMapping_endpointGroupRegion :: Lens.Lens' DestinationPortMapping (Prelude.Maybe Prelude.Text)
destinationPortMapping_endpointGroupRegion = Lens.lens (\DestinationPortMapping' {endpointGroupRegion} -> endpointGroupRegion) (\s@DestinationPortMapping' {} a -> s {endpointGroupRegion = a} :: DestinationPortMapping)

-- | The IP address type, which must be IPv4.
destinationPortMapping_ipAddressType :: Lens.Lens' DestinationPortMapping (Prelude.Maybe IpAddressType)
destinationPortMapping_ipAddressType = Lens.lens (\DestinationPortMapping' {ipAddressType} -> ipAddressType) (\s@DestinationPortMapping' {} a -> s {ipAddressType = a} :: DestinationPortMapping)

-- | The IP address\/port combinations (sockets) that map to a given
-- destination socket address.
destinationPortMapping_acceleratorSocketAddresses :: Lens.Lens' DestinationPortMapping (Prelude.Maybe [SocketAddress])
destinationPortMapping_acceleratorSocketAddresses = Lens.lens (\DestinationPortMapping' {acceleratorSocketAddresses} -> acceleratorSocketAddresses) (\s@DestinationPortMapping' {} a -> s {acceleratorSocketAddresses = a} :: DestinationPortMapping) Prelude.. Lens.mapping Lens.coerced

-- | The ID for the virtual private cloud (VPC) subnet.
destinationPortMapping_endpointId :: Lens.Lens' DestinationPortMapping (Prelude.Maybe Prelude.Text)
destinationPortMapping_endpointId = Lens.lens (\DestinationPortMapping' {endpointId} -> endpointId) (\s@DestinationPortMapping' {} a -> s {endpointId = a} :: DestinationPortMapping)

-- | Indicates whether or not a port mapping destination can receive traffic.
-- The value is either ALLOW, if traffic is allowed to the destination, or
-- DENY, if traffic is not allowed to the destination.
destinationPortMapping_destinationTrafficState :: Lens.Lens' DestinationPortMapping (Prelude.Maybe CustomRoutingDestinationTrafficState)
destinationPortMapping_destinationTrafficState = Lens.lens (\DestinationPortMapping' {destinationTrafficState} -> destinationTrafficState) (\s@DestinationPortMapping' {} a -> s {destinationTrafficState = a} :: DestinationPortMapping)

instance Core.FromJSON DestinationPortMapping where
  parseJSON =
    Core.withObject
      "DestinationPortMapping"
      ( \x ->
          DestinationPortMapping'
            Prelude.<$> (x Core..:? "DestinationSocketAddress")
            Prelude.<*> (x Core..:? "AcceleratorArn")
            Prelude.<*> (x Core..:? "EndpointGroupArn")
            Prelude.<*> (x Core..:? "EndpointGroupRegion")
            Prelude.<*> (x Core..:? "IpAddressType")
            Prelude.<*> ( x Core..:? "AcceleratorSocketAddresses"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "EndpointId")
            Prelude.<*> (x Core..:? "DestinationTrafficState")
      )

instance Prelude.Hashable DestinationPortMapping where
  hashWithSalt salt' DestinationPortMapping' {..} =
    salt'
      `Prelude.hashWithSalt` destinationTrafficState
      `Prelude.hashWithSalt` endpointId
      `Prelude.hashWithSalt` acceleratorSocketAddresses
      `Prelude.hashWithSalt` ipAddressType
      `Prelude.hashWithSalt` endpointGroupRegion
      `Prelude.hashWithSalt` endpointGroupArn
      `Prelude.hashWithSalt` acceleratorArn
      `Prelude.hashWithSalt` destinationSocketAddress

instance Prelude.NFData DestinationPortMapping where
  rnf DestinationPortMapping' {..} =
    Prelude.rnf destinationSocketAddress
      `Prelude.seq` Prelude.rnf destinationTrafficState
      `Prelude.seq` Prelude.rnf endpointId
      `Prelude.seq` Prelude.rnf acceleratorSocketAddresses
      `Prelude.seq` Prelude.rnf ipAddressType
      `Prelude.seq` Prelude.rnf endpointGroupRegion
      `Prelude.seq` Prelude.rnf endpointGroupArn
      `Prelude.seq` Prelude.rnf acceleratorArn
