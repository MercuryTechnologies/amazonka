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
-- Module      : Amazonka.EC2.Types.ScheduledInstancesNetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ScheduledInstancesNetworkInterface where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ScheduledInstancesIpv6Address
import Amazonka.EC2.Types.ScheduledInstancesPrivateIpAddressConfig
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a network interface for a Scheduled Instance.
--
-- /See:/ 'newScheduledInstancesNetworkInterface' smart constructor.
data ScheduledInstancesNetworkInterface = ScheduledInstancesNetworkInterface'
  { -- | The IDs of the security groups.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | Indicates whether to delete the interface when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to assign a public IPv4 address to instances launched
    -- in a VPC. The public IPv4 address can only be assigned to a network
    -- interface for eth0, and can only be assigned to a new network interface,
    -- not an existing one. You cannot specify more than one network interface
    -- in the request. If launching into a default subnet, the default value is
    -- @true@.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | The private IPv4 addresses.
    privateIpAddressConfigs :: Prelude.Maybe [ScheduledInstancesPrivateIpAddressConfig],
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The number of IPv6 addresses to assign to the network interface. The
    -- IPv6 addresses are automatically selected from the subnet range.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The number of secondary private IPv4 addresses.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The index of the device for the network interface attachment.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | The specific IPv6 addresses from the subnet range.
    ipv6Addresses :: Prelude.Maybe [ScheduledInstancesIpv6Address]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ScheduledInstancesNetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'scheduledInstancesNetworkInterface_groups' - The IDs of the security groups.
--
-- 'deleteOnTermination', 'scheduledInstancesNetworkInterface_deleteOnTermination' - Indicates whether to delete the interface when the instance is
-- terminated.
--
-- 'associatePublicIpAddress', 'scheduledInstancesNetworkInterface_associatePublicIpAddress' - Indicates whether to assign a public IPv4 address to instances launched
-- in a VPC. The public IPv4 address can only be assigned to a network
-- interface for eth0, and can only be assigned to a new network interface,
-- not an existing one. You cannot specify more than one network interface
-- in the request. If launching into a default subnet, the default value is
-- @true@.
--
-- 'privateIpAddressConfigs', 'scheduledInstancesNetworkInterface_privateIpAddressConfigs' - The private IPv4 addresses.
--
-- 'networkInterfaceId', 'scheduledInstancesNetworkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'subnetId', 'scheduledInstancesNetworkInterface_subnetId' - The ID of the subnet.
--
-- 'ipv6AddressCount', 'scheduledInstancesNetworkInterface_ipv6AddressCount' - The number of IPv6 addresses to assign to the network interface. The
-- IPv6 addresses are automatically selected from the subnet range.
--
-- 'privateIpAddress', 'scheduledInstancesNetworkInterface_privateIpAddress' - The IPv4 address of the network interface within the subnet.
--
-- 'secondaryPrivateIpAddressCount', 'scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount' - The number of secondary private IPv4 addresses.
--
-- 'description', 'scheduledInstancesNetworkInterface_description' - The description.
--
-- 'deviceIndex', 'scheduledInstancesNetworkInterface_deviceIndex' - The index of the device for the network interface attachment.
--
-- 'ipv6Addresses', 'scheduledInstancesNetworkInterface_ipv6Addresses' - The specific IPv6 addresses from the subnet range.
newScheduledInstancesNetworkInterface ::
  ScheduledInstancesNetworkInterface
newScheduledInstancesNetworkInterface =
  ScheduledInstancesNetworkInterface'
    { groups =
        Prelude.Nothing,
      deleteOnTermination = Prelude.Nothing,
      associatePublicIpAddress =
        Prelude.Nothing,
      privateIpAddressConfigs =
        Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      ipv6AddressCount = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      secondaryPrivateIpAddressCount =
        Prelude.Nothing,
      description = Prelude.Nothing,
      deviceIndex = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing
    }

-- | The IDs of the security groups.
scheduledInstancesNetworkInterface_groups :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe [Prelude.Text])
scheduledInstancesNetworkInterface_groups = Lens.lens (\ScheduledInstancesNetworkInterface' {groups} -> groups) (\s@ScheduledInstancesNetworkInterface' {} a -> s {groups = a} :: ScheduledInstancesNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether to delete the interface when the instance is
-- terminated.
scheduledInstancesNetworkInterface_deleteOnTermination :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Bool)
scheduledInstancesNetworkInterface_deleteOnTermination = Lens.lens (\ScheduledInstancesNetworkInterface' {deleteOnTermination} -> deleteOnTermination) (\s@ScheduledInstancesNetworkInterface' {} a -> s {deleteOnTermination = a} :: ScheduledInstancesNetworkInterface)

-- | Indicates whether to assign a public IPv4 address to instances launched
-- in a VPC. The public IPv4 address can only be assigned to a network
-- interface for eth0, and can only be assigned to a new network interface,
-- not an existing one. You cannot specify more than one network interface
-- in the request. If launching into a default subnet, the default value is
-- @true@.
scheduledInstancesNetworkInterface_associatePublicIpAddress :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Bool)
scheduledInstancesNetworkInterface_associatePublicIpAddress = Lens.lens (\ScheduledInstancesNetworkInterface' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@ScheduledInstancesNetworkInterface' {} a -> s {associatePublicIpAddress = a} :: ScheduledInstancesNetworkInterface)

-- | The private IPv4 addresses.
scheduledInstancesNetworkInterface_privateIpAddressConfigs :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe [ScheduledInstancesPrivateIpAddressConfig])
scheduledInstancesNetworkInterface_privateIpAddressConfigs = Lens.lens (\ScheduledInstancesNetworkInterface' {privateIpAddressConfigs} -> privateIpAddressConfigs) (\s@ScheduledInstancesNetworkInterface' {} a -> s {privateIpAddressConfigs = a} :: ScheduledInstancesNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the network interface.
scheduledInstancesNetworkInterface_networkInterfaceId :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Text)
scheduledInstancesNetworkInterface_networkInterfaceId = Lens.lens (\ScheduledInstancesNetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@ScheduledInstancesNetworkInterface' {} a -> s {networkInterfaceId = a} :: ScheduledInstancesNetworkInterface)

-- | The ID of the subnet.
scheduledInstancesNetworkInterface_subnetId :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Text)
scheduledInstancesNetworkInterface_subnetId = Lens.lens (\ScheduledInstancesNetworkInterface' {subnetId} -> subnetId) (\s@ScheduledInstancesNetworkInterface' {} a -> s {subnetId = a} :: ScheduledInstancesNetworkInterface)

-- | The number of IPv6 addresses to assign to the network interface. The
-- IPv6 addresses are automatically selected from the subnet range.
scheduledInstancesNetworkInterface_ipv6AddressCount :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Int)
scheduledInstancesNetworkInterface_ipv6AddressCount = Lens.lens (\ScheduledInstancesNetworkInterface' {ipv6AddressCount} -> ipv6AddressCount) (\s@ScheduledInstancesNetworkInterface' {} a -> s {ipv6AddressCount = a} :: ScheduledInstancesNetworkInterface)

-- | The IPv4 address of the network interface within the subnet.
scheduledInstancesNetworkInterface_privateIpAddress :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Text)
scheduledInstancesNetworkInterface_privateIpAddress = Lens.lens (\ScheduledInstancesNetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@ScheduledInstancesNetworkInterface' {} a -> s {privateIpAddress = a} :: ScheduledInstancesNetworkInterface)

-- | The number of secondary private IPv4 addresses.
scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Int)
scheduledInstancesNetworkInterface_secondaryPrivateIpAddressCount = Lens.lens (\ScheduledInstancesNetworkInterface' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@ScheduledInstancesNetworkInterface' {} a -> s {secondaryPrivateIpAddressCount = a} :: ScheduledInstancesNetworkInterface)

-- | The description.
scheduledInstancesNetworkInterface_description :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Text)
scheduledInstancesNetworkInterface_description = Lens.lens (\ScheduledInstancesNetworkInterface' {description} -> description) (\s@ScheduledInstancesNetworkInterface' {} a -> s {description = a} :: ScheduledInstancesNetworkInterface)

-- | The index of the device for the network interface attachment.
scheduledInstancesNetworkInterface_deviceIndex :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe Prelude.Int)
scheduledInstancesNetworkInterface_deviceIndex = Lens.lens (\ScheduledInstancesNetworkInterface' {deviceIndex} -> deviceIndex) (\s@ScheduledInstancesNetworkInterface' {} a -> s {deviceIndex = a} :: ScheduledInstancesNetworkInterface)

-- | The specific IPv6 addresses from the subnet range.
scheduledInstancesNetworkInterface_ipv6Addresses :: Lens.Lens' ScheduledInstancesNetworkInterface (Prelude.Maybe [ScheduledInstancesIpv6Address])
scheduledInstancesNetworkInterface_ipv6Addresses = Lens.lens (\ScheduledInstancesNetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@ScheduledInstancesNetworkInterface' {} a -> s {ipv6Addresses = a} :: ScheduledInstancesNetworkInterface) Prelude.. Lens.mapping Lens.coerced

instance
  Prelude.Hashable
    ScheduledInstancesNetworkInterface
  where
  hashWithSalt
    salt'
    ScheduledInstancesNetworkInterface' {..} =
      salt' `Prelude.hashWithSalt` ipv6Addresses
        `Prelude.hashWithSalt` deviceIndex
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` secondaryPrivateIpAddressCount
        `Prelude.hashWithSalt` privateIpAddress
        `Prelude.hashWithSalt` ipv6AddressCount
        `Prelude.hashWithSalt` subnetId
        `Prelude.hashWithSalt` networkInterfaceId
        `Prelude.hashWithSalt` privateIpAddressConfigs
        `Prelude.hashWithSalt` associatePublicIpAddress
        `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` groups

instance
  Prelude.NFData
    ScheduledInstancesNetworkInterface
  where
  rnf ScheduledInstancesNetworkInterface' {..} =
    Prelude.rnf groups
      `Prelude.seq` Prelude.rnf ipv6Addresses
      `Prelude.seq` Prelude.rnf deviceIndex
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf secondaryPrivateIpAddressCount
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf ipv6AddressCount
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf privateIpAddressConfigs
      `Prelude.seq` Prelude.rnf associatePublicIpAddress
      `Prelude.seq` Prelude.rnf deleteOnTermination

instance
  Core.ToQuery
    ScheduledInstancesNetworkInterface
  where
  toQuery ScheduledInstancesNetworkInterface' {..} =
    Prelude.mconcat
      [ Core.toQuery
          (Core.toQueryList "Group" Prelude.<$> groups),
        "DeleteOnTermination" Core.=: deleteOnTermination,
        "AssociatePublicIpAddress"
          Core.=: associatePublicIpAddress,
        Core.toQuery
          ( Core.toQueryList "PrivateIpAddressConfig"
              Prelude.<$> privateIpAddressConfigs
          ),
        "NetworkInterfaceId" Core.=: networkInterfaceId,
        "SubnetId" Core.=: subnetId,
        "Ipv6AddressCount" Core.=: ipv6AddressCount,
        "PrivateIpAddress" Core.=: privateIpAddress,
        "SecondaryPrivateIpAddressCount"
          Core.=: secondaryPrivateIpAddressCount,
        "Description" Core.=: description,
        "DeviceIndex" Core.=: deviceIndex,
        Core.toQuery
          ( Core.toQueryList "Ipv6Address"
              Prelude.<$> ipv6Addresses
          )
      ]
