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
-- Module      : Amazonka.DirectConnect.Types.VirtualInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.VirtualInterface where

import qualified Amazonka.Core as Core
import Amazonka.DirectConnect.Types.AddressFamily
import Amazonka.DirectConnect.Types.BGPPeer
import Amazonka.DirectConnect.Types.RouteFilterPrefix
import Amazonka.DirectConnect.Types.Tag
import Amazonka.DirectConnect.Types.VirtualInterfaceState
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a virtual interface.
--
-- /See:/ 'newVirtualInterface' smart constructor.
data VirtualInterface = VirtualInterface'
  { -- | The BGP peers configured on this virtual interface.
    bgpPeers :: Prelude.Maybe [BGPPeer],
    -- | The ID of the virtual private gateway. Applies only to private virtual
    -- interfaces.
    virtualGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The maximum transmission unit (MTU), in bytes. The supported values are
    -- 1500 and 9001. The default value is 1500.
    mtu :: Prelude.Maybe Prelude.Int,
    -- | The routes to be advertised to the Amazon Web Services network in this
    -- Region. Applies to public virtual interfaces.
    routeFilterPrefixes :: Prelude.Maybe [RouteFilterPrefix],
    -- | The IP address assigned to the customer interface.
    customerAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VLAN.
    vlan :: Prelude.Maybe Prelude.Int,
    -- | The location of the connection.
    location :: Prelude.Maybe Prelude.Text,
    -- | The IP address assigned to the Amazon interface.
    amazonAddress :: Prelude.Maybe Prelude.Text,
    -- | The address family for the BGP peer.
    addressFamily :: Prelude.Maybe AddressFamily,
    -- | The state of the virtual interface. The following are the possible
    -- values:
    --
    -- -   @confirming@: The creation of the virtual interface is pending
    --     confirmation from the virtual interface owner. If the owner of the
    --     virtual interface is different from the owner of the connection on
    --     which it is provisioned, then the virtual interface will remain in
    --     this state until it is confirmed by the virtual interface owner.
    --
    -- -   @verifying@: This state only applies to public virtual interfaces.
    --     Each public virtual interface needs validation before the virtual
    --     interface can be created.
    --
    -- -   @pending@: A virtual interface is in this state from the time that
    --     it is created until the virtual interface is ready to forward
    --     traffic.
    --
    -- -   @available@: A virtual interface that is able to forward traffic.
    --
    -- -   @down@: A virtual interface that is BGP down.
    --
    -- -   @deleting@: A virtual interface is in this state immediately after
    --     calling DeleteVirtualInterface until it can no longer forward
    --     traffic.
    --
    -- -   @deleted@: A virtual interface that cannot forward traffic.
    --
    -- -   @rejected@: The virtual interface owner has declined creation of the
    --     virtual interface. If a virtual interface in the @Confirming@ state
    --     is deleted by the virtual interface owner, the virtual interface
    --     enters the @Rejected@ state.
    --
    -- -   @unknown@: The state of the virtual interface is not available.
    virtualInterfaceState :: Prelude.Maybe VirtualInterfaceState,
    -- | The ID of the connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that terminates the logical connection. This
    -- device might be different than the device that terminates the physical
    -- connection.
    awsLogicalDeviceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The autonomous system number (ASN) for the Amazon side of the
    -- connection.
    amazonSideAsn :: Prelude.Maybe Prelude.Integer,
    -- | The type of virtual interface. The possible values are @private@ and
    -- @public@.
    virtualInterfaceType :: Prelude.Maybe Prelude.Text,
    -- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
    -- configuration.
    --
    -- The valid values are 1-2147483647.
    asn :: Prelude.Maybe Prelude.Int,
    -- | The authentication key for BGP configuration. This string has a minimum
    -- length of 6 characters and and a maximun lenth of 80 characters.
    authKey :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Prelude.Maybe Prelude.Bool,
    -- | The customer router configuration.
    customerRouterConfig :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the virtual
    -- interface.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region where the virtual interface is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name of the virtual interface assigned by the customer network. The
    -- name has a maximum of 100 characters. The following are valid
    -- characters: a-z, 0-9 and a hyphen (-).
    virtualInterfaceName :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that terminates the physical connection.
    awsDeviceV2 :: Prelude.Maybe Prelude.Text,
    -- | The ID of the virtual interface.
    virtualInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the virtual interface.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bgpPeers', 'virtualInterface_bgpPeers' - The BGP peers configured on this virtual interface.
--
-- 'virtualGatewayId', 'virtualInterface_virtualGatewayId' - The ID of the virtual private gateway. Applies only to private virtual
-- interfaces.
--
-- 'mtu', 'virtualInterface_mtu' - The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
--
-- 'routeFilterPrefixes', 'virtualInterface_routeFilterPrefixes' - The routes to be advertised to the Amazon Web Services network in this
-- Region. Applies to public virtual interfaces.
--
-- 'customerAddress', 'virtualInterface_customerAddress' - The IP address assigned to the customer interface.
--
-- 'vlan', 'virtualInterface_vlan' - The ID of the VLAN.
--
-- 'location', 'virtualInterface_location' - The location of the connection.
--
-- 'amazonAddress', 'virtualInterface_amazonAddress' - The IP address assigned to the Amazon interface.
--
-- 'addressFamily', 'virtualInterface_addressFamily' - The address family for the BGP peer.
--
-- 'virtualInterfaceState', 'virtualInterface_virtualInterfaceState' - The state of the virtual interface. The following are the possible
-- values:
--
-- -   @confirming@: The creation of the virtual interface is pending
--     confirmation from the virtual interface owner. If the owner of the
--     virtual interface is different from the owner of the connection on
--     which it is provisioned, then the virtual interface will remain in
--     this state until it is confirmed by the virtual interface owner.
--
-- -   @verifying@: This state only applies to public virtual interfaces.
--     Each public virtual interface needs validation before the virtual
--     interface can be created.
--
-- -   @pending@: A virtual interface is in this state from the time that
--     it is created until the virtual interface is ready to forward
--     traffic.
--
-- -   @available@: A virtual interface that is able to forward traffic.
--
-- -   @down@: A virtual interface that is BGP down.
--
-- -   @deleting@: A virtual interface is in this state immediately after
--     calling DeleteVirtualInterface until it can no longer forward
--     traffic.
--
-- -   @deleted@: A virtual interface that cannot forward traffic.
--
-- -   @rejected@: The virtual interface owner has declined creation of the
--     virtual interface. If a virtual interface in the @Confirming@ state
--     is deleted by the virtual interface owner, the virtual interface
--     enters the @Rejected@ state.
--
-- -   @unknown@: The state of the virtual interface is not available.
--
-- 'connectionId', 'virtualInterface_connectionId' - The ID of the connection.
--
-- 'awsLogicalDeviceId', 'virtualInterface_awsLogicalDeviceId' - The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
--
-- 'directConnectGatewayId', 'virtualInterface_directConnectGatewayId' - The ID of the Direct Connect gateway.
--
-- 'amazonSideAsn', 'virtualInterface_amazonSideAsn' - The autonomous system number (ASN) for the Amazon side of the
-- connection.
--
-- 'virtualInterfaceType', 'virtualInterface_virtualInterfaceType' - The type of virtual interface. The possible values are @private@ and
-- @public@.
--
-- 'asn', 'virtualInterface_asn' - The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
--
-- 'authKey', 'virtualInterface_authKey' - The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
--
-- 'jumboFrameCapable', 'virtualInterface_jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- 'customerRouterConfig', 'virtualInterface_customerRouterConfig' - The customer router configuration.
--
-- 'ownerAccount', 'virtualInterface_ownerAccount' - The ID of the Amazon Web Services account that owns the virtual
-- interface.
--
-- 'region', 'virtualInterface_region' - The Amazon Web Services Region where the virtual interface is located.
--
-- 'virtualInterfaceName', 'virtualInterface_virtualInterfaceName' - The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
--
-- 'awsDeviceV2', 'virtualInterface_awsDeviceV2' - The Direct Connect endpoint that terminates the physical connection.
--
-- 'virtualInterfaceId', 'virtualInterface_virtualInterfaceId' - The ID of the virtual interface.
--
-- 'tags', 'virtualInterface_tags' - The tags associated with the virtual interface.
newVirtualInterface ::
  VirtualInterface
newVirtualInterface =
  VirtualInterface'
    { bgpPeers = Prelude.Nothing,
      virtualGatewayId = Prelude.Nothing,
      mtu = Prelude.Nothing,
      routeFilterPrefixes = Prelude.Nothing,
      customerAddress = Prelude.Nothing,
      vlan = Prelude.Nothing,
      location = Prelude.Nothing,
      amazonAddress = Prelude.Nothing,
      addressFamily = Prelude.Nothing,
      virtualInterfaceState = Prelude.Nothing,
      connectionId = Prelude.Nothing,
      awsLogicalDeviceId = Prelude.Nothing,
      directConnectGatewayId = Prelude.Nothing,
      amazonSideAsn = Prelude.Nothing,
      virtualInterfaceType = Prelude.Nothing,
      asn = Prelude.Nothing,
      authKey = Prelude.Nothing,
      jumboFrameCapable = Prelude.Nothing,
      customerRouterConfig = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      region = Prelude.Nothing,
      virtualInterfaceName = Prelude.Nothing,
      awsDeviceV2 = Prelude.Nothing,
      virtualInterfaceId = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The BGP peers configured on this virtual interface.
virtualInterface_bgpPeers :: Lens.Lens' VirtualInterface (Prelude.Maybe [BGPPeer])
virtualInterface_bgpPeers = Lens.lens (\VirtualInterface' {bgpPeers} -> bgpPeers) (\s@VirtualInterface' {} a -> s {bgpPeers = a} :: VirtualInterface) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the virtual private gateway. Applies only to private virtual
-- interfaces.
virtualInterface_virtualGatewayId :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_virtualGatewayId = Lens.lens (\VirtualInterface' {virtualGatewayId} -> virtualGatewayId) (\s@VirtualInterface' {} a -> s {virtualGatewayId = a} :: VirtualInterface)

-- | The maximum transmission unit (MTU), in bytes. The supported values are
-- 1500 and 9001. The default value is 1500.
virtualInterface_mtu :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Int)
virtualInterface_mtu = Lens.lens (\VirtualInterface' {mtu} -> mtu) (\s@VirtualInterface' {} a -> s {mtu = a} :: VirtualInterface)

-- | The routes to be advertised to the Amazon Web Services network in this
-- Region. Applies to public virtual interfaces.
virtualInterface_routeFilterPrefixes :: Lens.Lens' VirtualInterface (Prelude.Maybe [RouteFilterPrefix])
virtualInterface_routeFilterPrefixes = Lens.lens (\VirtualInterface' {routeFilterPrefixes} -> routeFilterPrefixes) (\s@VirtualInterface' {} a -> s {routeFilterPrefixes = a} :: VirtualInterface) Prelude.. Lens.mapping Lens.coerced

-- | The IP address assigned to the customer interface.
virtualInterface_customerAddress :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_customerAddress = Lens.lens (\VirtualInterface' {customerAddress} -> customerAddress) (\s@VirtualInterface' {} a -> s {customerAddress = a} :: VirtualInterface)

-- | The ID of the VLAN.
virtualInterface_vlan :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Int)
virtualInterface_vlan = Lens.lens (\VirtualInterface' {vlan} -> vlan) (\s@VirtualInterface' {} a -> s {vlan = a} :: VirtualInterface)

-- | The location of the connection.
virtualInterface_location :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_location = Lens.lens (\VirtualInterface' {location} -> location) (\s@VirtualInterface' {} a -> s {location = a} :: VirtualInterface)

-- | The IP address assigned to the Amazon interface.
virtualInterface_amazonAddress :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_amazonAddress = Lens.lens (\VirtualInterface' {amazonAddress} -> amazonAddress) (\s@VirtualInterface' {} a -> s {amazonAddress = a} :: VirtualInterface)

-- | The address family for the BGP peer.
virtualInterface_addressFamily :: Lens.Lens' VirtualInterface (Prelude.Maybe AddressFamily)
virtualInterface_addressFamily = Lens.lens (\VirtualInterface' {addressFamily} -> addressFamily) (\s@VirtualInterface' {} a -> s {addressFamily = a} :: VirtualInterface)

-- | The state of the virtual interface. The following are the possible
-- values:
--
-- -   @confirming@: The creation of the virtual interface is pending
--     confirmation from the virtual interface owner. If the owner of the
--     virtual interface is different from the owner of the connection on
--     which it is provisioned, then the virtual interface will remain in
--     this state until it is confirmed by the virtual interface owner.
--
-- -   @verifying@: This state only applies to public virtual interfaces.
--     Each public virtual interface needs validation before the virtual
--     interface can be created.
--
-- -   @pending@: A virtual interface is in this state from the time that
--     it is created until the virtual interface is ready to forward
--     traffic.
--
-- -   @available@: A virtual interface that is able to forward traffic.
--
-- -   @down@: A virtual interface that is BGP down.
--
-- -   @deleting@: A virtual interface is in this state immediately after
--     calling DeleteVirtualInterface until it can no longer forward
--     traffic.
--
-- -   @deleted@: A virtual interface that cannot forward traffic.
--
-- -   @rejected@: The virtual interface owner has declined creation of the
--     virtual interface. If a virtual interface in the @Confirming@ state
--     is deleted by the virtual interface owner, the virtual interface
--     enters the @Rejected@ state.
--
-- -   @unknown@: The state of the virtual interface is not available.
virtualInterface_virtualInterfaceState :: Lens.Lens' VirtualInterface (Prelude.Maybe VirtualInterfaceState)
virtualInterface_virtualInterfaceState = Lens.lens (\VirtualInterface' {virtualInterfaceState} -> virtualInterfaceState) (\s@VirtualInterface' {} a -> s {virtualInterfaceState = a} :: VirtualInterface)

-- | The ID of the connection.
virtualInterface_connectionId :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_connectionId = Lens.lens (\VirtualInterface' {connectionId} -> connectionId) (\s@VirtualInterface' {} a -> s {connectionId = a} :: VirtualInterface)

-- | The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
virtualInterface_awsLogicalDeviceId :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_awsLogicalDeviceId = Lens.lens (\VirtualInterface' {awsLogicalDeviceId} -> awsLogicalDeviceId) (\s@VirtualInterface' {} a -> s {awsLogicalDeviceId = a} :: VirtualInterface)

-- | The ID of the Direct Connect gateway.
virtualInterface_directConnectGatewayId :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_directConnectGatewayId = Lens.lens (\VirtualInterface' {directConnectGatewayId} -> directConnectGatewayId) (\s@VirtualInterface' {} a -> s {directConnectGatewayId = a} :: VirtualInterface)

-- | The autonomous system number (ASN) for the Amazon side of the
-- connection.
virtualInterface_amazonSideAsn :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Integer)
virtualInterface_amazonSideAsn = Lens.lens (\VirtualInterface' {amazonSideAsn} -> amazonSideAsn) (\s@VirtualInterface' {} a -> s {amazonSideAsn = a} :: VirtualInterface)

-- | The type of virtual interface. The possible values are @private@ and
-- @public@.
virtualInterface_virtualInterfaceType :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_virtualInterfaceType = Lens.lens (\VirtualInterface' {virtualInterfaceType} -> virtualInterfaceType) (\s@VirtualInterface' {} a -> s {virtualInterfaceType = a} :: VirtualInterface)

-- | The autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration.
--
-- The valid values are 1-2147483647.
virtualInterface_asn :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Int)
virtualInterface_asn = Lens.lens (\VirtualInterface' {asn} -> asn) (\s@VirtualInterface' {} a -> s {asn = a} :: VirtualInterface)

-- | The authentication key for BGP configuration. This string has a minimum
-- length of 6 characters and and a maximun lenth of 80 characters.
virtualInterface_authKey :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_authKey = Lens.lens (\VirtualInterface' {authKey} -> authKey) (\s@VirtualInterface' {} a -> s {authKey = a} :: VirtualInterface)

-- | Indicates whether jumbo frames (9001 MTU) are supported.
virtualInterface_jumboFrameCapable :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Bool)
virtualInterface_jumboFrameCapable = Lens.lens (\VirtualInterface' {jumboFrameCapable} -> jumboFrameCapable) (\s@VirtualInterface' {} a -> s {jumboFrameCapable = a} :: VirtualInterface)

-- | The customer router configuration.
virtualInterface_customerRouterConfig :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_customerRouterConfig = Lens.lens (\VirtualInterface' {customerRouterConfig} -> customerRouterConfig) (\s@VirtualInterface' {} a -> s {customerRouterConfig = a} :: VirtualInterface)

-- | The ID of the Amazon Web Services account that owns the virtual
-- interface.
virtualInterface_ownerAccount :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_ownerAccount = Lens.lens (\VirtualInterface' {ownerAccount} -> ownerAccount) (\s@VirtualInterface' {} a -> s {ownerAccount = a} :: VirtualInterface)

-- | The Amazon Web Services Region where the virtual interface is located.
virtualInterface_region :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_region = Lens.lens (\VirtualInterface' {region} -> region) (\s@VirtualInterface' {} a -> s {region = a} :: VirtualInterface)

-- | The name of the virtual interface assigned by the customer network. The
-- name has a maximum of 100 characters. The following are valid
-- characters: a-z, 0-9 and a hyphen (-).
virtualInterface_virtualInterfaceName :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_virtualInterfaceName = Lens.lens (\VirtualInterface' {virtualInterfaceName} -> virtualInterfaceName) (\s@VirtualInterface' {} a -> s {virtualInterfaceName = a} :: VirtualInterface)

-- | The Direct Connect endpoint that terminates the physical connection.
virtualInterface_awsDeviceV2 :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_awsDeviceV2 = Lens.lens (\VirtualInterface' {awsDeviceV2} -> awsDeviceV2) (\s@VirtualInterface' {} a -> s {awsDeviceV2 = a} :: VirtualInterface)

-- | The ID of the virtual interface.
virtualInterface_virtualInterfaceId :: Lens.Lens' VirtualInterface (Prelude.Maybe Prelude.Text)
virtualInterface_virtualInterfaceId = Lens.lens (\VirtualInterface' {virtualInterfaceId} -> virtualInterfaceId) (\s@VirtualInterface' {} a -> s {virtualInterfaceId = a} :: VirtualInterface)

-- | The tags associated with the virtual interface.
virtualInterface_tags :: Lens.Lens' VirtualInterface (Prelude.Maybe (Prelude.NonEmpty Tag))
virtualInterface_tags = Lens.lens (\VirtualInterface' {tags} -> tags) (\s@VirtualInterface' {} a -> s {tags = a} :: VirtualInterface) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON VirtualInterface where
  parseJSON =
    Core.withObject
      "VirtualInterface"
      ( \x ->
          VirtualInterface'
            Prelude.<$> (x Core..:? "bgpPeers" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "virtualGatewayId")
            Prelude.<*> (x Core..:? "mtu")
            Prelude.<*> ( x Core..:? "routeFilterPrefixes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "customerAddress")
            Prelude.<*> (x Core..:? "vlan")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "amazonAddress")
            Prelude.<*> (x Core..:? "addressFamily")
            Prelude.<*> (x Core..:? "virtualInterfaceState")
            Prelude.<*> (x Core..:? "connectionId")
            Prelude.<*> (x Core..:? "awsLogicalDeviceId")
            Prelude.<*> (x Core..:? "directConnectGatewayId")
            Prelude.<*> (x Core..:? "amazonSideAsn")
            Prelude.<*> (x Core..:? "virtualInterfaceType")
            Prelude.<*> (x Core..:? "asn")
            Prelude.<*> (x Core..:? "authKey")
            Prelude.<*> (x Core..:? "jumboFrameCapable")
            Prelude.<*> (x Core..:? "customerRouterConfig")
            Prelude.<*> (x Core..:? "ownerAccount")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "virtualInterfaceName")
            Prelude.<*> (x Core..:? "awsDeviceV2")
            Prelude.<*> (x Core..:? "virtualInterfaceId")
            Prelude.<*> (x Core..:? "tags")
      )

instance Prelude.Hashable VirtualInterface where
  hashWithSalt salt' VirtualInterface' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` virtualInterfaceId
      `Prelude.hashWithSalt` awsDeviceV2
      `Prelude.hashWithSalt` virtualInterfaceName
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` customerRouterConfig
      `Prelude.hashWithSalt` jumboFrameCapable
      `Prelude.hashWithSalt` authKey
      `Prelude.hashWithSalt` asn
      `Prelude.hashWithSalt` virtualInterfaceType
      `Prelude.hashWithSalt` amazonSideAsn
      `Prelude.hashWithSalt` directConnectGatewayId
      `Prelude.hashWithSalt` awsLogicalDeviceId
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` virtualInterfaceState
      `Prelude.hashWithSalt` addressFamily
      `Prelude.hashWithSalt` amazonAddress
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` vlan
      `Prelude.hashWithSalt` customerAddress
      `Prelude.hashWithSalt` routeFilterPrefixes
      `Prelude.hashWithSalt` mtu
      `Prelude.hashWithSalt` virtualGatewayId
      `Prelude.hashWithSalt` bgpPeers

instance Prelude.NFData VirtualInterface where
  rnf VirtualInterface' {..} =
    Prelude.rnf bgpPeers `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf virtualInterfaceId
      `Prelude.seq` Prelude.rnf awsDeviceV2
      `Prelude.seq` Prelude.rnf virtualInterfaceName
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf customerRouterConfig
      `Prelude.seq` Prelude.rnf jumboFrameCapable
      `Prelude.seq` Prelude.rnf authKey
      `Prelude.seq` Prelude.rnf asn
      `Prelude.seq` Prelude.rnf virtualInterfaceType
      `Prelude.seq` Prelude.rnf amazonSideAsn
      `Prelude.seq` Prelude.rnf directConnectGatewayId
      `Prelude.seq` Prelude.rnf awsLogicalDeviceId
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf virtualInterfaceState
      `Prelude.seq` Prelude.rnf addressFamily
      `Prelude.seq` Prelude.rnf amazonAddress
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf vlan
      `Prelude.seq` Prelude.rnf customerAddress
      `Prelude.seq` Prelude.rnf routeFilterPrefixes
      `Prelude.seq` Prelude.rnf mtu
      `Prelude.seq` Prelude.rnf virtualGatewayId
