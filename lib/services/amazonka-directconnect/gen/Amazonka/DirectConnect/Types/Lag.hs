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
-- Module      : Amazonka.DirectConnect.Types.Lag
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectConnect.Types.Lag where

import qualified Amazonka.Core as Core
import Amazonka.DirectConnect.Types.Connection
import Amazonka.DirectConnect.Types.HasLogicalRedundancy
import Amazonka.DirectConnect.Types.LagState
import Amazonka.DirectConnect.Types.MacSecKey
import Amazonka.DirectConnect.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a link aggregation group (LAG).
--
-- /See:/ 'newLag' smart constructor.
data Lag = Lag'
  { -- | The ID of the LAG.
    lagId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the LAG supports MAC Security (MACsec).
    macSecCapable :: Prelude.Maybe Prelude.Bool,
    -- | The individual bandwidth of the physical connections bundled by the LAG.
    -- The possible values are 1Gbps and 10Gbps.
    connectionsBandwidth :: Prelude.Maybe Prelude.Text,
    -- | The minimum number of physical dedicated connections that must be
    -- operational for the LAG itself to be operational.
    minimumLinks :: Prelude.Maybe Prelude.Int,
    -- | The name of the LAG.
    lagName :: Prelude.Maybe Prelude.Text,
    -- | The location of the LAG.
    location :: Prelude.Maybe Prelude.Text,
    -- | The connections bundled by the LAG.
    connections :: Prelude.Maybe [Connection],
    -- | The Direct Connect endpoint that hosts the LAG.
    awsDevice :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the LAG supports a secondary BGP peer in the same
    -- address family (IPv4\/IPv6).
    hasLogicalRedundancy :: Prelude.Maybe HasLogicalRedundancy,
    -- | The Direct Connect endpoint that terminates the logical connection. This
    -- device might be different than the device that terminates the physical
    -- connection.
    awsLogicalDeviceId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the LAG can host other connections.
    allowsHostedConnections :: Prelude.Maybe Prelude.Bool,
    -- | The LAG MAC Security (MACsec) encryption mode.
    --
    -- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
    encryptionMode :: Prelude.Maybe Prelude.Text,
    -- | The number of physical dedicated connections bundled by the LAG, up to a
    -- maximum of 10.
    numberOfConnections :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether jumbo frames (9001 MTU) are supported.
    jumboFrameCapable :: Prelude.Maybe Prelude.Bool,
    -- | The state of the LAG. The following are the possible values:
    --
    -- -   @requested@: The initial state of a LAG. The LAG stays in the
    --     requested state until the Letter of Authorization (LOA) is
    --     available.
    --
    -- -   @pending@: The LAG has been approved and is being initialized.
    --
    -- -   @available@: The network link is established and the LAG is ready
    --     for use.
    --
    -- -   @down@: The network link is down.
    --
    -- -   @deleting@: The LAG is being deleted.
    --
    -- -   @deleted@: The LAG is deleted.
    --
    -- -   @unknown@: The state of the LAG is not available.
    lagState :: Prelude.Maybe LagState,
    -- | The ID of the Amazon Web Services account that owns the LAG.
    ownerAccount :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services Region where the connection is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The MAC Security (MACsec) security keys associated with the LAG.
    macSecKeys :: Prelude.Maybe [MacSecKey],
    -- | The name of the service provider associated with the LAG.
    providerName :: Prelude.Maybe Prelude.Text,
    -- | The Direct Connect endpoint that hosts the LAG.
    awsDeviceV2 :: Prelude.Maybe Prelude.Text,
    -- | The tags associated with the LAG.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Lag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lagId', 'lag_lagId' - The ID of the LAG.
--
-- 'macSecCapable', 'lag_macSecCapable' - Indicates whether the LAG supports MAC Security (MACsec).
--
-- 'connectionsBandwidth', 'lag_connectionsBandwidth' - The individual bandwidth of the physical connections bundled by the LAG.
-- The possible values are 1Gbps and 10Gbps.
--
-- 'minimumLinks', 'lag_minimumLinks' - The minimum number of physical dedicated connections that must be
-- operational for the LAG itself to be operational.
--
-- 'lagName', 'lag_lagName' - The name of the LAG.
--
-- 'location', 'lag_location' - The location of the LAG.
--
-- 'connections', 'lag_connections' - The connections bundled by the LAG.
--
-- 'awsDevice', 'lag_awsDevice' - The Direct Connect endpoint that hosts the LAG.
--
-- 'hasLogicalRedundancy', 'lag_hasLogicalRedundancy' - Indicates whether the LAG supports a secondary BGP peer in the same
-- address family (IPv4\/IPv6).
--
-- 'awsLogicalDeviceId', 'lag_awsLogicalDeviceId' - The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
--
-- 'allowsHostedConnections', 'lag_allowsHostedConnections' - Indicates whether the LAG can host other connections.
--
-- 'encryptionMode', 'lag_encryptionMode' - The LAG MAC Security (MACsec) encryption mode.
--
-- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
--
-- 'numberOfConnections', 'lag_numberOfConnections' - The number of physical dedicated connections bundled by the LAG, up to a
-- maximum of 10.
--
-- 'jumboFrameCapable', 'lag_jumboFrameCapable' - Indicates whether jumbo frames (9001 MTU) are supported.
--
-- 'lagState', 'lag_lagState' - The state of the LAG. The following are the possible values:
--
-- -   @requested@: The initial state of a LAG. The LAG stays in the
--     requested state until the Letter of Authorization (LOA) is
--     available.
--
-- -   @pending@: The LAG has been approved and is being initialized.
--
-- -   @available@: The network link is established and the LAG is ready
--     for use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The LAG is being deleted.
--
-- -   @deleted@: The LAG is deleted.
--
-- -   @unknown@: The state of the LAG is not available.
--
-- 'ownerAccount', 'lag_ownerAccount' - The ID of the Amazon Web Services account that owns the LAG.
--
-- 'region', 'lag_region' - The Amazon Web Services Region where the connection is located.
--
-- 'macSecKeys', 'lag_macSecKeys' - The MAC Security (MACsec) security keys associated with the LAG.
--
-- 'providerName', 'lag_providerName' - The name of the service provider associated with the LAG.
--
-- 'awsDeviceV2', 'lag_awsDeviceV2' - The Direct Connect endpoint that hosts the LAG.
--
-- 'tags', 'lag_tags' - The tags associated with the LAG.
newLag ::
  Lag
newLag =
  Lag'
    { lagId = Prelude.Nothing,
      macSecCapable = Prelude.Nothing,
      connectionsBandwidth = Prelude.Nothing,
      minimumLinks = Prelude.Nothing,
      lagName = Prelude.Nothing,
      location = Prelude.Nothing,
      connections = Prelude.Nothing,
      awsDevice = Prelude.Nothing,
      hasLogicalRedundancy = Prelude.Nothing,
      awsLogicalDeviceId = Prelude.Nothing,
      allowsHostedConnections = Prelude.Nothing,
      encryptionMode = Prelude.Nothing,
      numberOfConnections = Prelude.Nothing,
      jumboFrameCapable = Prelude.Nothing,
      lagState = Prelude.Nothing,
      ownerAccount = Prelude.Nothing,
      region = Prelude.Nothing,
      macSecKeys = Prelude.Nothing,
      providerName = Prelude.Nothing,
      awsDeviceV2 = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The ID of the LAG.
lag_lagId :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_lagId = Lens.lens (\Lag' {lagId} -> lagId) (\s@Lag' {} a -> s {lagId = a} :: Lag)

-- | Indicates whether the LAG supports MAC Security (MACsec).
lag_macSecCapable :: Lens.Lens' Lag (Prelude.Maybe Prelude.Bool)
lag_macSecCapable = Lens.lens (\Lag' {macSecCapable} -> macSecCapable) (\s@Lag' {} a -> s {macSecCapable = a} :: Lag)

-- | The individual bandwidth of the physical connections bundled by the LAG.
-- The possible values are 1Gbps and 10Gbps.
lag_connectionsBandwidth :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_connectionsBandwidth = Lens.lens (\Lag' {connectionsBandwidth} -> connectionsBandwidth) (\s@Lag' {} a -> s {connectionsBandwidth = a} :: Lag)

-- | The minimum number of physical dedicated connections that must be
-- operational for the LAG itself to be operational.
lag_minimumLinks :: Lens.Lens' Lag (Prelude.Maybe Prelude.Int)
lag_minimumLinks = Lens.lens (\Lag' {minimumLinks} -> minimumLinks) (\s@Lag' {} a -> s {minimumLinks = a} :: Lag)

-- | The name of the LAG.
lag_lagName :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_lagName = Lens.lens (\Lag' {lagName} -> lagName) (\s@Lag' {} a -> s {lagName = a} :: Lag)

-- | The location of the LAG.
lag_location :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_location = Lens.lens (\Lag' {location} -> location) (\s@Lag' {} a -> s {location = a} :: Lag)

-- | The connections bundled by the LAG.
lag_connections :: Lens.Lens' Lag (Prelude.Maybe [Connection])
lag_connections = Lens.lens (\Lag' {connections} -> connections) (\s@Lag' {} a -> s {connections = a} :: Lag) Prelude.. Lens.mapping Lens.coerced

-- | The Direct Connect endpoint that hosts the LAG.
lag_awsDevice :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_awsDevice = Lens.lens (\Lag' {awsDevice} -> awsDevice) (\s@Lag' {} a -> s {awsDevice = a} :: Lag)

-- | Indicates whether the LAG supports a secondary BGP peer in the same
-- address family (IPv4\/IPv6).
lag_hasLogicalRedundancy :: Lens.Lens' Lag (Prelude.Maybe HasLogicalRedundancy)
lag_hasLogicalRedundancy = Lens.lens (\Lag' {hasLogicalRedundancy} -> hasLogicalRedundancy) (\s@Lag' {} a -> s {hasLogicalRedundancy = a} :: Lag)

-- | The Direct Connect endpoint that terminates the logical connection. This
-- device might be different than the device that terminates the physical
-- connection.
lag_awsLogicalDeviceId :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_awsLogicalDeviceId = Lens.lens (\Lag' {awsLogicalDeviceId} -> awsLogicalDeviceId) (\s@Lag' {} a -> s {awsLogicalDeviceId = a} :: Lag)

-- | Indicates whether the LAG can host other connections.
lag_allowsHostedConnections :: Lens.Lens' Lag (Prelude.Maybe Prelude.Bool)
lag_allowsHostedConnections = Lens.lens (\Lag' {allowsHostedConnections} -> allowsHostedConnections) (\s@Lag' {} a -> s {allowsHostedConnections = a} :: Lag)

-- | The LAG MAC Security (MACsec) encryption mode.
--
-- The valid values are @no_encrypt@, @should_encrypt@, and @must_encrypt@.
lag_encryptionMode :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_encryptionMode = Lens.lens (\Lag' {encryptionMode} -> encryptionMode) (\s@Lag' {} a -> s {encryptionMode = a} :: Lag)

-- | The number of physical dedicated connections bundled by the LAG, up to a
-- maximum of 10.
lag_numberOfConnections :: Lens.Lens' Lag (Prelude.Maybe Prelude.Int)
lag_numberOfConnections = Lens.lens (\Lag' {numberOfConnections} -> numberOfConnections) (\s@Lag' {} a -> s {numberOfConnections = a} :: Lag)

-- | Indicates whether jumbo frames (9001 MTU) are supported.
lag_jumboFrameCapable :: Lens.Lens' Lag (Prelude.Maybe Prelude.Bool)
lag_jumboFrameCapable = Lens.lens (\Lag' {jumboFrameCapable} -> jumboFrameCapable) (\s@Lag' {} a -> s {jumboFrameCapable = a} :: Lag)

-- | The state of the LAG. The following are the possible values:
--
-- -   @requested@: The initial state of a LAG. The LAG stays in the
--     requested state until the Letter of Authorization (LOA) is
--     available.
--
-- -   @pending@: The LAG has been approved and is being initialized.
--
-- -   @available@: The network link is established and the LAG is ready
--     for use.
--
-- -   @down@: The network link is down.
--
-- -   @deleting@: The LAG is being deleted.
--
-- -   @deleted@: The LAG is deleted.
--
-- -   @unknown@: The state of the LAG is not available.
lag_lagState :: Lens.Lens' Lag (Prelude.Maybe LagState)
lag_lagState = Lens.lens (\Lag' {lagState} -> lagState) (\s@Lag' {} a -> s {lagState = a} :: Lag)

-- | The ID of the Amazon Web Services account that owns the LAG.
lag_ownerAccount :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_ownerAccount = Lens.lens (\Lag' {ownerAccount} -> ownerAccount) (\s@Lag' {} a -> s {ownerAccount = a} :: Lag)

-- | The Amazon Web Services Region where the connection is located.
lag_region :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_region = Lens.lens (\Lag' {region} -> region) (\s@Lag' {} a -> s {region = a} :: Lag)

-- | The MAC Security (MACsec) security keys associated with the LAG.
lag_macSecKeys :: Lens.Lens' Lag (Prelude.Maybe [MacSecKey])
lag_macSecKeys = Lens.lens (\Lag' {macSecKeys} -> macSecKeys) (\s@Lag' {} a -> s {macSecKeys = a} :: Lag) Prelude.. Lens.mapping Lens.coerced

-- | The name of the service provider associated with the LAG.
lag_providerName :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_providerName = Lens.lens (\Lag' {providerName} -> providerName) (\s@Lag' {} a -> s {providerName = a} :: Lag)

-- | The Direct Connect endpoint that hosts the LAG.
lag_awsDeviceV2 :: Lens.Lens' Lag (Prelude.Maybe Prelude.Text)
lag_awsDeviceV2 = Lens.lens (\Lag' {awsDeviceV2} -> awsDeviceV2) (\s@Lag' {} a -> s {awsDeviceV2 = a} :: Lag)

-- | The tags associated with the LAG.
lag_tags :: Lens.Lens' Lag (Prelude.Maybe (Prelude.NonEmpty Tag))
lag_tags = Lens.lens (\Lag' {tags} -> tags) (\s@Lag' {} a -> s {tags = a} :: Lag) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Lag where
  parseJSON =
    Core.withObject
      "Lag"
      ( \x ->
          Lag'
            Prelude.<$> (x Core..:? "lagId")
            Prelude.<*> (x Core..:? "macSecCapable")
            Prelude.<*> (x Core..:? "connectionsBandwidth")
            Prelude.<*> (x Core..:? "minimumLinks")
            Prelude.<*> (x Core..:? "lagName")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "connections" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "awsDevice")
            Prelude.<*> (x Core..:? "hasLogicalRedundancy")
            Prelude.<*> (x Core..:? "awsLogicalDeviceId")
            Prelude.<*> (x Core..:? "allowsHostedConnections")
            Prelude.<*> (x Core..:? "encryptionMode")
            Prelude.<*> (x Core..:? "numberOfConnections")
            Prelude.<*> (x Core..:? "jumboFrameCapable")
            Prelude.<*> (x Core..:? "lagState")
            Prelude.<*> (x Core..:? "ownerAccount")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "macSecKeys" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "providerName")
            Prelude.<*> (x Core..:? "awsDeviceV2")
            Prelude.<*> (x Core..:? "tags")
      )

instance Prelude.Hashable Lag where
  hashWithSalt salt' Lag' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` awsDeviceV2
      `Prelude.hashWithSalt` providerName
      `Prelude.hashWithSalt` macSecKeys
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` ownerAccount
      `Prelude.hashWithSalt` lagState
      `Prelude.hashWithSalt` jumboFrameCapable
      `Prelude.hashWithSalt` numberOfConnections
      `Prelude.hashWithSalt` encryptionMode
      `Prelude.hashWithSalt` allowsHostedConnections
      `Prelude.hashWithSalt` awsLogicalDeviceId
      `Prelude.hashWithSalt` hasLogicalRedundancy
      `Prelude.hashWithSalt` awsDevice
      `Prelude.hashWithSalt` connections
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` lagName
      `Prelude.hashWithSalt` minimumLinks
      `Prelude.hashWithSalt` connectionsBandwidth
      `Prelude.hashWithSalt` macSecCapable
      `Prelude.hashWithSalt` lagId

instance Prelude.NFData Lag where
  rnf Lag' {..} =
    Prelude.rnf lagId `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf awsDeviceV2
      `Prelude.seq` Prelude.rnf providerName
      `Prelude.seq` Prelude.rnf macSecKeys
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf ownerAccount
      `Prelude.seq` Prelude.rnf lagState
      `Prelude.seq` Prelude.rnf jumboFrameCapable
      `Prelude.seq` Prelude.rnf numberOfConnections
      `Prelude.seq` Prelude.rnf encryptionMode
      `Prelude.seq` Prelude.rnf allowsHostedConnections
      `Prelude.seq` Prelude.rnf awsLogicalDeviceId
      `Prelude.seq` Prelude.rnf hasLogicalRedundancy
      `Prelude.seq` Prelude.rnf awsDevice
      `Prelude.seq` Prelude.rnf connections
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf lagName
      `Prelude.seq` Prelude.rnf minimumLinks
      `Prelude.seq` Prelude.rnf connectionsBandwidth
      `Prelude.seq` Prelude.rnf macSecCapable
