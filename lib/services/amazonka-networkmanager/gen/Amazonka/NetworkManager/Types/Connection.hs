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
-- Module      : Amazonka.NetworkManager.Types.Connection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkManager.Types.Connection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.NetworkManager.Types.ConnectionState
import Amazonka.NetworkManager.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a connection.
--
-- /See:/ 'newConnection' smart constructor.
data Connection = Connection'
  { -- | The state of the connection.
    state :: Prelude.Maybe ConnectionState,
    -- | The date and time that the connection was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The ID of the global network.
    globalNetworkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the connection.
    connectionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the second device in the connection.
    connectedDeviceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the link for the second device in the connection.
    connectedLinkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the link for the first device in the connection.
    linkId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the first device in the connection.
    deviceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the connection.
    connectionArn :: Prelude.Maybe Prelude.Text,
    -- | The description of the connection.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags for the connection.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Connection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'state', 'connection_state' - The state of the connection.
--
-- 'createdAt', 'connection_createdAt' - The date and time that the connection was created.
--
-- 'globalNetworkId', 'connection_globalNetworkId' - The ID of the global network.
--
-- 'connectionId', 'connection_connectionId' - The ID of the connection.
--
-- 'connectedDeviceId', 'connection_connectedDeviceId' - The ID of the second device in the connection.
--
-- 'connectedLinkId', 'connection_connectedLinkId' - The ID of the link for the second device in the connection.
--
-- 'linkId', 'connection_linkId' - The ID of the link for the first device in the connection.
--
-- 'deviceId', 'connection_deviceId' - The ID of the first device in the connection.
--
-- 'connectionArn', 'connection_connectionArn' - The Amazon Resource Name (ARN) of the connection.
--
-- 'description', 'connection_description' - The description of the connection.
--
-- 'tags', 'connection_tags' - The tags for the connection.
newConnection ::
  Connection
newConnection =
  Connection'
    { state = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      globalNetworkId = Prelude.Nothing,
      connectionId = Prelude.Nothing,
      connectedDeviceId = Prelude.Nothing,
      connectedLinkId = Prelude.Nothing,
      linkId = Prelude.Nothing,
      deviceId = Prelude.Nothing,
      connectionArn = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The state of the connection.
connection_state :: Lens.Lens' Connection (Prelude.Maybe ConnectionState)
connection_state = Lens.lens (\Connection' {state} -> state) (\s@Connection' {} a -> s {state = a} :: Connection)

-- | The date and time that the connection was created.
connection_createdAt :: Lens.Lens' Connection (Prelude.Maybe Prelude.UTCTime)
connection_createdAt = Lens.lens (\Connection' {createdAt} -> createdAt) (\s@Connection' {} a -> s {createdAt = a} :: Connection) Prelude.. Lens.mapping Core._Time

-- | The ID of the global network.
connection_globalNetworkId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_globalNetworkId = Lens.lens (\Connection' {globalNetworkId} -> globalNetworkId) (\s@Connection' {} a -> s {globalNetworkId = a} :: Connection)

-- | The ID of the connection.
connection_connectionId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionId = Lens.lens (\Connection' {connectionId} -> connectionId) (\s@Connection' {} a -> s {connectionId = a} :: Connection)

-- | The ID of the second device in the connection.
connection_connectedDeviceId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectedDeviceId = Lens.lens (\Connection' {connectedDeviceId} -> connectedDeviceId) (\s@Connection' {} a -> s {connectedDeviceId = a} :: Connection)

-- | The ID of the link for the second device in the connection.
connection_connectedLinkId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectedLinkId = Lens.lens (\Connection' {connectedLinkId} -> connectedLinkId) (\s@Connection' {} a -> s {connectedLinkId = a} :: Connection)

-- | The ID of the link for the first device in the connection.
connection_linkId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_linkId = Lens.lens (\Connection' {linkId} -> linkId) (\s@Connection' {} a -> s {linkId = a} :: Connection)

-- | The ID of the first device in the connection.
connection_deviceId :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_deviceId = Lens.lens (\Connection' {deviceId} -> deviceId) (\s@Connection' {} a -> s {deviceId = a} :: Connection)

-- | The Amazon Resource Name (ARN) of the connection.
connection_connectionArn :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_connectionArn = Lens.lens (\Connection' {connectionArn} -> connectionArn) (\s@Connection' {} a -> s {connectionArn = a} :: Connection)

-- | The description of the connection.
connection_description :: Lens.Lens' Connection (Prelude.Maybe Prelude.Text)
connection_description = Lens.lens (\Connection' {description} -> description) (\s@Connection' {} a -> s {description = a} :: Connection)

-- | The tags for the connection.
connection_tags :: Lens.Lens' Connection (Prelude.Maybe [Tag])
connection_tags = Lens.lens (\Connection' {tags} -> tags) (\s@Connection' {} a -> s {tags = a} :: Connection) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON Connection where
  parseJSON =
    Core.withObject
      "Connection"
      ( \x ->
          Connection'
            Prelude.<$> (x Core..:? "State")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "GlobalNetworkId")
            Prelude.<*> (x Core..:? "ConnectionId")
            Prelude.<*> (x Core..:? "ConnectedDeviceId")
            Prelude.<*> (x Core..:? "ConnectedLinkId")
            Prelude.<*> (x Core..:? "LinkId")
            Prelude.<*> (x Core..:? "DeviceId")
            Prelude.<*> (x Core..:? "ConnectionArn")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable Connection where
  hashWithSalt salt' Connection' {..} =
    salt' `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` deviceId
      `Prelude.hashWithSalt` linkId
      `Prelude.hashWithSalt` connectedLinkId
      `Prelude.hashWithSalt` connectedDeviceId
      `Prelude.hashWithSalt` connectionId
      `Prelude.hashWithSalt` globalNetworkId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` state

instance Prelude.NFData Connection where
  rnf Connection' {..} =
    Prelude.rnf state `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf deviceId
      `Prelude.seq` Prelude.rnf linkId
      `Prelude.seq` Prelude.rnf connectedLinkId
      `Prelude.seq` Prelude.rnf connectedDeviceId
      `Prelude.seq` Prelude.rnf connectionId
      `Prelude.seq` Prelude.rnf globalNetworkId
      `Prelude.seq` Prelude.rnf createdAt
