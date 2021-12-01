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
-- Module      : Amazonka.GroundStation.Types.SocketAddress
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.SocketAddress where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the socket address.
--
-- /See:/ 'newSocketAddress' smart constructor.
data SocketAddress = SocketAddress'
  { -- | Name of a socket address.
    name :: Prelude.Text,
    -- | Port of a socket address.
    port :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SocketAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'socketAddress_name' - Name of a socket address.
--
-- 'port', 'socketAddress_port' - Port of a socket address.
newSocketAddress ::
  -- | 'name'
  Prelude.Text ->
  -- | 'port'
  Prelude.Int ->
  SocketAddress
newSocketAddress pName_ pPort_ =
  SocketAddress' {name = pName_, port = pPort_}

-- | Name of a socket address.
socketAddress_name :: Lens.Lens' SocketAddress Prelude.Text
socketAddress_name = Lens.lens (\SocketAddress' {name} -> name) (\s@SocketAddress' {} a -> s {name = a} :: SocketAddress)

-- | Port of a socket address.
socketAddress_port :: Lens.Lens' SocketAddress Prelude.Int
socketAddress_port = Lens.lens (\SocketAddress' {port} -> port) (\s@SocketAddress' {} a -> s {port = a} :: SocketAddress)

instance Core.FromJSON SocketAddress where
  parseJSON =
    Core.withObject
      "SocketAddress"
      ( \x ->
          SocketAddress'
            Prelude.<$> (x Core..: "name") Prelude.<*> (x Core..: "port")
      )

instance Prelude.Hashable SocketAddress where
  hashWithSalt salt' SocketAddress' {..} =
    salt' `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` name

instance Prelude.NFData SocketAddress where
  rnf SocketAddress' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf port

instance Core.ToJSON SocketAddress where
  toJSON SocketAddress' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Core..= name),
            Prelude.Just ("port" Core..= port)
          ]
      )
