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
-- Module      : Amazonka.AppMesh.Types.VirtualGatewayListener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualGatewayListener where

import Amazonka.AppMesh.Types.VirtualGatewayConnectionPool
import Amazonka.AppMesh.Types.VirtualGatewayHealthCheckPolicy
import Amazonka.AppMesh.Types.VirtualGatewayListenerTls
import Amazonka.AppMesh.Types.VirtualGatewayPortMapping
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a listener for a virtual gateway.
--
-- /See:/ 'newVirtualGatewayListener' smart constructor.
data VirtualGatewayListener = VirtualGatewayListener'
  { -- | The health check information for the listener.
    healthCheck :: Prelude.Maybe VirtualGatewayHealthCheckPolicy,
    -- | The connection pool information for the virtual gateway listener.
    connectionPool :: Prelude.Maybe VirtualGatewayConnectionPool,
    -- | A reference to an object that represents the Transport Layer Security
    -- (TLS) properties for the listener.
    tls :: Prelude.Maybe VirtualGatewayListenerTls,
    -- | The port mapping information for the listener.
    portMapping :: VirtualGatewayPortMapping
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualGatewayListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'healthCheck', 'virtualGatewayListener_healthCheck' - The health check information for the listener.
--
-- 'connectionPool', 'virtualGatewayListener_connectionPool' - The connection pool information for the virtual gateway listener.
--
-- 'tls', 'virtualGatewayListener_tls' - A reference to an object that represents the Transport Layer Security
-- (TLS) properties for the listener.
--
-- 'portMapping', 'virtualGatewayListener_portMapping' - The port mapping information for the listener.
newVirtualGatewayListener ::
  -- | 'portMapping'
  VirtualGatewayPortMapping ->
  VirtualGatewayListener
newVirtualGatewayListener pPortMapping_ =
  VirtualGatewayListener'
    { healthCheck =
        Prelude.Nothing,
      connectionPool = Prelude.Nothing,
      tls = Prelude.Nothing,
      portMapping = pPortMapping_
    }

-- | The health check information for the listener.
virtualGatewayListener_healthCheck :: Lens.Lens' VirtualGatewayListener (Prelude.Maybe VirtualGatewayHealthCheckPolicy)
virtualGatewayListener_healthCheck = Lens.lens (\VirtualGatewayListener' {healthCheck} -> healthCheck) (\s@VirtualGatewayListener' {} a -> s {healthCheck = a} :: VirtualGatewayListener)

-- | The connection pool information for the virtual gateway listener.
virtualGatewayListener_connectionPool :: Lens.Lens' VirtualGatewayListener (Prelude.Maybe VirtualGatewayConnectionPool)
virtualGatewayListener_connectionPool = Lens.lens (\VirtualGatewayListener' {connectionPool} -> connectionPool) (\s@VirtualGatewayListener' {} a -> s {connectionPool = a} :: VirtualGatewayListener)

-- | A reference to an object that represents the Transport Layer Security
-- (TLS) properties for the listener.
virtualGatewayListener_tls :: Lens.Lens' VirtualGatewayListener (Prelude.Maybe VirtualGatewayListenerTls)
virtualGatewayListener_tls = Lens.lens (\VirtualGatewayListener' {tls} -> tls) (\s@VirtualGatewayListener' {} a -> s {tls = a} :: VirtualGatewayListener)

-- | The port mapping information for the listener.
virtualGatewayListener_portMapping :: Lens.Lens' VirtualGatewayListener VirtualGatewayPortMapping
virtualGatewayListener_portMapping = Lens.lens (\VirtualGatewayListener' {portMapping} -> portMapping) (\s@VirtualGatewayListener' {} a -> s {portMapping = a} :: VirtualGatewayListener)

instance Core.FromJSON VirtualGatewayListener where
  parseJSON =
    Core.withObject
      "VirtualGatewayListener"
      ( \x ->
          VirtualGatewayListener'
            Prelude.<$> (x Core..:? "healthCheck")
            Prelude.<*> (x Core..:? "connectionPool")
            Prelude.<*> (x Core..:? "tls")
            Prelude.<*> (x Core..: "portMapping")
      )

instance Prelude.Hashable VirtualGatewayListener where
  hashWithSalt salt' VirtualGatewayListener' {..} =
    salt' `Prelude.hashWithSalt` portMapping
      `Prelude.hashWithSalt` tls
      `Prelude.hashWithSalt` connectionPool
      `Prelude.hashWithSalt` healthCheck

instance Prelude.NFData VirtualGatewayListener where
  rnf VirtualGatewayListener' {..} =
    Prelude.rnf healthCheck
      `Prelude.seq` Prelude.rnf portMapping
      `Prelude.seq` Prelude.rnf tls
      `Prelude.seq` Prelude.rnf connectionPool

instance Core.ToJSON VirtualGatewayListener where
  toJSON VirtualGatewayListener' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("healthCheck" Core..=) Prelude.<$> healthCheck,
            ("connectionPool" Core..=)
              Prelude.<$> connectionPool,
            ("tls" Core..=) Prelude.<$> tls,
            Prelude.Just ("portMapping" Core..= portMapping)
          ]
      )
