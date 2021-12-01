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
-- Module      : Amazonka.AppMesh.Types.PortMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.PortMapping where

import Amazonka.AppMesh.Types.PortProtocol
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a port mapping.
--
-- /See:/ 'newPortMapping' smart constructor.
data PortMapping = PortMapping'
  { -- | The port used for the port mapping.
    port :: Prelude.Natural,
    -- | The protocol used for the port mapping. Specify one protocol.
    protocol :: PortProtocol
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'port', 'portMapping_port' - The port used for the port mapping.
--
-- 'protocol', 'portMapping_protocol' - The protocol used for the port mapping. Specify one protocol.
newPortMapping ::
  -- | 'port'
  Prelude.Natural ->
  -- | 'protocol'
  PortProtocol ->
  PortMapping
newPortMapping pPort_ pProtocol_ =
  PortMapping' {port = pPort_, protocol = pProtocol_}

-- | The port used for the port mapping.
portMapping_port :: Lens.Lens' PortMapping Prelude.Natural
portMapping_port = Lens.lens (\PortMapping' {port} -> port) (\s@PortMapping' {} a -> s {port = a} :: PortMapping)

-- | The protocol used for the port mapping. Specify one protocol.
portMapping_protocol :: Lens.Lens' PortMapping PortProtocol
portMapping_protocol = Lens.lens (\PortMapping' {protocol} -> protocol) (\s@PortMapping' {} a -> s {protocol = a} :: PortMapping)

instance Core.FromJSON PortMapping where
  parseJSON =
    Core.withObject
      "PortMapping"
      ( \x ->
          PortMapping'
            Prelude.<$> (x Core..: "port")
            Prelude.<*> (x Core..: "protocol")
      )

instance Prelude.Hashable PortMapping where
  hashWithSalt salt' PortMapping' {..} =
    salt' `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` port

instance Prelude.NFData PortMapping where
  rnf PortMapping' {..} =
    Prelude.rnf port `Prelude.seq` Prelude.rnf protocol

instance Core.ToJSON PortMapping where
  toJSON PortMapping' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("port" Core..= port),
            Prelude.Just ("protocol" Core..= protocol)
          ]
      )
