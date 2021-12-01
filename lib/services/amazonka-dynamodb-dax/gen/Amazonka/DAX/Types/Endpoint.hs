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
-- Module      : Amazonka.DAX.Types.Endpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DAX.Types.Endpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the information required for client programs to connect to
-- the endpoint for a DAX cluster.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | The URL that applications should use to connect to the endpoint. The
    -- default ports are 8111 for the \"dax\" protocol and 9111 for the
    -- \"daxs\" protocol.
    url :: Prelude.Maybe Prelude.Text,
    -- | The DNS hostname of the endpoint.
    address :: Prelude.Maybe Prelude.Text,
    -- | The port number that applications should use to connect to the endpoint.
    port :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'endpoint_url' - The URL that applications should use to connect to the endpoint. The
-- default ports are 8111 for the \"dax\" protocol and 9111 for the
-- \"daxs\" protocol.
--
-- 'address', 'endpoint_address' - The DNS hostname of the endpoint.
--
-- 'port', 'endpoint_port' - The port number that applications should use to connect to the endpoint.
newEndpoint ::
  Endpoint
newEndpoint =
  Endpoint'
    { url = Prelude.Nothing,
      address = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | The URL that applications should use to connect to the endpoint. The
-- default ports are 8111 for the \"dax\" protocol and 9111 for the
-- \"daxs\" protocol.
endpoint_url :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_url = Lens.lens (\Endpoint' {url} -> url) (\s@Endpoint' {} a -> s {url = a} :: Endpoint)

-- | The DNS hostname of the endpoint.
endpoint_address :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_address = Lens.lens (\Endpoint' {address} -> address) (\s@Endpoint' {} a -> s {address = a} :: Endpoint)

-- | The port number that applications should use to connect to the endpoint.
endpoint_port :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Int)
endpoint_port = Lens.lens (\Endpoint' {port} -> port) (\s@Endpoint' {} a -> s {port = a} :: Endpoint)

instance Core.FromJSON Endpoint where
  parseJSON =
    Core.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Prelude.<$> (x Core..:? "URL")
            Prelude.<*> (x Core..:? "Address")
            Prelude.<*> (x Core..:? "Port")
      )

instance Prelude.Hashable Endpoint where
  hashWithSalt salt' Endpoint' {..} =
    salt' `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` address
      `Prelude.hashWithSalt` url

instance Prelude.NFData Endpoint where
  rnf Endpoint' {..} =
    Prelude.rnf url `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf address
