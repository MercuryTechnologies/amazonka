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
-- Module      : Amazonka.AppMesh.Types.GatewayRouteHostnameMatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GatewayRouteHostnameMatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing the gateway route host name to match.
--
-- /See:/ 'newGatewayRouteHostnameMatch' smart constructor.
data GatewayRouteHostnameMatch = GatewayRouteHostnameMatch'
  { -- | The specified ending characters of the host name to match on.
    suffix :: Prelude.Maybe Prelude.Text,
    -- | The exact host name to match on.
    exact :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayRouteHostnameMatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suffix', 'gatewayRouteHostnameMatch_suffix' - The specified ending characters of the host name to match on.
--
-- 'exact', 'gatewayRouteHostnameMatch_exact' - The exact host name to match on.
newGatewayRouteHostnameMatch ::
  GatewayRouteHostnameMatch
newGatewayRouteHostnameMatch =
  GatewayRouteHostnameMatch'
    { suffix =
        Prelude.Nothing,
      exact = Prelude.Nothing
    }

-- | The specified ending characters of the host name to match on.
gatewayRouteHostnameMatch_suffix :: Lens.Lens' GatewayRouteHostnameMatch (Prelude.Maybe Prelude.Text)
gatewayRouteHostnameMatch_suffix = Lens.lens (\GatewayRouteHostnameMatch' {suffix} -> suffix) (\s@GatewayRouteHostnameMatch' {} a -> s {suffix = a} :: GatewayRouteHostnameMatch)

-- | The exact host name to match on.
gatewayRouteHostnameMatch_exact :: Lens.Lens' GatewayRouteHostnameMatch (Prelude.Maybe Prelude.Text)
gatewayRouteHostnameMatch_exact = Lens.lens (\GatewayRouteHostnameMatch' {exact} -> exact) (\s@GatewayRouteHostnameMatch' {} a -> s {exact = a} :: GatewayRouteHostnameMatch)

instance Core.FromJSON GatewayRouteHostnameMatch where
  parseJSON =
    Core.withObject
      "GatewayRouteHostnameMatch"
      ( \x ->
          GatewayRouteHostnameMatch'
            Prelude.<$> (x Core..:? "suffix")
            Prelude.<*> (x Core..:? "exact")
      )

instance Prelude.Hashable GatewayRouteHostnameMatch where
  hashWithSalt salt' GatewayRouteHostnameMatch' {..} =
    salt' `Prelude.hashWithSalt` exact
      `Prelude.hashWithSalt` suffix

instance Prelude.NFData GatewayRouteHostnameMatch where
  rnf GatewayRouteHostnameMatch' {..} =
    Prelude.rnf suffix `Prelude.seq` Prelude.rnf exact

instance Core.ToJSON GatewayRouteHostnameMatch where
  toJSON GatewayRouteHostnameMatch' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("suffix" Core..=) Prelude.<$> suffix,
            ("exact" Core..=) Prelude.<$> exact
          ]
      )
