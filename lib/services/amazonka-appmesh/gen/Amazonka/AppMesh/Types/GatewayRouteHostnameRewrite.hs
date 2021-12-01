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
-- Module      : Amazonka.AppMesh.Types.GatewayRouteHostnameRewrite
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.GatewayRouteHostnameRewrite where

import Amazonka.AppMesh.Types.DefaultGatewayRouteRewrite
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing the gateway route host name to rewrite.
--
-- /See:/ 'newGatewayRouteHostnameRewrite' smart constructor.
data GatewayRouteHostnameRewrite = GatewayRouteHostnameRewrite'
  { -- | The default target host name to write to.
    defaultTargetHostname :: Prelude.Maybe DefaultGatewayRouteRewrite
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GatewayRouteHostnameRewrite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultTargetHostname', 'gatewayRouteHostnameRewrite_defaultTargetHostname' - The default target host name to write to.
newGatewayRouteHostnameRewrite ::
  GatewayRouteHostnameRewrite
newGatewayRouteHostnameRewrite =
  GatewayRouteHostnameRewrite'
    { defaultTargetHostname =
        Prelude.Nothing
    }

-- | The default target host name to write to.
gatewayRouteHostnameRewrite_defaultTargetHostname :: Lens.Lens' GatewayRouteHostnameRewrite (Prelude.Maybe DefaultGatewayRouteRewrite)
gatewayRouteHostnameRewrite_defaultTargetHostname = Lens.lens (\GatewayRouteHostnameRewrite' {defaultTargetHostname} -> defaultTargetHostname) (\s@GatewayRouteHostnameRewrite' {} a -> s {defaultTargetHostname = a} :: GatewayRouteHostnameRewrite)

instance Core.FromJSON GatewayRouteHostnameRewrite where
  parseJSON =
    Core.withObject
      "GatewayRouteHostnameRewrite"
      ( \x ->
          GatewayRouteHostnameRewrite'
            Prelude.<$> (x Core..:? "defaultTargetHostname")
      )

instance Prelude.Hashable GatewayRouteHostnameRewrite where
  hashWithSalt salt' GatewayRouteHostnameRewrite' {..} =
    salt' `Prelude.hashWithSalt` defaultTargetHostname

instance Prelude.NFData GatewayRouteHostnameRewrite where
  rnf GatewayRouteHostnameRewrite' {..} =
    Prelude.rnf defaultTargetHostname

instance Core.ToJSON GatewayRouteHostnameRewrite where
  toJSON GatewayRouteHostnameRewrite' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("defaultTargetHostname" Core..=)
              Prelude.<$> defaultTargetHostname
          ]
      )
