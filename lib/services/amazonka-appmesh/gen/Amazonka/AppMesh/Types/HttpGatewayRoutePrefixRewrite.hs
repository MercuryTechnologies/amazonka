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
-- Module      : Amazonka.AppMesh.Types.HttpGatewayRoutePrefixRewrite
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.HttpGatewayRoutePrefixRewrite where

import Amazonka.AppMesh.Types.DefaultGatewayRouteRewrite
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing the beginning characters of the route to rewrite.
--
-- /See:/ 'newHttpGatewayRoutePrefixRewrite' smart constructor.
data HttpGatewayRoutePrefixRewrite = HttpGatewayRoutePrefixRewrite'
  { -- | The value used to replace the incoming route prefix when rewritten.
    value :: Prelude.Maybe Prelude.Text,
    -- | The default prefix used to replace the incoming route prefix when
    -- rewritten.
    defaultPrefix :: Prelude.Maybe DefaultGatewayRouteRewrite
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HttpGatewayRoutePrefixRewrite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'httpGatewayRoutePrefixRewrite_value' - The value used to replace the incoming route prefix when rewritten.
--
-- 'defaultPrefix', 'httpGatewayRoutePrefixRewrite_defaultPrefix' - The default prefix used to replace the incoming route prefix when
-- rewritten.
newHttpGatewayRoutePrefixRewrite ::
  HttpGatewayRoutePrefixRewrite
newHttpGatewayRoutePrefixRewrite =
  HttpGatewayRoutePrefixRewrite'
    { value =
        Prelude.Nothing,
      defaultPrefix = Prelude.Nothing
    }

-- | The value used to replace the incoming route prefix when rewritten.
httpGatewayRoutePrefixRewrite_value :: Lens.Lens' HttpGatewayRoutePrefixRewrite (Prelude.Maybe Prelude.Text)
httpGatewayRoutePrefixRewrite_value = Lens.lens (\HttpGatewayRoutePrefixRewrite' {value} -> value) (\s@HttpGatewayRoutePrefixRewrite' {} a -> s {value = a} :: HttpGatewayRoutePrefixRewrite)

-- | The default prefix used to replace the incoming route prefix when
-- rewritten.
httpGatewayRoutePrefixRewrite_defaultPrefix :: Lens.Lens' HttpGatewayRoutePrefixRewrite (Prelude.Maybe DefaultGatewayRouteRewrite)
httpGatewayRoutePrefixRewrite_defaultPrefix = Lens.lens (\HttpGatewayRoutePrefixRewrite' {defaultPrefix} -> defaultPrefix) (\s@HttpGatewayRoutePrefixRewrite' {} a -> s {defaultPrefix = a} :: HttpGatewayRoutePrefixRewrite)

instance Core.FromJSON HttpGatewayRoutePrefixRewrite where
  parseJSON =
    Core.withObject
      "HttpGatewayRoutePrefixRewrite"
      ( \x ->
          HttpGatewayRoutePrefixRewrite'
            Prelude.<$> (x Core..:? "value")
            Prelude.<*> (x Core..:? "defaultPrefix")
      )

instance
  Prelude.Hashable
    HttpGatewayRoutePrefixRewrite
  where
  hashWithSalt salt' HttpGatewayRoutePrefixRewrite' {..} =
    salt' `Prelude.hashWithSalt` defaultPrefix
      `Prelude.hashWithSalt` value

instance Prelude.NFData HttpGatewayRoutePrefixRewrite where
  rnf HttpGatewayRoutePrefixRewrite' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf defaultPrefix

instance Core.ToJSON HttpGatewayRoutePrefixRewrite where
  toJSON HttpGatewayRoutePrefixRewrite' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("value" Core..=) Prelude.<$> value,
            ("defaultPrefix" Core..=) Prelude.<$> defaultPrefix
          ]
      )
