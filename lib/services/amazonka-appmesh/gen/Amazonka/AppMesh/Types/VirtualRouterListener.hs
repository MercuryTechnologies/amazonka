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
-- Module      : Amazonka.AppMesh.Types.VirtualRouterListener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppMesh.Types.VirtualRouterListener where

import Amazonka.AppMesh.Types.PortMapping
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a virtual router listener.
--
-- /See:/ 'newVirtualRouterListener' smart constructor.
data VirtualRouterListener = VirtualRouterListener'
  { portMapping :: PortMapping
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VirtualRouterListener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'portMapping', 'virtualRouterListener_portMapping' - Undocumented member.
newVirtualRouterListener ::
  -- | 'portMapping'
  PortMapping ->
  VirtualRouterListener
newVirtualRouterListener pPortMapping_ =
  VirtualRouterListener' {portMapping = pPortMapping_}

-- | Undocumented member.
virtualRouterListener_portMapping :: Lens.Lens' VirtualRouterListener PortMapping
virtualRouterListener_portMapping = Lens.lens (\VirtualRouterListener' {portMapping} -> portMapping) (\s@VirtualRouterListener' {} a -> s {portMapping = a} :: VirtualRouterListener)

instance Core.FromJSON VirtualRouterListener where
  parseJSON =
    Core.withObject
      "VirtualRouterListener"
      ( \x ->
          VirtualRouterListener'
            Prelude.<$> (x Core..: "portMapping")
      )

instance Prelude.Hashable VirtualRouterListener where
  hashWithSalt salt' VirtualRouterListener' {..} =
    salt' `Prelude.hashWithSalt` portMapping

instance Prelude.NFData VirtualRouterListener where
  rnf VirtualRouterListener' {..} =
    Prelude.rnf portMapping

instance Core.ToJSON VirtualRouterListener where
  toJSON VirtualRouterListener' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("portMapping" Core..= portMapping)]
      )
