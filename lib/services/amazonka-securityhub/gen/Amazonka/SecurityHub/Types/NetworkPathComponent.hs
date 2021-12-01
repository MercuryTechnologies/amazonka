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
-- Module      : Amazonka.SecurityHub.Types.NetworkPathComponent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.NetworkPathComponent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.NetworkHeader

-- | Information about a network path component.
--
-- /See:/ 'newNetworkPathComponent' smart constructor.
data NetworkPathComponent = NetworkPathComponent'
  { -- | The type of component.
    componentType :: Prelude.Maybe Prelude.Text,
    -- | Information about the component that comes before the current node in
    -- the network path.
    ingress :: Prelude.Maybe NetworkHeader,
    -- | The identifier of a component in the network path.
    componentId :: Prelude.Maybe Prelude.Text,
    -- | Information about the component that comes after the current component
    -- in the network path.
    egress :: Prelude.Maybe NetworkHeader
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkPathComponent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'componentType', 'networkPathComponent_componentType' - The type of component.
--
-- 'ingress', 'networkPathComponent_ingress' - Information about the component that comes before the current node in
-- the network path.
--
-- 'componentId', 'networkPathComponent_componentId' - The identifier of a component in the network path.
--
-- 'egress', 'networkPathComponent_egress' - Information about the component that comes after the current component
-- in the network path.
newNetworkPathComponent ::
  NetworkPathComponent
newNetworkPathComponent =
  NetworkPathComponent'
    { componentType =
        Prelude.Nothing,
      ingress = Prelude.Nothing,
      componentId = Prelude.Nothing,
      egress = Prelude.Nothing
    }

-- | The type of component.
networkPathComponent_componentType :: Lens.Lens' NetworkPathComponent (Prelude.Maybe Prelude.Text)
networkPathComponent_componentType = Lens.lens (\NetworkPathComponent' {componentType} -> componentType) (\s@NetworkPathComponent' {} a -> s {componentType = a} :: NetworkPathComponent)

-- | Information about the component that comes before the current node in
-- the network path.
networkPathComponent_ingress :: Lens.Lens' NetworkPathComponent (Prelude.Maybe NetworkHeader)
networkPathComponent_ingress = Lens.lens (\NetworkPathComponent' {ingress} -> ingress) (\s@NetworkPathComponent' {} a -> s {ingress = a} :: NetworkPathComponent)

-- | The identifier of a component in the network path.
networkPathComponent_componentId :: Lens.Lens' NetworkPathComponent (Prelude.Maybe Prelude.Text)
networkPathComponent_componentId = Lens.lens (\NetworkPathComponent' {componentId} -> componentId) (\s@NetworkPathComponent' {} a -> s {componentId = a} :: NetworkPathComponent)

-- | Information about the component that comes after the current component
-- in the network path.
networkPathComponent_egress :: Lens.Lens' NetworkPathComponent (Prelude.Maybe NetworkHeader)
networkPathComponent_egress = Lens.lens (\NetworkPathComponent' {egress} -> egress) (\s@NetworkPathComponent' {} a -> s {egress = a} :: NetworkPathComponent)

instance Core.FromJSON NetworkPathComponent where
  parseJSON =
    Core.withObject
      "NetworkPathComponent"
      ( \x ->
          NetworkPathComponent'
            Prelude.<$> (x Core..:? "ComponentType")
            Prelude.<*> (x Core..:? "Ingress")
            Prelude.<*> (x Core..:? "ComponentId")
            Prelude.<*> (x Core..:? "Egress")
      )

instance Prelude.Hashable NetworkPathComponent where
  hashWithSalt salt' NetworkPathComponent' {..} =
    salt' `Prelude.hashWithSalt` egress
      `Prelude.hashWithSalt` componentId
      `Prelude.hashWithSalt` ingress
      `Prelude.hashWithSalt` componentType

instance Prelude.NFData NetworkPathComponent where
  rnf NetworkPathComponent' {..} =
    Prelude.rnf componentType
      `Prelude.seq` Prelude.rnf egress
      `Prelude.seq` Prelude.rnf componentId
      `Prelude.seq` Prelude.rnf ingress

instance Core.ToJSON NetworkPathComponent where
  toJSON NetworkPathComponent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ComponentType" Core..=) Prelude.<$> componentType,
            ("Ingress" Core..=) Prelude.<$> ingress,
            ("ComponentId" Core..=) Prelude.<$> componentId,
            ("Egress" Core..=) Prelude.<$> egress
          ]
      )
