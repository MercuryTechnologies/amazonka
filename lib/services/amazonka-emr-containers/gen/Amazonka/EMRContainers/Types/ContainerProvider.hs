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
-- Module      : Amazonka.EMRContainers.Types.ContainerProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.ContainerProvider where

import qualified Amazonka.Core as Core
import Amazonka.EMRContainers.Types.ContainerInfo
import Amazonka.EMRContainers.Types.ContainerProviderType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The information about the container provider.
--
-- /See:/ 'newContainerProvider' smart constructor.
data ContainerProvider = ContainerProvider'
  { -- | The information about the container cluster.
    info :: Prelude.Maybe ContainerInfo,
    -- | The type of the container provider. EKS is the only supported type as of
    -- now.
    type' :: ContainerProviderType,
    -- | The ID of the container cluster.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ContainerProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'info', 'containerProvider_info' - The information about the container cluster.
--
-- 'type'', 'containerProvider_type' - The type of the container provider. EKS is the only supported type as of
-- now.
--
-- 'id', 'containerProvider_id' - The ID of the container cluster.
newContainerProvider ::
  -- | 'type''
  ContainerProviderType ->
  -- | 'id'
  Prelude.Text ->
  ContainerProvider
newContainerProvider pType_ pId_ =
  ContainerProvider'
    { info = Prelude.Nothing,
      type' = pType_,
      id = pId_
    }

-- | The information about the container cluster.
containerProvider_info :: Lens.Lens' ContainerProvider (Prelude.Maybe ContainerInfo)
containerProvider_info = Lens.lens (\ContainerProvider' {info} -> info) (\s@ContainerProvider' {} a -> s {info = a} :: ContainerProvider)

-- | The type of the container provider. EKS is the only supported type as of
-- now.
containerProvider_type :: Lens.Lens' ContainerProvider ContainerProviderType
containerProvider_type = Lens.lens (\ContainerProvider' {type'} -> type') (\s@ContainerProvider' {} a -> s {type' = a} :: ContainerProvider)

-- | The ID of the container cluster.
containerProvider_id :: Lens.Lens' ContainerProvider Prelude.Text
containerProvider_id = Lens.lens (\ContainerProvider' {id} -> id) (\s@ContainerProvider' {} a -> s {id = a} :: ContainerProvider)

instance Core.FromJSON ContainerProvider where
  parseJSON =
    Core.withObject
      "ContainerProvider"
      ( \x ->
          ContainerProvider'
            Prelude.<$> (x Core..:? "info")
            Prelude.<*> (x Core..: "type")
            Prelude.<*> (x Core..: "id")
      )

instance Prelude.Hashable ContainerProvider where
  hashWithSalt salt' ContainerProvider' {..} =
    salt' `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` info

instance Prelude.NFData ContainerProvider where
  rnf ContainerProvider' {..} =
    Prelude.rnf info `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf type'

instance Core.ToJSON ContainerProvider where
  toJSON ContainerProvider' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("info" Core..=) Prelude.<$> info,
            Prelude.Just ("type" Core..= type'),
            Prelude.Just ("id" Core..= id)
          ]
      )
