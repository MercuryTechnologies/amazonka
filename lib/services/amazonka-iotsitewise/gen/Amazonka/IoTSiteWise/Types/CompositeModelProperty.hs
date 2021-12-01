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
-- Module      : Amazonka.IoTSiteWise.Types.CompositeModelProperty
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.CompositeModelProperty where

import qualified Amazonka.Core as Core
import Amazonka.IoTSiteWise.Types.Property
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a composite model property on an asset.
--
-- /See:/ 'newCompositeModelProperty' smart constructor.
data CompositeModelProperty = CompositeModelProperty'
  { -- | The name of the property.
    name :: Prelude.Text,
    -- | The type of the composite model that defines this property.
    type' :: Prelude.Text,
    assetProperty :: Property
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CompositeModelProperty' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'compositeModelProperty_name' - The name of the property.
--
-- 'type'', 'compositeModelProperty_type' - The type of the composite model that defines this property.
--
-- 'assetProperty', 'compositeModelProperty_assetProperty' - Undocumented member.
newCompositeModelProperty ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  -- | 'assetProperty'
  Property ->
  CompositeModelProperty
newCompositeModelProperty
  pName_
  pType_
  pAssetProperty_ =
    CompositeModelProperty'
      { name = pName_,
        type' = pType_,
        assetProperty = pAssetProperty_
      }

-- | The name of the property.
compositeModelProperty_name :: Lens.Lens' CompositeModelProperty Prelude.Text
compositeModelProperty_name = Lens.lens (\CompositeModelProperty' {name} -> name) (\s@CompositeModelProperty' {} a -> s {name = a} :: CompositeModelProperty)

-- | The type of the composite model that defines this property.
compositeModelProperty_type :: Lens.Lens' CompositeModelProperty Prelude.Text
compositeModelProperty_type = Lens.lens (\CompositeModelProperty' {type'} -> type') (\s@CompositeModelProperty' {} a -> s {type' = a} :: CompositeModelProperty)

-- | Undocumented member.
compositeModelProperty_assetProperty :: Lens.Lens' CompositeModelProperty Property
compositeModelProperty_assetProperty = Lens.lens (\CompositeModelProperty' {assetProperty} -> assetProperty) (\s@CompositeModelProperty' {} a -> s {assetProperty = a} :: CompositeModelProperty)

instance Core.FromJSON CompositeModelProperty where
  parseJSON =
    Core.withObject
      "CompositeModelProperty"
      ( \x ->
          CompositeModelProperty'
            Prelude.<$> (x Core..: "name")
            Prelude.<*> (x Core..: "type")
            Prelude.<*> (x Core..: "assetProperty")
      )

instance Prelude.Hashable CompositeModelProperty where
  hashWithSalt salt' CompositeModelProperty' {..} =
    salt' `Prelude.hashWithSalt` assetProperty
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name

instance Prelude.NFData CompositeModelProperty where
  rnf CompositeModelProperty' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf assetProperty
      `Prelude.seq` Prelude.rnf type'
