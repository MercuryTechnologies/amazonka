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
-- Module      : Amazonka.IoTSiteWise.Types.AssetModelCompositeModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetModelCompositeModel where

import qualified Amazonka.Core as Core
import Amazonka.IoTSiteWise.Types.AssetModelProperty
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a composite model in an asset model. This
-- object contains the asset property definitions that you define in the
-- composite model.
--
-- /See:/ 'newAssetModelCompositeModel' smart constructor.
data AssetModelCompositeModel = AssetModelCompositeModel'
  { -- | The description of the composite model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The asset property definitions for this composite model.
    properties :: Prelude.Maybe [AssetModelProperty],
    -- | The name of the composite model.
    name :: Prelude.Text,
    -- | The type of the composite model. For alarm composite models, this type
    -- is @AWS\/ALARM@.
    type' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetModelCompositeModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'assetModelCompositeModel_description' - The description of the composite model.
--
-- 'properties', 'assetModelCompositeModel_properties' - The asset property definitions for this composite model.
--
-- 'name', 'assetModelCompositeModel_name' - The name of the composite model.
--
-- 'type'', 'assetModelCompositeModel_type' - The type of the composite model. For alarm composite models, this type
-- is @AWS\/ALARM@.
newAssetModelCompositeModel ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  AssetModelCompositeModel
newAssetModelCompositeModel pName_ pType_ =
  AssetModelCompositeModel'
    { description =
        Prelude.Nothing,
      properties = Prelude.Nothing,
      name = pName_,
      type' = pType_
    }

-- | The description of the composite model.
assetModelCompositeModel_description :: Lens.Lens' AssetModelCompositeModel (Prelude.Maybe Prelude.Text)
assetModelCompositeModel_description = Lens.lens (\AssetModelCompositeModel' {description} -> description) (\s@AssetModelCompositeModel' {} a -> s {description = a} :: AssetModelCompositeModel)

-- | The asset property definitions for this composite model.
assetModelCompositeModel_properties :: Lens.Lens' AssetModelCompositeModel (Prelude.Maybe [AssetModelProperty])
assetModelCompositeModel_properties = Lens.lens (\AssetModelCompositeModel' {properties} -> properties) (\s@AssetModelCompositeModel' {} a -> s {properties = a} :: AssetModelCompositeModel) Prelude.. Lens.mapping Lens.coerced

-- | The name of the composite model.
assetModelCompositeModel_name :: Lens.Lens' AssetModelCompositeModel Prelude.Text
assetModelCompositeModel_name = Lens.lens (\AssetModelCompositeModel' {name} -> name) (\s@AssetModelCompositeModel' {} a -> s {name = a} :: AssetModelCompositeModel)

-- | The type of the composite model. For alarm composite models, this type
-- is @AWS\/ALARM@.
assetModelCompositeModel_type :: Lens.Lens' AssetModelCompositeModel Prelude.Text
assetModelCompositeModel_type = Lens.lens (\AssetModelCompositeModel' {type'} -> type') (\s@AssetModelCompositeModel' {} a -> s {type' = a} :: AssetModelCompositeModel)

instance Core.FromJSON AssetModelCompositeModel where
  parseJSON =
    Core.withObject
      "AssetModelCompositeModel"
      ( \x ->
          AssetModelCompositeModel'
            Prelude.<$> (x Core..:? "description")
            Prelude.<*> (x Core..:? "properties" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "type")
      )

instance Prelude.Hashable AssetModelCompositeModel where
  hashWithSalt salt' AssetModelCompositeModel' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` description

instance Prelude.NFData AssetModelCompositeModel where
  rnf AssetModelCompositeModel' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf properties

instance Core.ToJSON AssetModelCompositeModel where
  toJSON AssetModelCompositeModel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("description" Core..=) Prelude.<$> description,
            ("properties" Core..=) Prelude.<$> properties,
            Prelude.Just ("name" Core..= name),
            Prelude.Just ("type" Core..= type')
          ]
      )
