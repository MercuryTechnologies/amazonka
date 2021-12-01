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
-- Module      : Amazonka.IoTSiteWise.Types.AssetCompositeModel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetCompositeModel where

import qualified Amazonka.Core as Core
import Amazonka.IoTSiteWise.Types.AssetProperty
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a composite model in an asset. This object
-- contains the asset\'s properties that you define in the composite model.
--
-- /See:/ 'newAssetCompositeModel' smart constructor.
data AssetCompositeModel = AssetCompositeModel'
  { -- | The description of the composite model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the composite model.
    name :: Prelude.Text,
    -- | The type of the composite model. For alarm composite models, this type
    -- is @AWS\/ALARM@.
    type' :: Prelude.Text,
    -- | The asset properties that this composite model defines.
    properties :: [AssetProperty]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetCompositeModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'assetCompositeModel_description' - The description of the composite model.
--
-- 'name', 'assetCompositeModel_name' - The name of the composite model.
--
-- 'type'', 'assetCompositeModel_type' - The type of the composite model. For alarm composite models, this type
-- is @AWS\/ALARM@.
--
-- 'properties', 'assetCompositeModel_properties' - The asset properties that this composite model defines.
newAssetCompositeModel ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  Prelude.Text ->
  AssetCompositeModel
newAssetCompositeModel pName_ pType_ =
  AssetCompositeModel'
    { description = Prelude.Nothing,
      name = pName_,
      type' = pType_,
      properties = Prelude.mempty
    }

-- | The description of the composite model.
assetCompositeModel_description :: Lens.Lens' AssetCompositeModel (Prelude.Maybe Prelude.Text)
assetCompositeModel_description = Lens.lens (\AssetCompositeModel' {description} -> description) (\s@AssetCompositeModel' {} a -> s {description = a} :: AssetCompositeModel)

-- | The name of the composite model.
assetCompositeModel_name :: Lens.Lens' AssetCompositeModel Prelude.Text
assetCompositeModel_name = Lens.lens (\AssetCompositeModel' {name} -> name) (\s@AssetCompositeModel' {} a -> s {name = a} :: AssetCompositeModel)

-- | The type of the composite model. For alarm composite models, this type
-- is @AWS\/ALARM@.
assetCompositeModel_type :: Lens.Lens' AssetCompositeModel Prelude.Text
assetCompositeModel_type = Lens.lens (\AssetCompositeModel' {type'} -> type') (\s@AssetCompositeModel' {} a -> s {type' = a} :: AssetCompositeModel)

-- | The asset properties that this composite model defines.
assetCompositeModel_properties :: Lens.Lens' AssetCompositeModel [AssetProperty]
assetCompositeModel_properties = Lens.lens (\AssetCompositeModel' {properties} -> properties) (\s@AssetCompositeModel' {} a -> s {properties = a} :: AssetCompositeModel) Prelude.. Lens.coerced

instance Core.FromJSON AssetCompositeModel where
  parseJSON =
    Core.withObject
      "AssetCompositeModel"
      ( \x ->
          AssetCompositeModel'
            Prelude.<$> (x Core..:? "description")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "type")
            Prelude.<*> (x Core..:? "properties" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AssetCompositeModel where
  hashWithSalt salt' AssetCompositeModel' {..} =
    salt' `Prelude.hashWithSalt` properties
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description

instance Prelude.NFData AssetCompositeModel where
  rnf AssetCompositeModel' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf properties
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf name
