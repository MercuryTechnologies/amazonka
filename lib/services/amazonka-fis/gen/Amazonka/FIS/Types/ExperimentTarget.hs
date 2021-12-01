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
-- Module      : Amazonka.FIS.Types.ExperimentTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTarget where

import qualified Amazonka.Core as Core
import Amazonka.FIS.Types.ExperimentTargetFilter
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a target for an experiment.
--
-- /See:/ 'newExperimentTarget' smart constructor.
data ExperimentTarget = ExperimentTarget'
  { -- | The resource type.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | The tags for the target resources.
    resourceTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The filters to apply to identify target resources using specific
    -- attributes.
    filters :: Prelude.Maybe [ExperimentTargetFilter],
    -- | The Amazon Resource Names (ARNs) of the resources.
    resourceArns :: Prelude.Maybe [Prelude.Text],
    -- | Scopes the identified resources to a specific count or percentage.
    selectionMode :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'experimentTarget_resourceType' - The resource type.
--
-- 'resourceTags', 'experimentTarget_resourceTags' - The tags for the target resources.
--
-- 'filters', 'experimentTarget_filters' - The filters to apply to identify target resources using specific
-- attributes.
--
-- 'resourceArns', 'experimentTarget_resourceArns' - The Amazon Resource Names (ARNs) of the resources.
--
-- 'selectionMode', 'experimentTarget_selectionMode' - Scopes the identified resources to a specific count or percentage.
newExperimentTarget ::
  ExperimentTarget
newExperimentTarget =
  ExperimentTarget'
    { resourceType = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      filters = Prelude.Nothing,
      resourceArns = Prelude.Nothing,
      selectionMode = Prelude.Nothing
    }

-- | The resource type.
experimentTarget_resourceType :: Lens.Lens' ExperimentTarget (Prelude.Maybe Prelude.Text)
experimentTarget_resourceType = Lens.lens (\ExperimentTarget' {resourceType} -> resourceType) (\s@ExperimentTarget' {} a -> s {resourceType = a} :: ExperimentTarget)

-- | The tags for the target resources.
experimentTarget_resourceTags :: Lens.Lens' ExperimentTarget (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
experimentTarget_resourceTags = Lens.lens (\ExperimentTarget' {resourceTags} -> resourceTags) (\s@ExperimentTarget' {} a -> s {resourceTags = a} :: ExperimentTarget) Prelude.. Lens.mapping Lens.coerced

-- | The filters to apply to identify target resources using specific
-- attributes.
experimentTarget_filters :: Lens.Lens' ExperimentTarget (Prelude.Maybe [ExperimentTargetFilter])
experimentTarget_filters = Lens.lens (\ExperimentTarget' {filters} -> filters) (\s@ExperimentTarget' {} a -> s {filters = a} :: ExperimentTarget) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the resources.
experimentTarget_resourceArns :: Lens.Lens' ExperimentTarget (Prelude.Maybe [Prelude.Text])
experimentTarget_resourceArns = Lens.lens (\ExperimentTarget' {resourceArns} -> resourceArns) (\s@ExperimentTarget' {} a -> s {resourceArns = a} :: ExperimentTarget) Prelude.. Lens.mapping Lens.coerced

-- | Scopes the identified resources to a specific count or percentage.
experimentTarget_selectionMode :: Lens.Lens' ExperimentTarget (Prelude.Maybe Prelude.Text)
experimentTarget_selectionMode = Lens.lens (\ExperimentTarget' {selectionMode} -> selectionMode) (\s@ExperimentTarget' {} a -> s {selectionMode = a} :: ExperimentTarget)

instance Core.FromJSON ExperimentTarget where
  parseJSON =
    Core.withObject
      "ExperimentTarget"
      ( \x ->
          ExperimentTarget'
            Prelude.<$> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "resourceTags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "filters" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "resourceArns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "selectionMode")
      )

instance Prelude.Hashable ExperimentTarget where
  hashWithSalt salt' ExperimentTarget' {..} =
    salt' `Prelude.hashWithSalt` selectionMode
      `Prelude.hashWithSalt` resourceArns
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` resourceTags
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ExperimentTarget where
  rnf ExperimentTarget' {..} =
    Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf selectionMode
      `Prelude.seq` Prelude.rnf resourceArns
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf resourceTags
