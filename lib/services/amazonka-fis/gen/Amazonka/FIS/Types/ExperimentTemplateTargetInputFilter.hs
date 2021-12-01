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
-- Module      : Amazonka.FIS.Types.ExperimentTemplateTargetInputFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FIS.Types.ExperimentTemplateTargetInputFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a filter used for the target resource input in an experiment
-- template.
--
-- /See:/ 'newExperimentTemplateTargetInputFilter' smart constructor.
data ExperimentTemplateTargetInputFilter = ExperimentTemplateTargetInputFilter'
  { -- | The attribute path for the filter.
    path :: Prelude.Text,
    -- | The attribute values for the filter.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExperimentTemplateTargetInputFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'experimentTemplateTargetInputFilter_path' - The attribute path for the filter.
--
-- 'values', 'experimentTemplateTargetInputFilter_values' - The attribute values for the filter.
newExperimentTemplateTargetInputFilter ::
  -- | 'path'
  Prelude.Text ->
  ExperimentTemplateTargetInputFilter
newExperimentTemplateTargetInputFilter pPath_ =
  ExperimentTemplateTargetInputFilter'
    { path = pPath_,
      values = Prelude.mempty
    }

-- | The attribute path for the filter.
experimentTemplateTargetInputFilter_path :: Lens.Lens' ExperimentTemplateTargetInputFilter Prelude.Text
experimentTemplateTargetInputFilter_path = Lens.lens (\ExperimentTemplateTargetInputFilter' {path} -> path) (\s@ExperimentTemplateTargetInputFilter' {} a -> s {path = a} :: ExperimentTemplateTargetInputFilter)

-- | The attribute values for the filter.
experimentTemplateTargetInputFilter_values :: Lens.Lens' ExperimentTemplateTargetInputFilter [Prelude.Text]
experimentTemplateTargetInputFilter_values = Lens.lens (\ExperimentTemplateTargetInputFilter' {values} -> values) (\s@ExperimentTemplateTargetInputFilter' {} a -> s {values = a} :: ExperimentTemplateTargetInputFilter) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    ExperimentTemplateTargetInputFilter
  where
  hashWithSalt
    salt'
    ExperimentTemplateTargetInputFilter' {..} =
      salt' `Prelude.hashWithSalt` values
        `Prelude.hashWithSalt` path

instance
  Prelude.NFData
    ExperimentTemplateTargetInputFilter
  where
  rnf ExperimentTemplateTargetInputFilter' {..} =
    Prelude.rnf path `Prelude.seq` Prelude.rnf values

instance
  Core.ToJSON
    ExperimentTemplateTargetInputFilter
  where
  toJSON ExperimentTemplateTargetInputFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("path" Core..= path),
            Prelude.Just ("values" Core..= values)
          ]
      )
