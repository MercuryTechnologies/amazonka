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
-- Module      : Amazonka.SageMaker.Types.ParameterRanges
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ParameterRanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.CategoricalParameterRange
import Amazonka.SageMaker.Types.ContinuousParameterRange
import Amazonka.SageMaker.Types.IntegerParameterRange

-- | Specifies ranges of integer, continuous, and categorical hyperparameters
-- that a hyperparameter tuning job searches. The hyperparameter tuning job
-- launches training jobs with hyperparameter values within these ranges to
-- find the combination of values that result in the training job with the
-- best performance as measured by the objective metric of the
-- hyperparameter tuning job.
--
-- You can specify a maximum of 20 hyperparameters that a hyperparameter
-- tuning job can search over. Every possible value of a categorical
-- parameter range counts against this limit.
--
-- /See:/ 'newParameterRanges' smart constructor.
data ParameterRanges = ParameterRanges'
  { -- | The array of CategoricalParameterRange objects that specify ranges of
    -- categorical hyperparameters that a hyperparameter tuning job searches.
    categoricalParameterRanges :: Prelude.Maybe [CategoricalParameterRange],
    -- | The array of IntegerParameterRange objects that specify ranges of
    -- integer hyperparameters that a hyperparameter tuning job searches.
    integerParameterRanges :: Prelude.Maybe [IntegerParameterRange],
    -- | The array of ContinuousParameterRange objects that specify ranges of
    -- continuous hyperparameters that a hyperparameter tuning job searches.
    continuousParameterRanges :: Prelude.Maybe [ContinuousParameterRange]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParameterRanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'categoricalParameterRanges', 'parameterRanges_categoricalParameterRanges' - The array of CategoricalParameterRange objects that specify ranges of
-- categorical hyperparameters that a hyperparameter tuning job searches.
--
-- 'integerParameterRanges', 'parameterRanges_integerParameterRanges' - The array of IntegerParameterRange objects that specify ranges of
-- integer hyperparameters that a hyperparameter tuning job searches.
--
-- 'continuousParameterRanges', 'parameterRanges_continuousParameterRanges' - The array of ContinuousParameterRange objects that specify ranges of
-- continuous hyperparameters that a hyperparameter tuning job searches.
newParameterRanges ::
  ParameterRanges
newParameterRanges =
  ParameterRanges'
    { categoricalParameterRanges =
        Prelude.Nothing,
      integerParameterRanges = Prelude.Nothing,
      continuousParameterRanges = Prelude.Nothing
    }

-- | The array of CategoricalParameterRange objects that specify ranges of
-- categorical hyperparameters that a hyperparameter tuning job searches.
parameterRanges_categoricalParameterRanges :: Lens.Lens' ParameterRanges (Prelude.Maybe [CategoricalParameterRange])
parameterRanges_categoricalParameterRanges = Lens.lens (\ParameterRanges' {categoricalParameterRanges} -> categoricalParameterRanges) (\s@ParameterRanges' {} a -> s {categoricalParameterRanges = a} :: ParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | The array of IntegerParameterRange objects that specify ranges of
-- integer hyperparameters that a hyperparameter tuning job searches.
parameterRanges_integerParameterRanges :: Lens.Lens' ParameterRanges (Prelude.Maybe [IntegerParameterRange])
parameterRanges_integerParameterRanges = Lens.lens (\ParameterRanges' {integerParameterRanges} -> integerParameterRanges) (\s@ParameterRanges' {} a -> s {integerParameterRanges = a} :: ParameterRanges) Prelude.. Lens.mapping Lens.coerced

-- | The array of ContinuousParameterRange objects that specify ranges of
-- continuous hyperparameters that a hyperparameter tuning job searches.
parameterRanges_continuousParameterRanges :: Lens.Lens' ParameterRanges (Prelude.Maybe [ContinuousParameterRange])
parameterRanges_continuousParameterRanges = Lens.lens (\ParameterRanges' {continuousParameterRanges} -> continuousParameterRanges) (\s@ParameterRanges' {} a -> s {continuousParameterRanges = a} :: ParameterRanges) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON ParameterRanges where
  parseJSON =
    Core.withObject
      "ParameterRanges"
      ( \x ->
          ParameterRanges'
            Prelude.<$> ( x Core..:? "CategoricalParameterRanges"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "IntegerParameterRanges"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "ContinuousParameterRanges"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable ParameterRanges where
  hashWithSalt salt' ParameterRanges' {..} =
    salt'
      `Prelude.hashWithSalt` continuousParameterRanges
      `Prelude.hashWithSalt` integerParameterRanges
      `Prelude.hashWithSalt` categoricalParameterRanges

instance Prelude.NFData ParameterRanges where
  rnf ParameterRanges' {..} =
    Prelude.rnf categoricalParameterRanges
      `Prelude.seq` Prelude.rnf continuousParameterRanges
      `Prelude.seq` Prelude.rnf integerParameterRanges

instance Core.ToJSON ParameterRanges where
  toJSON ParameterRanges' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("CategoricalParameterRanges" Core..=)
              Prelude.<$> categoricalParameterRanges,
            ("IntegerParameterRanges" Core..=)
              Prelude.<$> integerParameterRanges,
            ("ContinuousParameterRanges" Core..=)
              Prelude.<$> continuousParameterRanges
          ]
      )
