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
-- Module      : Amazonka.Personalize.Types.DefaultContinuousHyperParameterRange
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.DefaultContinuousHyperParameterRange where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the name and default range of a continuous hyperparameter and
-- whether the hyperparameter is tunable. A tunable hyperparameter can have
-- its value determined during hyperparameter optimization (HPO).
--
-- /See:/ 'newDefaultContinuousHyperParameterRange' smart constructor.
data DefaultContinuousHyperParameterRange = DefaultContinuousHyperParameterRange'
  { -- | The maximum allowable value for the hyperparameter.
    maxValue :: Prelude.Maybe Prelude.Double,
    -- | Whether the hyperparameter is tunable.
    isTunable :: Prelude.Maybe Prelude.Bool,
    -- | The name of the hyperparameter.
    name :: Prelude.Maybe Prelude.Text,
    -- | The minimum allowable value for the hyperparameter.
    minValue :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultContinuousHyperParameterRange' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxValue', 'defaultContinuousHyperParameterRange_maxValue' - The maximum allowable value for the hyperparameter.
--
-- 'isTunable', 'defaultContinuousHyperParameterRange_isTunable' - Whether the hyperparameter is tunable.
--
-- 'name', 'defaultContinuousHyperParameterRange_name' - The name of the hyperparameter.
--
-- 'minValue', 'defaultContinuousHyperParameterRange_minValue' - The minimum allowable value for the hyperparameter.
newDefaultContinuousHyperParameterRange ::
  DefaultContinuousHyperParameterRange
newDefaultContinuousHyperParameterRange =
  DefaultContinuousHyperParameterRange'
    { maxValue =
        Prelude.Nothing,
      isTunable = Prelude.Nothing,
      name = Prelude.Nothing,
      minValue = Prelude.Nothing
    }

-- | The maximum allowable value for the hyperparameter.
defaultContinuousHyperParameterRange_maxValue :: Lens.Lens' DefaultContinuousHyperParameterRange (Prelude.Maybe Prelude.Double)
defaultContinuousHyperParameterRange_maxValue = Lens.lens (\DefaultContinuousHyperParameterRange' {maxValue} -> maxValue) (\s@DefaultContinuousHyperParameterRange' {} a -> s {maxValue = a} :: DefaultContinuousHyperParameterRange)

-- | Whether the hyperparameter is tunable.
defaultContinuousHyperParameterRange_isTunable :: Lens.Lens' DefaultContinuousHyperParameterRange (Prelude.Maybe Prelude.Bool)
defaultContinuousHyperParameterRange_isTunable = Lens.lens (\DefaultContinuousHyperParameterRange' {isTunable} -> isTunable) (\s@DefaultContinuousHyperParameterRange' {} a -> s {isTunable = a} :: DefaultContinuousHyperParameterRange)

-- | The name of the hyperparameter.
defaultContinuousHyperParameterRange_name :: Lens.Lens' DefaultContinuousHyperParameterRange (Prelude.Maybe Prelude.Text)
defaultContinuousHyperParameterRange_name = Lens.lens (\DefaultContinuousHyperParameterRange' {name} -> name) (\s@DefaultContinuousHyperParameterRange' {} a -> s {name = a} :: DefaultContinuousHyperParameterRange)

-- | The minimum allowable value for the hyperparameter.
defaultContinuousHyperParameterRange_minValue :: Lens.Lens' DefaultContinuousHyperParameterRange (Prelude.Maybe Prelude.Double)
defaultContinuousHyperParameterRange_minValue = Lens.lens (\DefaultContinuousHyperParameterRange' {minValue} -> minValue) (\s@DefaultContinuousHyperParameterRange' {} a -> s {minValue = a} :: DefaultContinuousHyperParameterRange)

instance
  Core.FromJSON
    DefaultContinuousHyperParameterRange
  where
  parseJSON =
    Core.withObject
      "DefaultContinuousHyperParameterRange"
      ( \x ->
          DefaultContinuousHyperParameterRange'
            Prelude.<$> (x Core..:? "maxValue")
            Prelude.<*> (x Core..:? "isTunable")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "minValue")
      )

instance
  Prelude.Hashable
    DefaultContinuousHyperParameterRange
  where
  hashWithSalt
    salt'
    DefaultContinuousHyperParameterRange' {..} =
      salt' `Prelude.hashWithSalt` minValue
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` isTunable
        `Prelude.hashWithSalt` maxValue

instance
  Prelude.NFData
    DefaultContinuousHyperParameterRange
  where
  rnf DefaultContinuousHyperParameterRange' {..} =
    Prelude.rnf maxValue
      `Prelude.seq` Prelude.rnf minValue
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf isTunable
