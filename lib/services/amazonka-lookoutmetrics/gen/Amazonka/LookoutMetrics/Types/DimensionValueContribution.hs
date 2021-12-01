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
-- Module      : Amazonka.LookoutMetrics.Types.DimensionValueContribution
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.DimensionValueContribution where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The severity of a value of a dimension that contributed to an anomaly.
--
-- /See:/ 'newDimensionValueContribution' smart constructor.
data DimensionValueContribution = DimensionValueContribution'
  { -- | The value of the dimension.
    dimensionValue :: Prelude.Maybe Prelude.Text,
    -- | The severity score of the value.
    contributionScore :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DimensionValueContribution' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dimensionValue', 'dimensionValueContribution_dimensionValue' - The value of the dimension.
--
-- 'contributionScore', 'dimensionValueContribution_contributionScore' - The severity score of the value.
newDimensionValueContribution ::
  DimensionValueContribution
newDimensionValueContribution =
  DimensionValueContribution'
    { dimensionValue =
        Prelude.Nothing,
      contributionScore = Prelude.Nothing
    }

-- | The value of the dimension.
dimensionValueContribution_dimensionValue :: Lens.Lens' DimensionValueContribution (Prelude.Maybe Prelude.Text)
dimensionValueContribution_dimensionValue = Lens.lens (\DimensionValueContribution' {dimensionValue} -> dimensionValue) (\s@DimensionValueContribution' {} a -> s {dimensionValue = a} :: DimensionValueContribution)

-- | The severity score of the value.
dimensionValueContribution_contributionScore :: Lens.Lens' DimensionValueContribution (Prelude.Maybe Prelude.Double)
dimensionValueContribution_contributionScore = Lens.lens (\DimensionValueContribution' {contributionScore} -> contributionScore) (\s@DimensionValueContribution' {} a -> s {contributionScore = a} :: DimensionValueContribution)

instance Core.FromJSON DimensionValueContribution where
  parseJSON =
    Core.withObject
      "DimensionValueContribution"
      ( \x ->
          DimensionValueContribution'
            Prelude.<$> (x Core..:? "DimensionValue")
            Prelude.<*> (x Core..:? "ContributionScore")
      )

instance Prelude.Hashable DimensionValueContribution where
  hashWithSalt salt' DimensionValueContribution' {..} =
    salt' `Prelude.hashWithSalt` contributionScore
      `Prelude.hashWithSalt` dimensionValue

instance Prelude.NFData DimensionValueContribution where
  rnf DimensionValueContribution' {..} =
    Prelude.rnf dimensionValue
      `Prelude.seq` Prelude.rnf contributionScore
