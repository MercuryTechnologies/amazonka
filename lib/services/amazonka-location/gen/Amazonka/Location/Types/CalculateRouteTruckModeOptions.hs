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
-- Module      : Amazonka.Location.Types.CalculateRouteTruckModeOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.CalculateRouteTruckModeOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Location.Types.TruckDimensions
import Amazonka.Location.Types.TruckWeight
import qualified Amazonka.Prelude as Prelude

-- | Contains details about additional route preferences for requests that
-- specify @TravelMode@ as @Truck@.
--
-- /See:/ 'newCalculateRouteTruckModeOptions' smart constructor.
data CalculateRouteTruckModeOptions = CalculateRouteTruckModeOptions'
  { -- | Specifies the truck\'s weight specifications including total weight and
    -- unit of measurement. Used to avoid roads that can\'t support the
    -- truck\'s weight.
    weight :: Prelude.Maybe TruckWeight,
    -- | Avoids ferries when calculating routes.
    --
    -- Default Value: @false@
    --
    -- Valid Values: @false@ | @true@
    avoidTolls :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the truck\'s dimension specifications including length,
    -- height, width, and unit of measurement. Used to avoid roads that can\'t
    -- support the truck\'s dimensions.
    dimensions :: Prelude.Maybe TruckDimensions,
    -- | Avoids ferries when calculating routes.
    --
    -- Default Value: @false@
    --
    -- Valid Values: @false@ | @true@
    avoidFerries :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculateRouteTruckModeOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'weight', 'calculateRouteTruckModeOptions_weight' - Specifies the truck\'s weight specifications including total weight and
-- unit of measurement. Used to avoid roads that can\'t support the
-- truck\'s weight.
--
-- 'avoidTolls', 'calculateRouteTruckModeOptions_avoidTolls' - Avoids ferries when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
--
-- 'dimensions', 'calculateRouteTruckModeOptions_dimensions' - Specifies the truck\'s dimension specifications including length,
-- height, width, and unit of measurement. Used to avoid roads that can\'t
-- support the truck\'s dimensions.
--
-- 'avoidFerries', 'calculateRouteTruckModeOptions_avoidFerries' - Avoids ferries when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
newCalculateRouteTruckModeOptions ::
  CalculateRouteTruckModeOptions
newCalculateRouteTruckModeOptions =
  CalculateRouteTruckModeOptions'
    { weight =
        Prelude.Nothing,
      avoidTolls = Prelude.Nothing,
      dimensions = Prelude.Nothing,
      avoidFerries = Prelude.Nothing
    }

-- | Specifies the truck\'s weight specifications including total weight and
-- unit of measurement. Used to avoid roads that can\'t support the
-- truck\'s weight.
calculateRouteTruckModeOptions_weight :: Lens.Lens' CalculateRouteTruckModeOptions (Prelude.Maybe TruckWeight)
calculateRouteTruckModeOptions_weight = Lens.lens (\CalculateRouteTruckModeOptions' {weight} -> weight) (\s@CalculateRouteTruckModeOptions' {} a -> s {weight = a} :: CalculateRouteTruckModeOptions)

-- | Avoids ferries when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
calculateRouteTruckModeOptions_avoidTolls :: Lens.Lens' CalculateRouteTruckModeOptions (Prelude.Maybe Prelude.Bool)
calculateRouteTruckModeOptions_avoidTolls = Lens.lens (\CalculateRouteTruckModeOptions' {avoidTolls} -> avoidTolls) (\s@CalculateRouteTruckModeOptions' {} a -> s {avoidTolls = a} :: CalculateRouteTruckModeOptions)

-- | Specifies the truck\'s dimension specifications including length,
-- height, width, and unit of measurement. Used to avoid roads that can\'t
-- support the truck\'s dimensions.
calculateRouteTruckModeOptions_dimensions :: Lens.Lens' CalculateRouteTruckModeOptions (Prelude.Maybe TruckDimensions)
calculateRouteTruckModeOptions_dimensions = Lens.lens (\CalculateRouteTruckModeOptions' {dimensions} -> dimensions) (\s@CalculateRouteTruckModeOptions' {} a -> s {dimensions = a} :: CalculateRouteTruckModeOptions)

-- | Avoids ferries when calculating routes.
--
-- Default Value: @false@
--
-- Valid Values: @false@ | @true@
calculateRouteTruckModeOptions_avoidFerries :: Lens.Lens' CalculateRouteTruckModeOptions (Prelude.Maybe Prelude.Bool)
calculateRouteTruckModeOptions_avoidFerries = Lens.lens (\CalculateRouteTruckModeOptions' {avoidFerries} -> avoidFerries) (\s@CalculateRouteTruckModeOptions' {} a -> s {avoidFerries = a} :: CalculateRouteTruckModeOptions)

instance
  Prelude.Hashable
    CalculateRouteTruckModeOptions
  where
  hashWithSalt
    salt'
    CalculateRouteTruckModeOptions' {..} =
      salt' `Prelude.hashWithSalt` avoidFerries
        `Prelude.hashWithSalt` dimensions
        `Prelude.hashWithSalt` avoidTolls
        `Prelude.hashWithSalt` weight

instance
  Prelude.NFData
    CalculateRouteTruckModeOptions
  where
  rnf CalculateRouteTruckModeOptions' {..} =
    Prelude.rnf weight
      `Prelude.seq` Prelude.rnf avoidFerries
      `Prelude.seq` Prelude.rnf dimensions
      `Prelude.seq` Prelude.rnf avoidTolls

instance Core.ToJSON CalculateRouteTruckModeOptions where
  toJSON CalculateRouteTruckModeOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Weight" Core..=) Prelude.<$> weight,
            ("AvoidTolls" Core..=) Prelude.<$> avoidTolls,
            ("Dimensions" Core..=) Prelude.<$> dimensions,
            ("AvoidFerries" Core..=) Prelude.<$> avoidFerries
          ]
      )
