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
-- Module      : Amazonka.Location.Types.TruckDimensions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Location.Types.TruckDimensions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Location.Types.DimensionUnit
import qualified Amazonka.Prelude as Prelude

-- | Contains details about the truck dimensions in the unit of measurement
-- that you specify. Used to filter out roads that can\'t support or allow
-- the specified dimensions for requests that specify @TravelMode@ as
-- @Truck@.
--
-- /See:/ 'newTruckDimensions' smart constructor.
data TruckDimensions = TruckDimensions'
  { -- | The length of the truck.
    --
    -- -   For example, @15.5@.
    length :: Prelude.Maybe Prelude.Double,
    -- | The height of the truck.
    --
    -- -   For example, @4.5@.
    height :: Prelude.Maybe Prelude.Double,
    -- | The width of the truck.
    --
    -- -   For example, @4.5@.
    width :: Prelude.Maybe Prelude.Double,
    -- | Specifies the unit of measurement for the truck dimensions.
    --
    -- Default Value: @Meters@
    unit :: Prelude.Maybe DimensionUnit
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TruckDimensions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'length', 'truckDimensions_length' - The length of the truck.
--
-- -   For example, @15.5@.
--
-- 'height', 'truckDimensions_height' - The height of the truck.
--
-- -   For example, @4.5@.
--
-- 'width', 'truckDimensions_width' - The width of the truck.
--
-- -   For example, @4.5@.
--
-- 'unit', 'truckDimensions_unit' - Specifies the unit of measurement for the truck dimensions.
--
-- Default Value: @Meters@
newTruckDimensions ::
  TruckDimensions
newTruckDimensions =
  TruckDimensions'
    { length = Prelude.Nothing,
      height = Prelude.Nothing,
      width = Prelude.Nothing,
      unit = Prelude.Nothing
    }

-- | The length of the truck.
--
-- -   For example, @15.5@.
truckDimensions_length :: Lens.Lens' TruckDimensions (Prelude.Maybe Prelude.Double)
truckDimensions_length = Lens.lens (\TruckDimensions' {length} -> length) (\s@TruckDimensions' {} a -> s {length = a} :: TruckDimensions)

-- | The height of the truck.
--
-- -   For example, @4.5@.
truckDimensions_height :: Lens.Lens' TruckDimensions (Prelude.Maybe Prelude.Double)
truckDimensions_height = Lens.lens (\TruckDimensions' {height} -> height) (\s@TruckDimensions' {} a -> s {height = a} :: TruckDimensions)

-- | The width of the truck.
--
-- -   For example, @4.5@.
truckDimensions_width :: Lens.Lens' TruckDimensions (Prelude.Maybe Prelude.Double)
truckDimensions_width = Lens.lens (\TruckDimensions' {width} -> width) (\s@TruckDimensions' {} a -> s {width = a} :: TruckDimensions)

-- | Specifies the unit of measurement for the truck dimensions.
--
-- Default Value: @Meters@
truckDimensions_unit :: Lens.Lens' TruckDimensions (Prelude.Maybe DimensionUnit)
truckDimensions_unit = Lens.lens (\TruckDimensions' {unit} -> unit) (\s@TruckDimensions' {} a -> s {unit = a} :: TruckDimensions)

instance Prelude.Hashable TruckDimensions where
  hashWithSalt salt' TruckDimensions' {..} =
    salt' `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` width
      `Prelude.hashWithSalt` height
      `Prelude.hashWithSalt` length

instance Prelude.NFData TruckDimensions where
  rnf TruckDimensions' {..} =
    Prelude.rnf length `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf width
      `Prelude.seq` Prelude.rnf height

instance Core.ToJSON TruckDimensions where
  toJSON TruckDimensions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Length" Core..=) Prelude.<$> length,
            ("Height" Core..=) Prelude.<$> height,
            ("Width" Core..=) Prelude.<$> width,
            ("Unit" Core..=) Prelude.<$> unit
          ]
      )
