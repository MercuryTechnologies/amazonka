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
-- Module      : Amazonka.SnowDeviceManagement.Types.Capacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SnowDeviceManagement.Types.Capacity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The physical capacity of the Amazon Web Services Snow Family device.
--
-- /See:/ 'newCapacity' smart constructor.
data Capacity = Capacity'
  { -- | The amount of capacity used on the device.
    used :: Prelude.Maybe Prelude.Integer,
    -- | The name of the type of capacity, such as memory.
    name :: Prelude.Maybe Prelude.Text,
    -- | The total capacity on the device.
    total :: Prelude.Maybe Prelude.Integer,
    -- | The unit of measure for the type of capacity.
    unit :: Prelude.Maybe Prelude.Text,
    -- | The amount of capacity available for use on the device.
    available :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Capacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'used', 'capacity_used' - The amount of capacity used on the device.
--
-- 'name', 'capacity_name' - The name of the type of capacity, such as memory.
--
-- 'total', 'capacity_total' - The total capacity on the device.
--
-- 'unit', 'capacity_unit' - The unit of measure for the type of capacity.
--
-- 'available', 'capacity_available' - The amount of capacity available for use on the device.
newCapacity ::
  Capacity
newCapacity =
  Capacity'
    { used = Prelude.Nothing,
      name = Prelude.Nothing,
      total = Prelude.Nothing,
      unit = Prelude.Nothing,
      available = Prelude.Nothing
    }

-- | The amount of capacity used on the device.
capacity_used :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Integer)
capacity_used = Lens.lens (\Capacity' {used} -> used) (\s@Capacity' {} a -> s {used = a} :: Capacity)

-- | The name of the type of capacity, such as memory.
capacity_name :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Text)
capacity_name = Lens.lens (\Capacity' {name} -> name) (\s@Capacity' {} a -> s {name = a} :: Capacity)

-- | The total capacity on the device.
capacity_total :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Integer)
capacity_total = Lens.lens (\Capacity' {total} -> total) (\s@Capacity' {} a -> s {total = a} :: Capacity)

-- | The unit of measure for the type of capacity.
capacity_unit :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Text)
capacity_unit = Lens.lens (\Capacity' {unit} -> unit) (\s@Capacity' {} a -> s {unit = a} :: Capacity)

-- | The amount of capacity available for use on the device.
capacity_available :: Lens.Lens' Capacity (Prelude.Maybe Prelude.Integer)
capacity_available = Lens.lens (\Capacity' {available} -> available) (\s@Capacity' {} a -> s {available = a} :: Capacity)

instance Core.FromJSON Capacity where
  parseJSON =
    Core.withObject
      "Capacity"
      ( \x ->
          Capacity'
            Prelude.<$> (x Core..:? "used")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "total")
            Prelude.<*> (x Core..:? "unit")
            Prelude.<*> (x Core..:? "available")
      )

instance Prelude.Hashable Capacity where
  hashWithSalt salt' Capacity' {..} =
    salt' `Prelude.hashWithSalt` available
      `Prelude.hashWithSalt` unit
      `Prelude.hashWithSalt` total
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` used

instance Prelude.NFData Capacity where
  rnf Capacity' {..} =
    Prelude.rnf used
      `Prelude.seq` Prelude.rnf available
      `Prelude.seq` Prelude.rnf unit
      `Prelude.seq` Prelude.rnf total
      `Prelude.seq` Prelude.rnf name
