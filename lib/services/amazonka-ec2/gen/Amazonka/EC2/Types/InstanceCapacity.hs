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
-- Module      : Amazonka.EC2.Types.InstanceCapacity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceCapacity where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the number of instances that can be launched onto the
-- Dedicated Host.
--
-- /See:/ 'newInstanceCapacity' smart constructor.
data InstanceCapacity = InstanceCapacity'
  { -- | The number of instances that can be launched onto the Dedicated Host
    -- based on the host\'s available capacity.
    availableCapacity :: Prelude.Maybe Prelude.Int,
    -- | The instance type supported by the Dedicated Host.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The total number of instances that can be launched onto the Dedicated
    -- Host if there are no instances running on it.
    totalCapacity :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceCapacity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'availableCapacity', 'instanceCapacity_availableCapacity' - The number of instances that can be launched onto the Dedicated Host
-- based on the host\'s available capacity.
--
-- 'instanceType', 'instanceCapacity_instanceType' - The instance type supported by the Dedicated Host.
--
-- 'totalCapacity', 'instanceCapacity_totalCapacity' - The total number of instances that can be launched onto the Dedicated
-- Host if there are no instances running on it.
newInstanceCapacity ::
  InstanceCapacity
newInstanceCapacity =
  InstanceCapacity'
    { availableCapacity =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      totalCapacity = Prelude.Nothing
    }

-- | The number of instances that can be launched onto the Dedicated Host
-- based on the host\'s available capacity.
instanceCapacity_availableCapacity :: Lens.Lens' InstanceCapacity (Prelude.Maybe Prelude.Int)
instanceCapacity_availableCapacity = Lens.lens (\InstanceCapacity' {availableCapacity} -> availableCapacity) (\s@InstanceCapacity' {} a -> s {availableCapacity = a} :: InstanceCapacity)

-- | The instance type supported by the Dedicated Host.
instanceCapacity_instanceType :: Lens.Lens' InstanceCapacity (Prelude.Maybe Prelude.Text)
instanceCapacity_instanceType = Lens.lens (\InstanceCapacity' {instanceType} -> instanceType) (\s@InstanceCapacity' {} a -> s {instanceType = a} :: InstanceCapacity)

-- | The total number of instances that can be launched onto the Dedicated
-- Host if there are no instances running on it.
instanceCapacity_totalCapacity :: Lens.Lens' InstanceCapacity (Prelude.Maybe Prelude.Int)
instanceCapacity_totalCapacity = Lens.lens (\InstanceCapacity' {totalCapacity} -> totalCapacity) (\s@InstanceCapacity' {} a -> s {totalCapacity = a} :: InstanceCapacity)

instance Core.FromXML InstanceCapacity where
  parseXML x =
    InstanceCapacity'
      Prelude.<$> (x Core..@? "availableCapacity")
      Prelude.<*> (x Core..@? "instanceType")
      Prelude.<*> (x Core..@? "totalCapacity")

instance Prelude.Hashable InstanceCapacity where
  hashWithSalt salt' InstanceCapacity' {..} =
    salt' `Prelude.hashWithSalt` totalCapacity
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` availableCapacity

instance Prelude.NFData InstanceCapacity where
  rnf InstanceCapacity' {..} =
    Prelude.rnf availableCapacity
      `Prelude.seq` Prelude.rnf totalCapacity
      `Prelude.seq` Prelude.rnf instanceType
