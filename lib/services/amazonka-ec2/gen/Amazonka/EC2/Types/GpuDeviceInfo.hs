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
-- Module      : Amazonka.EC2.Types.GpuDeviceInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.GpuDeviceInfo where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.GpuDeviceMemoryInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the GPU accelerators for the instance type.
--
-- /See:/ 'newGpuDeviceInfo' smart constructor.
data GpuDeviceInfo = GpuDeviceInfo'
  { -- | Describes the memory available to the GPU accelerator.
    memoryInfo :: Prelude.Maybe GpuDeviceMemoryInfo,
    -- | The manufacturer of the GPU accelerator.
    manufacturer :: Prelude.Maybe Prelude.Text,
    -- | The number of GPUs for the instance type.
    count :: Prelude.Maybe Prelude.Int,
    -- | The name of the GPU accelerator.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GpuDeviceInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'memoryInfo', 'gpuDeviceInfo_memoryInfo' - Describes the memory available to the GPU accelerator.
--
-- 'manufacturer', 'gpuDeviceInfo_manufacturer' - The manufacturer of the GPU accelerator.
--
-- 'count', 'gpuDeviceInfo_count' - The number of GPUs for the instance type.
--
-- 'name', 'gpuDeviceInfo_name' - The name of the GPU accelerator.
newGpuDeviceInfo ::
  GpuDeviceInfo
newGpuDeviceInfo =
  GpuDeviceInfo'
    { memoryInfo = Prelude.Nothing,
      manufacturer = Prelude.Nothing,
      count = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | Describes the memory available to the GPU accelerator.
gpuDeviceInfo_memoryInfo :: Lens.Lens' GpuDeviceInfo (Prelude.Maybe GpuDeviceMemoryInfo)
gpuDeviceInfo_memoryInfo = Lens.lens (\GpuDeviceInfo' {memoryInfo} -> memoryInfo) (\s@GpuDeviceInfo' {} a -> s {memoryInfo = a} :: GpuDeviceInfo)

-- | The manufacturer of the GPU accelerator.
gpuDeviceInfo_manufacturer :: Lens.Lens' GpuDeviceInfo (Prelude.Maybe Prelude.Text)
gpuDeviceInfo_manufacturer = Lens.lens (\GpuDeviceInfo' {manufacturer} -> manufacturer) (\s@GpuDeviceInfo' {} a -> s {manufacturer = a} :: GpuDeviceInfo)

-- | The number of GPUs for the instance type.
gpuDeviceInfo_count :: Lens.Lens' GpuDeviceInfo (Prelude.Maybe Prelude.Int)
gpuDeviceInfo_count = Lens.lens (\GpuDeviceInfo' {count} -> count) (\s@GpuDeviceInfo' {} a -> s {count = a} :: GpuDeviceInfo)

-- | The name of the GPU accelerator.
gpuDeviceInfo_name :: Lens.Lens' GpuDeviceInfo (Prelude.Maybe Prelude.Text)
gpuDeviceInfo_name = Lens.lens (\GpuDeviceInfo' {name} -> name) (\s@GpuDeviceInfo' {} a -> s {name = a} :: GpuDeviceInfo)

instance Core.FromXML GpuDeviceInfo where
  parseXML x =
    GpuDeviceInfo'
      Prelude.<$> (x Core..@? "memoryInfo")
      Prelude.<*> (x Core..@? "manufacturer")
      Prelude.<*> (x Core..@? "count")
      Prelude.<*> (x Core..@? "name")

instance Prelude.Hashable GpuDeviceInfo where
  hashWithSalt salt' GpuDeviceInfo' {..} =
    salt' `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` manufacturer
      `Prelude.hashWithSalt` memoryInfo

instance Prelude.NFData GpuDeviceInfo where
  rnf GpuDeviceInfo' {..} =
    Prelude.rnf memoryInfo
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf count
      `Prelude.seq` Prelude.rnf manufacturer
