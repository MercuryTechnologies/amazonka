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
-- Module      : Amazonka.IoTAnalytics.Types.ResourceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ResourceConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.IoTAnalytics.Types.ComputeType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration of the resource used to execute the @containerAction@.
--
-- /See:/ 'newResourceConfiguration' smart constructor.
data ResourceConfiguration = ResourceConfiguration'
  { -- | The type of the compute resource used to execute the @containerAction@.
    -- Possible values are: @ACU_1@ (vCPU=4, memory=16 GiB) or @ACU_2@ (vCPU=8,
    -- memory=32 GiB).
    computeType :: ComputeType,
    -- | The size, in GB, of the persistent storage available to the resource
    -- instance used to execute the @containerAction@ (min: 1, max: 50).
    volumeSizeInGB :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'computeType', 'resourceConfiguration_computeType' - The type of the compute resource used to execute the @containerAction@.
-- Possible values are: @ACU_1@ (vCPU=4, memory=16 GiB) or @ACU_2@ (vCPU=8,
-- memory=32 GiB).
--
-- 'volumeSizeInGB', 'resourceConfiguration_volumeSizeInGB' - The size, in GB, of the persistent storage available to the resource
-- instance used to execute the @containerAction@ (min: 1, max: 50).
newResourceConfiguration ::
  -- | 'computeType'
  ComputeType ->
  -- | 'volumeSizeInGB'
  Prelude.Natural ->
  ResourceConfiguration
newResourceConfiguration
  pComputeType_
  pVolumeSizeInGB_ =
    ResourceConfiguration'
      { computeType = pComputeType_,
        volumeSizeInGB = pVolumeSizeInGB_
      }

-- | The type of the compute resource used to execute the @containerAction@.
-- Possible values are: @ACU_1@ (vCPU=4, memory=16 GiB) or @ACU_2@ (vCPU=8,
-- memory=32 GiB).
resourceConfiguration_computeType :: Lens.Lens' ResourceConfiguration ComputeType
resourceConfiguration_computeType = Lens.lens (\ResourceConfiguration' {computeType} -> computeType) (\s@ResourceConfiguration' {} a -> s {computeType = a} :: ResourceConfiguration)

-- | The size, in GB, of the persistent storage available to the resource
-- instance used to execute the @containerAction@ (min: 1, max: 50).
resourceConfiguration_volumeSizeInGB :: Lens.Lens' ResourceConfiguration Prelude.Natural
resourceConfiguration_volumeSizeInGB = Lens.lens (\ResourceConfiguration' {volumeSizeInGB} -> volumeSizeInGB) (\s@ResourceConfiguration' {} a -> s {volumeSizeInGB = a} :: ResourceConfiguration)

instance Core.FromJSON ResourceConfiguration where
  parseJSON =
    Core.withObject
      "ResourceConfiguration"
      ( \x ->
          ResourceConfiguration'
            Prelude.<$> (x Core..: "computeType")
            Prelude.<*> (x Core..: "volumeSizeInGB")
      )

instance Prelude.Hashable ResourceConfiguration where
  hashWithSalt salt' ResourceConfiguration' {..} =
    salt' `Prelude.hashWithSalt` volumeSizeInGB
      `Prelude.hashWithSalt` computeType

instance Prelude.NFData ResourceConfiguration where
  rnf ResourceConfiguration' {..} =
    Prelude.rnf computeType
      `Prelude.seq` Prelude.rnf volumeSizeInGB

instance Core.ToJSON ResourceConfiguration where
  toJSON ResourceConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("computeType" Core..= computeType),
            Prelude.Just
              ("volumeSizeInGB" Core..= volumeSizeInGB)
          ]
      )
