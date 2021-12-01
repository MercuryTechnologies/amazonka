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
-- Module      : Amazonka.OpsWorks.Types.LoadBasedAutoScalingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorks.Types.LoadBasedAutoScalingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.OpsWorks.Types.AutoScalingThresholds
import qualified Amazonka.Prelude as Prelude

-- | Describes a layer\'s load-based auto scaling configuration.
--
-- /See:/ 'newLoadBasedAutoScalingConfiguration' smart constructor.
data LoadBasedAutoScalingConfiguration = LoadBasedAutoScalingConfiguration'
  { -- | An @AutoScalingThresholds@ object that describes the upscaling
    -- configuration, which defines how and when AWS OpsWorks Stacks increases
    -- the number of instances.
    upScaling :: Prelude.Maybe AutoScalingThresholds,
    -- | Whether load-based auto scaling is enabled for the layer.
    enable :: Prelude.Maybe Prelude.Bool,
    -- | An @AutoScalingThresholds@ object that describes the downscaling
    -- configuration, which defines how and when AWS OpsWorks Stacks reduces
    -- the number of instances.
    downScaling :: Prelude.Maybe AutoScalingThresholds,
    -- | The layer ID.
    layerId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBasedAutoScalingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upScaling', 'loadBasedAutoScalingConfiguration_upScaling' - An @AutoScalingThresholds@ object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks Stacks increases
-- the number of instances.
--
-- 'enable', 'loadBasedAutoScalingConfiguration_enable' - Whether load-based auto scaling is enabled for the layer.
--
-- 'downScaling', 'loadBasedAutoScalingConfiguration_downScaling' - An @AutoScalingThresholds@ object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks Stacks reduces
-- the number of instances.
--
-- 'layerId', 'loadBasedAutoScalingConfiguration_layerId' - The layer ID.
newLoadBasedAutoScalingConfiguration ::
  LoadBasedAutoScalingConfiguration
newLoadBasedAutoScalingConfiguration =
  LoadBasedAutoScalingConfiguration'
    { upScaling =
        Prelude.Nothing,
      enable = Prelude.Nothing,
      downScaling = Prelude.Nothing,
      layerId = Prelude.Nothing
    }

-- | An @AutoScalingThresholds@ object that describes the upscaling
-- configuration, which defines how and when AWS OpsWorks Stacks increases
-- the number of instances.
loadBasedAutoScalingConfiguration_upScaling :: Lens.Lens' LoadBasedAutoScalingConfiguration (Prelude.Maybe AutoScalingThresholds)
loadBasedAutoScalingConfiguration_upScaling = Lens.lens (\LoadBasedAutoScalingConfiguration' {upScaling} -> upScaling) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {upScaling = a} :: LoadBasedAutoScalingConfiguration)

-- | Whether load-based auto scaling is enabled for the layer.
loadBasedAutoScalingConfiguration_enable :: Lens.Lens' LoadBasedAutoScalingConfiguration (Prelude.Maybe Prelude.Bool)
loadBasedAutoScalingConfiguration_enable = Lens.lens (\LoadBasedAutoScalingConfiguration' {enable} -> enable) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {enable = a} :: LoadBasedAutoScalingConfiguration)

-- | An @AutoScalingThresholds@ object that describes the downscaling
-- configuration, which defines how and when AWS OpsWorks Stacks reduces
-- the number of instances.
loadBasedAutoScalingConfiguration_downScaling :: Lens.Lens' LoadBasedAutoScalingConfiguration (Prelude.Maybe AutoScalingThresholds)
loadBasedAutoScalingConfiguration_downScaling = Lens.lens (\LoadBasedAutoScalingConfiguration' {downScaling} -> downScaling) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {downScaling = a} :: LoadBasedAutoScalingConfiguration)

-- | The layer ID.
loadBasedAutoScalingConfiguration_layerId :: Lens.Lens' LoadBasedAutoScalingConfiguration (Prelude.Maybe Prelude.Text)
loadBasedAutoScalingConfiguration_layerId = Lens.lens (\LoadBasedAutoScalingConfiguration' {layerId} -> layerId) (\s@LoadBasedAutoScalingConfiguration' {} a -> s {layerId = a} :: LoadBasedAutoScalingConfiguration)

instance
  Core.FromJSON
    LoadBasedAutoScalingConfiguration
  where
  parseJSON =
    Core.withObject
      "LoadBasedAutoScalingConfiguration"
      ( \x ->
          LoadBasedAutoScalingConfiguration'
            Prelude.<$> (x Core..:? "UpScaling")
            Prelude.<*> (x Core..:? "Enable")
            Prelude.<*> (x Core..:? "DownScaling")
            Prelude.<*> (x Core..:? "LayerId")
      )

instance
  Prelude.Hashable
    LoadBasedAutoScalingConfiguration
  where
  hashWithSalt
    salt'
    LoadBasedAutoScalingConfiguration' {..} =
      salt' `Prelude.hashWithSalt` layerId
        `Prelude.hashWithSalt` downScaling
        `Prelude.hashWithSalt` enable
        `Prelude.hashWithSalt` upScaling

instance
  Prelude.NFData
    LoadBasedAutoScalingConfiguration
  where
  rnf LoadBasedAutoScalingConfiguration' {..} =
    Prelude.rnf upScaling
      `Prelude.seq` Prelude.rnf layerId
      `Prelude.seq` Prelude.rnf downScaling
      `Prelude.seq` Prelude.rnf enable
