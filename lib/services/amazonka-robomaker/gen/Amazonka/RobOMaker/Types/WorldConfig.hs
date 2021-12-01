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
-- Module      : Amazonka.RobOMaker.Types.WorldConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.WorldConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Configuration information for a world.
--
-- /See:/ 'newWorldConfig' smart constructor.
data WorldConfig = WorldConfig'
  { -- | The world generated by Simulation WorldForge.
    world :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorldConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'world', 'worldConfig_world' - The world generated by Simulation WorldForge.
newWorldConfig ::
  WorldConfig
newWorldConfig =
  WorldConfig' {world = Prelude.Nothing}

-- | The world generated by Simulation WorldForge.
worldConfig_world :: Lens.Lens' WorldConfig (Prelude.Maybe Prelude.Text)
worldConfig_world = Lens.lens (\WorldConfig' {world} -> world) (\s@WorldConfig' {} a -> s {world = a} :: WorldConfig)

instance Core.FromJSON WorldConfig where
  parseJSON =
    Core.withObject
      "WorldConfig"
      ( \x ->
          WorldConfig' Prelude.<$> (x Core..:? "world")
      )

instance Prelude.Hashable WorldConfig where
  hashWithSalt salt' WorldConfig' {..} =
    salt' `Prelude.hashWithSalt` world

instance Prelude.NFData WorldConfig where
  rnf WorldConfig' {..} = Prelude.rnf world

instance Core.ToJSON WorldConfig where
  toJSON WorldConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [("world" Core..=) Prelude.<$> world]
      )
