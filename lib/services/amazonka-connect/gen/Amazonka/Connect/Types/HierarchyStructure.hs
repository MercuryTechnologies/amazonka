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
-- Module      : Amazonka.Connect.Types.HierarchyStructure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.HierarchyStructure where

import Amazonka.Connect.Types.HierarchyLevel
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a hierarchy structure.
--
-- /See:/ 'newHierarchyStructure' smart constructor.
data HierarchyStructure = HierarchyStructure'
  { -- | Information about level five.
    levelFive :: Prelude.Maybe HierarchyLevel,
    -- | Information about level three.
    levelThree :: Prelude.Maybe HierarchyLevel,
    -- | Information about level four.
    levelFour :: Prelude.Maybe HierarchyLevel,
    -- | Information about level two.
    levelTwo :: Prelude.Maybe HierarchyLevel,
    -- | Information about level one.
    levelOne :: Prelude.Maybe HierarchyLevel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HierarchyStructure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'levelFive', 'hierarchyStructure_levelFive' - Information about level five.
--
-- 'levelThree', 'hierarchyStructure_levelThree' - Information about level three.
--
-- 'levelFour', 'hierarchyStructure_levelFour' - Information about level four.
--
-- 'levelTwo', 'hierarchyStructure_levelTwo' - Information about level two.
--
-- 'levelOne', 'hierarchyStructure_levelOne' - Information about level one.
newHierarchyStructure ::
  HierarchyStructure
newHierarchyStructure =
  HierarchyStructure'
    { levelFive = Prelude.Nothing,
      levelThree = Prelude.Nothing,
      levelFour = Prelude.Nothing,
      levelTwo = Prelude.Nothing,
      levelOne = Prelude.Nothing
    }

-- | Information about level five.
hierarchyStructure_levelFive :: Lens.Lens' HierarchyStructure (Prelude.Maybe HierarchyLevel)
hierarchyStructure_levelFive = Lens.lens (\HierarchyStructure' {levelFive} -> levelFive) (\s@HierarchyStructure' {} a -> s {levelFive = a} :: HierarchyStructure)

-- | Information about level three.
hierarchyStructure_levelThree :: Lens.Lens' HierarchyStructure (Prelude.Maybe HierarchyLevel)
hierarchyStructure_levelThree = Lens.lens (\HierarchyStructure' {levelThree} -> levelThree) (\s@HierarchyStructure' {} a -> s {levelThree = a} :: HierarchyStructure)

-- | Information about level four.
hierarchyStructure_levelFour :: Lens.Lens' HierarchyStructure (Prelude.Maybe HierarchyLevel)
hierarchyStructure_levelFour = Lens.lens (\HierarchyStructure' {levelFour} -> levelFour) (\s@HierarchyStructure' {} a -> s {levelFour = a} :: HierarchyStructure)

-- | Information about level two.
hierarchyStructure_levelTwo :: Lens.Lens' HierarchyStructure (Prelude.Maybe HierarchyLevel)
hierarchyStructure_levelTwo = Lens.lens (\HierarchyStructure' {levelTwo} -> levelTwo) (\s@HierarchyStructure' {} a -> s {levelTwo = a} :: HierarchyStructure)

-- | Information about level one.
hierarchyStructure_levelOne :: Lens.Lens' HierarchyStructure (Prelude.Maybe HierarchyLevel)
hierarchyStructure_levelOne = Lens.lens (\HierarchyStructure' {levelOne} -> levelOne) (\s@HierarchyStructure' {} a -> s {levelOne = a} :: HierarchyStructure)

instance Core.FromJSON HierarchyStructure where
  parseJSON =
    Core.withObject
      "HierarchyStructure"
      ( \x ->
          HierarchyStructure'
            Prelude.<$> (x Core..:? "LevelFive")
            Prelude.<*> (x Core..:? "LevelThree")
            Prelude.<*> (x Core..:? "LevelFour")
            Prelude.<*> (x Core..:? "LevelTwo")
            Prelude.<*> (x Core..:? "LevelOne")
      )

instance Prelude.Hashable HierarchyStructure where
  hashWithSalt salt' HierarchyStructure' {..} =
    salt' `Prelude.hashWithSalt` levelOne
      `Prelude.hashWithSalt` levelTwo
      `Prelude.hashWithSalt` levelFour
      `Prelude.hashWithSalt` levelThree
      `Prelude.hashWithSalt` levelFive

instance Prelude.NFData HierarchyStructure where
  rnf HierarchyStructure' {..} =
    Prelude.rnf levelFive
      `Prelude.seq` Prelude.rnf levelOne
      `Prelude.seq` Prelude.rnf levelTwo
      `Prelude.seq` Prelude.rnf levelFour
      `Prelude.seq` Prelude.rnf levelThree
