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
-- Module      : Amazonka.Lightsail.Types.RelationalDatabaseBlueprint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RelationalDatabaseBlueprint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Lightsail.Types.RelationalDatabaseEngine
import qualified Amazonka.Prelude as Prelude

-- | Describes a database image, or blueprint. A blueprint describes the
-- major engine version of a database.
--
-- /See:/ 'newRelationalDatabaseBlueprint' smart constructor.
data RelationalDatabaseBlueprint = RelationalDatabaseBlueprint'
  { -- | The database engine version for the database blueprint (for example,
    -- @5.7.23@).
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A Boolean value indicating whether the engine version is the default for
    -- the database blueprint.
    isEngineDefault :: Prelude.Maybe Prelude.Bool,
    -- | The description of the database engine version for the database
    -- blueprint.
    engineVersionDescription :: Prelude.Maybe Prelude.Text,
    -- | The database software of the database blueprint (for example, @MySQL@).
    engine :: Prelude.Maybe RelationalDatabaseEngine,
    -- | The ID for the database blueprint.
    blueprintId :: Prelude.Maybe Prelude.Text,
    -- | The description of the database engine for the database blueprint.
    engineDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabaseBlueprint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'relationalDatabaseBlueprint_engineVersion' - The database engine version for the database blueprint (for example,
-- @5.7.23@).
--
-- 'isEngineDefault', 'relationalDatabaseBlueprint_isEngineDefault' - A Boolean value indicating whether the engine version is the default for
-- the database blueprint.
--
-- 'engineVersionDescription', 'relationalDatabaseBlueprint_engineVersionDescription' - The description of the database engine version for the database
-- blueprint.
--
-- 'engine', 'relationalDatabaseBlueprint_engine' - The database software of the database blueprint (for example, @MySQL@).
--
-- 'blueprintId', 'relationalDatabaseBlueprint_blueprintId' - The ID for the database blueprint.
--
-- 'engineDescription', 'relationalDatabaseBlueprint_engineDescription' - The description of the database engine for the database blueprint.
newRelationalDatabaseBlueprint ::
  RelationalDatabaseBlueprint
newRelationalDatabaseBlueprint =
  RelationalDatabaseBlueprint'
    { engineVersion =
        Prelude.Nothing,
      isEngineDefault = Prelude.Nothing,
      engineVersionDescription = Prelude.Nothing,
      engine = Prelude.Nothing,
      blueprintId = Prelude.Nothing,
      engineDescription = Prelude.Nothing
    }

-- | The database engine version for the database blueprint (for example,
-- @5.7.23@).
relationalDatabaseBlueprint_engineVersion :: Lens.Lens' RelationalDatabaseBlueprint (Prelude.Maybe Prelude.Text)
relationalDatabaseBlueprint_engineVersion = Lens.lens (\RelationalDatabaseBlueprint' {engineVersion} -> engineVersion) (\s@RelationalDatabaseBlueprint' {} a -> s {engineVersion = a} :: RelationalDatabaseBlueprint)

-- | A Boolean value indicating whether the engine version is the default for
-- the database blueprint.
relationalDatabaseBlueprint_isEngineDefault :: Lens.Lens' RelationalDatabaseBlueprint (Prelude.Maybe Prelude.Bool)
relationalDatabaseBlueprint_isEngineDefault = Lens.lens (\RelationalDatabaseBlueprint' {isEngineDefault} -> isEngineDefault) (\s@RelationalDatabaseBlueprint' {} a -> s {isEngineDefault = a} :: RelationalDatabaseBlueprint)

-- | The description of the database engine version for the database
-- blueprint.
relationalDatabaseBlueprint_engineVersionDescription :: Lens.Lens' RelationalDatabaseBlueprint (Prelude.Maybe Prelude.Text)
relationalDatabaseBlueprint_engineVersionDescription = Lens.lens (\RelationalDatabaseBlueprint' {engineVersionDescription} -> engineVersionDescription) (\s@RelationalDatabaseBlueprint' {} a -> s {engineVersionDescription = a} :: RelationalDatabaseBlueprint)

-- | The database software of the database blueprint (for example, @MySQL@).
relationalDatabaseBlueprint_engine :: Lens.Lens' RelationalDatabaseBlueprint (Prelude.Maybe RelationalDatabaseEngine)
relationalDatabaseBlueprint_engine = Lens.lens (\RelationalDatabaseBlueprint' {engine} -> engine) (\s@RelationalDatabaseBlueprint' {} a -> s {engine = a} :: RelationalDatabaseBlueprint)

-- | The ID for the database blueprint.
relationalDatabaseBlueprint_blueprintId :: Lens.Lens' RelationalDatabaseBlueprint (Prelude.Maybe Prelude.Text)
relationalDatabaseBlueprint_blueprintId = Lens.lens (\RelationalDatabaseBlueprint' {blueprintId} -> blueprintId) (\s@RelationalDatabaseBlueprint' {} a -> s {blueprintId = a} :: RelationalDatabaseBlueprint)

-- | The description of the database engine for the database blueprint.
relationalDatabaseBlueprint_engineDescription :: Lens.Lens' RelationalDatabaseBlueprint (Prelude.Maybe Prelude.Text)
relationalDatabaseBlueprint_engineDescription = Lens.lens (\RelationalDatabaseBlueprint' {engineDescription} -> engineDescription) (\s@RelationalDatabaseBlueprint' {} a -> s {engineDescription = a} :: RelationalDatabaseBlueprint)

instance Core.FromJSON RelationalDatabaseBlueprint where
  parseJSON =
    Core.withObject
      "RelationalDatabaseBlueprint"
      ( \x ->
          RelationalDatabaseBlueprint'
            Prelude.<$> (x Core..:? "engineVersion")
            Prelude.<*> (x Core..:? "isEngineDefault")
            Prelude.<*> (x Core..:? "engineVersionDescription")
            Prelude.<*> (x Core..:? "engine")
            Prelude.<*> (x Core..:? "blueprintId")
            Prelude.<*> (x Core..:? "engineDescription")
      )

instance Prelude.Hashable RelationalDatabaseBlueprint where
  hashWithSalt salt' RelationalDatabaseBlueprint' {..} =
    salt' `Prelude.hashWithSalt` engineDescription
      `Prelude.hashWithSalt` blueprintId
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` engineVersionDescription
      `Prelude.hashWithSalt` isEngineDefault
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData RelationalDatabaseBlueprint where
  rnf RelationalDatabaseBlueprint' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf engineDescription
      `Prelude.seq` Prelude.rnf blueprintId
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf engineVersionDescription
      `Prelude.seq` Prelude.rnf isEngineDefault
