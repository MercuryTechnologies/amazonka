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
-- Module      : Amazonka.Lightsail.Types.RelationalDatabaseHardware
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RelationalDatabaseHardware where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the hardware of a database.
--
-- /See:/ 'newRelationalDatabaseHardware' smart constructor.
data RelationalDatabaseHardware = RelationalDatabaseHardware'
  { -- | The number of vCPUs for the database.
    cpuCount :: Prelude.Maybe Prelude.Int,
    -- | The size of the disk for the database.
    diskSizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The amount of RAM in GB for the database.
    ramSizeInGb :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabaseHardware' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuCount', 'relationalDatabaseHardware_cpuCount' - The number of vCPUs for the database.
--
-- 'diskSizeInGb', 'relationalDatabaseHardware_diskSizeInGb' - The size of the disk for the database.
--
-- 'ramSizeInGb', 'relationalDatabaseHardware_ramSizeInGb' - The amount of RAM in GB for the database.
newRelationalDatabaseHardware ::
  RelationalDatabaseHardware
newRelationalDatabaseHardware =
  RelationalDatabaseHardware'
    { cpuCount =
        Prelude.Nothing,
      diskSizeInGb = Prelude.Nothing,
      ramSizeInGb = Prelude.Nothing
    }

-- | The number of vCPUs for the database.
relationalDatabaseHardware_cpuCount :: Lens.Lens' RelationalDatabaseHardware (Prelude.Maybe Prelude.Int)
relationalDatabaseHardware_cpuCount = Lens.lens (\RelationalDatabaseHardware' {cpuCount} -> cpuCount) (\s@RelationalDatabaseHardware' {} a -> s {cpuCount = a} :: RelationalDatabaseHardware)

-- | The size of the disk for the database.
relationalDatabaseHardware_diskSizeInGb :: Lens.Lens' RelationalDatabaseHardware (Prelude.Maybe Prelude.Int)
relationalDatabaseHardware_diskSizeInGb = Lens.lens (\RelationalDatabaseHardware' {diskSizeInGb} -> diskSizeInGb) (\s@RelationalDatabaseHardware' {} a -> s {diskSizeInGb = a} :: RelationalDatabaseHardware)

-- | The amount of RAM in GB for the database.
relationalDatabaseHardware_ramSizeInGb :: Lens.Lens' RelationalDatabaseHardware (Prelude.Maybe Prelude.Double)
relationalDatabaseHardware_ramSizeInGb = Lens.lens (\RelationalDatabaseHardware' {ramSizeInGb} -> ramSizeInGb) (\s@RelationalDatabaseHardware' {} a -> s {ramSizeInGb = a} :: RelationalDatabaseHardware)

instance Core.FromJSON RelationalDatabaseHardware where
  parseJSON =
    Core.withObject
      "RelationalDatabaseHardware"
      ( \x ->
          RelationalDatabaseHardware'
            Prelude.<$> (x Core..:? "cpuCount")
            Prelude.<*> (x Core..:? "diskSizeInGb")
            Prelude.<*> (x Core..:? "ramSizeInGb")
      )

instance Prelude.Hashable RelationalDatabaseHardware where
  hashWithSalt salt' RelationalDatabaseHardware' {..} =
    salt' `Prelude.hashWithSalt` ramSizeInGb
      `Prelude.hashWithSalt` diskSizeInGb
      `Prelude.hashWithSalt` cpuCount

instance Prelude.NFData RelationalDatabaseHardware where
  rnf RelationalDatabaseHardware' {..} =
    Prelude.rnf cpuCount
      `Prelude.seq` Prelude.rnf ramSizeInGb
      `Prelude.seq` Prelude.rnf diskSizeInGb
