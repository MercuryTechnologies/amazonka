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
-- Module      : Amazonka.Lightsail.Types.RelationalDatabaseBundle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.RelationalDatabaseBundle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a database bundle. A bundle describes the performance
-- specifications of the database.
--
-- /See:/ 'newRelationalDatabaseBundle' smart constructor.
data RelationalDatabaseBundle = RelationalDatabaseBundle'
  { -- | A Boolean value indicating whether the database bundle is encrypted.
    isEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | The number of virtual CPUs (vCPUs) for the database bundle.
    cpuCount :: Prelude.Maybe Prelude.Int,
    -- | The data transfer rate per month in GB for the database bundle.
    transferPerMonthInGb :: Prelude.Maybe Prelude.Int,
    -- | The ID for the database bundle.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The name for the database bundle.
    name :: Prelude.Maybe Prelude.Text,
    -- | The size of the disk for the database bundle.
    diskSizeInGb :: Prelude.Maybe Prelude.Int,
    -- | The cost of the database bundle in US currency.
    price :: Prelude.Maybe Prelude.Double,
    -- | A Boolean value indicating whether the database bundle is active.
    isActive :: Prelude.Maybe Prelude.Bool,
    -- | The amount of RAM in GB (for example, @2.0@) for the database bundle.
    ramSizeInGb :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelationalDatabaseBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isEncrypted', 'relationalDatabaseBundle_isEncrypted' - A Boolean value indicating whether the database bundle is encrypted.
--
-- 'cpuCount', 'relationalDatabaseBundle_cpuCount' - The number of virtual CPUs (vCPUs) for the database bundle.
--
-- 'transferPerMonthInGb', 'relationalDatabaseBundle_transferPerMonthInGb' - The data transfer rate per month in GB for the database bundle.
--
-- 'bundleId', 'relationalDatabaseBundle_bundleId' - The ID for the database bundle.
--
-- 'name', 'relationalDatabaseBundle_name' - The name for the database bundle.
--
-- 'diskSizeInGb', 'relationalDatabaseBundle_diskSizeInGb' - The size of the disk for the database bundle.
--
-- 'price', 'relationalDatabaseBundle_price' - The cost of the database bundle in US currency.
--
-- 'isActive', 'relationalDatabaseBundle_isActive' - A Boolean value indicating whether the database bundle is active.
--
-- 'ramSizeInGb', 'relationalDatabaseBundle_ramSizeInGb' - The amount of RAM in GB (for example, @2.0@) for the database bundle.
newRelationalDatabaseBundle ::
  RelationalDatabaseBundle
newRelationalDatabaseBundle =
  RelationalDatabaseBundle'
    { isEncrypted =
        Prelude.Nothing,
      cpuCount = Prelude.Nothing,
      transferPerMonthInGb = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      name = Prelude.Nothing,
      diskSizeInGb = Prelude.Nothing,
      price = Prelude.Nothing,
      isActive = Prelude.Nothing,
      ramSizeInGb = Prelude.Nothing
    }

-- | A Boolean value indicating whether the database bundle is encrypted.
relationalDatabaseBundle_isEncrypted :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Bool)
relationalDatabaseBundle_isEncrypted = Lens.lens (\RelationalDatabaseBundle' {isEncrypted} -> isEncrypted) (\s@RelationalDatabaseBundle' {} a -> s {isEncrypted = a} :: RelationalDatabaseBundle)

-- | The number of virtual CPUs (vCPUs) for the database bundle.
relationalDatabaseBundle_cpuCount :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Int)
relationalDatabaseBundle_cpuCount = Lens.lens (\RelationalDatabaseBundle' {cpuCount} -> cpuCount) (\s@RelationalDatabaseBundle' {} a -> s {cpuCount = a} :: RelationalDatabaseBundle)

-- | The data transfer rate per month in GB for the database bundle.
relationalDatabaseBundle_transferPerMonthInGb :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Int)
relationalDatabaseBundle_transferPerMonthInGb = Lens.lens (\RelationalDatabaseBundle' {transferPerMonthInGb} -> transferPerMonthInGb) (\s@RelationalDatabaseBundle' {} a -> s {transferPerMonthInGb = a} :: RelationalDatabaseBundle)

-- | The ID for the database bundle.
relationalDatabaseBundle_bundleId :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Text)
relationalDatabaseBundle_bundleId = Lens.lens (\RelationalDatabaseBundle' {bundleId} -> bundleId) (\s@RelationalDatabaseBundle' {} a -> s {bundleId = a} :: RelationalDatabaseBundle)

-- | The name for the database bundle.
relationalDatabaseBundle_name :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Text)
relationalDatabaseBundle_name = Lens.lens (\RelationalDatabaseBundle' {name} -> name) (\s@RelationalDatabaseBundle' {} a -> s {name = a} :: RelationalDatabaseBundle)

-- | The size of the disk for the database bundle.
relationalDatabaseBundle_diskSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Int)
relationalDatabaseBundle_diskSizeInGb = Lens.lens (\RelationalDatabaseBundle' {diskSizeInGb} -> diskSizeInGb) (\s@RelationalDatabaseBundle' {} a -> s {diskSizeInGb = a} :: RelationalDatabaseBundle)

-- | The cost of the database bundle in US currency.
relationalDatabaseBundle_price :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Double)
relationalDatabaseBundle_price = Lens.lens (\RelationalDatabaseBundle' {price} -> price) (\s@RelationalDatabaseBundle' {} a -> s {price = a} :: RelationalDatabaseBundle)

-- | A Boolean value indicating whether the database bundle is active.
relationalDatabaseBundle_isActive :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Bool)
relationalDatabaseBundle_isActive = Lens.lens (\RelationalDatabaseBundle' {isActive} -> isActive) (\s@RelationalDatabaseBundle' {} a -> s {isActive = a} :: RelationalDatabaseBundle)

-- | The amount of RAM in GB (for example, @2.0@) for the database bundle.
relationalDatabaseBundle_ramSizeInGb :: Lens.Lens' RelationalDatabaseBundle (Prelude.Maybe Prelude.Double)
relationalDatabaseBundle_ramSizeInGb = Lens.lens (\RelationalDatabaseBundle' {ramSizeInGb} -> ramSizeInGb) (\s@RelationalDatabaseBundle' {} a -> s {ramSizeInGb = a} :: RelationalDatabaseBundle)

instance Core.FromJSON RelationalDatabaseBundle where
  parseJSON =
    Core.withObject
      "RelationalDatabaseBundle"
      ( \x ->
          RelationalDatabaseBundle'
            Prelude.<$> (x Core..:? "isEncrypted")
            Prelude.<*> (x Core..:? "cpuCount")
            Prelude.<*> (x Core..:? "transferPerMonthInGb")
            Prelude.<*> (x Core..:? "bundleId")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "diskSizeInGb")
            Prelude.<*> (x Core..:? "price")
            Prelude.<*> (x Core..:? "isActive")
            Prelude.<*> (x Core..:? "ramSizeInGb")
      )

instance Prelude.Hashable RelationalDatabaseBundle where
  hashWithSalt salt' RelationalDatabaseBundle' {..} =
    salt' `Prelude.hashWithSalt` ramSizeInGb
      `Prelude.hashWithSalt` isActive
      `Prelude.hashWithSalt` price
      `Prelude.hashWithSalt` diskSizeInGb
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` transferPerMonthInGb
      `Prelude.hashWithSalt` cpuCount
      `Prelude.hashWithSalt` isEncrypted

instance Prelude.NFData RelationalDatabaseBundle where
  rnf RelationalDatabaseBundle' {..} =
    Prelude.rnf isEncrypted
      `Prelude.seq` Prelude.rnf ramSizeInGb
      `Prelude.seq` Prelude.rnf isActive
      `Prelude.seq` Prelude.rnf price
      `Prelude.seq` Prelude.rnf diskSizeInGb
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf transferPerMonthInGb
      `Prelude.seq` Prelude.rnf cpuCount
