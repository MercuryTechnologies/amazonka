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
-- Module      : Amazonka.RDS.Types.UpgradeTarget
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.UpgradeTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The version of the database engine that a DB instance can be upgraded
-- to.
--
-- /See:/ 'newUpgradeTarget' smart constructor.
data UpgradeTarget = UpgradeTarget'
  { -- | The version number of the upgrade target database engine.
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A list of the supported DB engine modes for the target engine version.
    supportedEngineModes :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates whether upgrading to the target version requires
    -- upgrading the major version of the database engine.
    isMajorVersionUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | The name of the upgrade target database engine.
    engine :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether you can use Aurora global databases with
    -- the target engine version.
    supportsGlobalDatabases :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the target version is applied to any
    -- source DB instances that have @AutoMinorVersionUpgrade@ set to true.
    autoUpgrade :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether you can use Aurora parallel query with
    -- the target engine version.
    supportsParallelQuery :: Prelude.Maybe Prelude.Bool,
    -- | The version of the database engine that a DB instance can be upgraded
    -- to.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpgradeTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'upgradeTarget_engineVersion' - The version number of the upgrade target database engine.
--
-- 'supportedEngineModes', 'upgradeTarget_supportedEngineModes' - A list of the supported DB engine modes for the target engine version.
--
-- 'isMajorVersionUpgrade', 'upgradeTarget_isMajorVersionUpgrade' - A value that indicates whether upgrading to the target version requires
-- upgrading the major version of the database engine.
--
-- 'engine', 'upgradeTarget_engine' - The name of the upgrade target database engine.
--
-- 'supportsGlobalDatabases', 'upgradeTarget_supportsGlobalDatabases' - A value that indicates whether you can use Aurora global databases with
-- the target engine version.
--
-- 'autoUpgrade', 'upgradeTarget_autoUpgrade' - A value that indicates whether the target version is applied to any
-- source DB instances that have @AutoMinorVersionUpgrade@ set to true.
--
-- 'supportsParallelQuery', 'upgradeTarget_supportsParallelQuery' - A value that indicates whether you can use Aurora parallel query with
-- the target engine version.
--
-- 'description', 'upgradeTarget_description' - The version of the database engine that a DB instance can be upgraded
-- to.
newUpgradeTarget ::
  UpgradeTarget
newUpgradeTarget =
  UpgradeTarget'
    { engineVersion = Prelude.Nothing,
      supportedEngineModes = Prelude.Nothing,
      isMajorVersionUpgrade = Prelude.Nothing,
      engine = Prelude.Nothing,
      supportsGlobalDatabases = Prelude.Nothing,
      autoUpgrade = Prelude.Nothing,
      supportsParallelQuery = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The version number of the upgrade target database engine.
upgradeTarget_engineVersion :: Lens.Lens' UpgradeTarget (Prelude.Maybe Prelude.Text)
upgradeTarget_engineVersion = Lens.lens (\UpgradeTarget' {engineVersion} -> engineVersion) (\s@UpgradeTarget' {} a -> s {engineVersion = a} :: UpgradeTarget)

-- | A list of the supported DB engine modes for the target engine version.
upgradeTarget_supportedEngineModes :: Lens.Lens' UpgradeTarget (Prelude.Maybe [Prelude.Text])
upgradeTarget_supportedEngineModes = Lens.lens (\UpgradeTarget' {supportedEngineModes} -> supportedEngineModes) (\s@UpgradeTarget' {} a -> s {supportedEngineModes = a} :: UpgradeTarget) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether upgrading to the target version requires
-- upgrading the major version of the database engine.
upgradeTarget_isMajorVersionUpgrade :: Lens.Lens' UpgradeTarget (Prelude.Maybe Prelude.Bool)
upgradeTarget_isMajorVersionUpgrade = Lens.lens (\UpgradeTarget' {isMajorVersionUpgrade} -> isMajorVersionUpgrade) (\s@UpgradeTarget' {} a -> s {isMajorVersionUpgrade = a} :: UpgradeTarget)

-- | The name of the upgrade target database engine.
upgradeTarget_engine :: Lens.Lens' UpgradeTarget (Prelude.Maybe Prelude.Text)
upgradeTarget_engine = Lens.lens (\UpgradeTarget' {engine} -> engine) (\s@UpgradeTarget' {} a -> s {engine = a} :: UpgradeTarget)

-- | A value that indicates whether you can use Aurora global databases with
-- the target engine version.
upgradeTarget_supportsGlobalDatabases :: Lens.Lens' UpgradeTarget (Prelude.Maybe Prelude.Bool)
upgradeTarget_supportsGlobalDatabases = Lens.lens (\UpgradeTarget' {supportsGlobalDatabases} -> supportsGlobalDatabases) (\s@UpgradeTarget' {} a -> s {supportsGlobalDatabases = a} :: UpgradeTarget)

-- | A value that indicates whether the target version is applied to any
-- source DB instances that have @AutoMinorVersionUpgrade@ set to true.
upgradeTarget_autoUpgrade :: Lens.Lens' UpgradeTarget (Prelude.Maybe Prelude.Bool)
upgradeTarget_autoUpgrade = Lens.lens (\UpgradeTarget' {autoUpgrade} -> autoUpgrade) (\s@UpgradeTarget' {} a -> s {autoUpgrade = a} :: UpgradeTarget)

-- | A value that indicates whether you can use Aurora parallel query with
-- the target engine version.
upgradeTarget_supportsParallelQuery :: Lens.Lens' UpgradeTarget (Prelude.Maybe Prelude.Bool)
upgradeTarget_supportsParallelQuery = Lens.lens (\UpgradeTarget' {supportsParallelQuery} -> supportsParallelQuery) (\s@UpgradeTarget' {} a -> s {supportsParallelQuery = a} :: UpgradeTarget)

-- | The version of the database engine that a DB instance can be upgraded
-- to.
upgradeTarget_description :: Lens.Lens' UpgradeTarget (Prelude.Maybe Prelude.Text)
upgradeTarget_description = Lens.lens (\UpgradeTarget' {description} -> description) (\s@UpgradeTarget' {} a -> s {description = a} :: UpgradeTarget)

instance Core.FromXML UpgradeTarget where
  parseXML x =
    UpgradeTarget'
      Prelude.<$> (x Core..@? "EngineVersion")
      Prelude.<*> ( x Core..@? "SupportedEngineModes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "IsMajorVersionUpgrade")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "SupportsGlobalDatabases")
      Prelude.<*> (x Core..@? "AutoUpgrade")
      Prelude.<*> (x Core..@? "SupportsParallelQuery")
      Prelude.<*> (x Core..@? "Description")

instance Prelude.Hashable UpgradeTarget where
  hashWithSalt salt' UpgradeTarget' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` supportsParallelQuery
      `Prelude.hashWithSalt` autoUpgrade
      `Prelude.hashWithSalt` supportsGlobalDatabases
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` isMajorVersionUpgrade
      `Prelude.hashWithSalt` supportedEngineModes
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData UpgradeTarget where
  rnf UpgradeTarget' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf supportsParallelQuery
      `Prelude.seq` Prelude.rnf autoUpgrade
      `Prelude.seq` Prelude.rnf supportsGlobalDatabases
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf isMajorVersionUpgrade
      `Prelude.seq` Prelude.rnf supportedEngineModes
