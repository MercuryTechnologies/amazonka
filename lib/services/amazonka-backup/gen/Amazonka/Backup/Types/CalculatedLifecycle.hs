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
-- Module      : Amazonka.Backup.Types.CalculatedLifecycle
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.CalculatedLifecycle where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains @DeleteAt@ and @MoveToColdStorageAt@ timestamps, which are used
-- to specify a lifecycle for a recovery point.
--
-- The lifecycle defines when a protected resource is transitioned to cold
-- storage and when it expires. Backup transitions and expires backups
-- automatically according to the lifecycle that you define.
--
-- Backups transitioned to cold storage must be stored in cold storage for
-- a minimum of 90 days. Therefore, the “expire after days” setting must be
-- 90 days greater than the “transition to cold after days” setting. The
-- “transition to cold after days” setting cannot be changed after a backup
-- has been transitioned to cold.
--
-- Only Amazon EFS file system backups can be transitioned to cold storage.
--
-- /See:/ 'newCalculatedLifecycle' smart constructor.
data CalculatedLifecycle = CalculatedLifecycle'
  { -- | A timestamp that specifies when to delete a recovery point.
    deleteAt :: Prelude.Maybe Core.POSIX,
    -- | A timestamp that specifies when to transition a recovery point to cold
    -- storage.
    moveToColdStorageAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CalculatedLifecycle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deleteAt', 'calculatedLifecycle_deleteAt' - A timestamp that specifies when to delete a recovery point.
--
-- 'moveToColdStorageAt', 'calculatedLifecycle_moveToColdStorageAt' - A timestamp that specifies when to transition a recovery point to cold
-- storage.
newCalculatedLifecycle ::
  CalculatedLifecycle
newCalculatedLifecycle =
  CalculatedLifecycle'
    { deleteAt = Prelude.Nothing,
      moveToColdStorageAt = Prelude.Nothing
    }

-- | A timestamp that specifies when to delete a recovery point.
calculatedLifecycle_deleteAt :: Lens.Lens' CalculatedLifecycle (Prelude.Maybe Prelude.UTCTime)
calculatedLifecycle_deleteAt = Lens.lens (\CalculatedLifecycle' {deleteAt} -> deleteAt) (\s@CalculatedLifecycle' {} a -> s {deleteAt = a} :: CalculatedLifecycle) Prelude.. Lens.mapping Core._Time

-- | A timestamp that specifies when to transition a recovery point to cold
-- storage.
calculatedLifecycle_moveToColdStorageAt :: Lens.Lens' CalculatedLifecycle (Prelude.Maybe Prelude.UTCTime)
calculatedLifecycle_moveToColdStorageAt = Lens.lens (\CalculatedLifecycle' {moveToColdStorageAt} -> moveToColdStorageAt) (\s@CalculatedLifecycle' {} a -> s {moveToColdStorageAt = a} :: CalculatedLifecycle) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON CalculatedLifecycle where
  parseJSON =
    Core.withObject
      "CalculatedLifecycle"
      ( \x ->
          CalculatedLifecycle'
            Prelude.<$> (x Core..:? "DeleteAt")
            Prelude.<*> (x Core..:? "MoveToColdStorageAt")
      )

instance Prelude.Hashable CalculatedLifecycle where
  hashWithSalt salt' CalculatedLifecycle' {..} =
    salt' `Prelude.hashWithSalt` moveToColdStorageAt
      `Prelude.hashWithSalt` deleteAt

instance Prelude.NFData CalculatedLifecycle where
  rnf CalculatedLifecycle' {..} =
    Prelude.rnf deleteAt
      `Prelude.seq` Prelude.rnf moveToColdStorageAt
