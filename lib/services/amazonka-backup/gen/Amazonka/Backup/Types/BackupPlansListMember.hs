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
-- Module      : Amazonka.Backup.Types.BackupPlansListMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.BackupPlansListMember where

import Amazonka.Backup.Types.AdvancedBackupSetting
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata about a backup plan.
--
-- /See:/ 'newBackupPlansListMember' smart constructor.
data BackupPlansListMember = BackupPlansListMember'
  { -- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
    -- most 1,024 bytes long. Version IDs cannot be edited.
    versionId :: Prelude.Maybe Prelude.Text,
    -- | The display name of a saved backup plan.
    backupPlanName :: Prelude.Maybe Prelude.Text,
    -- | Contains a list of @BackupOptions@ for a resource type.
    advancedBackupSettings :: Prelude.Maybe [AdvancedBackupSetting],
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | A unique string that identifies the request and allows failed requests
    -- to be retried without the risk of running the operation twice.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
    backupPlanArn :: Prelude.Maybe Prelude.Text,
    -- | The last time a job to back up resources was run with this rule. A date
    -- and time, in Unix format and Coordinated Universal Time (UTC). The value
    -- of @LastExecutionDate@ is accurate to milliseconds. For example, the
    -- value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087
    -- AM.
    lastExecutionDate :: Prelude.Maybe Core.POSIX,
    -- | The date and time a resource backup plan is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The date and time a backup plan is deleted, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    deletionDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BackupPlansListMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'backupPlansListMember_versionId' - Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
--
-- 'backupPlanName', 'backupPlansListMember_backupPlanName' - The display name of a saved backup plan.
--
-- 'advancedBackupSettings', 'backupPlansListMember_advancedBackupSettings' - Contains a list of @BackupOptions@ for a resource type.
--
-- 'backupPlanId', 'backupPlansListMember_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'creatorRequestId', 'backupPlansListMember_creatorRequestId' - A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice.
--
-- 'backupPlanArn', 'backupPlansListMember_backupPlanArn' - An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
--
-- 'lastExecutionDate', 'backupPlansListMember_lastExecutionDate' - The last time a job to back up resources was run with this rule. A date
-- and time, in Unix format and Coordinated Universal Time (UTC). The value
-- of @LastExecutionDate@ is accurate to milliseconds. For example, the
-- value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087
-- AM.
--
-- 'creationDate', 'backupPlansListMember_creationDate' - The date and time a resource backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'deletionDate', 'backupPlansListMember_deletionDate' - The date and time a backup plan is deleted, in Unix format and
-- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
newBackupPlansListMember ::
  BackupPlansListMember
newBackupPlansListMember =
  BackupPlansListMember'
    { versionId = Prelude.Nothing,
      backupPlanName = Prelude.Nothing,
      advancedBackupSettings = Prelude.Nothing,
      backupPlanId = Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      backupPlanArn = Prelude.Nothing,
      lastExecutionDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      deletionDate = Prelude.Nothing
    }

-- | Unique, randomly generated, Unicode, UTF-8 encoded strings that are at
-- most 1,024 bytes long. Version IDs cannot be edited.
backupPlansListMember_versionId :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.Text)
backupPlansListMember_versionId = Lens.lens (\BackupPlansListMember' {versionId} -> versionId) (\s@BackupPlansListMember' {} a -> s {versionId = a} :: BackupPlansListMember)

-- | The display name of a saved backup plan.
backupPlansListMember_backupPlanName :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.Text)
backupPlansListMember_backupPlanName = Lens.lens (\BackupPlansListMember' {backupPlanName} -> backupPlanName) (\s@BackupPlansListMember' {} a -> s {backupPlanName = a} :: BackupPlansListMember)

-- | Contains a list of @BackupOptions@ for a resource type.
backupPlansListMember_advancedBackupSettings :: Lens.Lens' BackupPlansListMember (Prelude.Maybe [AdvancedBackupSetting])
backupPlansListMember_advancedBackupSettings = Lens.lens (\BackupPlansListMember' {advancedBackupSettings} -> advancedBackupSettings) (\s@BackupPlansListMember' {} a -> s {advancedBackupSettings = a} :: BackupPlansListMember) Prelude.. Lens.mapping Lens.coerced

-- | Uniquely identifies a backup plan.
backupPlansListMember_backupPlanId :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.Text)
backupPlansListMember_backupPlanId = Lens.lens (\BackupPlansListMember' {backupPlanId} -> backupPlanId) (\s@BackupPlansListMember' {} a -> s {backupPlanId = a} :: BackupPlansListMember)

-- | A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice.
backupPlansListMember_creatorRequestId :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.Text)
backupPlansListMember_creatorRequestId = Lens.lens (\BackupPlansListMember' {creatorRequestId} -> creatorRequestId) (\s@BackupPlansListMember' {} a -> s {creatorRequestId = a} :: BackupPlansListMember)

-- | An Amazon Resource Name (ARN) that uniquely identifies a backup plan;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:plan:8F81F553-3A74-4A3F-B93D-B3360DC80C50@.
backupPlansListMember_backupPlanArn :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.Text)
backupPlansListMember_backupPlanArn = Lens.lens (\BackupPlansListMember' {backupPlanArn} -> backupPlanArn) (\s@BackupPlansListMember' {} a -> s {backupPlanArn = a} :: BackupPlansListMember)

-- | The last time a job to back up resources was run with this rule. A date
-- and time, in Unix format and Coordinated Universal Time (UTC). The value
-- of @LastExecutionDate@ is accurate to milliseconds. For example, the
-- value 1516925490.087 represents Friday, January 26, 2018 12:11:30.087
-- AM.
backupPlansListMember_lastExecutionDate :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.UTCTime)
backupPlansListMember_lastExecutionDate = Lens.lens (\BackupPlansListMember' {lastExecutionDate} -> lastExecutionDate) (\s@BackupPlansListMember' {} a -> s {lastExecutionDate = a} :: BackupPlansListMember) Prelude.. Lens.mapping Core._Time

-- | The date and time a resource backup plan is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
backupPlansListMember_creationDate :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.UTCTime)
backupPlansListMember_creationDate = Lens.lens (\BackupPlansListMember' {creationDate} -> creationDate) (\s@BackupPlansListMember' {} a -> s {creationDate = a} :: BackupPlansListMember) Prelude.. Lens.mapping Core._Time

-- | The date and time a backup plan is deleted, in Unix format and
-- Coordinated Universal Time (UTC). The value of @DeletionDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
backupPlansListMember_deletionDate :: Lens.Lens' BackupPlansListMember (Prelude.Maybe Prelude.UTCTime)
backupPlansListMember_deletionDate = Lens.lens (\BackupPlansListMember' {deletionDate} -> deletionDate) (\s@BackupPlansListMember' {} a -> s {deletionDate = a} :: BackupPlansListMember) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON BackupPlansListMember where
  parseJSON =
    Core.withObject
      "BackupPlansListMember"
      ( \x ->
          BackupPlansListMember'
            Prelude.<$> (x Core..:? "VersionId")
            Prelude.<*> (x Core..:? "BackupPlanName")
            Prelude.<*> ( x Core..:? "AdvancedBackupSettings"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "BackupPlanId")
            Prelude.<*> (x Core..:? "CreatorRequestId")
            Prelude.<*> (x Core..:? "BackupPlanArn")
            Prelude.<*> (x Core..:? "LastExecutionDate")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "DeletionDate")
      )

instance Prelude.Hashable BackupPlansListMember where
  hashWithSalt salt' BackupPlansListMember' {..} =
    salt' `Prelude.hashWithSalt` deletionDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastExecutionDate
      `Prelude.hashWithSalt` backupPlanArn
      `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` backupPlanId
      `Prelude.hashWithSalt` advancedBackupSettings
      `Prelude.hashWithSalt` backupPlanName
      `Prelude.hashWithSalt` versionId

instance Prelude.NFData BackupPlansListMember where
  rnf BackupPlansListMember' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf deletionDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastExecutionDate
      `Prelude.seq` Prelude.rnf backupPlanArn
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf advancedBackupSettings
      `Prelude.seq` Prelude.rnf backupPlanName
