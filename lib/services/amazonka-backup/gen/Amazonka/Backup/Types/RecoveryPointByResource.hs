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
-- Module      : Amazonka.Backup.Types.RecoveryPointByResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.RecoveryPointByResource where

import Amazonka.Backup.Types.RecoveryPointStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains detailed information about a saved recovery point.
--
-- /See:/ 'newRecoveryPointByResource' smart constructor.
data RecoveryPointByResource = RecoveryPointByResource'
  { -- | A status code specifying the state of the recovery point.
    status :: Prelude.Maybe RecoveryPointStatus,
    -- | A message explaining the reason of the recovery point deletion failure.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
    -- for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of a backup.
    backupSizeBytes :: Prelude.Maybe Prelude.Integer,
    -- | The server-side encryption key that is used to protect your backups; for
    -- example,
    -- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
    encryptionKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time a recovery point is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The name of a logical container where backups are stored. Backup vaults
    -- are identified by names that are unique to the account used to create
    -- them and the Amazon Web Services Region where they are created. They
    -- consist of lowercase letters, numbers, and hyphens.
    backupVaultName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecoveryPointByResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'recoveryPointByResource_status' - A status code specifying the state of the recovery point.
--
-- 'statusMessage', 'recoveryPointByResource_statusMessage' - A message explaining the reason of the recovery point deletion failure.
--
-- 'recoveryPointArn', 'recoveryPointByResource_recoveryPointArn' - An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'backupSizeBytes', 'recoveryPointByResource_backupSizeBytes' - The size, in bytes, of a backup.
--
-- 'encryptionKeyArn', 'recoveryPointByResource_encryptionKeyArn' - The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
--
-- 'creationDate', 'recoveryPointByResource_creationDate' - The date and time a recovery point is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'backupVaultName', 'recoveryPointByResource_backupVaultName' - The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
newRecoveryPointByResource ::
  RecoveryPointByResource
newRecoveryPointByResource =
  RecoveryPointByResource'
    { status = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      recoveryPointArn = Prelude.Nothing,
      backupSizeBytes = Prelude.Nothing,
      encryptionKeyArn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      backupVaultName = Prelude.Nothing
    }

-- | A status code specifying the state of the recovery point.
recoveryPointByResource_status :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe RecoveryPointStatus)
recoveryPointByResource_status = Lens.lens (\RecoveryPointByResource' {status} -> status) (\s@RecoveryPointByResource' {} a -> s {status = a} :: RecoveryPointByResource)

-- | A message explaining the reason of the recovery point deletion failure.
recoveryPointByResource_statusMessage :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Text)
recoveryPointByResource_statusMessage = Lens.lens (\RecoveryPointByResource' {statusMessage} -> statusMessage) (\s@RecoveryPointByResource' {} a -> s {statusMessage = a} :: RecoveryPointByResource)

-- | An Amazon Resource Name (ARN) that uniquely identifies a recovery point;
-- for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
recoveryPointByResource_recoveryPointArn :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Text)
recoveryPointByResource_recoveryPointArn = Lens.lens (\RecoveryPointByResource' {recoveryPointArn} -> recoveryPointArn) (\s@RecoveryPointByResource' {} a -> s {recoveryPointArn = a} :: RecoveryPointByResource)

-- | The size, in bytes, of a backup.
recoveryPointByResource_backupSizeBytes :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Integer)
recoveryPointByResource_backupSizeBytes = Lens.lens (\RecoveryPointByResource' {backupSizeBytes} -> backupSizeBytes) (\s@RecoveryPointByResource' {} a -> s {backupSizeBytes = a} :: RecoveryPointByResource)

-- | The server-side encryption key that is used to protect your backups; for
-- example,
-- @arn:aws:kms:us-west-2:111122223333:key\/1234abcd-12ab-34cd-56ef-1234567890ab@.
recoveryPointByResource_encryptionKeyArn :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Text)
recoveryPointByResource_encryptionKeyArn = Lens.lens (\RecoveryPointByResource' {encryptionKeyArn} -> encryptionKeyArn) (\s@RecoveryPointByResource' {} a -> s {encryptionKeyArn = a} :: RecoveryPointByResource)

-- | The date and time a recovery point is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
recoveryPointByResource_creationDate :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.UTCTime)
recoveryPointByResource_creationDate = Lens.lens (\RecoveryPointByResource' {creationDate} -> creationDate) (\s@RecoveryPointByResource' {} a -> s {creationDate = a} :: RecoveryPointByResource) Prelude.. Lens.mapping Core._Time

-- | The name of a logical container where backups are stored. Backup vaults
-- are identified by names that are unique to the account used to create
-- them and the Amazon Web Services Region where they are created. They
-- consist of lowercase letters, numbers, and hyphens.
recoveryPointByResource_backupVaultName :: Lens.Lens' RecoveryPointByResource (Prelude.Maybe Prelude.Text)
recoveryPointByResource_backupVaultName = Lens.lens (\RecoveryPointByResource' {backupVaultName} -> backupVaultName) (\s@RecoveryPointByResource' {} a -> s {backupVaultName = a} :: RecoveryPointByResource)

instance Core.FromJSON RecoveryPointByResource where
  parseJSON =
    Core.withObject
      "RecoveryPointByResource"
      ( \x ->
          RecoveryPointByResource'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "RecoveryPointArn")
            Prelude.<*> (x Core..:? "BackupSizeBytes")
            Prelude.<*> (x Core..:? "EncryptionKeyArn")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "BackupVaultName")
      )

instance Prelude.Hashable RecoveryPointByResource where
  hashWithSalt salt' RecoveryPointByResource' {..} =
    salt' `Prelude.hashWithSalt` backupVaultName
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` encryptionKeyArn
      `Prelude.hashWithSalt` backupSizeBytes
      `Prelude.hashWithSalt` recoveryPointArn
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` status

instance Prelude.NFData RecoveryPointByResource where
  rnf RecoveryPointByResource' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf backupVaultName
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf encryptionKeyArn
      `Prelude.seq` Prelude.rnf backupSizeBytes
      `Prelude.seq` Prelude.rnf recoveryPointArn
      `Prelude.seq` Prelude.rnf statusMessage
