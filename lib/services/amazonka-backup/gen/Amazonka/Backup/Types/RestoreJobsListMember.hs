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
-- Module      : Amazonka.Backup.Types.RestoreJobsListMember
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Backup.Types.RestoreJobsListMember where

import Amazonka.Backup.Types.RestoreJobStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata about a restore job.
--
-- /See:/ 'newRestoreJobsListMember' smart constructor.
data RestoreJobsListMember = RestoreJobsListMember'
  { -- | A status code specifying the state of the job initiated by Backup to
    -- restore a recovery point.
    status :: Prelude.Maybe RestoreJobStatus,
    -- | Specifies the IAM role ARN used to create the target recovery point; for
    -- example, @arn:aws:iam::123456789012:role\/S3Access@.
    iamRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The amount of time in minutes that a job restoring a recovery point is
    -- expected to take.
    expectedCompletionTimeMinutes :: Prelude.Maybe Prelude.Integer,
    -- | Uniquely identifies the job that restores a recovery point.
    restoreJobId :: Prelude.Maybe Prelude.Text,
    -- | The resource type of the listed restore jobs; for example, an Amazon
    -- Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database
    -- Service (Amazon RDS) database. For Windows Volume Shadow Copy Service
    -- (VSS) backups, the only supported resource type is Amazon EC2.
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | Contains an estimated percentage complete of a job at the time the job
    -- status was queried.
    percentDone :: Prelude.Maybe Prelude.Text,
    -- | The account ID that owns the restore job.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
    -- format of the ARN depends on the resource type.
    createdResourceArn :: Prelude.Maybe Prelude.Text,
    -- | A detailed message explaining the status of the job to restore a
    -- recovery point.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | An ARN that uniquely identifies a recovery point; for example,
    -- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
    recoveryPointArn :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of the restored resource.
    backupSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The date and time a restore job is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The date and time a job to restore a recovery point is completed, in
    -- Unix format and Coordinated Universal Time (UTC). The value of
    -- @CompletionDate@ is accurate to milliseconds. For example, the value
    -- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
    completionDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreJobsListMember' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'restoreJobsListMember_status' - A status code specifying the state of the job initiated by Backup to
-- restore a recovery point.
--
-- 'iamRoleArn', 'restoreJobsListMember_iamRoleArn' - Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
--
-- 'expectedCompletionTimeMinutes', 'restoreJobsListMember_expectedCompletionTimeMinutes' - The amount of time in minutes that a job restoring a recovery point is
-- expected to take.
--
-- 'restoreJobId', 'restoreJobsListMember_restoreJobId' - Uniquely identifies the job that restores a recovery point.
--
-- 'resourceType', 'restoreJobsListMember_resourceType' - The resource type of the listed restore jobs; for example, an Amazon
-- Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database
-- Service (Amazon RDS) database. For Windows Volume Shadow Copy Service
-- (VSS) backups, the only supported resource type is Amazon EC2.
--
-- 'percentDone', 'restoreJobsListMember_percentDone' - Contains an estimated percentage complete of a job at the time the job
-- status was queried.
--
-- 'accountId', 'restoreJobsListMember_accountId' - The account ID that owns the restore job.
--
-- 'createdResourceArn', 'restoreJobsListMember_createdResourceArn' - An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
--
-- 'statusMessage', 'restoreJobsListMember_statusMessage' - A detailed message explaining the status of the job to restore a
-- recovery point.
--
-- 'recoveryPointArn', 'restoreJobsListMember_recoveryPointArn' - An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
--
-- 'backupSizeInBytes', 'restoreJobsListMember_backupSizeInBytes' - The size, in bytes, of the restored resource.
--
-- 'creationDate', 'restoreJobsListMember_creationDate' - The date and time a restore job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'completionDate', 'restoreJobsListMember_completionDate' - The date and time a job to restore a recovery point is completed, in
-- Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
newRestoreJobsListMember ::
  RestoreJobsListMember
newRestoreJobsListMember =
  RestoreJobsListMember'
    { status = Prelude.Nothing,
      iamRoleArn = Prelude.Nothing,
      expectedCompletionTimeMinutes = Prelude.Nothing,
      restoreJobId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      percentDone = Prelude.Nothing,
      accountId = Prelude.Nothing,
      createdResourceArn = Prelude.Nothing,
      statusMessage = Prelude.Nothing,
      recoveryPointArn = Prelude.Nothing,
      backupSizeInBytes = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      completionDate = Prelude.Nothing
    }

-- | A status code specifying the state of the job initiated by Backup to
-- restore a recovery point.
restoreJobsListMember_status :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe RestoreJobStatus)
restoreJobsListMember_status = Lens.lens (\RestoreJobsListMember' {status} -> status) (\s@RestoreJobsListMember' {} a -> s {status = a} :: RestoreJobsListMember)

-- | Specifies the IAM role ARN used to create the target recovery point; for
-- example, @arn:aws:iam::123456789012:role\/S3Access@.
restoreJobsListMember_iamRoleArn :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.Text)
restoreJobsListMember_iamRoleArn = Lens.lens (\RestoreJobsListMember' {iamRoleArn} -> iamRoleArn) (\s@RestoreJobsListMember' {} a -> s {iamRoleArn = a} :: RestoreJobsListMember)

-- | The amount of time in minutes that a job restoring a recovery point is
-- expected to take.
restoreJobsListMember_expectedCompletionTimeMinutes :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.Integer)
restoreJobsListMember_expectedCompletionTimeMinutes = Lens.lens (\RestoreJobsListMember' {expectedCompletionTimeMinutes} -> expectedCompletionTimeMinutes) (\s@RestoreJobsListMember' {} a -> s {expectedCompletionTimeMinutes = a} :: RestoreJobsListMember)

-- | Uniquely identifies the job that restores a recovery point.
restoreJobsListMember_restoreJobId :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.Text)
restoreJobsListMember_restoreJobId = Lens.lens (\RestoreJobsListMember' {restoreJobId} -> restoreJobId) (\s@RestoreJobsListMember' {} a -> s {restoreJobId = a} :: RestoreJobsListMember)

-- | The resource type of the listed restore jobs; for example, an Amazon
-- Elastic Block Store (Amazon EBS) volume or an Amazon Relational Database
-- Service (Amazon RDS) database. For Windows Volume Shadow Copy Service
-- (VSS) backups, the only supported resource type is Amazon EC2.
restoreJobsListMember_resourceType :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.Text)
restoreJobsListMember_resourceType = Lens.lens (\RestoreJobsListMember' {resourceType} -> resourceType) (\s@RestoreJobsListMember' {} a -> s {resourceType = a} :: RestoreJobsListMember)

-- | Contains an estimated percentage complete of a job at the time the job
-- status was queried.
restoreJobsListMember_percentDone :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.Text)
restoreJobsListMember_percentDone = Lens.lens (\RestoreJobsListMember' {percentDone} -> percentDone) (\s@RestoreJobsListMember' {} a -> s {percentDone = a} :: RestoreJobsListMember)

-- | The account ID that owns the restore job.
restoreJobsListMember_accountId :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.Text)
restoreJobsListMember_accountId = Lens.lens (\RestoreJobsListMember' {accountId} -> accountId) (\s@RestoreJobsListMember' {} a -> s {accountId = a} :: RestoreJobsListMember)

-- | An Amazon Resource Name (ARN) that uniquely identifies a resource. The
-- format of the ARN depends on the resource type.
restoreJobsListMember_createdResourceArn :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.Text)
restoreJobsListMember_createdResourceArn = Lens.lens (\RestoreJobsListMember' {createdResourceArn} -> createdResourceArn) (\s@RestoreJobsListMember' {} a -> s {createdResourceArn = a} :: RestoreJobsListMember)

-- | A detailed message explaining the status of the job to restore a
-- recovery point.
restoreJobsListMember_statusMessage :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.Text)
restoreJobsListMember_statusMessage = Lens.lens (\RestoreJobsListMember' {statusMessage} -> statusMessage) (\s@RestoreJobsListMember' {} a -> s {statusMessage = a} :: RestoreJobsListMember)

-- | An ARN that uniquely identifies a recovery point; for example,
-- @arn:aws:backup:us-east-1:123456789012:recovery-point:1EB3B5E7-9EB0-435A-A80B-108B488B0D45@.
restoreJobsListMember_recoveryPointArn :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.Text)
restoreJobsListMember_recoveryPointArn = Lens.lens (\RestoreJobsListMember' {recoveryPointArn} -> recoveryPointArn) (\s@RestoreJobsListMember' {} a -> s {recoveryPointArn = a} :: RestoreJobsListMember)

-- | The size, in bytes, of the restored resource.
restoreJobsListMember_backupSizeInBytes :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.Integer)
restoreJobsListMember_backupSizeInBytes = Lens.lens (\RestoreJobsListMember' {backupSizeInBytes} -> backupSizeInBytes) (\s@RestoreJobsListMember' {} a -> s {backupSizeInBytes = a} :: RestoreJobsListMember)

-- | The date and time a restore job is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
restoreJobsListMember_creationDate :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.UTCTime)
restoreJobsListMember_creationDate = Lens.lens (\RestoreJobsListMember' {creationDate} -> creationDate) (\s@RestoreJobsListMember' {} a -> s {creationDate = a} :: RestoreJobsListMember) Prelude.. Lens.mapping Core._Time

-- | The date and time a job to restore a recovery point is completed, in
-- Unix format and Coordinated Universal Time (UTC). The value of
-- @CompletionDate@ is accurate to milliseconds. For example, the value
-- 1516925490.087 represents Friday, January 26, 2018 12:11:30.087 AM.
restoreJobsListMember_completionDate :: Lens.Lens' RestoreJobsListMember (Prelude.Maybe Prelude.UTCTime)
restoreJobsListMember_completionDate = Lens.lens (\RestoreJobsListMember' {completionDate} -> completionDate) (\s@RestoreJobsListMember' {} a -> s {completionDate = a} :: RestoreJobsListMember) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON RestoreJobsListMember where
  parseJSON =
    Core.withObject
      "RestoreJobsListMember"
      ( \x ->
          RestoreJobsListMember'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "IamRoleArn")
            Prelude.<*> (x Core..:? "ExpectedCompletionTimeMinutes")
            Prelude.<*> (x Core..:? "RestoreJobId")
            Prelude.<*> (x Core..:? "ResourceType")
            Prelude.<*> (x Core..:? "PercentDone")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "CreatedResourceArn")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..:? "RecoveryPointArn")
            Prelude.<*> (x Core..:? "BackupSizeInBytes")
            Prelude.<*> (x Core..:? "CreationDate")
            Prelude.<*> (x Core..:? "CompletionDate")
      )

instance Prelude.Hashable RestoreJobsListMember where
  hashWithSalt salt' RestoreJobsListMember' {..} =
    salt' `Prelude.hashWithSalt` completionDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` backupSizeInBytes
      `Prelude.hashWithSalt` recoveryPointArn
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` createdResourceArn
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` percentDone
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` restoreJobId
      `Prelude.hashWithSalt` expectedCompletionTimeMinutes
      `Prelude.hashWithSalt` iamRoleArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData RestoreJobsListMember where
  rnf RestoreJobsListMember' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf completionDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf backupSizeInBytes
      `Prelude.seq` Prelude.rnf recoveryPointArn
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf createdResourceArn
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf percentDone
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf restoreJobId
      `Prelude.seq` Prelude.rnf expectedCompletionTimeMinutes
      `Prelude.seq` Prelude.rnf iamRoleArn
