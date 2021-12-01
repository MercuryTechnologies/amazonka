{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.RDS.RestoreDBClusterFromS3
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Aurora DB cluster from MySQL data stored in an Amazon
-- S3 bucket. Amazon RDS must be authorized to access the Amazon S3 bucket
-- and the data must be created using the Percona XtraBackup utility as
-- described in
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/AuroraMySQL.Migrating.ExtMySQL.html#AuroraMySQL.Migrating.ExtMySQL.S3 Migrating Data from MySQL by Using an Amazon S3 Bucket>
-- in the /Amazon Aurora User Guide/.
--
-- This action only restores the DB cluster, not the DB instances for that
-- DB cluster. You must invoke the @CreateDBInstance@ action to create DB
-- instances for the restored DB cluster, specifying the identifier of the
-- restored DB cluster in @DBClusterIdentifier@. You can create DB
-- instances only after the @RestoreDBClusterFromS3@ action has completed
-- and the DB cluster is available.
--
-- For more information on Amazon Aurora, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?>
-- in the /Amazon Aurora User Guide./
--
-- This action only applies to Aurora DB clusters. The source DB engine
-- must be MySQL.
module Amazonka.RDS.RestoreDBClusterFromS3
  ( -- * Creating a Request
    RestoreDBClusterFromS3 (..),
    newRestoreDBClusterFromS3,

    -- * Request Lenses
    restoreDBClusterFromS3_engineVersion,
    restoreDBClusterFromS3_deletionProtection,
    restoreDBClusterFromS3_storageEncrypted,
    restoreDBClusterFromS3_dbSubnetGroupName,
    restoreDBClusterFromS3_domain,
    restoreDBClusterFromS3_backtrackWindow,
    restoreDBClusterFromS3_preferredMaintenanceWindow,
    restoreDBClusterFromS3_availabilityZones,
    restoreDBClusterFromS3_characterSetName,
    restoreDBClusterFromS3_kmsKeyId,
    restoreDBClusterFromS3_preferredBackupWindow,
    restoreDBClusterFromS3_backupRetentionPeriod,
    restoreDBClusterFromS3_vpcSecurityGroupIds,
    restoreDBClusterFromS3_databaseName,
    restoreDBClusterFromS3_dbClusterParameterGroupName,
    restoreDBClusterFromS3_s3Prefix,
    restoreDBClusterFromS3_optionGroupName,
    restoreDBClusterFromS3_copyTagsToSnapshot,
    restoreDBClusterFromS3_domainIAMRoleName,
    restoreDBClusterFromS3_tags,
    restoreDBClusterFromS3_port,
    restoreDBClusterFromS3_enableIAMDatabaseAuthentication,
    restoreDBClusterFromS3_enableCloudwatchLogsExports,
    restoreDBClusterFromS3_dbClusterIdentifier,
    restoreDBClusterFromS3_engine,
    restoreDBClusterFromS3_masterUsername,
    restoreDBClusterFromS3_masterUserPassword,
    restoreDBClusterFromS3_sourceEngine,
    restoreDBClusterFromS3_sourceEngineVersion,
    restoreDBClusterFromS3_s3BucketName,
    restoreDBClusterFromS3_s3IngestionRoleArn,

    -- * Destructuring the Response
    RestoreDBClusterFromS3Response (..),
    newRestoreDBClusterFromS3Response,

    -- * Response Lenses
    restoreDBClusterFromS3Response_dbCluster,
    restoreDBClusterFromS3Response_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreDBClusterFromS3' smart constructor.
data RestoreDBClusterFromS3 = RestoreDBClusterFromS3'
  { -- | The version number of the database engine to use.
    --
    -- To list all of the available engine versions for @aurora@ (for MySQL
    -- 5.6-compatible Aurora), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for @aurora-mysql@ (for
    -- MySQL 5.7-compatible Aurora), use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- To list all of the available engine versions for @aurora-postgresql@,
    -- use the following command:
    --
    -- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
    --
    -- __Aurora MySQL__
    --
    -- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
    -- @5.7.mysql_aurora.2.04.5@
    --
    -- __Aurora PostgreSQL__
    --
    -- Example: @9.6.3@, @10.7@
    engineVersion :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether the DB cluster has deletion protection
    -- enabled. The database can\'t be deleted when deletion protection is
    -- enabled. By default, deletion protection is disabled.
    deletionProtection :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether the restored DB cluster is encrypted.
    storageEncrypted :: Prelude.Maybe Prelude.Bool,
    -- | A DB subnet group to associate with the restored DB cluster.
    --
    -- Constraints: If supplied, must match the name of an existing
    -- DBSubnetGroup.
    --
    -- Example: @mySubnetgroup@
    dbSubnetGroupName :: Prelude.Maybe Prelude.Text,
    -- | Specify the Active Directory directory ID to restore the DB cluster in.
    -- The domain must be created prior to this operation.
    --
    -- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
    -- Authentication to authenticate users that connect to the DB cluster. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
    -- in the /Amazon Aurora User Guide/.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The target backtrack window, in seconds. To disable backtracking, set
    -- this value to 0.
    --
    -- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
    --
    -- Default: 0
    --
    -- Constraints:
    --
    -- -   If specified, this value must be set to a number from 0 to 259,200
    --     (72 hours).
    backtrackWindow :: Prelude.Maybe Prelude.Integer,
    -- | The weekly time range during which system maintenance can occur, in
    -- Universal Coordinated Time (UTC).
    --
    -- Format: @ddd:hh24:mi-ddd:hh24:mi@
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region, occurring on a random
    -- day of the week. To see the time blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window>
    -- in the /Amazon Aurora User Guide./
    --
    -- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
    --
    -- Constraints: Minimum 30-minute window.
    preferredMaintenanceWindow :: Prelude.Maybe Prelude.Text,
    -- | A list of Availability Zones (AZs) where instances in the restored DB
    -- cluster can be created.
    availabilityZones :: Prelude.Maybe [Prelude.Text],
    -- | A value that indicates that the restored DB cluster should be associated
    -- with the specified CharacterSet.
    characterSetName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services KMS key identifier for an encrypted DB cluster.
    --
    -- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
    -- ARN, or alias name for the Amazon Web Services KMS customer master key
    -- (CMK). To use a CMK in a different Amazon Web Services account, specify
    -- the key ARN or alias ARN.
    --
    -- If the StorageEncrypted parameter is enabled, and you do not specify a
    -- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
    -- default CMK. There is a default CMK for your Amazon Web Services
    -- account. Your Amazon Web Services account has a different default CMK
    -- for each Amazon Web Services Region.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The daily time range during which automated backups are created if
    -- automated backups are enabled using the @BackupRetentionPeriod@
    -- parameter.
    --
    -- The default is a 30-minute window selected at random from an 8-hour
    -- block of time for each Amazon Web Services Region. To view the time
    -- blocks available, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
    -- in the /Amazon Aurora User Guide./
    --
    -- Constraints:
    --
    -- -   Must be in the format @hh24:mi-hh24:mi@.
    --
    -- -   Must be in Universal Coordinated Time (UTC).
    --
    -- -   Must not conflict with the preferred maintenance window.
    --
    -- -   Must be at least 30 minutes.
    preferredBackupWindow :: Prelude.Maybe Prelude.Text,
    -- | The number of days for which automated backups of the restored DB
    -- cluster are retained. You must specify a minimum value of 1.
    --
    -- Default: 1
    --
    -- Constraints:
    --
    -- -   Must be a value from 1 to 35
    backupRetentionPeriod :: Prelude.Maybe Prelude.Int,
    -- | A list of EC2 VPC security groups to associate with the restored DB
    -- cluster.
    vpcSecurityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The database name for the restored DB cluster.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | The name of the DB cluster parameter group to associate with the
    -- restored DB cluster. If this argument is omitted, @default.aurora5.6@ is
    -- used.
    --
    -- Constraints:
    --
    -- -   If supplied, must match the name of an existing
    --     DBClusterParameterGroup.
    dbClusterParameterGroupName :: Prelude.Maybe Prelude.Text,
    -- | The prefix for all of the file names that contain the data used to
    -- create the Amazon Aurora DB cluster. If you do not specify a
    -- __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created
    -- by using all of the files in the Amazon S3 bucket.
    s3Prefix :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates that the restored DB cluster should be associated
    -- with the specified option group.
    --
    -- Permanent options can\'t be removed from an option group. An option
    -- group can\'t be removed from a DB cluster once it is associated with a
    -- DB cluster.
    optionGroupName :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to copy all tags from the restored DB
    -- cluster to snapshots of the restored DB cluster. The default is not to
    -- copy them.
    copyTagsToSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | Specify the name of the IAM role to be used when making API calls to the
    -- Directory Service.
    domainIAMRoleName :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe [Tag],
    -- | The port number on which the instances in the restored DB cluster accept
    -- connections.
    --
    -- Default: @3306@
    port :: Prelude.Maybe Prelude.Int,
    -- | A value that indicates whether to enable mapping of Amazon Web Services
    -- Identity and Access Management (IAM) accounts to database accounts. By
    -- default, mapping is disabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
    -- in the /Amazon Aurora User Guide./
    enableIAMDatabaseAuthentication :: Prelude.Maybe Prelude.Bool,
    -- | The list of logs that the restored DB cluster is to export to CloudWatch
    -- Logs. The values in the list depend on the DB engine being used. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
    -- in the /Amazon Aurora User Guide/.
    enableCloudwatchLogsExports :: Prelude.Maybe [Prelude.Text],
    -- | The name of the DB cluster to create from the source data in the Amazon
    -- S3 bucket. This parameter isn\'t case-sensitive.
    --
    -- Constraints:
    --
    -- -   Must contain from 1 to 63 letters, numbers, or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t end with a hyphen or contain two consecutive hyphens.
    --
    -- Example: @my-cluster1@
    dbClusterIdentifier :: Prelude.Text,
    -- | The name of the database engine to be used for this DB cluster.
    --
    -- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@
    -- (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
    engine :: Prelude.Text,
    -- | The name of the master user for the restored DB cluster.
    --
    -- Constraints:
    --
    -- -   Must be 1 to 16 letters or numbers.
    --
    -- -   First character must be a letter.
    --
    -- -   Can\'t be a reserved word for the chosen database engine.
    masterUsername :: Prelude.Text,
    -- | The password for the master database user. This password can contain any
    -- printable ASCII character except \"\/\", \"\"\", or \"\@\".
    --
    -- Constraints: Must contain from 8 to 41 characters.
    masterUserPassword :: Prelude.Text,
    -- | The identifier for the database engine that was backed up to create the
    -- files stored in the Amazon S3 bucket.
    --
    -- Valid values: @mysql@
    sourceEngine :: Prelude.Text,
    -- | The version of the database that the backup files were created from.
    --
    -- MySQL versions 5.5, 5.6, and 5.7 are supported.
    --
    -- Example: @5.6.40@, @5.7.28@
    sourceEngineVersion :: Prelude.Text,
    -- | The name of the Amazon S3 bucket that contains the data used to create
    -- the Amazon Aurora DB cluster.
    s3BucketName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
    -- Access Management (IAM) role that authorizes Amazon RDS to access the
    -- Amazon S3 bucket on your behalf.
    s3IngestionRoleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBClusterFromS3' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'engineVersion', 'restoreDBClusterFromS3_engineVersion' - The version number of the database engine to use.
--
-- To list all of the available engine versions for @aurora@ (for MySQL
-- 5.6-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL 5.7-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-postgresql@,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
--
-- __Aurora MySQL__
--
-- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
-- @5.7.mysql_aurora.2.04.5@
--
-- __Aurora PostgreSQL__
--
-- Example: @9.6.3@, @10.7@
--
-- 'deletionProtection', 'restoreDBClusterFromS3_deletionProtection' - A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
--
-- 'storageEncrypted', 'restoreDBClusterFromS3_storageEncrypted' - A value that indicates whether the restored DB cluster is encrypted.
--
-- 'dbSubnetGroupName', 'restoreDBClusterFromS3_dbSubnetGroupName' - A DB subnet group to associate with the restored DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
--
-- 'domain', 'restoreDBClusterFromS3_domain' - Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
--
-- 'backtrackWindow', 'restoreDBClusterFromS3_backtrackWindow' - The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
--
-- 'preferredMaintenanceWindow', 'restoreDBClusterFromS3_preferredMaintenanceWindow' - The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
--
-- 'availabilityZones', 'restoreDBClusterFromS3_availabilityZones' - A list of Availability Zones (AZs) where instances in the restored DB
-- cluster can be created.
--
-- 'characterSetName', 'restoreDBClusterFromS3_characterSetName' - A value that indicates that the restored DB cluster should be associated
-- with the specified CharacterSet.
--
-- 'kmsKeyId', 'restoreDBClusterFromS3_kmsKeyId' - The Amazon Web Services KMS key identifier for an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK). To use a CMK in a different Amazon Web Services account, specify
-- the key ARN or alias ARN.
--
-- If the StorageEncrypted parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default CMK. There is a default CMK for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default CMK
-- for each Amazon Web Services Region.
--
-- 'preferredBackupWindow', 'restoreDBClusterFromS3_preferredBackupWindow' - The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To view the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
--
-- 'backupRetentionPeriod', 'restoreDBClusterFromS3_backupRetentionPeriod' - The number of days for which automated backups of the restored DB
-- cluster are retained. You must specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
--
-- 'vpcSecurityGroupIds', 'restoreDBClusterFromS3_vpcSecurityGroupIds' - A list of EC2 VPC security groups to associate with the restored DB
-- cluster.
--
-- 'databaseName', 'restoreDBClusterFromS3_databaseName' - The database name for the restored DB cluster.
--
-- 'dbClusterParameterGroupName', 'restoreDBClusterFromS3_dbClusterParameterGroupName' - The name of the DB cluster parameter group to associate with the
-- restored DB cluster. If this argument is omitted, @default.aurora5.6@ is
-- used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
--
-- 's3Prefix', 'restoreDBClusterFromS3_s3Prefix' - The prefix for all of the file names that contain the data used to
-- create the Amazon Aurora DB cluster. If you do not specify a
-- __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created
-- by using all of the files in the Amazon S3 bucket.
--
-- 'optionGroupName', 'restoreDBClusterFromS3_optionGroupName' - A value that indicates that the restored DB cluster should be associated
-- with the specified option group.
--
-- Permanent options can\'t be removed from an option group. An option
-- group can\'t be removed from a DB cluster once it is associated with a
-- DB cluster.
--
-- 'copyTagsToSnapshot', 'restoreDBClusterFromS3_copyTagsToSnapshot' - A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
--
-- 'domainIAMRoleName', 'restoreDBClusterFromS3_domainIAMRoleName' - Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
--
-- 'tags', 'restoreDBClusterFromS3_tags' - Undocumented member.
--
-- 'port', 'restoreDBClusterFromS3_port' - The port number on which the instances in the restored DB cluster accept
-- connections.
--
-- Default: @3306@
--
-- 'enableIAMDatabaseAuthentication', 'restoreDBClusterFromS3_enableIAMDatabaseAuthentication' - A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
--
-- 'enableCloudwatchLogsExports', 'restoreDBClusterFromS3_enableCloudwatchLogsExports' - The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
--
-- 'dbClusterIdentifier', 'restoreDBClusterFromS3_dbClusterIdentifier' - The name of the DB cluster to create from the source data in the Amazon
-- S3 bucket. This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
--
-- 'engine', 'restoreDBClusterFromS3_engine' - The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@
-- (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
--
-- 'masterUsername', 'restoreDBClusterFromS3_masterUsername' - The name of the master user for the restored DB cluster.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
--
-- 'masterUserPassword', 'restoreDBClusterFromS3_masterUserPassword' - The password for the master database user. This password can contain any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
--
-- 'sourceEngine', 'restoreDBClusterFromS3_sourceEngine' - The identifier for the database engine that was backed up to create the
-- files stored in the Amazon S3 bucket.
--
-- Valid values: @mysql@
--
-- 'sourceEngineVersion', 'restoreDBClusterFromS3_sourceEngineVersion' - The version of the database that the backup files were created from.
--
-- MySQL versions 5.5, 5.6, and 5.7 are supported.
--
-- Example: @5.6.40@, @5.7.28@
--
-- 's3BucketName', 'restoreDBClusterFromS3_s3BucketName' - The name of the Amazon S3 bucket that contains the data used to create
-- the Amazon Aurora DB cluster.
--
-- 's3IngestionRoleArn', 'restoreDBClusterFromS3_s3IngestionRoleArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role that authorizes Amazon RDS to access the
-- Amazon S3 bucket on your behalf.
newRestoreDBClusterFromS3 ::
  -- | 'dbClusterIdentifier'
  Prelude.Text ->
  -- | 'engine'
  Prelude.Text ->
  -- | 'masterUsername'
  Prelude.Text ->
  -- | 'masterUserPassword'
  Prelude.Text ->
  -- | 'sourceEngine'
  Prelude.Text ->
  -- | 'sourceEngineVersion'
  Prelude.Text ->
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 's3IngestionRoleArn'
  Prelude.Text ->
  RestoreDBClusterFromS3
newRestoreDBClusterFromS3
  pDBClusterIdentifier_
  pEngine_
  pMasterUsername_
  pMasterUserPassword_
  pSourceEngine_
  pSourceEngineVersion_
  pS3BucketName_
  pS3IngestionRoleArn_ =
    RestoreDBClusterFromS3'
      { engineVersion =
          Prelude.Nothing,
        deletionProtection = Prelude.Nothing,
        storageEncrypted = Prelude.Nothing,
        dbSubnetGroupName = Prelude.Nothing,
        domain = Prelude.Nothing,
        backtrackWindow = Prelude.Nothing,
        preferredMaintenanceWindow = Prelude.Nothing,
        availabilityZones = Prelude.Nothing,
        characterSetName = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        preferredBackupWindow = Prelude.Nothing,
        backupRetentionPeriod = Prelude.Nothing,
        vpcSecurityGroupIds = Prelude.Nothing,
        databaseName = Prelude.Nothing,
        dbClusterParameterGroupName = Prelude.Nothing,
        s3Prefix = Prelude.Nothing,
        optionGroupName = Prelude.Nothing,
        copyTagsToSnapshot = Prelude.Nothing,
        domainIAMRoleName = Prelude.Nothing,
        tags = Prelude.Nothing,
        port = Prelude.Nothing,
        enableIAMDatabaseAuthentication = Prelude.Nothing,
        enableCloudwatchLogsExports = Prelude.Nothing,
        dbClusterIdentifier = pDBClusterIdentifier_,
        engine = pEngine_,
        masterUsername = pMasterUsername_,
        masterUserPassword = pMasterUserPassword_,
        sourceEngine = pSourceEngine_,
        sourceEngineVersion = pSourceEngineVersion_,
        s3BucketName = pS3BucketName_,
        s3IngestionRoleArn = pS3IngestionRoleArn_
      }

-- | The version number of the database engine to use.
--
-- To list all of the available engine versions for @aurora@ (for MySQL
-- 5.6-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-mysql@ (for
-- MySQL 5.7-compatible Aurora), use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-mysql --query \"DBEngineVersions[].EngineVersion\"@
--
-- To list all of the available engine versions for @aurora-postgresql@,
-- use the following command:
--
-- @aws rds describe-db-engine-versions --engine aurora-postgresql --query \"DBEngineVersions[].EngineVersion\"@
--
-- __Aurora MySQL__
--
-- Example: @5.6.10a@, @5.6.mysql_aurora.1.19.2@, @5.7.12@,
-- @5.7.mysql_aurora.2.04.5@
--
-- __Aurora PostgreSQL__
--
-- Example: @9.6.3@, @10.7@
restoreDBClusterFromS3_engineVersion :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_engineVersion = Lens.lens (\RestoreDBClusterFromS3' {engineVersion} -> engineVersion) (\s@RestoreDBClusterFromS3' {} a -> s {engineVersion = a} :: RestoreDBClusterFromS3)

-- | A value that indicates whether the DB cluster has deletion protection
-- enabled. The database can\'t be deleted when deletion protection is
-- enabled. By default, deletion protection is disabled.
restoreDBClusterFromS3_deletionProtection :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS3_deletionProtection = Lens.lens (\RestoreDBClusterFromS3' {deletionProtection} -> deletionProtection) (\s@RestoreDBClusterFromS3' {} a -> s {deletionProtection = a} :: RestoreDBClusterFromS3)

-- | A value that indicates whether the restored DB cluster is encrypted.
restoreDBClusterFromS3_storageEncrypted :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS3_storageEncrypted = Lens.lens (\RestoreDBClusterFromS3' {storageEncrypted} -> storageEncrypted) (\s@RestoreDBClusterFromS3' {} a -> s {storageEncrypted = a} :: RestoreDBClusterFromS3)

-- | A DB subnet group to associate with the restored DB cluster.
--
-- Constraints: If supplied, must match the name of an existing
-- DBSubnetGroup.
--
-- Example: @mySubnetgroup@
restoreDBClusterFromS3_dbSubnetGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_dbSubnetGroupName = Lens.lens (\RestoreDBClusterFromS3' {dbSubnetGroupName} -> dbSubnetGroupName) (\s@RestoreDBClusterFromS3' {} a -> s {dbSubnetGroupName = a} :: RestoreDBClusterFromS3)

-- | Specify the Active Directory directory ID to restore the DB cluster in.
-- The domain must be created prior to this operation.
--
-- For Amazon Aurora DB clusters, Amazon RDS can use Kerberos
-- Authentication to authenticate users that connect to the DB cluster. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/kerberos-authentication.html Kerberos Authentication>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterFromS3_domain :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_domain = Lens.lens (\RestoreDBClusterFromS3' {domain} -> domain) (\s@RestoreDBClusterFromS3' {} a -> s {domain = a} :: RestoreDBClusterFromS3)

-- | The target backtrack window, in seconds. To disable backtracking, set
-- this value to 0.
--
-- Currently, Backtrack is only supported for Aurora MySQL DB clusters.
--
-- Default: 0
--
-- Constraints:
--
-- -   If specified, this value must be set to a number from 0 to 259,200
--     (72 hours).
restoreDBClusterFromS3_backtrackWindow :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Integer)
restoreDBClusterFromS3_backtrackWindow = Lens.lens (\RestoreDBClusterFromS3' {backtrackWindow} -> backtrackWindow) (\s@RestoreDBClusterFromS3' {} a -> s {backtrackWindow = a} :: RestoreDBClusterFromS3)

-- | The weekly time range during which system maintenance can occur, in
-- Universal Coordinated Time (UTC).
--
-- Format: @ddd:hh24:mi-ddd:hh24:mi@
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region, occurring on a random
-- day of the week. To see the time blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_UpgradeDBInstance.Maintenance.html#AdjustingTheMaintenanceWindow.Aurora Adjusting the Preferred Maintenance Window>
-- in the /Amazon Aurora User Guide./
--
-- Valid Days: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
--
-- Constraints: Minimum 30-minute window.
restoreDBClusterFromS3_preferredMaintenanceWindow :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_preferredMaintenanceWindow = Lens.lens (\RestoreDBClusterFromS3' {preferredMaintenanceWindow} -> preferredMaintenanceWindow) (\s@RestoreDBClusterFromS3' {} a -> s {preferredMaintenanceWindow = a} :: RestoreDBClusterFromS3)

-- | A list of Availability Zones (AZs) where instances in the restored DB
-- cluster can be created.
restoreDBClusterFromS3_availabilityZones :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromS3_availabilityZones = Lens.lens (\RestoreDBClusterFromS3' {availabilityZones} -> availabilityZones) (\s@RestoreDBClusterFromS3' {} a -> s {availabilityZones = a} :: RestoreDBClusterFromS3) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates that the restored DB cluster should be associated
-- with the specified CharacterSet.
restoreDBClusterFromS3_characterSetName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_characterSetName = Lens.lens (\RestoreDBClusterFromS3' {characterSetName} -> characterSetName) (\s@RestoreDBClusterFromS3' {} a -> s {characterSetName = a} :: RestoreDBClusterFromS3)

-- | The Amazon Web Services KMS key identifier for an encrypted DB cluster.
--
-- The Amazon Web Services KMS key identifier is the key ARN, key ID, alias
-- ARN, or alias name for the Amazon Web Services KMS customer master key
-- (CMK). To use a CMK in a different Amazon Web Services account, specify
-- the key ARN or alias ARN.
--
-- If the StorageEncrypted parameter is enabled, and you do not specify a
-- value for the @KmsKeyId@ parameter, then Amazon RDS will use your
-- default CMK. There is a default CMK for your Amazon Web Services
-- account. Your Amazon Web Services account has a different default CMK
-- for each Amazon Web Services Region.
restoreDBClusterFromS3_kmsKeyId :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_kmsKeyId = Lens.lens (\RestoreDBClusterFromS3' {kmsKeyId} -> kmsKeyId) (\s@RestoreDBClusterFromS3' {} a -> s {kmsKeyId = a} :: RestoreDBClusterFromS3)

-- | The daily time range during which automated backups are created if
-- automated backups are enabled using the @BackupRetentionPeriod@
-- parameter.
--
-- The default is a 30-minute window selected at random from an 8-hour
-- block of time for each Amazon Web Services Region. To view the time
-- blocks available, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.Backups.BackupWindow Backup window>
-- in the /Amazon Aurora User Guide./
--
-- Constraints:
--
-- -   Must be in the format @hh24:mi-hh24:mi@.
--
-- -   Must be in Universal Coordinated Time (UTC).
--
-- -   Must not conflict with the preferred maintenance window.
--
-- -   Must be at least 30 minutes.
restoreDBClusterFromS3_preferredBackupWindow :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_preferredBackupWindow = Lens.lens (\RestoreDBClusterFromS3' {preferredBackupWindow} -> preferredBackupWindow) (\s@RestoreDBClusterFromS3' {} a -> s {preferredBackupWindow = a} :: RestoreDBClusterFromS3)

-- | The number of days for which automated backups of the restored DB
-- cluster are retained. You must specify a minimum value of 1.
--
-- Default: 1
--
-- Constraints:
--
-- -   Must be a value from 1 to 35
restoreDBClusterFromS3_backupRetentionPeriod :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Int)
restoreDBClusterFromS3_backupRetentionPeriod = Lens.lens (\RestoreDBClusterFromS3' {backupRetentionPeriod} -> backupRetentionPeriod) (\s@RestoreDBClusterFromS3' {} a -> s {backupRetentionPeriod = a} :: RestoreDBClusterFromS3)

-- | A list of EC2 VPC security groups to associate with the restored DB
-- cluster.
restoreDBClusterFromS3_vpcSecurityGroupIds :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromS3_vpcSecurityGroupIds = Lens.lens (\RestoreDBClusterFromS3' {vpcSecurityGroupIds} -> vpcSecurityGroupIds) (\s@RestoreDBClusterFromS3' {} a -> s {vpcSecurityGroupIds = a} :: RestoreDBClusterFromS3) Prelude.. Lens.mapping Lens.coerced

-- | The database name for the restored DB cluster.
restoreDBClusterFromS3_databaseName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_databaseName = Lens.lens (\RestoreDBClusterFromS3' {databaseName} -> databaseName) (\s@RestoreDBClusterFromS3' {} a -> s {databaseName = a} :: RestoreDBClusterFromS3)

-- | The name of the DB cluster parameter group to associate with the
-- restored DB cluster. If this argument is omitted, @default.aurora5.6@ is
-- used.
--
-- Constraints:
--
-- -   If supplied, must match the name of an existing
--     DBClusterParameterGroup.
restoreDBClusterFromS3_dbClusterParameterGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_dbClusterParameterGroupName = Lens.lens (\RestoreDBClusterFromS3' {dbClusterParameterGroupName} -> dbClusterParameterGroupName) (\s@RestoreDBClusterFromS3' {} a -> s {dbClusterParameterGroupName = a} :: RestoreDBClusterFromS3)

-- | The prefix for all of the file names that contain the data used to
-- create the Amazon Aurora DB cluster. If you do not specify a
-- __SourceS3Prefix__ value, then the Amazon Aurora DB cluster is created
-- by using all of the files in the Amazon S3 bucket.
restoreDBClusterFromS3_s3Prefix :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_s3Prefix = Lens.lens (\RestoreDBClusterFromS3' {s3Prefix} -> s3Prefix) (\s@RestoreDBClusterFromS3' {} a -> s {s3Prefix = a} :: RestoreDBClusterFromS3)

-- | A value that indicates that the restored DB cluster should be associated
-- with the specified option group.
--
-- Permanent options can\'t be removed from an option group. An option
-- group can\'t be removed from a DB cluster once it is associated with a
-- DB cluster.
restoreDBClusterFromS3_optionGroupName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_optionGroupName = Lens.lens (\RestoreDBClusterFromS3' {optionGroupName} -> optionGroupName) (\s@RestoreDBClusterFromS3' {} a -> s {optionGroupName = a} :: RestoreDBClusterFromS3)

-- | A value that indicates whether to copy all tags from the restored DB
-- cluster to snapshots of the restored DB cluster. The default is not to
-- copy them.
restoreDBClusterFromS3_copyTagsToSnapshot :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS3_copyTagsToSnapshot = Lens.lens (\RestoreDBClusterFromS3' {copyTagsToSnapshot} -> copyTagsToSnapshot) (\s@RestoreDBClusterFromS3' {} a -> s {copyTagsToSnapshot = a} :: RestoreDBClusterFromS3)

-- | Specify the name of the IAM role to be used when making API calls to the
-- Directory Service.
restoreDBClusterFromS3_domainIAMRoleName :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Text)
restoreDBClusterFromS3_domainIAMRoleName = Lens.lens (\RestoreDBClusterFromS3' {domainIAMRoleName} -> domainIAMRoleName) (\s@RestoreDBClusterFromS3' {} a -> s {domainIAMRoleName = a} :: RestoreDBClusterFromS3)

-- | Undocumented member.
restoreDBClusterFromS3_tags :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe [Tag])
restoreDBClusterFromS3_tags = Lens.lens (\RestoreDBClusterFromS3' {tags} -> tags) (\s@RestoreDBClusterFromS3' {} a -> s {tags = a} :: RestoreDBClusterFromS3) Prelude.. Lens.mapping Lens.coerced

-- | The port number on which the instances in the restored DB cluster accept
-- connections.
--
-- Default: @3306@
restoreDBClusterFromS3_port :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Int)
restoreDBClusterFromS3_port = Lens.lens (\RestoreDBClusterFromS3' {port} -> port) (\s@RestoreDBClusterFromS3' {} a -> s {port = a} :: RestoreDBClusterFromS3)

-- | A value that indicates whether to enable mapping of Amazon Web Services
-- Identity and Access Management (IAM) accounts to database accounts. By
-- default, mapping is disabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.IAMDBAuth.html IAM Database Authentication>
-- in the /Amazon Aurora User Guide./
restoreDBClusterFromS3_enableIAMDatabaseAuthentication :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe Prelude.Bool)
restoreDBClusterFromS3_enableIAMDatabaseAuthentication = Lens.lens (\RestoreDBClusterFromS3' {enableIAMDatabaseAuthentication} -> enableIAMDatabaseAuthentication) (\s@RestoreDBClusterFromS3' {} a -> s {enableIAMDatabaseAuthentication = a} :: RestoreDBClusterFromS3)

-- | The list of logs that the restored DB cluster is to export to CloudWatch
-- Logs. The values in the list depend on the DB engine being used. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_LogAccess.html#USER_LogAccess.Procedural.UploadtoCloudWatch Publishing Database Logs to Amazon CloudWatch Logs>
-- in the /Amazon Aurora User Guide/.
restoreDBClusterFromS3_enableCloudwatchLogsExports :: Lens.Lens' RestoreDBClusterFromS3 (Prelude.Maybe [Prelude.Text])
restoreDBClusterFromS3_enableCloudwatchLogsExports = Lens.lens (\RestoreDBClusterFromS3' {enableCloudwatchLogsExports} -> enableCloudwatchLogsExports) (\s@RestoreDBClusterFromS3' {} a -> s {enableCloudwatchLogsExports = a} :: RestoreDBClusterFromS3) Prelude.. Lens.mapping Lens.coerced

-- | The name of the DB cluster to create from the source data in the Amazon
-- S3 bucket. This parameter isn\'t case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 letters, numbers, or hyphens.
--
-- -   First character must be a letter.
--
-- -   Can\'t end with a hyphen or contain two consecutive hyphens.
--
-- Example: @my-cluster1@
restoreDBClusterFromS3_dbClusterIdentifier :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_dbClusterIdentifier = Lens.lens (\RestoreDBClusterFromS3' {dbClusterIdentifier} -> dbClusterIdentifier) (\s@RestoreDBClusterFromS3' {} a -> s {dbClusterIdentifier = a} :: RestoreDBClusterFromS3)

-- | The name of the database engine to be used for this DB cluster.
--
-- Valid Values: @aurora@ (for MySQL 5.6-compatible Aurora), @aurora-mysql@
-- (for MySQL 5.7-compatible Aurora), and @aurora-postgresql@
restoreDBClusterFromS3_engine :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_engine = Lens.lens (\RestoreDBClusterFromS3' {engine} -> engine) (\s@RestoreDBClusterFromS3' {} a -> s {engine = a} :: RestoreDBClusterFromS3)

-- | The name of the master user for the restored DB cluster.
--
-- Constraints:
--
-- -   Must be 1 to 16 letters or numbers.
--
-- -   First character must be a letter.
--
-- -   Can\'t be a reserved word for the chosen database engine.
restoreDBClusterFromS3_masterUsername :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_masterUsername = Lens.lens (\RestoreDBClusterFromS3' {masterUsername} -> masterUsername) (\s@RestoreDBClusterFromS3' {} a -> s {masterUsername = a} :: RestoreDBClusterFromS3)

-- | The password for the master database user. This password can contain any
-- printable ASCII character except \"\/\", \"\"\", or \"\@\".
--
-- Constraints: Must contain from 8 to 41 characters.
restoreDBClusterFromS3_masterUserPassword :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_masterUserPassword = Lens.lens (\RestoreDBClusterFromS3' {masterUserPassword} -> masterUserPassword) (\s@RestoreDBClusterFromS3' {} a -> s {masterUserPassword = a} :: RestoreDBClusterFromS3)

-- | The identifier for the database engine that was backed up to create the
-- files stored in the Amazon S3 bucket.
--
-- Valid values: @mysql@
restoreDBClusterFromS3_sourceEngine :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_sourceEngine = Lens.lens (\RestoreDBClusterFromS3' {sourceEngine} -> sourceEngine) (\s@RestoreDBClusterFromS3' {} a -> s {sourceEngine = a} :: RestoreDBClusterFromS3)

-- | The version of the database that the backup files were created from.
--
-- MySQL versions 5.5, 5.6, and 5.7 are supported.
--
-- Example: @5.6.40@, @5.7.28@
restoreDBClusterFromS3_sourceEngineVersion :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_sourceEngineVersion = Lens.lens (\RestoreDBClusterFromS3' {sourceEngineVersion} -> sourceEngineVersion) (\s@RestoreDBClusterFromS3' {} a -> s {sourceEngineVersion = a} :: RestoreDBClusterFromS3)

-- | The name of the Amazon S3 bucket that contains the data used to create
-- the Amazon Aurora DB cluster.
restoreDBClusterFromS3_s3BucketName :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_s3BucketName = Lens.lens (\RestoreDBClusterFromS3' {s3BucketName} -> s3BucketName) (\s@RestoreDBClusterFromS3' {} a -> s {s3BucketName = a} :: RestoreDBClusterFromS3)

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Identity and
-- Access Management (IAM) role that authorizes Amazon RDS to access the
-- Amazon S3 bucket on your behalf.
restoreDBClusterFromS3_s3IngestionRoleArn :: Lens.Lens' RestoreDBClusterFromS3 Prelude.Text
restoreDBClusterFromS3_s3IngestionRoleArn = Lens.lens (\RestoreDBClusterFromS3' {s3IngestionRoleArn} -> s3IngestionRoleArn) (\s@RestoreDBClusterFromS3' {} a -> s {s3IngestionRoleArn = a} :: RestoreDBClusterFromS3)

instance Core.AWSRequest RestoreDBClusterFromS3 where
  type
    AWSResponse RestoreDBClusterFromS3 =
      RestoreDBClusterFromS3Response
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RestoreDBClusterFromS3Result"
      ( \s h x ->
          RestoreDBClusterFromS3Response'
            Prelude.<$> (x Core..@? "DBCluster")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreDBClusterFromS3 where
  hashWithSalt salt' RestoreDBClusterFromS3' {..} =
    salt' `Prelude.hashWithSalt` s3IngestionRoleArn
      `Prelude.hashWithSalt` s3BucketName
      `Prelude.hashWithSalt` sourceEngineVersion
      `Prelude.hashWithSalt` sourceEngine
      `Prelude.hashWithSalt` masterUserPassword
      `Prelude.hashWithSalt` masterUsername
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` dbClusterIdentifier
      `Prelude.hashWithSalt` enableCloudwatchLogsExports
      `Prelude.hashWithSalt` enableIAMDatabaseAuthentication
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` domainIAMRoleName
      `Prelude.hashWithSalt` copyTagsToSnapshot
      `Prelude.hashWithSalt` optionGroupName
      `Prelude.hashWithSalt` s3Prefix
      `Prelude.hashWithSalt` dbClusterParameterGroupName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` vpcSecurityGroupIds
      `Prelude.hashWithSalt` backupRetentionPeriod
      `Prelude.hashWithSalt` preferredBackupWindow
      `Prelude.hashWithSalt` kmsKeyId
      `Prelude.hashWithSalt` characterSetName
      `Prelude.hashWithSalt` availabilityZones
      `Prelude.hashWithSalt` preferredMaintenanceWindow
      `Prelude.hashWithSalt` backtrackWindow
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` dbSubnetGroupName
      `Prelude.hashWithSalt` storageEncrypted
      `Prelude.hashWithSalt` deletionProtection
      `Prelude.hashWithSalt` engineVersion

instance Prelude.NFData RestoreDBClusterFromS3 where
  rnf RestoreDBClusterFromS3' {..} =
    Prelude.rnf engineVersion
      `Prelude.seq` Prelude.rnf s3IngestionRoleArn
      `Prelude.seq` Prelude.rnf s3BucketName
      `Prelude.seq` Prelude.rnf sourceEngineVersion
      `Prelude.seq` Prelude.rnf sourceEngine
      `Prelude.seq` Prelude.rnf masterUserPassword
      `Prelude.seq` Prelude.rnf masterUsername
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf dbClusterIdentifier
      `Prelude.seq` Prelude.rnf enableCloudwatchLogsExports
      `Prelude.seq` Prelude.rnf enableIAMDatabaseAuthentication
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf domainIAMRoleName
      `Prelude.seq` Prelude.rnf copyTagsToSnapshot
      `Prelude.seq` Prelude.rnf optionGroupName
      `Prelude.seq` Prelude.rnf s3Prefix
      `Prelude.seq` Prelude.rnf dbClusterParameterGroupName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf vpcSecurityGroupIds
      `Prelude.seq` Prelude.rnf backupRetentionPeriod
      `Prelude.seq` Prelude.rnf preferredBackupWindow
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf characterSetName
      `Prelude.seq` Prelude.rnf availabilityZones
      `Prelude.seq` Prelude.rnf preferredMaintenanceWindow
      `Prelude.seq` Prelude.rnf backtrackWindow
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf dbSubnetGroupName
      `Prelude.seq` Prelude.rnf storageEncrypted
      `Prelude.seq` Prelude.rnf deletionProtection

instance Core.ToHeaders RestoreDBClusterFromS3 where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RestoreDBClusterFromS3 where
  toPath = Prelude.const "/"

instance Core.ToQuery RestoreDBClusterFromS3 where
  toQuery RestoreDBClusterFromS3' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("RestoreDBClusterFromS3" :: Prelude.ByteString),
        "Version"
          Core.=: ("2014-10-31" :: Prelude.ByteString),
        "EngineVersion" Core.=: engineVersion,
        "DeletionProtection" Core.=: deletionProtection,
        "StorageEncrypted" Core.=: storageEncrypted,
        "DBSubnetGroupName" Core.=: dbSubnetGroupName,
        "Domain" Core.=: domain,
        "BacktrackWindow" Core.=: backtrackWindow,
        "PreferredMaintenanceWindow"
          Core.=: preferredMaintenanceWindow,
        "AvailabilityZones"
          Core.=: Core.toQuery
            ( Core.toQueryList "AvailabilityZone"
                Prelude.<$> availabilityZones
            ),
        "CharacterSetName" Core.=: characterSetName,
        "KmsKeyId" Core.=: kmsKeyId,
        "PreferredBackupWindow"
          Core.=: preferredBackupWindow,
        "BackupRetentionPeriod"
          Core.=: backupRetentionPeriod,
        "VpcSecurityGroupIds"
          Core.=: Core.toQuery
            ( Core.toQueryList "VpcSecurityGroupId"
                Prelude.<$> vpcSecurityGroupIds
            ),
        "DatabaseName" Core.=: databaseName,
        "DBClusterParameterGroupName"
          Core.=: dbClusterParameterGroupName,
        "S3Prefix" Core.=: s3Prefix,
        "OptionGroupName" Core.=: optionGroupName,
        "CopyTagsToSnapshot" Core.=: copyTagsToSnapshot,
        "DomainIAMRoleName" Core.=: domainIAMRoleName,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "Tag" Prelude.<$> tags),
        "Port" Core.=: port,
        "EnableIAMDatabaseAuthentication"
          Core.=: enableIAMDatabaseAuthentication,
        "EnableCloudwatchLogsExports"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> enableCloudwatchLogsExports
            ),
        "DBClusterIdentifier" Core.=: dbClusterIdentifier,
        "Engine" Core.=: engine,
        "MasterUsername" Core.=: masterUsername,
        "MasterUserPassword" Core.=: masterUserPassword,
        "SourceEngine" Core.=: sourceEngine,
        "SourceEngineVersion" Core.=: sourceEngineVersion,
        "S3BucketName" Core.=: s3BucketName,
        "S3IngestionRoleArn" Core.=: s3IngestionRoleArn
      ]

-- | /See:/ 'newRestoreDBClusterFromS3Response' smart constructor.
data RestoreDBClusterFromS3Response = RestoreDBClusterFromS3Response'
  { dbCluster :: Prelude.Maybe DBCluster,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreDBClusterFromS3Response' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbCluster', 'restoreDBClusterFromS3Response_dbCluster' - Undocumented member.
--
-- 'httpStatus', 'restoreDBClusterFromS3Response_httpStatus' - The response's http status code.
newRestoreDBClusterFromS3Response ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreDBClusterFromS3Response
newRestoreDBClusterFromS3Response pHttpStatus_ =
  RestoreDBClusterFromS3Response'
    { dbCluster =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
restoreDBClusterFromS3Response_dbCluster :: Lens.Lens' RestoreDBClusterFromS3Response (Prelude.Maybe DBCluster)
restoreDBClusterFromS3Response_dbCluster = Lens.lens (\RestoreDBClusterFromS3Response' {dbCluster} -> dbCluster) (\s@RestoreDBClusterFromS3Response' {} a -> s {dbCluster = a} :: RestoreDBClusterFromS3Response)

-- | The response's http status code.
restoreDBClusterFromS3Response_httpStatus :: Lens.Lens' RestoreDBClusterFromS3Response Prelude.Int
restoreDBClusterFromS3Response_httpStatus = Lens.lens (\RestoreDBClusterFromS3Response' {httpStatus} -> httpStatus) (\s@RestoreDBClusterFromS3Response' {} a -> s {httpStatus = a} :: RestoreDBClusterFromS3Response)

instance
  Prelude.NFData
    RestoreDBClusterFromS3Response
  where
  rnf RestoreDBClusterFromS3Response' {..} =
    Prelude.rnf dbCluster
      `Prelude.seq` Prelude.rnf httpStatus
