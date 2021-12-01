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
-- Module      : Amazonka.DMS.Types.PostgreSQLSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DMS.Types.PostgreSQLSettings where

import qualified Amazonka.Core as Core
import Amazonka.DMS.Types.PluginNameValue
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information that defines a PostgreSQL endpoint.
--
-- /See:/ 'newPostgreSQLSettings' smart constructor.
data PostgreSQLSettings = PostgreSQLSettings'
  { -- | Sets the client statement timeout for the PostgreSQL instance, in
    -- seconds. The default value is 60 seconds.
    --
    -- Example: @executeTimeout=100;@
    executeTimeout :: Prelude.Maybe Prelude.Int,
    -- | Specifies the maximum size (in KB) of any .csv file used to transfer
    -- data to PostgreSQL.
    --
    -- Example: @maxFileSize=512@
    maxFileSize :: Prelude.Maybe Prelude.Int,
    -- | When set to @true@, this value causes a task to fail if the actual size
    -- of a LOB column is greater than the specified @LobMaxSize@.
    --
    -- If task is set to Limited LOB mode and this option is set to true, the
    -- task fails instead of truncating the LOB data.
    failTasksOnLobTruncation :: Prelude.Maybe Prelude.Bool,
    -- | Fully qualified domain name of the endpoint.
    serverName :: Prelude.Maybe Prelude.Text,
    -- | The schema in which the operational DDL database artifacts are created.
    --
    -- Example: @ddlArtifactsSchema=xyzddlschema;@
    ddlArtifactsSchema :: Prelude.Maybe Prelude.Text,
    -- | Sets the name of a previously created logical replication slot for a
    -- change data capture (CDC) load of the PostgreSQL source instance.
    --
    -- When used with the @CdcStartPosition@ request parameter for the DMS API
    -- , this attribute also makes it possible to use native CDC start points.
    -- DMS verifies that the specified logical replication slot exists before
    -- starting the CDC load task. It also verifies that the task was created
    -- with a valid setting of @CdcStartPosition@. If the specified slot
    -- doesn\'t exist or the task doesn\'t have a valid @CdcStartPosition@
    -- setting, DMS raises an error.
    --
    -- For more information about setting the @CdcStartPosition@ request
    -- parameter, see
    -- <dms/latest/userguide/CHAP_Task.CDC.html#CHAP_Task.CDC.StartPoint.Native Determining a CDC native start point>
    -- in the /Database Migration Service User Guide/. For more information
    -- about using @CdcStartPosition@, see
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html CreateReplicationTask>,
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>,
    -- and
    -- <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html ModifyReplicationTask>.
    slotName :: Prelude.Maybe Prelude.Text,
    -- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
    -- as the trusted entity and grants the required permissions to access the
    -- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
    -- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
    -- Secrets Manager secret that allows access to the PostgreSQL endpoint.
    --
    -- You can specify one of two sets of values for these permissions. You can
    -- specify the values for this setting and @SecretsManagerSecretId@. Or you
    -- can specify clear-text values for @UserName@, @Password@, @ServerName@,
    -- and @Port@. You can\'t specify both. For more information on creating
    -- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
    -- @SecretsManagerSecretId@ required to access it, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
    -- in the /Database Migration Service User Guide/.
    secretsManagerAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | Endpoint connection user name.
    username :: Prelude.Maybe Prelude.Text,
    -- | Sets the WAL heartbeat frequency (in minutes).
    heartbeatFrequency :: Prelude.Maybe Prelude.Int,
    -- | Endpoint connection password.
    password :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Database name for the endpoint.
    databaseName :: Prelude.Maybe Prelude.Text,
    -- | For use with change data capture (CDC) only, this attribute has DMS
    -- bypass foreign keys and user triggers to reduce the time it takes to
    -- bulk load data.
    --
    -- Example: @afterConnectScript=SET session_replication_role=\'replica\'@
    afterConnectScript :: Prelude.Maybe Prelude.Text,
    -- | The full ARN, partial ARN, or friendly name of the
    -- @SecretsManagerSecret@ that contains the PostgreSQL endpoint connection
    -- details.
    secretsManagerSecretId :: Prelude.Maybe Prelude.Text,
    -- | To capture DDL events, DMS creates various artifacts in the PostgreSQL
    -- database when the task starts. You can later remove these artifacts.
    --
    -- If this value is set to @N@, you don\'t have to create tables or
    -- triggers on the source database.
    captureDdls :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the plugin to use to create a replication slot.
    pluginName :: Prelude.Maybe PluginNameValue,
    -- | Endpoint TCP port.
    port :: Prelude.Maybe Prelude.Int,
    -- | Sets the schema in which the heartbeat artifacts are created.
    heartbeatSchema :: Prelude.Maybe Prelude.Text,
    -- | The write-ahead log (WAL) heartbeat feature mimics a dummy transaction.
    -- By doing this, it prevents idle logical replication slots from holding
    -- onto old WAL logs, which can result in storage full situations on the
    -- source. This heartbeat keeps @restart_lsn@ moving and prevents storage
    -- full scenarios.
    heartbeatEnable :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostgreSQLSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'executeTimeout', 'postgreSQLSettings_executeTimeout' - Sets the client statement timeout for the PostgreSQL instance, in
-- seconds. The default value is 60 seconds.
--
-- Example: @executeTimeout=100;@
--
-- 'maxFileSize', 'postgreSQLSettings_maxFileSize' - Specifies the maximum size (in KB) of any .csv file used to transfer
-- data to PostgreSQL.
--
-- Example: @maxFileSize=512@
--
-- 'failTasksOnLobTruncation', 'postgreSQLSettings_failTasksOnLobTruncation' - When set to @true@, this value causes a task to fail if the actual size
-- of a LOB column is greater than the specified @LobMaxSize@.
--
-- If task is set to Limited LOB mode and this option is set to true, the
-- task fails instead of truncating the LOB data.
--
-- 'serverName', 'postgreSQLSettings_serverName' - Fully qualified domain name of the endpoint.
--
-- 'ddlArtifactsSchema', 'postgreSQLSettings_ddlArtifactsSchema' - The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@
--
-- 'slotName', 'postgreSQLSettings_slotName' - Sets the name of a previously created logical replication slot for a
-- change data capture (CDC) load of the PostgreSQL source instance.
--
-- When used with the @CdcStartPosition@ request parameter for the DMS API
-- , this attribute also makes it possible to use native CDC start points.
-- DMS verifies that the specified logical replication slot exists before
-- starting the CDC load task. It also verifies that the task was created
-- with a valid setting of @CdcStartPosition@. If the specified slot
-- doesn\'t exist or the task doesn\'t have a valid @CdcStartPosition@
-- setting, DMS raises an error.
--
-- For more information about setting the @CdcStartPosition@ request
-- parameter, see
-- <dms/latest/userguide/CHAP_Task.CDC.html#CHAP_Task.CDC.StartPoint.Native Determining a CDC native start point>
-- in the /Database Migration Service User Guide/. For more information
-- about using @CdcStartPosition@, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html CreateReplicationTask>,
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>,
-- and
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html ModifyReplicationTask>.
--
-- 'secretsManagerAccessRoleArn', 'postgreSQLSettings_secretsManagerAccessRoleArn' - The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the PostgreSQL endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
--
-- 'username', 'postgreSQLSettings_username' - Endpoint connection user name.
--
-- 'heartbeatFrequency', 'postgreSQLSettings_heartbeatFrequency' - Sets the WAL heartbeat frequency (in minutes).
--
-- 'password', 'postgreSQLSettings_password' - Endpoint connection password.
--
-- 'databaseName', 'postgreSQLSettings_databaseName' - Database name for the endpoint.
--
-- 'afterConnectScript', 'postgreSQLSettings_afterConnectScript' - For use with change data capture (CDC) only, this attribute has DMS
-- bypass foreign keys and user triggers to reduce the time it takes to
-- bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role=\'replica\'@
--
-- 'secretsManagerSecretId', 'postgreSQLSettings_secretsManagerSecretId' - The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the PostgreSQL endpoint connection
-- details.
--
-- 'captureDdls', 'postgreSQLSettings_captureDdls' - To capture DDL events, DMS creates various artifacts in the PostgreSQL
-- database when the task starts. You can later remove these artifacts.
--
-- If this value is set to @N@, you don\'t have to create tables or
-- triggers on the source database.
--
-- 'pluginName', 'postgreSQLSettings_pluginName' - Specifies the plugin to use to create a replication slot.
--
-- 'port', 'postgreSQLSettings_port' - Endpoint TCP port.
--
-- 'heartbeatSchema', 'postgreSQLSettings_heartbeatSchema' - Sets the schema in which the heartbeat artifacts are created.
--
-- 'heartbeatEnable', 'postgreSQLSettings_heartbeatEnable' - The write-ahead log (WAL) heartbeat feature mimics a dummy transaction.
-- By doing this, it prevents idle logical replication slots from holding
-- onto old WAL logs, which can result in storage full situations on the
-- source. This heartbeat keeps @restart_lsn@ moving and prevents storage
-- full scenarios.
newPostgreSQLSettings ::
  PostgreSQLSettings
newPostgreSQLSettings =
  PostgreSQLSettings'
    { executeTimeout =
        Prelude.Nothing,
      maxFileSize = Prelude.Nothing,
      failTasksOnLobTruncation = Prelude.Nothing,
      serverName = Prelude.Nothing,
      ddlArtifactsSchema = Prelude.Nothing,
      slotName = Prelude.Nothing,
      secretsManagerAccessRoleArn = Prelude.Nothing,
      username = Prelude.Nothing,
      heartbeatFrequency = Prelude.Nothing,
      password = Prelude.Nothing,
      databaseName = Prelude.Nothing,
      afterConnectScript = Prelude.Nothing,
      secretsManagerSecretId = Prelude.Nothing,
      captureDdls = Prelude.Nothing,
      pluginName = Prelude.Nothing,
      port = Prelude.Nothing,
      heartbeatSchema = Prelude.Nothing,
      heartbeatEnable = Prelude.Nothing
    }

-- | Sets the client statement timeout for the PostgreSQL instance, in
-- seconds. The default value is 60 seconds.
--
-- Example: @executeTimeout=100;@
postgreSQLSettings_executeTimeout :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Int)
postgreSQLSettings_executeTimeout = Lens.lens (\PostgreSQLSettings' {executeTimeout} -> executeTimeout) (\s@PostgreSQLSettings' {} a -> s {executeTimeout = a} :: PostgreSQLSettings)

-- | Specifies the maximum size (in KB) of any .csv file used to transfer
-- data to PostgreSQL.
--
-- Example: @maxFileSize=512@
postgreSQLSettings_maxFileSize :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Int)
postgreSQLSettings_maxFileSize = Lens.lens (\PostgreSQLSettings' {maxFileSize} -> maxFileSize) (\s@PostgreSQLSettings' {} a -> s {maxFileSize = a} :: PostgreSQLSettings)

-- | When set to @true@, this value causes a task to fail if the actual size
-- of a LOB column is greater than the specified @LobMaxSize@.
--
-- If task is set to Limited LOB mode and this option is set to true, the
-- task fails instead of truncating the LOB data.
postgreSQLSettings_failTasksOnLobTruncation :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Bool)
postgreSQLSettings_failTasksOnLobTruncation = Lens.lens (\PostgreSQLSettings' {failTasksOnLobTruncation} -> failTasksOnLobTruncation) (\s@PostgreSQLSettings' {} a -> s {failTasksOnLobTruncation = a} :: PostgreSQLSettings)

-- | Fully qualified domain name of the endpoint.
postgreSQLSettings_serverName :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_serverName = Lens.lens (\PostgreSQLSettings' {serverName} -> serverName) (\s@PostgreSQLSettings' {} a -> s {serverName = a} :: PostgreSQLSettings)

-- | The schema in which the operational DDL database artifacts are created.
--
-- Example: @ddlArtifactsSchema=xyzddlschema;@
postgreSQLSettings_ddlArtifactsSchema :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_ddlArtifactsSchema = Lens.lens (\PostgreSQLSettings' {ddlArtifactsSchema} -> ddlArtifactsSchema) (\s@PostgreSQLSettings' {} a -> s {ddlArtifactsSchema = a} :: PostgreSQLSettings)

-- | Sets the name of a previously created logical replication slot for a
-- change data capture (CDC) load of the PostgreSQL source instance.
--
-- When used with the @CdcStartPosition@ request parameter for the DMS API
-- , this attribute also makes it possible to use native CDC start points.
-- DMS verifies that the specified logical replication slot exists before
-- starting the CDC load task. It also verifies that the task was created
-- with a valid setting of @CdcStartPosition@. If the specified slot
-- doesn\'t exist or the task doesn\'t have a valid @CdcStartPosition@
-- setting, DMS raises an error.
--
-- For more information about setting the @CdcStartPosition@ request
-- parameter, see
-- <dms/latest/userguide/CHAP_Task.CDC.html#CHAP_Task.CDC.StartPoint.Native Determining a CDC native start point>
-- in the /Database Migration Service User Guide/. For more information
-- about using @CdcStartPosition@, see
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_CreateReplicationTask.html CreateReplicationTask>,
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartReplicationTask.html StartReplicationTask>,
-- and
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_ModifyReplicationTask.html ModifyReplicationTask>.
postgreSQLSettings_slotName :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_slotName = Lens.lens (\PostgreSQLSettings' {slotName} -> slotName) (\s@PostgreSQLSettings' {} a -> s {slotName = a} :: PostgreSQLSettings)

-- | The full Amazon Resource Name (ARN) of the IAM role that specifies DMS
-- as the trusted entity and grants the required permissions to access the
-- value in @SecretsManagerSecret@. The role must allow the @iam:PassRole@
-- action. @SecretsManagerSecret@ has the value of the Amazon Web Services
-- Secrets Manager secret that allows access to the PostgreSQL endpoint.
--
-- You can specify one of two sets of values for these permissions. You can
-- specify the values for this setting and @SecretsManagerSecretId@. Or you
-- can specify clear-text values for @UserName@, @Password@, @ServerName@,
-- and @Port@. You can\'t specify both. For more information on creating
-- this @SecretsManagerSecret@ and the @SecretsManagerAccessRoleArn@ and
-- @SecretsManagerSecretId@ required to access it, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Security.html#security-iam-secretsmanager Using secrets to access Database Migration Service resources>
-- in the /Database Migration Service User Guide/.
postgreSQLSettings_secretsManagerAccessRoleArn :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_secretsManagerAccessRoleArn = Lens.lens (\PostgreSQLSettings' {secretsManagerAccessRoleArn} -> secretsManagerAccessRoleArn) (\s@PostgreSQLSettings' {} a -> s {secretsManagerAccessRoleArn = a} :: PostgreSQLSettings)

-- | Endpoint connection user name.
postgreSQLSettings_username :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_username = Lens.lens (\PostgreSQLSettings' {username} -> username) (\s@PostgreSQLSettings' {} a -> s {username = a} :: PostgreSQLSettings)

-- | Sets the WAL heartbeat frequency (in minutes).
postgreSQLSettings_heartbeatFrequency :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Int)
postgreSQLSettings_heartbeatFrequency = Lens.lens (\PostgreSQLSettings' {heartbeatFrequency} -> heartbeatFrequency) (\s@PostgreSQLSettings' {} a -> s {heartbeatFrequency = a} :: PostgreSQLSettings)

-- | Endpoint connection password.
postgreSQLSettings_password :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_password = Lens.lens (\PostgreSQLSettings' {password} -> password) (\s@PostgreSQLSettings' {} a -> s {password = a} :: PostgreSQLSettings) Prelude.. Lens.mapping Core._Sensitive

-- | Database name for the endpoint.
postgreSQLSettings_databaseName :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_databaseName = Lens.lens (\PostgreSQLSettings' {databaseName} -> databaseName) (\s@PostgreSQLSettings' {} a -> s {databaseName = a} :: PostgreSQLSettings)

-- | For use with change data capture (CDC) only, this attribute has DMS
-- bypass foreign keys and user triggers to reduce the time it takes to
-- bulk load data.
--
-- Example: @afterConnectScript=SET session_replication_role=\'replica\'@
postgreSQLSettings_afterConnectScript :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_afterConnectScript = Lens.lens (\PostgreSQLSettings' {afterConnectScript} -> afterConnectScript) (\s@PostgreSQLSettings' {} a -> s {afterConnectScript = a} :: PostgreSQLSettings)

-- | The full ARN, partial ARN, or friendly name of the
-- @SecretsManagerSecret@ that contains the PostgreSQL endpoint connection
-- details.
postgreSQLSettings_secretsManagerSecretId :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_secretsManagerSecretId = Lens.lens (\PostgreSQLSettings' {secretsManagerSecretId} -> secretsManagerSecretId) (\s@PostgreSQLSettings' {} a -> s {secretsManagerSecretId = a} :: PostgreSQLSettings)

-- | To capture DDL events, DMS creates various artifacts in the PostgreSQL
-- database when the task starts. You can later remove these artifacts.
--
-- If this value is set to @N@, you don\'t have to create tables or
-- triggers on the source database.
postgreSQLSettings_captureDdls :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Bool)
postgreSQLSettings_captureDdls = Lens.lens (\PostgreSQLSettings' {captureDdls} -> captureDdls) (\s@PostgreSQLSettings' {} a -> s {captureDdls = a} :: PostgreSQLSettings)

-- | Specifies the plugin to use to create a replication slot.
postgreSQLSettings_pluginName :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe PluginNameValue)
postgreSQLSettings_pluginName = Lens.lens (\PostgreSQLSettings' {pluginName} -> pluginName) (\s@PostgreSQLSettings' {} a -> s {pluginName = a} :: PostgreSQLSettings)

-- | Endpoint TCP port.
postgreSQLSettings_port :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Int)
postgreSQLSettings_port = Lens.lens (\PostgreSQLSettings' {port} -> port) (\s@PostgreSQLSettings' {} a -> s {port = a} :: PostgreSQLSettings)

-- | Sets the schema in which the heartbeat artifacts are created.
postgreSQLSettings_heartbeatSchema :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Text)
postgreSQLSettings_heartbeatSchema = Lens.lens (\PostgreSQLSettings' {heartbeatSchema} -> heartbeatSchema) (\s@PostgreSQLSettings' {} a -> s {heartbeatSchema = a} :: PostgreSQLSettings)

-- | The write-ahead log (WAL) heartbeat feature mimics a dummy transaction.
-- By doing this, it prevents idle logical replication slots from holding
-- onto old WAL logs, which can result in storage full situations on the
-- source. This heartbeat keeps @restart_lsn@ moving and prevents storage
-- full scenarios.
postgreSQLSettings_heartbeatEnable :: Lens.Lens' PostgreSQLSettings (Prelude.Maybe Prelude.Bool)
postgreSQLSettings_heartbeatEnable = Lens.lens (\PostgreSQLSettings' {heartbeatEnable} -> heartbeatEnable) (\s@PostgreSQLSettings' {} a -> s {heartbeatEnable = a} :: PostgreSQLSettings)

instance Core.FromJSON PostgreSQLSettings where
  parseJSON =
    Core.withObject
      "PostgreSQLSettings"
      ( \x ->
          PostgreSQLSettings'
            Prelude.<$> (x Core..:? "ExecuteTimeout")
            Prelude.<*> (x Core..:? "MaxFileSize")
            Prelude.<*> (x Core..:? "FailTasksOnLobTruncation")
            Prelude.<*> (x Core..:? "ServerName")
            Prelude.<*> (x Core..:? "DdlArtifactsSchema")
            Prelude.<*> (x Core..:? "SlotName")
            Prelude.<*> (x Core..:? "SecretsManagerAccessRoleArn")
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "HeartbeatFrequency")
            Prelude.<*> (x Core..:? "Password")
            Prelude.<*> (x Core..:? "DatabaseName")
            Prelude.<*> (x Core..:? "AfterConnectScript")
            Prelude.<*> (x Core..:? "SecretsManagerSecretId")
            Prelude.<*> (x Core..:? "CaptureDdls")
            Prelude.<*> (x Core..:? "PluginName")
            Prelude.<*> (x Core..:? "Port")
            Prelude.<*> (x Core..:? "HeartbeatSchema")
            Prelude.<*> (x Core..:? "HeartbeatEnable")
      )

instance Prelude.Hashable PostgreSQLSettings where
  hashWithSalt salt' PostgreSQLSettings' {..} =
    salt' `Prelude.hashWithSalt` heartbeatEnable
      `Prelude.hashWithSalt` heartbeatSchema
      `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` pluginName
      `Prelude.hashWithSalt` captureDdls
      `Prelude.hashWithSalt` secretsManagerSecretId
      `Prelude.hashWithSalt` afterConnectScript
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` heartbeatFrequency
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` secretsManagerAccessRoleArn
      `Prelude.hashWithSalt` slotName
      `Prelude.hashWithSalt` ddlArtifactsSchema
      `Prelude.hashWithSalt` serverName
      `Prelude.hashWithSalt` failTasksOnLobTruncation
      `Prelude.hashWithSalt` maxFileSize
      `Prelude.hashWithSalt` executeTimeout

instance Prelude.NFData PostgreSQLSettings where
  rnf PostgreSQLSettings' {..} =
    Prelude.rnf executeTimeout
      `Prelude.seq` Prelude.rnf heartbeatEnable
      `Prelude.seq` Prelude.rnf heartbeatSchema
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf pluginName
      `Prelude.seq` Prelude.rnf captureDdls
      `Prelude.seq` Prelude.rnf secretsManagerSecretId
      `Prelude.seq` Prelude.rnf afterConnectScript
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf heartbeatFrequency
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf secretsManagerAccessRoleArn
      `Prelude.seq` Prelude.rnf slotName
      `Prelude.seq` Prelude.rnf ddlArtifactsSchema
      `Prelude.seq` Prelude.rnf serverName
      `Prelude.seq` Prelude.rnf failTasksOnLobTruncation
      `Prelude.seq` Prelude.rnf maxFileSize

instance Core.ToJSON PostgreSQLSettings where
  toJSON PostgreSQLSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExecuteTimeout" Core..=)
              Prelude.<$> executeTimeout,
            ("MaxFileSize" Core..=) Prelude.<$> maxFileSize,
            ("FailTasksOnLobTruncation" Core..=)
              Prelude.<$> failTasksOnLobTruncation,
            ("ServerName" Core..=) Prelude.<$> serverName,
            ("DdlArtifactsSchema" Core..=)
              Prelude.<$> ddlArtifactsSchema,
            ("SlotName" Core..=) Prelude.<$> slotName,
            ("SecretsManagerAccessRoleArn" Core..=)
              Prelude.<$> secretsManagerAccessRoleArn,
            ("Username" Core..=) Prelude.<$> username,
            ("HeartbeatFrequency" Core..=)
              Prelude.<$> heartbeatFrequency,
            ("Password" Core..=) Prelude.<$> password,
            ("DatabaseName" Core..=) Prelude.<$> databaseName,
            ("AfterConnectScript" Core..=)
              Prelude.<$> afterConnectScript,
            ("SecretsManagerSecretId" Core..=)
              Prelude.<$> secretsManagerSecretId,
            ("CaptureDdls" Core..=) Prelude.<$> captureDdls,
            ("PluginName" Core..=) Prelude.<$> pluginName,
            ("Port" Core..=) Prelude.<$> port,
            ("HeartbeatSchema" Core..=)
              Prelude.<$> heartbeatSchema,
            ("HeartbeatEnable" Core..=)
              Prelude.<$> heartbeatEnable
          ]
      )
