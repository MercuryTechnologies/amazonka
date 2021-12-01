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
-- Module      : Amazonka.LookoutMetrics.Types.RedshiftSourceConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LookoutMetrics.Types.RedshiftSourceConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LookoutMetrics.Types.VpcConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the Amazon Redshift database configuration.
--
-- /See:/ 'newRedshiftSourceConfig' smart constructor.
data RedshiftSourceConfig = RedshiftSourceConfig'
  { -- | A string identifying the Redshift cluster.
    clusterIdentifier :: Prelude.Text,
    -- | The name of the database host.
    databaseHost :: Prelude.Text,
    -- | The port number where the database can be accessed.
    databasePort :: Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
    secretManagerArn :: Prelude.Text,
    -- | The Redshift database name.
    databaseName :: Prelude.Text,
    -- | The table name of the Redshift database.
    tableName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the role providing access to the
    -- database.
    roleArn :: Prelude.Text,
    -- | Contains information about the Amazon Virtual Private Cloud (VPC)
    -- configuration.
    vpcConfiguration :: VpcConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RedshiftSourceConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'redshiftSourceConfig_clusterIdentifier' - A string identifying the Redshift cluster.
--
-- 'databaseHost', 'redshiftSourceConfig_databaseHost' - The name of the database host.
--
-- 'databasePort', 'redshiftSourceConfig_databasePort' - The port number where the database can be accessed.
--
-- 'secretManagerArn', 'redshiftSourceConfig_secretManagerArn' - The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
--
-- 'databaseName', 'redshiftSourceConfig_databaseName' - The Redshift database name.
--
-- 'tableName', 'redshiftSourceConfig_tableName' - The table name of the Redshift database.
--
-- 'roleArn', 'redshiftSourceConfig_roleArn' - The Amazon Resource Name (ARN) of the role providing access to the
-- database.
--
-- 'vpcConfiguration', 'redshiftSourceConfig_vpcConfiguration' - Contains information about the Amazon Virtual Private Cloud (VPC)
-- configuration.
newRedshiftSourceConfig ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  -- | 'databaseHost'
  Prelude.Text ->
  -- | 'databasePort'
  Prelude.Natural ->
  -- | 'secretManagerArn'
  Prelude.Text ->
  -- | 'databaseName'
  Prelude.Text ->
  -- | 'tableName'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'vpcConfiguration'
  VpcConfiguration ->
  RedshiftSourceConfig
newRedshiftSourceConfig
  pClusterIdentifier_
  pDatabaseHost_
  pDatabasePort_
  pSecretManagerArn_
  pDatabaseName_
  pTableName_
  pRoleArn_
  pVpcConfiguration_ =
    RedshiftSourceConfig'
      { clusterIdentifier =
          pClusterIdentifier_,
        databaseHost = pDatabaseHost_,
        databasePort = pDatabasePort_,
        secretManagerArn = pSecretManagerArn_,
        databaseName = pDatabaseName_,
        tableName = pTableName_,
        roleArn = pRoleArn_,
        vpcConfiguration = pVpcConfiguration_
      }

-- | A string identifying the Redshift cluster.
redshiftSourceConfig_clusterIdentifier :: Lens.Lens' RedshiftSourceConfig Prelude.Text
redshiftSourceConfig_clusterIdentifier = Lens.lens (\RedshiftSourceConfig' {clusterIdentifier} -> clusterIdentifier) (\s@RedshiftSourceConfig' {} a -> s {clusterIdentifier = a} :: RedshiftSourceConfig)

-- | The name of the database host.
redshiftSourceConfig_databaseHost :: Lens.Lens' RedshiftSourceConfig Prelude.Text
redshiftSourceConfig_databaseHost = Lens.lens (\RedshiftSourceConfig' {databaseHost} -> databaseHost) (\s@RedshiftSourceConfig' {} a -> s {databaseHost = a} :: RedshiftSourceConfig)

-- | The port number where the database can be accessed.
redshiftSourceConfig_databasePort :: Lens.Lens' RedshiftSourceConfig Prelude.Natural
redshiftSourceConfig_databasePort = Lens.lens (\RedshiftSourceConfig' {databasePort} -> databasePort) (\s@RedshiftSourceConfig' {} a -> s {databasePort = a} :: RedshiftSourceConfig)

-- | The Amazon Resource Name (ARN) of the AWS Secrets Manager role.
redshiftSourceConfig_secretManagerArn :: Lens.Lens' RedshiftSourceConfig Prelude.Text
redshiftSourceConfig_secretManagerArn = Lens.lens (\RedshiftSourceConfig' {secretManagerArn} -> secretManagerArn) (\s@RedshiftSourceConfig' {} a -> s {secretManagerArn = a} :: RedshiftSourceConfig)

-- | The Redshift database name.
redshiftSourceConfig_databaseName :: Lens.Lens' RedshiftSourceConfig Prelude.Text
redshiftSourceConfig_databaseName = Lens.lens (\RedshiftSourceConfig' {databaseName} -> databaseName) (\s@RedshiftSourceConfig' {} a -> s {databaseName = a} :: RedshiftSourceConfig)

-- | The table name of the Redshift database.
redshiftSourceConfig_tableName :: Lens.Lens' RedshiftSourceConfig Prelude.Text
redshiftSourceConfig_tableName = Lens.lens (\RedshiftSourceConfig' {tableName} -> tableName) (\s@RedshiftSourceConfig' {} a -> s {tableName = a} :: RedshiftSourceConfig)

-- | The Amazon Resource Name (ARN) of the role providing access to the
-- database.
redshiftSourceConfig_roleArn :: Lens.Lens' RedshiftSourceConfig Prelude.Text
redshiftSourceConfig_roleArn = Lens.lens (\RedshiftSourceConfig' {roleArn} -> roleArn) (\s@RedshiftSourceConfig' {} a -> s {roleArn = a} :: RedshiftSourceConfig)

-- | Contains information about the Amazon Virtual Private Cloud (VPC)
-- configuration.
redshiftSourceConfig_vpcConfiguration :: Lens.Lens' RedshiftSourceConfig VpcConfiguration
redshiftSourceConfig_vpcConfiguration = Lens.lens (\RedshiftSourceConfig' {vpcConfiguration} -> vpcConfiguration) (\s@RedshiftSourceConfig' {} a -> s {vpcConfiguration = a} :: RedshiftSourceConfig)

instance Core.FromJSON RedshiftSourceConfig where
  parseJSON =
    Core.withObject
      "RedshiftSourceConfig"
      ( \x ->
          RedshiftSourceConfig'
            Prelude.<$> (x Core..: "ClusterIdentifier")
            Prelude.<*> (x Core..: "DatabaseHost")
            Prelude.<*> (x Core..: "DatabasePort")
            Prelude.<*> (x Core..: "SecretManagerArn")
            Prelude.<*> (x Core..: "DatabaseName")
            Prelude.<*> (x Core..: "TableName")
            Prelude.<*> (x Core..: "RoleArn")
            Prelude.<*> (x Core..: "VpcConfiguration")
      )

instance Prelude.Hashable RedshiftSourceConfig where
  hashWithSalt salt' RedshiftSourceConfig' {..} =
    salt' `Prelude.hashWithSalt` vpcConfiguration
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` databaseName
      `Prelude.hashWithSalt` secretManagerArn
      `Prelude.hashWithSalt` databasePort
      `Prelude.hashWithSalt` databaseHost
      `Prelude.hashWithSalt` clusterIdentifier

instance Prelude.NFData RedshiftSourceConfig where
  rnf RedshiftSourceConfig' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf vpcConfiguration
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf tableName
      `Prelude.seq` Prelude.rnf databaseName
      `Prelude.seq` Prelude.rnf secretManagerArn
      `Prelude.seq` Prelude.rnf databasePort
      `Prelude.seq` Prelude.rnf databaseHost

instance Core.ToJSON RedshiftSourceConfig where
  toJSON RedshiftSourceConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ClusterIdentifier" Core..= clusterIdentifier),
            Prelude.Just ("DatabaseHost" Core..= databaseHost),
            Prelude.Just ("DatabasePort" Core..= databasePort),
            Prelude.Just
              ("SecretManagerArn" Core..= secretManagerArn),
            Prelude.Just ("DatabaseName" Core..= databaseName),
            Prelude.Just ("TableName" Core..= tableName),
            Prelude.Just ("RoleArn" Core..= roleArn),
            Prelude.Just
              ("VpcConfiguration" Core..= vpcConfiguration)
          ]
      )
