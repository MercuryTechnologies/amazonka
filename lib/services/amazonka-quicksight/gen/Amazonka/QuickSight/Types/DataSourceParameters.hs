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
-- Module      : Amazonka.QuickSight.Types.DataSourceParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSourceParameters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AmazonElasticsearchParameters
import Amazonka.QuickSight.Types.AmazonOpenSearchParameters
import Amazonka.QuickSight.Types.AthenaParameters
import Amazonka.QuickSight.Types.AuroraParameters
import Amazonka.QuickSight.Types.AuroraPostgreSqlParameters
import Amazonka.QuickSight.Types.AwsIotAnalyticsParameters
import Amazonka.QuickSight.Types.JiraParameters
import Amazonka.QuickSight.Types.MariaDbParameters
import Amazonka.QuickSight.Types.MySqlParameters
import Amazonka.QuickSight.Types.OracleParameters
import Amazonka.QuickSight.Types.PostgreSqlParameters
import Amazonka.QuickSight.Types.PrestoParameters
import Amazonka.QuickSight.Types.RdsParameters
import Amazonka.QuickSight.Types.RedshiftParameters
import Amazonka.QuickSight.Types.S3Parameters
import Amazonka.QuickSight.Types.ServiceNowParameters
import Amazonka.QuickSight.Types.SnowflakeParameters
import Amazonka.QuickSight.Types.SparkParameters
import Amazonka.QuickSight.Types.SqlServerParameters
import Amazonka.QuickSight.Types.TeradataParameters
import Amazonka.QuickSight.Types.TwitterParameters

-- | The parameters that Amazon QuickSight uses to connect to your underlying
-- data source. This is a variant type structure. For this structure to be
-- valid, only one of the attributes can be non-null.
--
-- /See:/ 'newDataSourceParameters' smart constructor.
data DataSourceParameters = DataSourceParameters'
  { -- | The parameters for Amazon RDS.
    rdsParameters :: Prelude.Maybe RdsParameters,
    -- | The parameters for S3.
    s3Parameters :: Prelude.Maybe S3Parameters,
    -- | The parameters for Amazon Athena.
    athenaParameters :: Prelude.Maybe AthenaParameters,
    -- | The parameters for Teradata.
    teradataParameters :: Prelude.Maybe TeradataParameters,
    -- | The parameters for Amazon Aurora MySQL.
    auroraParameters :: Prelude.Maybe AuroraParameters,
    -- | The parameters for IoT Analytics.
    awsIotAnalyticsParameters :: Prelude.Maybe AwsIotAnalyticsParameters,
    amazonOpenSearchParameters :: Prelude.Maybe AmazonOpenSearchParameters,
    -- | The parameters for SQL Server.
    sqlServerParameters :: Prelude.Maybe SqlServerParameters,
    -- | The parameters for Amazon Redshift.
    redshiftParameters :: Prelude.Maybe RedshiftParameters,
    -- | The parameters for PostgreSQL.
    postgreSqlParameters :: Prelude.Maybe PostgreSqlParameters,
    -- | The parameters for Oracle.
    oracleParameters :: Prelude.Maybe OracleParameters,
    -- | The parameters for OpenSearch.
    amazonElasticsearchParameters :: Prelude.Maybe AmazonElasticsearchParameters,
    -- | The parameters for Twitter.
    twitterParameters :: Prelude.Maybe TwitterParameters,
    -- | The parameters for MariaDB.
    mariaDbParameters :: Prelude.Maybe MariaDbParameters,
    -- | The parameters for Jira.
    jiraParameters :: Prelude.Maybe JiraParameters,
    -- | The parameters for Snowflake.
    snowflakeParameters :: Prelude.Maybe SnowflakeParameters,
    -- | The parameters for Presto.
    prestoParameters :: Prelude.Maybe PrestoParameters,
    -- | The parameters for MySQL.
    mySqlParameters :: Prelude.Maybe MySqlParameters,
    -- | The parameters for Amazon Aurora.
    auroraPostgreSqlParameters :: Prelude.Maybe AuroraPostgreSqlParameters,
    -- | The parameters for Spark.
    sparkParameters :: Prelude.Maybe SparkParameters,
    -- | The parameters for ServiceNow.
    serviceNowParameters :: Prelude.Maybe ServiceNowParameters
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSourceParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rdsParameters', 'dataSourceParameters_rdsParameters' - The parameters for Amazon RDS.
--
-- 's3Parameters', 'dataSourceParameters_s3Parameters' - The parameters for S3.
--
-- 'athenaParameters', 'dataSourceParameters_athenaParameters' - The parameters for Amazon Athena.
--
-- 'teradataParameters', 'dataSourceParameters_teradataParameters' - The parameters for Teradata.
--
-- 'auroraParameters', 'dataSourceParameters_auroraParameters' - The parameters for Amazon Aurora MySQL.
--
-- 'awsIotAnalyticsParameters', 'dataSourceParameters_awsIotAnalyticsParameters' - The parameters for IoT Analytics.
--
-- 'amazonOpenSearchParameters', 'dataSourceParameters_amazonOpenSearchParameters' - Undocumented member.
--
-- 'sqlServerParameters', 'dataSourceParameters_sqlServerParameters' - The parameters for SQL Server.
--
-- 'redshiftParameters', 'dataSourceParameters_redshiftParameters' - The parameters for Amazon Redshift.
--
-- 'postgreSqlParameters', 'dataSourceParameters_postgreSqlParameters' - The parameters for PostgreSQL.
--
-- 'oracleParameters', 'dataSourceParameters_oracleParameters' - The parameters for Oracle.
--
-- 'amazonElasticsearchParameters', 'dataSourceParameters_amazonElasticsearchParameters' - The parameters for OpenSearch.
--
-- 'twitterParameters', 'dataSourceParameters_twitterParameters' - The parameters for Twitter.
--
-- 'mariaDbParameters', 'dataSourceParameters_mariaDbParameters' - The parameters for MariaDB.
--
-- 'jiraParameters', 'dataSourceParameters_jiraParameters' - The parameters for Jira.
--
-- 'snowflakeParameters', 'dataSourceParameters_snowflakeParameters' - The parameters for Snowflake.
--
-- 'prestoParameters', 'dataSourceParameters_prestoParameters' - The parameters for Presto.
--
-- 'mySqlParameters', 'dataSourceParameters_mySqlParameters' - The parameters for MySQL.
--
-- 'auroraPostgreSqlParameters', 'dataSourceParameters_auroraPostgreSqlParameters' - The parameters for Amazon Aurora.
--
-- 'sparkParameters', 'dataSourceParameters_sparkParameters' - The parameters for Spark.
--
-- 'serviceNowParameters', 'dataSourceParameters_serviceNowParameters' - The parameters for ServiceNow.
newDataSourceParameters ::
  DataSourceParameters
newDataSourceParameters =
  DataSourceParameters'
    { rdsParameters =
        Prelude.Nothing,
      s3Parameters = Prelude.Nothing,
      athenaParameters = Prelude.Nothing,
      teradataParameters = Prelude.Nothing,
      auroraParameters = Prelude.Nothing,
      awsIotAnalyticsParameters = Prelude.Nothing,
      amazonOpenSearchParameters = Prelude.Nothing,
      sqlServerParameters = Prelude.Nothing,
      redshiftParameters = Prelude.Nothing,
      postgreSqlParameters = Prelude.Nothing,
      oracleParameters = Prelude.Nothing,
      amazonElasticsearchParameters = Prelude.Nothing,
      twitterParameters = Prelude.Nothing,
      mariaDbParameters = Prelude.Nothing,
      jiraParameters = Prelude.Nothing,
      snowflakeParameters = Prelude.Nothing,
      prestoParameters = Prelude.Nothing,
      mySqlParameters = Prelude.Nothing,
      auroraPostgreSqlParameters = Prelude.Nothing,
      sparkParameters = Prelude.Nothing,
      serviceNowParameters = Prelude.Nothing
    }

-- | The parameters for Amazon RDS.
dataSourceParameters_rdsParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe RdsParameters)
dataSourceParameters_rdsParameters = Lens.lens (\DataSourceParameters' {rdsParameters} -> rdsParameters) (\s@DataSourceParameters' {} a -> s {rdsParameters = a} :: DataSourceParameters)

-- | The parameters for S3.
dataSourceParameters_s3Parameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe S3Parameters)
dataSourceParameters_s3Parameters = Lens.lens (\DataSourceParameters' {s3Parameters} -> s3Parameters) (\s@DataSourceParameters' {} a -> s {s3Parameters = a} :: DataSourceParameters)

-- | The parameters for Amazon Athena.
dataSourceParameters_athenaParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AthenaParameters)
dataSourceParameters_athenaParameters = Lens.lens (\DataSourceParameters' {athenaParameters} -> athenaParameters) (\s@DataSourceParameters' {} a -> s {athenaParameters = a} :: DataSourceParameters)

-- | The parameters for Teradata.
dataSourceParameters_teradataParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe TeradataParameters)
dataSourceParameters_teradataParameters = Lens.lens (\DataSourceParameters' {teradataParameters} -> teradataParameters) (\s@DataSourceParameters' {} a -> s {teradataParameters = a} :: DataSourceParameters)

-- | The parameters for Amazon Aurora MySQL.
dataSourceParameters_auroraParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AuroraParameters)
dataSourceParameters_auroraParameters = Lens.lens (\DataSourceParameters' {auroraParameters} -> auroraParameters) (\s@DataSourceParameters' {} a -> s {auroraParameters = a} :: DataSourceParameters)

-- | The parameters for IoT Analytics.
dataSourceParameters_awsIotAnalyticsParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AwsIotAnalyticsParameters)
dataSourceParameters_awsIotAnalyticsParameters = Lens.lens (\DataSourceParameters' {awsIotAnalyticsParameters} -> awsIotAnalyticsParameters) (\s@DataSourceParameters' {} a -> s {awsIotAnalyticsParameters = a} :: DataSourceParameters)

-- | Undocumented member.
dataSourceParameters_amazonOpenSearchParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AmazonOpenSearchParameters)
dataSourceParameters_amazonOpenSearchParameters = Lens.lens (\DataSourceParameters' {amazonOpenSearchParameters} -> amazonOpenSearchParameters) (\s@DataSourceParameters' {} a -> s {amazonOpenSearchParameters = a} :: DataSourceParameters)

-- | The parameters for SQL Server.
dataSourceParameters_sqlServerParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe SqlServerParameters)
dataSourceParameters_sqlServerParameters = Lens.lens (\DataSourceParameters' {sqlServerParameters} -> sqlServerParameters) (\s@DataSourceParameters' {} a -> s {sqlServerParameters = a} :: DataSourceParameters)

-- | The parameters for Amazon Redshift.
dataSourceParameters_redshiftParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe RedshiftParameters)
dataSourceParameters_redshiftParameters = Lens.lens (\DataSourceParameters' {redshiftParameters} -> redshiftParameters) (\s@DataSourceParameters' {} a -> s {redshiftParameters = a} :: DataSourceParameters)

-- | The parameters for PostgreSQL.
dataSourceParameters_postgreSqlParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe PostgreSqlParameters)
dataSourceParameters_postgreSqlParameters = Lens.lens (\DataSourceParameters' {postgreSqlParameters} -> postgreSqlParameters) (\s@DataSourceParameters' {} a -> s {postgreSqlParameters = a} :: DataSourceParameters)

-- | The parameters for Oracle.
dataSourceParameters_oracleParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe OracleParameters)
dataSourceParameters_oracleParameters = Lens.lens (\DataSourceParameters' {oracleParameters} -> oracleParameters) (\s@DataSourceParameters' {} a -> s {oracleParameters = a} :: DataSourceParameters)

-- | The parameters for OpenSearch.
dataSourceParameters_amazonElasticsearchParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AmazonElasticsearchParameters)
dataSourceParameters_amazonElasticsearchParameters = Lens.lens (\DataSourceParameters' {amazonElasticsearchParameters} -> amazonElasticsearchParameters) (\s@DataSourceParameters' {} a -> s {amazonElasticsearchParameters = a} :: DataSourceParameters)

-- | The parameters for Twitter.
dataSourceParameters_twitterParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe TwitterParameters)
dataSourceParameters_twitterParameters = Lens.lens (\DataSourceParameters' {twitterParameters} -> twitterParameters) (\s@DataSourceParameters' {} a -> s {twitterParameters = a} :: DataSourceParameters)

-- | The parameters for MariaDB.
dataSourceParameters_mariaDbParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe MariaDbParameters)
dataSourceParameters_mariaDbParameters = Lens.lens (\DataSourceParameters' {mariaDbParameters} -> mariaDbParameters) (\s@DataSourceParameters' {} a -> s {mariaDbParameters = a} :: DataSourceParameters)

-- | The parameters for Jira.
dataSourceParameters_jiraParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe JiraParameters)
dataSourceParameters_jiraParameters = Lens.lens (\DataSourceParameters' {jiraParameters} -> jiraParameters) (\s@DataSourceParameters' {} a -> s {jiraParameters = a} :: DataSourceParameters)

-- | The parameters for Snowflake.
dataSourceParameters_snowflakeParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe SnowflakeParameters)
dataSourceParameters_snowflakeParameters = Lens.lens (\DataSourceParameters' {snowflakeParameters} -> snowflakeParameters) (\s@DataSourceParameters' {} a -> s {snowflakeParameters = a} :: DataSourceParameters)

-- | The parameters for Presto.
dataSourceParameters_prestoParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe PrestoParameters)
dataSourceParameters_prestoParameters = Lens.lens (\DataSourceParameters' {prestoParameters} -> prestoParameters) (\s@DataSourceParameters' {} a -> s {prestoParameters = a} :: DataSourceParameters)

-- | The parameters for MySQL.
dataSourceParameters_mySqlParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe MySqlParameters)
dataSourceParameters_mySqlParameters = Lens.lens (\DataSourceParameters' {mySqlParameters} -> mySqlParameters) (\s@DataSourceParameters' {} a -> s {mySqlParameters = a} :: DataSourceParameters)

-- | The parameters for Amazon Aurora.
dataSourceParameters_auroraPostgreSqlParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe AuroraPostgreSqlParameters)
dataSourceParameters_auroraPostgreSqlParameters = Lens.lens (\DataSourceParameters' {auroraPostgreSqlParameters} -> auroraPostgreSqlParameters) (\s@DataSourceParameters' {} a -> s {auroraPostgreSqlParameters = a} :: DataSourceParameters)

-- | The parameters for Spark.
dataSourceParameters_sparkParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe SparkParameters)
dataSourceParameters_sparkParameters = Lens.lens (\DataSourceParameters' {sparkParameters} -> sparkParameters) (\s@DataSourceParameters' {} a -> s {sparkParameters = a} :: DataSourceParameters)

-- | The parameters for ServiceNow.
dataSourceParameters_serviceNowParameters :: Lens.Lens' DataSourceParameters (Prelude.Maybe ServiceNowParameters)
dataSourceParameters_serviceNowParameters = Lens.lens (\DataSourceParameters' {serviceNowParameters} -> serviceNowParameters) (\s@DataSourceParameters' {} a -> s {serviceNowParameters = a} :: DataSourceParameters)

instance Core.FromJSON DataSourceParameters where
  parseJSON =
    Core.withObject
      "DataSourceParameters"
      ( \x ->
          DataSourceParameters'
            Prelude.<$> (x Core..:? "RdsParameters")
            Prelude.<*> (x Core..:? "S3Parameters")
            Prelude.<*> (x Core..:? "AthenaParameters")
            Prelude.<*> (x Core..:? "TeradataParameters")
            Prelude.<*> (x Core..:? "AuroraParameters")
            Prelude.<*> (x Core..:? "AwsIotAnalyticsParameters")
            Prelude.<*> (x Core..:? "AmazonOpenSearchParameters")
            Prelude.<*> (x Core..:? "SqlServerParameters")
            Prelude.<*> (x Core..:? "RedshiftParameters")
            Prelude.<*> (x Core..:? "PostgreSqlParameters")
            Prelude.<*> (x Core..:? "OracleParameters")
            Prelude.<*> (x Core..:? "AmazonElasticsearchParameters")
            Prelude.<*> (x Core..:? "TwitterParameters")
            Prelude.<*> (x Core..:? "MariaDbParameters")
            Prelude.<*> (x Core..:? "JiraParameters")
            Prelude.<*> (x Core..:? "SnowflakeParameters")
            Prelude.<*> (x Core..:? "PrestoParameters")
            Prelude.<*> (x Core..:? "MySqlParameters")
            Prelude.<*> (x Core..:? "AuroraPostgreSqlParameters")
            Prelude.<*> (x Core..:? "SparkParameters")
            Prelude.<*> (x Core..:? "ServiceNowParameters")
      )

instance Prelude.Hashable DataSourceParameters where
  hashWithSalt salt' DataSourceParameters' {..} =
    salt' `Prelude.hashWithSalt` serviceNowParameters
      `Prelude.hashWithSalt` sparkParameters
      `Prelude.hashWithSalt` auroraPostgreSqlParameters
      `Prelude.hashWithSalt` mySqlParameters
      `Prelude.hashWithSalt` prestoParameters
      `Prelude.hashWithSalt` snowflakeParameters
      `Prelude.hashWithSalt` jiraParameters
      `Prelude.hashWithSalt` mariaDbParameters
      `Prelude.hashWithSalt` twitterParameters
      `Prelude.hashWithSalt` amazonElasticsearchParameters
      `Prelude.hashWithSalt` oracleParameters
      `Prelude.hashWithSalt` postgreSqlParameters
      `Prelude.hashWithSalt` redshiftParameters
      `Prelude.hashWithSalt` sqlServerParameters
      `Prelude.hashWithSalt` amazonOpenSearchParameters
      `Prelude.hashWithSalt` awsIotAnalyticsParameters
      `Prelude.hashWithSalt` auroraParameters
      `Prelude.hashWithSalt` teradataParameters
      `Prelude.hashWithSalt` athenaParameters
      `Prelude.hashWithSalt` s3Parameters
      `Prelude.hashWithSalt` rdsParameters

instance Prelude.NFData DataSourceParameters where
  rnf DataSourceParameters' {..} =
    Prelude.rnf rdsParameters
      `Prelude.seq` Prelude.rnf serviceNowParameters
      `Prelude.seq` Prelude.rnf sparkParameters
      `Prelude.seq` Prelude.rnf auroraPostgreSqlParameters
      `Prelude.seq` Prelude.rnf mySqlParameters
      `Prelude.seq` Prelude.rnf prestoParameters
      `Prelude.seq` Prelude.rnf snowflakeParameters
      `Prelude.seq` Prelude.rnf jiraParameters
      `Prelude.seq` Prelude.rnf mariaDbParameters
      `Prelude.seq` Prelude.rnf twitterParameters
      `Prelude.seq` Prelude.rnf amazonElasticsearchParameters
      `Prelude.seq` Prelude.rnf oracleParameters
      `Prelude.seq` Prelude.rnf postgreSqlParameters
      `Prelude.seq` Prelude.rnf redshiftParameters
      `Prelude.seq` Prelude.rnf sqlServerParameters
      `Prelude.seq` Prelude.rnf amazonOpenSearchParameters
      `Prelude.seq` Prelude.rnf awsIotAnalyticsParameters
      `Prelude.seq` Prelude.rnf auroraParameters
      `Prelude.seq` Prelude.rnf teradataParameters
      `Prelude.seq` Prelude.rnf athenaParameters
      `Prelude.seq` Prelude.rnf s3Parameters

instance Core.ToJSON DataSourceParameters where
  toJSON DataSourceParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RdsParameters" Core..=) Prelude.<$> rdsParameters,
            ("S3Parameters" Core..=) Prelude.<$> s3Parameters,
            ("AthenaParameters" Core..=)
              Prelude.<$> athenaParameters,
            ("TeradataParameters" Core..=)
              Prelude.<$> teradataParameters,
            ("AuroraParameters" Core..=)
              Prelude.<$> auroraParameters,
            ("AwsIotAnalyticsParameters" Core..=)
              Prelude.<$> awsIotAnalyticsParameters,
            ("AmazonOpenSearchParameters" Core..=)
              Prelude.<$> amazonOpenSearchParameters,
            ("SqlServerParameters" Core..=)
              Prelude.<$> sqlServerParameters,
            ("RedshiftParameters" Core..=)
              Prelude.<$> redshiftParameters,
            ("PostgreSqlParameters" Core..=)
              Prelude.<$> postgreSqlParameters,
            ("OracleParameters" Core..=)
              Prelude.<$> oracleParameters,
            ("AmazonElasticsearchParameters" Core..=)
              Prelude.<$> amazonElasticsearchParameters,
            ("TwitterParameters" Core..=)
              Prelude.<$> twitterParameters,
            ("MariaDbParameters" Core..=)
              Prelude.<$> mariaDbParameters,
            ("JiraParameters" Core..=)
              Prelude.<$> jiraParameters,
            ("SnowflakeParameters" Core..=)
              Prelude.<$> snowflakeParameters,
            ("PrestoParameters" Core..=)
              Prelude.<$> prestoParameters,
            ("MySqlParameters" Core..=)
              Prelude.<$> mySqlParameters,
            ("AuroraPostgreSqlParameters" Core..=)
              Prelude.<$> auroraPostgreSqlParameters,
            ("SparkParameters" Core..=)
              Prelude.<$> sparkParameters,
            ("ServiceNowParameters" Core..=)
              Prelude.<$> serviceNowParameters
          ]
      )
