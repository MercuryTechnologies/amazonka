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
-- Module      : Amazonka.RedshiftData.BatchExecuteStatement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Runs one or more SQL statements, which can be data manipulation language
-- (DML) or data definition language (DDL). Depending on the authorization
-- method, use one of the following combinations of request parameters:
--
-- -   Secrets Manager - specify the Amazon Resource Name (ARN) of the
--     secret, the database name, and the cluster identifier that matches
--     the cluster in the secret.
--
-- -   Temporary credentials - specify the cluster identifier, the database
--     name, and the database user name. Permission to call the
--     @redshift:GetClusterCredentials@ operation is required to use this
--     method.
module Amazonka.RedshiftData.BatchExecuteStatement
  ( -- * Creating a Request
    BatchExecuteStatement (..),
    newBatchExecuteStatement,

    -- * Request Lenses
    batchExecuteStatement_dbUser,
    batchExecuteStatement_statementName,
    batchExecuteStatement_secretArn,
    batchExecuteStatement_withEvent,
    batchExecuteStatement_clusterIdentifier,
    batchExecuteStatement_database,
    batchExecuteStatement_sqls,

    -- * Destructuring the Response
    BatchExecuteStatementResponse (..),
    newBatchExecuteStatementResponse,

    -- * Response Lenses
    batchExecuteStatementResponse_dbUser,
    batchExecuteStatementResponse_database,
    batchExecuteStatementResponse_createdAt,
    batchExecuteStatementResponse_clusterIdentifier,
    batchExecuteStatementResponse_id,
    batchExecuteStatementResponse_secretArn,
    batchExecuteStatementResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftData.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchExecuteStatement' smart constructor.
data BatchExecuteStatement = BatchExecuteStatement'
  { -- | The database user name. This parameter is required when authenticating
    -- using temporary credentials.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The name of the SQL statements. You can name the SQL statements when you
    -- create them to identify the query.
    statementName :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the secret that enables access to the database. This
    -- parameter is required when authenticating using Secrets Manager.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | A value that indicates whether to send an event to the Amazon
    -- EventBridge event bus after the SQL statements run.
    withEvent :: Prelude.Maybe Prelude.Bool,
    -- | The cluster identifier. This parameter is required when authenticating
    -- using either Secrets Manager or temporary credentials.
    clusterIdentifier :: Prelude.Text,
    -- | The name of the database. This parameter is required when authenticating
    -- using either Secrets Manager or temporary credentials.
    database :: Prelude.Text,
    -- | One or more SQL statements to run.
    sqls :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchExecuteStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbUser', 'batchExecuteStatement_dbUser' - The database user name. This parameter is required when authenticating
-- using temporary credentials.
--
-- 'statementName', 'batchExecuteStatement_statementName' - The name of the SQL statements. You can name the SQL statements when you
-- create them to identify the query.
--
-- 'secretArn', 'batchExecuteStatement_secretArn' - The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
--
-- 'withEvent', 'batchExecuteStatement_withEvent' - A value that indicates whether to send an event to the Amazon
-- EventBridge event bus after the SQL statements run.
--
-- 'clusterIdentifier', 'batchExecuteStatement_clusterIdentifier' - The cluster identifier. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
--
-- 'database', 'batchExecuteStatement_database' - The name of the database. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
--
-- 'sqls', 'batchExecuteStatement_sqls' - One or more SQL statements to run.
newBatchExecuteStatement ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  -- | 'database'
  Prelude.Text ->
  -- | 'sqls'
  Prelude.NonEmpty Prelude.Text ->
  BatchExecuteStatement
newBatchExecuteStatement
  pClusterIdentifier_
  pDatabase_
  pSqls_ =
    BatchExecuteStatement'
      { dbUser = Prelude.Nothing,
        statementName = Prelude.Nothing,
        secretArn = Prelude.Nothing,
        withEvent = Prelude.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        database = pDatabase_,
        sqls = Lens.coerced Lens.# pSqls_
      }

-- | The database user name. This parameter is required when authenticating
-- using temporary credentials.
batchExecuteStatement_dbUser :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_dbUser = Lens.lens (\BatchExecuteStatement' {dbUser} -> dbUser) (\s@BatchExecuteStatement' {} a -> s {dbUser = a} :: BatchExecuteStatement)

-- | The name of the SQL statements. You can name the SQL statements when you
-- create them to identify the query.
batchExecuteStatement_statementName :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_statementName = Lens.lens (\BatchExecuteStatement' {statementName} -> statementName) (\s@BatchExecuteStatement' {} a -> s {statementName = a} :: BatchExecuteStatement)

-- | The name or ARN of the secret that enables access to the database. This
-- parameter is required when authenticating using Secrets Manager.
batchExecuteStatement_secretArn :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Text)
batchExecuteStatement_secretArn = Lens.lens (\BatchExecuteStatement' {secretArn} -> secretArn) (\s@BatchExecuteStatement' {} a -> s {secretArn = a} :: BatchExecuteStatement)

-- | A value that indicates whether to send an event to the Amazon
-- EventBridge event bus after the SQL statements run.
batchExecuteStatement_withEvent :: Lens.Lens' BatchExecuteStatement (Prelude.Maybe Prelude.Bool)
batchExecuteStatement_withEvent = Lens.lens (\BatchExecuteStatement' {withEvent} -> withEvent) (\s@BatchExecuteStatement' {} a -> s {withEvent = a} :: BatchExecuteStatement)

-- | The cluster identifier. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
batchExecuteStatement_clusterIdentifier :: Lens.Lens' BatchExecuteStatement Prelude.Text
batchExecuteStatement_clusterIdentifier = Lens.lens (\BatchExecuteStatement' {clusterIdentifier} -> clusterIdentifier) (\s@BatchExecuteStatement' {} a -> s {clusterIdentifier = a} :: BatchExecuteStatement)

-- | The name of the database. This parameter is required when authenticating
-- using either Secrets Manager or temporary credentials.
batchExecuteStatement_database :: Lens.Lens' BatchExecuteStatement Prelude.Text
batchExecuteStatement_database = Lens.lens (\BatchExecuteStatement' {database} -> database) (\s@BatchExecuteStatement' {} a -> s {database = a} :: BatchExecuteStatement)

-- | One or more SQL statements to run.
batchExecuteStatement_sqls :: Lens.Lens' BatchExecuteStatement (Prelude.NonEmpty Prelude.Text)
batchExecuteStatement_sqls = Lens.lens (\BatchExecuteStatement' {sqls} -> sqls) (\s@BatchExecuteStatement' {} a -> s {sqls = a} :: BatchExecuteStatement) Prelude.. Lens.coerced

instance Core.AWSRequest BatchExecuteStatement where
  type
    AWSResponse BatchExecuteStatement =
      BatchExecuteStatementResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchExecuteStatementResponse'
            Prelude.<$> (x Core..?> "DbUser")
            Prelude.<*> (x Core..?> "Database")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "ClusterIdentifier")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "SecretArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchExecuteStatement where
  hashWithSalt salt' BatchExecuteStatement' {..} =
    salt' `Prelude.hashWithSalt` sqls
      `Prelude.hashWithSalt` database
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` withEvent
      `Prelude.hashWithSalt` secretArn
      `Prelude.hashWithSalt` statementName
      `Prelude.hashWithSalt` dbUser

instance Prelude.NFData BatchExecuteStatement where
  rnf BatchExecuteStatement' {..} =
    Prelude.rnf dbUser `Prelude.seq` Prelude.rnf sqls
      `Prelude.seq` Prelude.rnf database
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf withEvent
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf statementName

instance Core.ToHeaders BatchExecuteStatement where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "RedshiftData.BatchExecuteStatement" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchExecuteStatement where
  toJSON BatchExecuteStatement' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DbUser" Core..=) Prelude.<$> dbUser,
            ("StatementName" Core..=) Prelude.<$> statementName,
            ("SecretArn" Core..=) Prelude.<$> secretArn,
            ("WithEvent" Core..=) Prelude.<$> withEvent,
            Prelude.Just
              ("ClusterIdentifier" Core..= clusterIdentifier),
            Prelude.Just ("Database" Core..= database),
            Prelude.Just ("Sqls" Core..= sqls)
          ]
      )

instance Core.ToPath BatchExecuteStatement where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchExecuteStatement where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchExecuteStatementResponse' smart constructor.
data BatchExecuteStatementResponse = BatchExecuteStatementResponse'
  { -- | The database user name.
    dbUser :: Prelude.Maybe Prelude.Text,
    -- | The name of the database.
    database :: Prelude.Maybe Prelude.Text,
    -- | The date and time (UTC) the statement was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The cluster identifier.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the SQL statement whose results are to be fetched.
    -- This value is a universally unique identifier (UUID) generated by Amazon
    -- Redshift Data API. This identifier is returned by
    -- @BatchExecuteStatment@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name or ARN of the secret that enables access to the database.
    secretArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchExecuteStatementResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dbUser', 'batchExecuteStatementResponse_dbUser' - The database user name.
--
-- 'database', 'batchExecuteStatementResponse_database' - The name of the database.
--
-- 'createdAt', 'batchExecuteStatementResponse_createdAt' - The date and time (UTC) the statement was created.
--
-- 'clusterIdentifier', 'batchExecuteStatementResponse_clusterIdentifier' - The cluster identifier.
--
-- 'id', 'batchExecuteStatementResponse_id' - The identifier of the SQL statement whose results are to be fetched.
-- This value is a universally unique identifier (UUID) generated by Amazon
-- Redshift Data API. This identifier is returned by
-- @BatchExecuteStatment@.
--
-- 'secretArn', 'batchExecuteStatementResponse_secretArn' - The name or ARN of the secret that enables access to the database.
--
-- 'httpStatus', 'batchExecuteStatementResponse_httpStatus' - The response's http status code.
newBatchExecuteStatementResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchExecuteStatementResponse
newBatchExecuteStatementResponse pHttpStatus_ =
  BatchExecuteStatementResponse'
    { dbUser =
        Prelude.Nothing,
      database = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      id = Prelude.Nothing,
      secretArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The database user name.
batchExecuteStatementResponse_dbUser :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.Text)
batchExecuteStatementResponse_dbUser = Lens.lens (\BatchExecuteStatementResponse' {dbUser} -> dbUser) (\s@BatchExecuteStatementResponse' {} a -> s {dbUser = a} :: BatchExecuteStatementResponse)

-- | The name of the database.
batchExecuteStatementResponse_database :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.Text)
batchExecuteStatementResponse_database = Lens.lens (\BatchExecuteStatementResponse' {database} -> database) (\s@BatchExecuteStatementResponse' {} a -> s {database = a} :: BatchExecuteStatementResponse)

-- | The date and time (UTC) the statement was created.
batchExecuteStatementResponse_createdAt :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.UTCTime)
batchExecuteStatementResponse_createdAt = Lens.lens (\BatchExecuteStatementResponse' {createdAt} -> createdAt) (\s@BatchExecuteStatementResponse' {} a -> s {createdAt = a} :: BatchExecuteStatementResponse) Prelude.. Lens.mapping Core._Time

-- | The cluster identifier.
batchExecuteStatementResponse_clusterIdentifier :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.Text)
batchExecuteStatementResponse_clusterIdentifier = Lens.lens (\BatchExecuteStatementResponse' {clusterIdentifier} -> clusterIdentifier) (\s@BatchExecuteStatementResponse' {} a -> s {clusterIdentifier = a} :: BatchExecuteStatementResponse)

-- | The identifier of the SQL statement whose results are to be fetched.
-- This value is a universally unique identifier (UUID) generated by Amazon
-- Redshift Data API. This identifier is returned by
-- @BatchExecuteStatment@.
batchExecuteStatementResponse_id :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.Text)
batchExecuteStatementResponse_id = Lens.lens (\BatchExecuteStatementResponse' {id} -> id) (\s@BatchExecuteStatementResponse' {} a -> s {id = a} :: BatchExecuteStatementResponse)

-- | The name or ARN of the secret that enables access to the database.
batchExecuteStatementResponse_secretArn :: Lens.Lens' BatchExecuteStatementResponse (Prelude.Maybe Prelude.Text)
batchExecuteStatementResponse_secretArn = Lens.lens (\BatchExecuteStatementResponse' {secretArn} -> secretArn) (\s@BatchExecuteStatementResponse' {} a -> s {secretArn = a} :: BatchExecuteStatementResponse)

-- | The response's http status code.
batchExecuteStatementResponse_httpStatus :: Lens.Lens' BatchExecuteStatementResponse Prelude.Int
batchExecuteStatementResponse_httpStatus = Lens.lens (\BatchExecuteStatementResponse' {httpStatus} -> httpStatus) (\s@BatchExecuteStatementResponse' {} a -> s {httpStatus = a} :: BatchExecuteStatementResponse)

instance Prelude.NFData BatchExecuteStatementResponse where
  rnf BatchExecuteStatementResponse' {..} =
    Prelude.rnf dbUser
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf secretArn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf database
