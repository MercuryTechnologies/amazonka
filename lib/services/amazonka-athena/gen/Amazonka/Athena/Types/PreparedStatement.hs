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
-- Module      : Amazonka.Athena.Types.PreparedStatement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Athena.Types.PreparedStatement where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A prepared SQL statement for use with Athena.
--
-- /See:/ 'newPreparedStatement' smart constructor.
data PreparedStatement = PreparedStatement'
  { -- | The last modified time of the prepared statement.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The query string for the prepared statement.
    queryStatement :: Prelude.Maybe Prelude.Text,
    -- | The name of the prepared statement.
    statementName :: Prelude.Maybe Prelude.Text,
    -- | The description of the prepared statement.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the workgroup to which the prepared statement belongs.
    workGroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PreparedStatement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTime', 'preparedStatement_lastModifiedTime' - The last modified time of the prepared statement.
--
-- 'queryStatement', 'preparedStatement_queryStatement' - The query string for the prepared statement.
--
-- 'statementName', 'preparedStatement_statementName' - The name of the prepared statement.
--
-- 'description', 'preparedStatement_description' - The description of the prepared statement.
--
-- 'workGroupName', 'preparedStatement_workGroupName' - The name of the workgroup to which the prepared statement belongs.
newPreparedStatement ::
  PreparedStatement
newPreparedStatement =
  PreparedStatement'
    { lastModifiedTime =
        Prelude.Nothing,
      queryStatement = Prelude.Nothing,
      statementName = Prelude.Nothing,
      description = Prelude.Nothing,
      workGroupName = Prelude.Nothing
    }

-- | The last modified time of the prepared statement.
preparedStatement_lastModifiedTime :: Lens.Lens' PreparedStatement (Prelude.Maybe Prelude.UTCTime)
preparedStatement_lastModifiedTime = Lens.lens (\PreparedStatement' {lastModifiedTime} -> lastModifiedTime) (\s@PreparedStatement' {} a -> s {lastModifiedTime = a} :: PreparedStatement) Prelude.. Lens.mapping Core._Time

-- | The query string for the prepared statement.
preparedStatement_queryStatement :: Lens.Lens' PreparedStatement (Prelude.Maybe Prelude.Text)
preparedStatement_queryStatement = Lens.lens (\PreparedStatement' {queryStatement} -> queryStatement) (\s@PreparedStatement' {} a -> s {queryStatement = a} :: PreparedStatement)

-- | The name of the prepared statement.
preparedStatement_statementName :: Lens.Lens' PreparedStatement (Prelude.Maybe Prelude.Text)
preparedStatement_statementName = Lens.lens (\PreparedStatement' {statementName} -> statementName) (\s@PreparedStatement' {} a -> s {statementName = a} :: PreparedStatement)

-- | The description of the prepared statement.
preparedStatement_description :: Lens.Lens' PreparedStatement (Prelude.Maybe Prelude.Text)
preparedStatement_description = Lens.lens (\PreparedStatement' {description} -> description) (\s@PreparedStatement' {} a -> s {description = a} :: PreparedStatement)

-- | The name of the workgroup to which the prepared statement belongs.
preparedStatement_workGroupName :: Lens.Lens' PreparedStatement (Prelude.Maybe Prelude.Text)
preparedStatement_workGroupName = Lens.lens (\PreparedStatement' {workGroupName} -> workGroupName) (\s@PreparedStatement' {} a -> s {workGroupName = a} :: PreparedStatement)

instance Core.FromJSON PreparedStatement where
  parseJSON =
    Core.withObject
      "PreparedStatement"
      ( \x ->
          PreparedStatement'
            Prelude.<$> (x Core..:? "LastModifiedTime")
            Prelude.<*> (x Core..:? "QueryStatement")
            Prelude.<*> (x Core..:? "StatementName")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "WorkGroupName")
      )

instance Prelude.Hashable PreparedStatement where
  hashWithSalt salt' PreparedStatement' {..} =
    salt' `Prelude.hashWithSalt` workGroupName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` statementName
      `Prelude.hashWithSalt` queryStatement
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData PreparedStatement where
  rnf PreparedStatement' {..} =
    Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf workGroupName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf statementName
      `Prelude.seq` Prelude.rnf queryStatement
