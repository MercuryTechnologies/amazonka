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
-- Module      : Amazonka.DataBrew.Types.DatabaseTableOutputOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.DatabaseTableOutputOptions where

import qualified Amazonka.Core as Core
import Amazonka.DataBrew.Types.S3Location
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents options that specify how and where DataBrew writes the
-- database output generated by recipe jobs.
--
-- /See:/ 'newDatabaseTableOutputOptions' smart constructor.
data DatabaseTableOutputOptions = DatabaseTableOutputOptions'
  { -- | Represents an Amazon S3 location (bucket name and object key) where
    -- DataBrew can store intermediate results.
    tempDirectory :: Prelude.Maybe S3Location,
    -- | A prefix for the name of a table DataBrew will create in the database.
    tableName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatabaseTableOutputOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tempDirectory', 'databaseTableOutputOptions_tempDirectory' - Represents an Amazon S3 location (bucket name and object key) where
-- DataBrew can store intermediate results.
--
-- 'tableName', 'databaseTableOutputOptions_tableName' - A prefix for the name of a table DataBrew will create in the database.
newDatabaseTableOutputOptions ::
  -- | 'tableName'
  Prelude.Text ->
  DatabaseTableOutputOptions
newDatabaseTableOutputOptions pTableName_ =
  DatabaseTableOutputOptions'
    { tempDirectory =
        Prelude.Nothing,
      tableName = pTableName_
    }

-- | Represents an Amazon S3 location (bucket name and object key) where
-- DataBrew can store intermediate results.
databaseTableOutputOptions_tempDirectory :: Lens.Lens' DatabaseTableOutputOptions (Prelude.Maybe S3Location)
databaseTableOutputOptions_tempDirectory = Lens.lens (\DatabaseTableOutputOptions' {tempDirectory} -> tempDirectory) (\s@DatabaseTableOutputOptions' {} a -> s {tempDirectory = a} :: DatabaseTableOutputOptions)

-- | A prefix for the name of a table DataBrew will create in the database.
databaseTableOutputOptions_tableName :: Lens.Lens' DatabaseTableOutputOptions Prelude.Text
databaseTableOutputOptions_tableName = Lens.lens (\DatabaseTableOutputOptions' {tableName} -> tableName) (\s@DatabaseTableOutputOptions' {} a -> s {tableName = a} :: DatabaseTableOutputOptions)

instance Core.FromJSON DatabaseTableOutputOptions where
  parseJSON =
    Core.withObject
      "DatabaseTableOutputOptions"
      ( \x ->
          DatabaseTableOutputOptions'
            Prelude.<$> (x Core..:? "TempDirectory")
            Prelude.<*> (x Core..: "TableName")
      )

instance Prelude.Hashable DatabaseTableOutputOptions where
  hashWithSalt salt' DatabaseTableOutputOptions' {..} =
    salt' `Prelude.hashWithSalt` tableName
      `Prelude.hashWithSalt` tempDirectory

instance Prelude.NFData DatabaseTableOutputOptions where
  rnf DatabaseTableOutputOptions' {..} =
    Prelude.rnf tempDirectory
      `Prelude.seq` Prelude.rnf tableName

instance Core.ToJSON DatabaseTableOutputOptions where
  toJSON DatabaseTableOutputOptions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TempDirectory" Core..=) Prelude.<$> tempDirectory,
            Prelude.Just ("TableName" Core..= tableName)
          ]
      )
