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
-- Module      : Amazonka.Glue.Types.SchemaVersionErrorItem
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SchemaVersionErrorItem where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.ErrorDetails
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the error details for an operation on a schema
-- version.
--
-- /See:/ 'newSchemaVersionErrorItem' smart constructor.
data SchemaVersionErrorItem = SchemaVersionErrorItem'
  { -- | The version number of the schema.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The details of the error for the schema version.
    errorDetails :: Prelude.Maybe ErrorDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaVersionErrorItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionNumber', 'schemaVersionErrorItem_versionNumber' - The version number of the schema.
--
-- 'errorDetails', 'schemaVersionErrorItem_errorDetails' - The details of the error for the schema version.
newSchemaVersionErrorItem ::
  SchemaVersionErrorItem
newSchemaVersionErrorItem =
  SchemaVersionErrorItem'
    { versionNumber =
        Prelude.Nothing,
      errorDetails = Prelude.Nothing
    }

-- | The version number of the schema.
schemaVersionErrorItem_versionNumber :: Lens.Lens' SchemaVersionErrorItem (Prelude.Maybe Prelude.Natural)
schemaVersionErrorItem_versionNumber = Lens.lens (\SchemaVersionErrorItem' {versionNumber} -> versionNumber) (\s@SchemaVersionErrorItem' {} a -> s {versionNumber = a} :: SchemaVersionErrorItem)

-- | The details of the error for the schema version.
schemaVersionErrorItem_errorDetails :: Lens.Lens' SchemaVersionErrorItem (Prelude.Maybe ErrorDetails)
schemaVersionErrorItem_errorDetails = Lens.lens (\SchemaVersionErrorItem' {errorDetails} -> errorDetails) (\s@SchemaVersionErrorItem' {} a -> s {errorDetails = a} :: SchemaVersionErrorItem)

instance Core.FromJSON SchemaVersionErrorItem where
  parseJSON =
    Core.withObject
      "SchemaVersionErrorItem"
      ( \x ->
          SchemaVersionErrorItem'
            Prelude.<$> (x Core..:? "VersionNumber")
            Prelude.<*> (x Core..:? "ErrorDetails")
      )

instance Prelude.Hashable SchemaVersionErrorItem where
  hashWithSalt salt' SchemaVersionErrorItem' {..} =
    salt' `Prelude.hashWithSalt` errorDetails
      `Prelude.hashWithSalt` versionNumber

instance Prelude.NFData SchemaVersionErrorItem where
  rnf SchemaVersionErrorItem' {..} =
    Prelude.rnf versionNumber
      `Prelude.seq` Prelude.rnf errorDetails
