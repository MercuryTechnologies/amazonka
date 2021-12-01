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
-- Module      : Amazonka.Schemas.Types.SchemaSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Schemas.Types.SchemaSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of schema details.
--
-- /See:/ 'newSchemaSummary' smart constructor.
data SchemaSummary = SchemaSummary'
  { -- | The name of the schema.
    schemaName :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the schema.
    schemaArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that schema was modified.
    lastModified :: Prelude.Maybe Core.POSIX,
    -- | Tags associated with the schema.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The number of versions available for the schema.
    versionCount :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SchemaSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'schemaName', 'schemaSummary_schemaName' - The name of the schema.
--
-- 'schemaArn', 'schemaSummary_schemaArn' - The ARN of the schema.
--
-- 'lastModified', 'schemaSummary_lastModified' - The date and time that schema was modified.
--
-- 'tags', 'schemaSummary_tags' - Tags associated with the schema.
--
-- 'versionCount', 'schemaSummary_versionCount' - The number of versions available for the schema.
newSchemaSummary ::
  SchemaSummary
newSchemaSummary =
  SchemaSummary'
    { schemaName = Prelude.Nothing,
      schemaArn = Prelude.Nothing,
      lastModified = Prelude.Nothing,
      tags = Prelude.Nothing,
      versionCount = Prelude.Nothing
    }

-- | The name of the schema.
schemaSummary_schemaName :: Lens.Lens' SchemaSummary (Prelude.Maybe Prelude.Text)
schemaSummary_schemaName = Lens.lens (\SchemaSummary' {schemaName} -> schemaName) (\s@SchemaSummary' {} a -> s {schemaName = a} :: SchemaSummary)

-- | The ARN of the schema.
schemaSummary_schemaArn :: Lens.Lens' SchemaSummary (Prelude.Maybe Prelude.Text)
schemaSummary_schemaArn = Lens.lens (\SchemaSummary' {schemaArn} -> schemaArn) (\s@SchemaSummary' {} a -> s {schemaArn = a} :: SchemaSummary)

-- | The date and time that schema was modified.
schemaSummary_lastModified :: Lens.Lens' SchemaSummary (Prelude.Maybe Prelude.UTCTime)
schemaSummary_lastModified = Lens.lens (\SchemaSummary' {lastModified} -> lastModified) (\s@SchemaSummary' {} a -> s {lastModified = a} :: SchemaSummary) Prelude.. Lens.mapping Core._Time

-- | Tags associated with the schema.
schemaSummary_tags :: Lens.Lens' SchemaSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
schemaSummary_tags = Lens.lens (\SchemaSummary' {tags} -> tags) (\s@SchemaSummary' {} a -> s {tags = a} :: SchemaSummary) Prelude.. Lens.mapping Lens.coerced

-- | The number of versions available for the schema.
schemaSummary_versionCount :: Lens.Lens' SchemaSummary (Prelude.Maybe Prelude.Integer)
schemaSummary_versionCount = Lens.lens (\SchemaSummary' {versionCount} -> versionCount) (\s@SchemaSummary' {} a -> s {versionCount = a} :: SchemaSummary)

instance Core.FromJSON SchemaSummary where
  parseJSON =
    Core.withObject
      "SchemaSummary"
      ( \x ->
          SchemaSummary'
            Prelude.<$> (x Core..:? "SchemaName")
            Prelude.<*> (x Core..:? "SchemaArn")
            Prelude.<*> (x Core..:? "LastModified")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "VersionCount")
      )

instance Prelude.Hashable SchemaSummary where
  hashWithSalt salt' SchemaSummary' {..} =
    salt' `Prelude.hashWithSalt` versionCount
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lastModified
      `Prelude.hashWithSalt` schemaArn
      `Prelude.hashWithSalt` schemaName

instance Prelude.NFData SchemaSummary where
  rnf SchemaSummary' {..} =
    Prelude.rnf schemaName
      `Prelude.seq` Prelude.rnf versionCount
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastModified
      `Prelude.seq` Prelude.rnf schemaArn
