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
-- Module      : Amazonka.CloudFront.Types.FieldLevelEncryptionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.FieldLevelEncryptionSummary where

import Amazonka.CloudFront.Types.ContentTypeProfileConfig
import Amazonka.CloudFront.Types.QueryArgProfileConfig
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of a field-level encryption item.
--
-- /See:/ 'newFieldLevelEncryptionSummary' smart constructor.
data FieldLevelEncryptionSummary = FieldLevelEncryptionSummary'
  { -- | A summary of a query argument-profile mapping.
    queryArgProfileConfig :: Prelude.Maybe QueryArgProfileConfig,
    -- | A summary of a content type-profile mapping.
    contentTypeProfileConfig :: Prelude.Maybe ContentTypeProfileConfig,
    -- | An optional comment about the field-level encryption item. The comment
    -- cannot be longer than 128 characters.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The unique ID of a field-level encryption item.
    id :: Prelude.Text,
    -- | The last time that the summary of field-level encryption items was
    -- modified.
    lastModifiedTime :: Core.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FieldLevelEncryptionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryArgProfileConfig', 'fieldLevelEncryptionSummary_queryArgProfileConfig' - A summary of a query argument-profile mapping.
--
-- 'contentTypeProfileConfig', 'fieldLevelEncryptionSummary_contentTypeProfileConfig' - A summary of a content type-profile mapping.
--
-- 'comment', 'fieldLevelEncryptionSummary_comment' - An optional comment about the field-level encryption item. The comment
-- cannot be longer than 128 characters.
--
-- 'id', 'fieldLevelEncryptionSummary_id' - The unique ID of a field-level encryption item.
--
-- 'lastModifiedTime', 'fieldLevelEncryptionSummary_lastModifiedTime' - The last time that the summary of field-level encryption items was
-- modified.
newFieldLevelEncryptionSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  FieldLevelEncryptionSummary
newFieldLevelEncryptionSummary
  pId_
  pLastModifiedTime_ =
    FieldLevelEncryptionSummary'
      { queryArgProfileConfig =
          Prelude.Nothing,
        contentTypeProfileConfig = Prelude.Nothing,
        comment = Prelude.Nothing,
        id = pId_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_
      }

-- | A summary of a query argument-profile mapping.
fieldLevelEncryptionSummary_queryArgProfileConfig :: Lens.Lens' FieldLevelEncryptionSummary (Prelude.Maybe QueryArgProfileConfig)
fieldLevelEncryptionSummary_queryArgProfileConfig = Lens.lens (\FieldLevelEncryptionSummary' {queryArgProfileConfig} -> queryArgProfileConfig) (\s@FieldLevelEncryptionSummary' {} a -> s {queryArgProfileConfig = a} :: FieldLevelEncryptionSummary)

-- | A summary of a content type-profile mapping.
fieldLevelEncryptionSummary_contentTypeProfileConfig :: Lens.Lens' FieldLevelEncryptionSummary (Prelude.Maybe ContentTypeProfileConfig)
fieldLevelEncryptionSummary_contentTypeProfileConfig = Lens.lens (\FieldLevelEncryptionSummary' {contentTypeProfileConfig} -> contentTypeProfileConfig) (\s@FieldLevelEncryptionSummary' {} a -> s {contentTypeProfileConfig = a} :: FieldLevelEncryptionSummary)

-- | An optional comment about the field-level encryption item. The comment
-- cannot be longer than 128 characters.
fieldLevelEncryptionSummary_comment :: Lens.Lens' FieldLevelEncryptionSummary (Prelude.Maybe Prelude.Text)
fieldLevelEncryptionSummary_comment = Lens.lens (\FieldLevelEncryptionSummary' {comment} -> comment) (\s@FieldLevelEncryptionSummary' {} a -> s {comment = a} :: FieldLevelEncryptionSummary)

-- | The unique ID of a field-level encryption item.
fieldLevelEncryptionSummary_id :: Lens.Lens' FieldLevelEncryptionSummary Prelude.Text
fieldLevelEncryptionSummary_id = Lens.lens (\FieldLevelEncryptionSummary' {id} -> id) (\s@FieldLevelEncryptionSummary' {} a -> s {id = a} :: FieldLevelEncryptionSummary)

-- | The last time that the summary of field-level encryption items was
-- modified.
fieldLevelEncryptionSummary_lastModifiedTime :: Lens.Lens' FieldLevelEncryptionSummary Prelude.UTCTime
fieldLevelEncryptionSummary_lastModifiedTime = Lens.lens (\FieldLevelEncryptionSummary' {lastModifiedTime} -> lastModifiedTime) (\s@FieldLevelEncryptionSummary' {} a -> s {lastModifiedTime = a} :: FieldLevelEncryptionSummary) Prelude.. Core._Time

instance Core.FromXML FieldLevelEncryptionSummary where
  parseXML x =
    FieldLevelEncryptionSummary'
      Prelude.<$> (x Core..@? "QueryArgProfileConfig")
      Prelude.<*> (x Core..@? "ContentTypeProfileConfig")
      Prelude.<*> (x Core..@? "Comment")
      Prelude.<*> (x Core..@ "Id")
      Prelude.<*> (x Core..@ "LastModifiedTime")

instance Prelude.Hashable FieldLevelEncryptionSummary where
  hashWithSalt salt' FieldLevelEncryptionSummary' {..} =
    salt' `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` contentTypeProfileConfig
      `Prelude.hashWithSalt` queryArgProfileConfig

instance Prelude.NFData FieldLevelEncryptionSummary where
  rnf FieldLevelEncryptionSummary' {..} =
    Prelude.rnf queryArgProfileConfig
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf contentTypeProfileConfig
