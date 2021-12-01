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
-- Module      : Amazonka.Translate.Types.ParallelDataProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Translate.Types.ParallelDataProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Translate.Types.EncryptionKey
import Amazonka.Translate.Types.ParallelDataConfig
import Amazonka.Translate.Types.ParallelDataStatus

-- | The properties of a parallel data resource.
--
-- /See:/ 'newParallelDataProperties' smart constructor.
data ParallelDataProperties = ParallelDataProperties'
  { -- | The status of the parallel data resource. When the parallel data is
    -- ready for you to use, the status is @ACTIVE@.
    status :: Prelude.Maybe ParallelDataStatus,
    -- | The time at which the parallel data resource was last updated.
    lastUpdatedAt :: Prelude.Maybe Core.POSIX,
    -- | The number of records successfully imported from the parallel data input
    -- file.
    importedRecordCount :: Prelude.Maybe Prelude.Integer,
    -- | The Amazon Resource Name (ARN) of the parallel data resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The language codes for the target languages available in the parallel
    -- data file. All possible target languages are returned as an array.
    targetLanguageCodes :: Prelude.Maybe [Prelude.Text],
    -- | The time at which the parallel data resource was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The number of records unsuccessfully imported from the parallel data
    -- input file.
    failedRecordCount :: Prelude.Maybe Prelude.Integer,
    -- | The number of UTF-8 characters that Amazon Translate imported from the
    -- parallel data input file. This number includes only the characters in
    -- your translation examples. It does not include characters that are used
    -- to format your file. For example, if you provided a Translation Memory
    -- Exchange (.tmx) file, this number does not include the tags.
    importedDataSize :: Prelude.Maybe Prelude.Integer,
    -- | The custom name assigned to the parallel data resource.
    name :: Prelude.Maybe Prelude.Text,
    -- | The source language of the translations in the parallel data file.
    sourceLanguageCode :: Prelude.Maybe Prelude.Text,
    -- | The time that the most recent update was attempted.
    latestUpdateAttemptAt :: Prelude.Maybe Core.POSIX,
    encryptionKey :: Prelude.Maybe EncryptionKey,
    -- | The status of the most recent update attempt for the parallel data
    -- resource.
    latestUpdateAttemptStatus :: Prelude.Maybe ParallelDataStatus,
    -- | Additional information from Amazon Translate about the parallel data
    -- resource.
    message :: Prelude.Maybe Prelude.Text,
    -- | The description assigned to the parallel data resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | The number of items in the input file that Amazon Translate skipped when
    -- you created or updated the parallel data resource. For example, Amazon
    -- Translate skips empty records, empty target texts, and empty lines.
    skippedRecordCount :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the format and S3 location of the parallel data input file.
    parallelDataConfig :: Prelude.Maybe ParallelDataConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ParallelDataProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'parallelDataProperties_status' - The status of the parallel data resource. When the parallel data is
-- ready for you to use, the status is @ACTIVE@.
--
-- 'lastUpdatedAt', 'parallelDataProperties_lastUpdatedAt' - The time at which the parallel data resource was last updated.
--
-- 'importedRecordCount', 'parallelDataProperties_importedRecordCount' - The number of records successfully imported from the parallel data input
-- file.
--
-- 'arn', 'parallelDataProperties_arn' - The Amazon Resource Name (ARN) of the parallel data resource.
--
-- 'targetLanguageCodes', 'parallelDataProperties_targetLanguageCodes' - The language codes for the target languages available in the parallel
-- data file. All possible target languages are returned as an array.
--
-- 'createdAt', 'parallelDataProperties_createdAt' - The time at which the parallel data resource was created.
--
-- 'failedRecordCount', 'parallelDataProperties_failedRecordCount' - The number of records unsuccessfully imported from the parallel data
-- input file.
--
-- 'importedDataSize', 'parallelDataProperties_importedDataSize' - The number of UTF-8 characters that Amazon Translate imported from the
-- parallel data input file. This number includes only the characters in
-- your translation examples. It does not include characters that are used
-- to format your file. For example, if you provided a Translation Memory
-- Exchange (.tmx) file, this number does not include the tags.
--
-- 'name', 'parallelDataProperties_name' - The custom name assigned to the parallel data resource.
--
-- 'sourceLanguageCode', 'parallelDataProperties_sourceLanguageCode' - The source language of the translations in the parallel data file.
--
-- 'latestUpdateAttemptAt', 'parallelDataProperties_latestUpdateAttemptAt' - The time that the most recent update was attempted.
--
-- 'encryptionKey', 'parallelDataProperties_encryptionKey' - Undocumented member.
--
-- 'latestUpdateAttemptStatus', 'parallelDataProperties_latestUpdateAttemptStatus' - The status of the most recent update attempt for the parallel data
-- resource.
--
-- 'message', 'parallelDataProperties_message' - Additional information from Amazon Translate about the parallel data
-- resource.
--
-- 'description', 'parallelDataProperties_description' - The description assigned to the parallel data resource.
--
-- 'skippedRecordCount', 'parallelDataProperties_skippedRecordCount' - The number of items in the input file that Amazon Translate skipped when
-- you created or updated the parallel data resource. For example, Amazon
-- Translate skips empty records, empty target texts, and empty lines.
--
-- 'parallelDataConfig', 'parallelDataProperties_parallelDataConfig' - Specifies the format and S3 location of the parallel data input file.
newParallelDataProperties ::
  ParallelDataProperties
newParallelDataProperties =
  ParallelDataProperties'
    { status = Prelude.Nothing,
      lastUpdatedAt = Prelude.Nothing,
      importedRecordCount = Prelude.Nothing,
      arn = Prelude.Nothing,
      targetLanguageCodes = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      failedRecordCount = Prelude.Nothing,
      importedDataSize = Prelude.Nothing,
      name = Prelude.Nothing,
      sourceLanguageCode = Prelude.Nothing,
      latestUpdateAttemptAt = Prelude.Nothing,
      encryptionKey = Prelude.Nothing,
      latestUpdateAttemptStatus = Prelude.Nothing,
      message = Prelude.Nothing,
      description = Prelude.Nothing,
      skippedRecordCount = Prelude.Nothing,
      parallelDataConfig = Prelude.Nothing
    }

-- | The status of the parallel data resource. When the parallel data is
-- ready for you to use, the status is @ACTIVE@.
parallelDataProperties_status :: Lens.Lens' ParallelDataProperties (Prelude.Maybe ParallelDataStatus)
parallelDataProperties_status = Lens.lens (\ParallelDataProperties' {status} -> status) (\s@ParallelDataProperties' {} a -> s {status = a} :: ParallelDataProperties)

-- | The time at which the parallel data resource was last updated.
parallelDataProperties_lastUpdatedAt :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.UTCTime)
parallelDataProperties_lastUpdatedAt = Lens.lens (\ParallelDataProperties' {lastUpdatedAt} -> lastUpdatedAt) (\s@ParallelDataProperties' {} a -> s {lastUpdatedAt = a} :: ParallelDataProperties) Prelude.. Lens.mapping Core._Time

-- | The number of records successfully imported from the parallel data input
-- file.
parallelDataProperties_importedRecordCount :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.Integer)
parallelDataProperties_importedRecordCount = Lens.lens (\ParallelDataProperties' {importedRecordCount} -> importedRecordCount) (\s@ParallelDataProperties' {} a -> s {importedRecordCount = a} :: ParallelDataProperties)

-- | The Amazon Resource Name (ARN) of the parallel data resource.
parallelDataProperties_arn :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.Text)
parallelDataProperties_arn = Lens.lens (\ParallelDataProperties' {arn} -> arn) (\s@ParallelDataProperties' {} a -> s {arn = a} :: ParallelDataProperties)

-- | The language codes for the target languages available in the parallel
-- data file. All possible target languages are returned as an array.
parallelDataProperties_targetLanguageCodes :: Lens.Lens' ParallelDataProperties (Prelude.Maybe [Prelude.Text])
parallelDataProperties_targetLanguageCodes = Lens.lens (\ParallelDataProperties' {targetLanguageCodes} -> targetLanguageCodes) (\s@ParallelDataProperties' {} a -> s {targetLanguageCodes = a} :: ParallelDataProperties) Prelude.. Lens.mapping Lens.coerced

-- | The time at which the parallel data resource was created.
parallelDataProperties_createdAt :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.UTCTime)
parallelDataProperties_createdAt = Lens.lens (\ParallelDataProperties' {createdAt} -> createdAt) (\s@ParallelDataProperties' {} a -> s {createdAt = a} :: ParallelDataProperties) Prelude.. Lens.mapping Core._Time

-- | The number of records unsuccessfully imported from the parallel data
-- input file.
parallelDataProperties_failedRecordCount :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.Integer)
parallelDataProperties_failedRecordCount = Lens.lens (\ParallelDataProperties' {failedRecordCount} -> failedRecordCount) (\s@ParallelDataProperties' {} a -> s {failedRecordCount = a} :: ParallelDataProperties)

-- | The number of UTF-8 characters that Amazon Translate imported from the
-- parallel data input file. This number includes only the characters in
-- your translation examples. It does not include characters that are used
-- to format your file. For example, if you provided a Translation Memory
-- Exchange (.tmx) file, this number does not include the tags.
parallelDataProperties_importedDataSize :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.Integer)
parallelDataProperties_importedDataSize = Lens.lens (\ParallelDataProperties' {importedDataSize} -> importedDataSize) (\s@ParallelDataProperties' {} a -> s {importedDataSize = a} :: ParallelDataProperties)

-- | The custom name assigned to the parallel data resource.
parallelDataProperties_name :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.Text)
parallelDataProperties_name = Lens.lens (\ParallelDataProperties' {name} -> name) (\s@ParallelDataProperties' {} a -> s {name = a} :: ParallelDataProperties)

-- | The source language of the translations in the parallel data file.
parallelDataProperties_sourceLanguageCode :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.Text)
parallelDataProperties_sourceLanguageCode = Lens.lens (\ParallelDataProperties' {sourceLanguageCode} -> sourceLanguageCode) (\s@ParallelDataProperties' {} a -> s {sourceLanguageCode = a} :: ParallelDataProperties)

-- | The time that the most recent update was attempted.
parallelDataProperties_latestUpdateAttemptAt :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.UTCTime)
parallelDataProperties_latestUpdateAttemptAt = Lens.lens (\ParallelDataProperties' {latestUpdateAttemptAt} -> latestUpdateAttemptAt) (\s@ParallelDataProperties' {} a -> s {latestUpdateAttemptAt = a} :: ParallelDataProperties) Prelude.. Lens.mapping Core._Time

-- | Undocumented member.
parallelDataProperties_encryptionKey :: Lens.Lens' ParallelDataProperties (Prelude.Maybe EncryptionKey)
parallelDataProperties_encryptionKey = Lens.lens (\ParallelDataProperties' {encryptionKey} -> encryptionKey) (\s@ParallelDataProperties' {} a -> s {encryptionKey = a} :: ParallelDataProperties)

-- | The status of the most recent update attempt for the parallel data
-- resource.
parallelDataProperties_latestUpdateAttemptStatus :: Lens.Lens' ParallelDataProperties (Prelude.Maybe ParallelDataStatus)
parallelDataProperties_latestUpdateAttemptStatus = Lens.lens (\ParallelDataProperties' {latestUpdateAttemptStatus} -> latestUpdateAttemptStatus) (\s@ParallelDataProperties' {} a -> s {latestUpdateAttemptStatus = a} :: ParallelDataProperties)

-- | Additional information from Amazon Translate about the parallel data
-- resource.
parallelDataProperties_message :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.Text)
parallelDataProperties_message = Lens.lens (\ParallelDataProperties' {message} -> message) (\s@ParallelDataProperties' {} a -> s {message = a} :: ParallelDataProperties)

-- | The description assigned to the parallel data resource.
parallelDataProperties_description :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.Text)
parallelDataProperties_description = Lens.lens (\ParallelDataProperties' {description} -> description) (\s@ParallelDataProperties' {} a -> s {description = a} :: ParallelDataProperties)

-- | The number of items in the input file that Amazon Translate skipped when
-- you created or updated the parallel data resource. For example, Amazon
-- Translate skips empty records, empty target texts, and empty lines.
parallelDataProperties_skippedRecordCount :: Lens.Lens' ParallelDataProperties (Prelude.Maybe Prelude.Integer)
parallelDataProperties_skippedRecordCount = Lens.lens (\ParallelDataProperties' {skippedRecordCount} -> skippedRecordCount) (\s@ParallelDataProperties' {} a -> s {skippedRecordCount = a} :: ParallelDataProperties)

-- | Specifies the format and S3 location of the parallel data input file.
parallelDataProperties_parallelDataConfig :: Lens.Lens' ParallelDataProperties (Prelude.Maybe ParallelDataConfig)
parallelDataProperties_parallelDataConfig = Lens.lens (\ParallelDataProperties' {parallelDataConfig} -> parallelDataConfig) (\s@ParallelDataProperties' {} a -> s {parallelDataConfig = a} :: ParallelDataProperties)

instance Core.FromJSON ParallelDataProperties where
  parseJSON =
    Core.withObject
      "ParallelDataProperties"
      ( \x ->
          ParallelDataProperties'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "LastUpdatedAt")
            Prelude.<*> (x Core..:? "ImportedRecordCount")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> ( x Core..:? "TargetLanguageCodes"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "FailedRecordCount")
            Prelude.<*> (x Core..:? "ImportedDataSize")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "SourceLanguageCode")
            Prelude.<*> (x Core..:? "LatestUpdateAttemptAt")
            Prelude.<*> (x Core..:? "EncryptionKey")
            Prelude.<*> (x Core..:? "LatestUpdateAttemptStatus")
            Prelude.<*> (x Core..:? "Message")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "SkippedRecordCount")
            Prelude.<*> (x Core..:? "ParallelDataConfig")
      )

instance Prelude.Hashable ParallelDataProperties where
  hashWithSalt salt' ParallelDataProperties' {..} =
    salt' `Prelude.hashWithSalt` parallelDataConfig
      `Prelude.hashWithSalt` skippedRecordCount
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` latestUpdateAttemptStatus
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` latestUpdateAttemptAt
      `Prelude.hashWithSalt` sourceLanguageCode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` importedDataSize
      `Prelude.hashWithSalt` failedRecordCount
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` targetLanguageCodes
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` importedRecordCount
      `Prelude.hashWithSalt` lastUpdatedAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData ParallelDataProperties where
  rnf ParallelDataProperties' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf parallelDataConfig
      `Prelude.seq` Prelude.rnf skippedRecordCount
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf latestUpdateAttemptStatus
      `Prelude.seq` Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf latestUpdateAttemptAt
      `Prelude.seq` Prelude.rnf sourceLanguageCode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf importedDataSize
      `Prelude.seq` Prelude.rnf failedRecordCount
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf targetLanguageCodes
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf importedRecordCount
      `Prelude.seq` Prelude.rnf lastUpdatedAt
