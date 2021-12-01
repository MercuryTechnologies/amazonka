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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableRestoreSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableRestoreSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the restore for the table.
--
-- /See:/ 'newAwsDynamoDbTableRestoreSummary' smart constructor.
data AwsDynamoDbTableRestoreSummary = AwsDynamoDbTableRestoreSummary'
  { -- | The ARN of the source table for the backup.
    sourceTableArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the source backup from which the table was restored.
    sourceBackupArn :: Prelude.Maybe Prelude.Text,
    -- | Whether a restore is currently in progress.
    restoreInProgress :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the point in time that the table was restored to.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    restoreDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableRestoreSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceTableArn', 'awsDynamoDbTableRestoreSummary_sourceTableArn' - The ARN of the source table for the backup.
--
-- 'sourceBackupArn', 'awsDynamoDbTableRestoreSummary_sourceBackupArn' - The ARN of the source backup from which the table was restored.
--
-- 'restoreInProgress', 'awsDynamoDbTableRestoreSummary_restoreInProgress' - Whether a restore is currently in progress.
--
-- 'restoreDateTime', 'awsDynamoDbTableRestoreSummary_restoreDateTime' - Indicates the point in time that the table was restored to.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
newAwsDynamoDbTableRestoreSummary ::
  AwsDynamoDbTableRestoreSummary
newAwsDynamoDbTableRestoreSummary =
  AwsDynamoDbTableRestoreSummary'
    { sourceTableArn =
        Prelude.Nothing,
      sourceBackupArn = Prelude.Nothing,
      restoreInProgress = Prelude.Nothing,
      restoreDateTime = Prelude.Nothing
    }

-- | The ARN of the source table for the backup.
awsDynamoDbTableRestoreSummary_sourceTableArn :: Lens.Lens' AwsDynamoDbTableRestoreSummary (Prelude.Maybe Prelude.Text)
awsDynamoDbTableRestoreSummary_sourceTableArn = Lens.lens (\AwsDynamoDbTableRestoreSummary' {sourceTableArn} -> sourceTableArn) (\s@AwsDynamoDbTableRestoreSummary' {} a -> s {sourceTableArn = a} :: AwsDynamoDbTableRestoreSummary)

-- | The ARN of the source backup from which the table was restored.
awsDynamoDbTableRestoreSummary_sourceBackupArn :: Lens.Lens' AwsDynamoDbTableRestoreSummary (Prelude.Maybe Prelude.Text)
awsDynamoDbTableRestoreSummary_sourceBackupArn = Lens.lens (\AwsDynamoDbTableRestoreSummary' {sourceBackupArn} -> sourceBackupArn) (\s@AwsDynamoDbTableRestoreSummary' {} a -> s {sourceBackupArn = a} :: AwsDynamoDbTableRestoreSummary)

-- | Whether a restore is currently in progress.
awsDynamoDbTableRestoreSummary_restoreInProgress :: Lens.Lens' AwsDynamoDbTableRestoreSummary (Prelude.Maybe Prelude.Bool)
awsDynamoDbTableRestoreSummary_restoreInProgress = Lens.lens (\AwsDynamoDbTableRestoreSummary' {restoreInProgress} -> restoreInProgress) (\s@AwsDynamoDbTableRestoreSummary' {} a -> s {restoreInProgress = a} :: AwsDynamoDbTableRestoreSummary)

-- | Indicates the point in time that the table was restored to.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsDynamoDbTableRestoreSummary_restoreDateTime :: Lens.Lens' AwsDynamoDbTableRestoreSummary (Prelude.Maybe Prelude.Text)
awsDynamoDbTableRestoreSummary_restoreDateTime = Lens.lens (\AwsDynamoDbTableRestoreSummary' {restoreDateTime} -> restoreDateTime) (\s@AwsDynamoDbTableRestoreSummary' {} a -> s {restoreDateTime = a} :: AwsDynamoDbTableRestoreSummary)

instance Core.FromJSON AwsDynamoDbTableRestoreSummary where
  parseJSON =
    Core.withObject
      "AwsDynamoDbTableRestoreSummary"
      ( \x ->
          AwsDynamoDbTableRestoreSummary'
            Prelude.<$> (x Core..:? "SourceTableArn")
            Prelude.<*> (x Core..:? "SourceBackupArn")
            Prelude.<*> (x Core..:? "RestoreInProgress")
            Prelude.<*> (x Core..:? "RestoreDateTime")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableRestoreSummary
  where
  hashWithSalt
    salt'
    AwsDynamoDbTableRestoreSummary' {..} =
      salt' `Prelude.hashWithSalt` restoreDateTime
        `Prelude.hashWithSalt` restoreInProgress
        `Prelude.hashWithSalt` sourceBackupArn
        `Prelude.hashWithSalt` sourceTableArn

instance
  Prelude.NFData
    AwsDynamoDbTableRestoreSummary
  where
  rnf AwsDynamoDbTableRestoreSummary' {..} =
    Prelude.rnf sourceTableArn
      `Prelude.seq` Prelude.rnf restoreDateTime
      `Prelude.seq` Prelude.rnf restoreInProgress
      `Prelude.seq` Prelude.rnf sourceBackupArn

instance Core.ToJSON AwsDynamoDbTableRestoreSummary where
  toJSON AwsDynamoDbTableRestoreSummary' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceTableArn" Core..=)
              Prelude.<$> sourceTableArn,
            ("SourceBackupArn" Core..=)
              Prelude.<$> sourceBackupArn,
            ("RestoreInProgress" Core..=)
              Prelude.<$> restoreInProgress,
            ("RestoreDateTime" Core..=)
              Prelude.<$> restoreDateTime
          ]
      )
