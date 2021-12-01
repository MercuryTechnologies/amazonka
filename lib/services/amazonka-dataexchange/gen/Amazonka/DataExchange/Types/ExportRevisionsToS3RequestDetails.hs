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
-- Module      : Amazonka.DataExchange.Types.ExportRevisionsToS3RequestDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ExportRevisionsToS3RequestDetails where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types.ExportServerSideEncryption
import Amazonka.DataExchange.Types.RevisionDestinationEntry
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details of the operation to be performed by the job.
--
-- /See:/ 'newExportRevisionsToS3RequestDetails' smart constructor.
data ExportRevisionsToS3RequestDetails = ExportRevisionsToS3RequestDetails'
  { -- | Encryption configuration for the export job.
    encryption :: Prelude.Maybe ExportServerSideEncryption,
    -- | The destination for the revision.
    revisionDestinations :: [RevisionDestinationEntry],
    -- | The unique identifier for the data set associated with this export job.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportRevisionsToS3RequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryption', 'exportRevisionsToS3RequestDetails_encryption' - Encryption configuration for the export job.
--
-- 'revisionDestinations', 'exportRevisionsToS3RequestDetails_revisionDestinations' - The destination for the revision.
--
-- 'dataSetId', 'exportRevisionsToS3RequestDetails_dataSetId' - The unique identifier for the data set associated with this export job.
newExportRevisionsToS3RequestDetails ::
  -- | 'dataSetId'
  Prelude.Text ->
  ExportRevisionsToS3RequestDetails
newExportRevisionsToS3RequestDetails pDataSetId_ =
  ExportRevisionsToS3RequestDetails'
    { encryption =
        Prelude.Nothing,
      revisionDestinations = Prelude.mempty,
      dataSetId = pDataSetId_
    }

-- | Encryption configuration for the export job.
exportRevisionsToS3RequestDetails_encryption :: Lens.Lens' ExportRevisionsToS3RequestDetails (Prelude.Maybe ExportServerSideEncryption)
exportRevisionsToS3RequestDetails_encryption = Lens.lens (\ExportRevisionsToS3RequestDetails' {encryption} -> encryption) (\s@ExportRevisionsToS3RequestDetails' {} a -> s {encryption = a} :: ExportRevisionsToS3RequestDetails)

-- | The destination for the revision.
exportRevisionsToS3RequestDetails_revisionDestinations :: Lens.Lens' ExportRevisionsToS3RequestDetails [RevisionDestinationEntry]
exportRevisionsToS3RequestDetails_revisionDestinations = Lens.lens (\ExportRevisionsToS3RequestDetails' {revisionDestinations} -> revisionDestinations) (\s@ExportRevisionsToS3RequestDetails' {} a -> s {revisionDestinations = a} :: ExportRevisionsToS3RequestDetails) Prelude.. Lens.coerced

-- | The unique identifier for the data set associated with this export job.
exportRevisionsToS3RequestDetails_dataSetId :: Lens.Lens' ExportRevisionsToS3RequestDetails Prelude.Text
exportRevisionsToS3RequestDetails_dataSetId = Lens.lens (\ExportRevisionsToS3RequestDetails' {dataSetId} -> dataSetId) (\s@ExportRevisionsToS3RequestDetails' {} a -> s {dataSetId = a} :: ExportRevisionsToS3RequestDetails)

instance
  Prelude.Hashable
    ExportRevisionsToS3RequestDetails
  where
  hashWithSalt
    salt'
    ExportRevisionsToS3RequestDetails' {..} =
      salt' `Prelude.hashWithSalt` dataSetId
        `Prelude.hashWithSalt` revisionDestinations
        `Prelude.hashWithSalt` encryption

instance
  Prelude.NFData
    ExportRevisionsToS3RequestDetails
  where
  rnf ExportRevisionsToS3RequestDetails' {..} =
    Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionDestinations

instance
  Core.ToJSON
    ExportRevisionsToS3RequestDetails
  where
  toJSON ExportRevisionsToS3RequestDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Encryption" Core..=) Prelude.<$> encryption,
            Prelude.Just
              ( "RevisionDestinations"
                  Core..= revisionDestinations
              ),
            Prelude.Just ("DataSetId" Core..= dataSetId)
          ]
      )
