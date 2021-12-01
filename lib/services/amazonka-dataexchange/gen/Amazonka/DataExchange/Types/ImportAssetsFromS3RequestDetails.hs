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
-- Module      : Amazonka.DataExchange.Types.ImportAssetsFromS3RequestDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ImportAssetsFromS3RequestDetails where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types.AssetSourceEntry
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details of the operation to be performed by the job.
--
-- /See:/ 'newImportAssetsFromS3RequestDetails' smart constructor.
data ImportAssetsFromS3RequestDetails = ImportAssetsFromS3RequestDetails'
  { -- | The unique identifier for the data set associated with this import job.
    dataSetId :: Prelude.Text,
    -- | Is a list of S3 bucket and object key pairs.
    assetSources :: [AssetSourceEntry],
    -- | The unique identifier for the revision associated with this import
    -- request.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAssetsFromS3RequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'importAssetsFromS3RequestDetails_dataSetId' - The unique identifier for the data set associated with this import job.
--
-- 'assetSources', 'importAssetsFromS3RequestDetails_assetSources' - Is a list of S3 bucket and object key pairs.
--
-- 'revisionId', 'importAssetsFromS3RequestDetails_revisionId' - The unique identifier for the revision associated with this import
-- request.
newImportAssetsFromS3RequestDetails ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ImportAssetsFromS3RequestDetails
newImportAssetsFromS3RequestDetails
  pDataSetId_
  pRevisionId_ =
    ImportAssetsFromS3RequestDetails'
      { dataSetId =
          pDataSetId_,
        assetSources = Prelude.mempty,
        revisionId = pRevisionId_
      }

-- | The unique identifier for the data set associated with this import job.
importAssetsFromS3RequestDetails_dataSetId :: Lens.Lens' ImportAssetsFromS3RequestDetails Prelude.Text
importAssetsFromS3RequestDetails_dataSetId = Lens.lens (\ImportAssetsFromS3RequestDetails' {dataSetId} -> dataSetId) (\s@ImportAssetsFromS3RequestDetails' {} a -> s {dataSetId = a} :: ImportAssetsFromS3RequestDetails)

-- | Is a list of S3 bucket and object key pairs.
importAssetsFromS3RequestDetails_assetSources :: Lens.Lens' ImportAssetsFromS3RequestDetails [AssetSourceEntry]
importAssetsFromS3RequestDetails_assetSources = Lens.lens (\ImportAssetsFromS3RequestDetails' {assetSources} -> assetSources) (\s@ImportAssetsFromS3RequestDetails' {} a -> s {assetSources = a} :: ImportAssetsFromS3RequestDetails) Prelude.. Lens.coerced

-- | The unique identifier for the revision associated with this import
-- request.
importAssetsFromS3RequestDetails_revisionId :: Lens.Lens' ImportAssetsFromS3RequestDetails Prelude.Text
importAssetsFromS3RequestDetails_revisionId = Lens.lens (\ImportAssetsFromS3RequestDetails' {revisionId} -> revisionId) (\s@ImportAssetsFromS3RequestDetails' {} a -> s {revisionId = a} :: ImportAssetsFromS3RequestDetails)

instance
  Prelude.Hashable
    ImportAssetsFromS3RequestDetails
  where
  hashWithSalt
    salt'
    ImportAssetsFromS3RequestDetails' {..} =
      salt' `Prelude.hashWithSalt` revisionId
        `Prelude.hashWithSalt` assetSources
        `Prelude.hashWithSalt` dataSetId

instance
  Prelude.NFData
    ImportAssetsFromS3RequestDetails
  where
  rnf ImportAssetsFromS3RequestDetails' {..} =
    Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf assetSources

instance Core.ToJSON ImportAssetsFromS3RequestDetails where
  toJSON ImportAssetsFromS3RequestDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DataSetId" Core..= dataSetId),
            Prelude.Just ("AssetSources" Core..= assetSources),
            Prelude.Just ("RevisionId" Core..= revisionId)
          ]
      )
