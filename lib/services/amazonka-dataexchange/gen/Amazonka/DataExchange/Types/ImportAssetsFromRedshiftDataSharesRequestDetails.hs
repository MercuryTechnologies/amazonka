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
-- Module      : Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesRequestDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ImportAssetsFromRedshiftDataSharesRequestDetails where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types.RedshiftDataShareAssetSourceEntry
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details from an import from Amazon Redshift datashare request.
--
-- /See:/ 'newImportAssetsFromRedshiftDataSharesRequestDetails' smart constructor.
data ImportAssetsFromRedshiftDataSharesRequestDetails = ImportAssetsFromRedshiftDataSharesRequestDetails'
  { -- | The unique identifier for the data set associated with this import job.
    dataSetId :: Prelude.Text,
    -- | A list of Amazon Redshift datashare assets.
    assetSources :: [RedshiftDataShareAssetSourceEntry],
    -- | The unique identifier for the revision associated with this import job.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAssetsFromRedshiftDataSharesRequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'importAssetsFromRedshiftDataSharesRequestDetails_dataSetId' - The unique identifier for the data set associated with this import job.
--
-- 'assetSources', 'importAssetsFromRedshiftDataSharesRequestDetails_assetSources' - A list of Amazon Redshift datashare assets.
--
-- 'revisionId', 'importAssetsFromRedshiftDataSharesRequestDetails_revisionId' - The unique identifier for the revision associated with this import job.
newImportAssetsFromRedshiftDataSharesRequestDetails ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ImportAssetsFromRedshiftDataSharesRequestDetails
newImportAssetsFromRedshiftDataSharesRequestDetails
  pDataSetId_
  pRevisionId_ =
    ImportAssetsFromRedshiftDataSharesRequestDetails'
      { dataSetId =
          pDataSetId_,
        assetSources =
          Prelude.mempty,
        revisionId = pRevisionId_
      }

-- | The unique identifier for the data set associated with this import job.
importAssetsFromRedshiftDataSharesRequestDetails_dataSetId :: Lens.Lens' ImportAssetsFromRedshiftDataSharesRequestDetails Prelude.Text
importAssetsFromRedshiftDataSharesRequestDetails_dataSetId = Lens.lens (\ImportAssetsFromRedshiftDataSharesRequestDetails' {dataSetId} -> dataSetId) (\s@ImportAssetsFromRedshiftDataSharesRequestDetails' {} a -> s {dataSetId = a} :: ImportAssetsFromRedshiftDataSharesRequestDetails)

-- | A list of Amazon Redshift datashare assets.
importAssetsFromRedshiftDataSharesRequestDetails_assetSources :: Lens.Lens' ImportAssetsFromRedshiftDataSharesRequestDetails [RedshiftDataShareAssetSourceEntry]
importAssetsFromRedshiftDataSharesRequestDetails_assetSources = Lens.lens (\ImportAssetsFromRedshiftDataSharesRequestDetails' {assetSources} -> assetSources) (\s@ImportAssetsFromRedshiftDataSharesRequestDetails' {} a -> s {assetSources = a} :: ImportAssetsFromRedshiftDataSharesRequestDetails) Prelude.. Lens.coerced

-- | The unique identifier for the revision associated with this import job.
importAssetsFromRedshiftDataSharesRequestDetails_revisionId :: Lens.Lens' ImportAssetsFromRedshiftDataSharesRequestDetails Prelude.Text
importAssetsFromRedshiftDataSharesRequestDetails_revisionId = Lens.lens (\ImportAssetsFromRedshiftDataSharesRequestDetails' {revisionId} -> revisionId) (\s@ImportAssetsFromRedshiftDataSharesRequestDetails' {} a -> s {revisionId = a} :: ImportAssetsFromRedshiftDataSharesRequestDetails)

instance
  Prelude.Hashable
    ImportAssetsFromRedshiftDataSharesRequestDetails
  where
  hashWithSalt
    salt'
    ImportAssetsFromRedshiftDataSharesRequestDetails' {..} =
      salt' `Prelude.hashWithSalt` revisionId
        `Prelude.hashWithSalt` assetSources
        `Prelude.hashWithSalt` dataSetId

instance
  Prelude.NFData
    ImportAssetsFromRedshiftDataSharesRequestDetails
  where
  rnf
    ImportAssetsFromRedshiftDataSharesRequestDetails' {..} =
      Prelude.rnf dataSetId
        `Prelude.seq` Prelude.rnf revisionId
        `Prelude.seq` Prelude.rnf assetSources

instance
  Core.ToJSON
    ImportAssetsFromRedshiftDataSharesRequestDetails
  where
  toJSON
    ImportAssetsFromRedshiftDataSharesRequestDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ Prelude.Just ("DataSetId" Core..= dataSetId),
              Prelude.Just ("AssetSources" Core..= assetSources),
              Prelude.Just ("RevisionId" Core..= revisionId)
            ]
        )
