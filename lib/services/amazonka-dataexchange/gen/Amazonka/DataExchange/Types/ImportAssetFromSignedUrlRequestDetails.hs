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
-- Module      : Amazonka.DataExchange.Types.ImportAssetFromSignedUrlRequestDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.ImportAssetFromSignedUrlRequestDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details of the operation to be performed by the job.
--
-- /See:/ 'newImportAssetFromSignedUrlRequestDetails' smart constructor.
data ImportAssetFromSignedUrlRequestDetails = ImportAssetFromSignedUrlRequestDetails'
  { -- | The unique identifier for the data set associated with this import job.
    dataSetId :: Prelude.Text,
    -- | The Base64-encoded Md5 hash for the asset, used to ensure the integrity
    -- of the file at that location.
    md5Hash :: Prelude.Text,
    -- | The name of the asset. When importing from Amazon S3, the S3 object key
    -- is used as the asset name.
    assetName :: Prelude.Text,
    -- | The unique identifier for the revision associated with this import
    -- request.
    revisionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportAssetFromSignedUrlRequestDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetId', 'importAssetFromSignedUrlRequestDetails_dataSetId' - The unique identifier for the data set associated with this import job.
--
-- 'md5Hash', 'importAssetFromSignedUrlRequestDetails_md5Hash' - The Base64-encoded Md5 hash for the asset, used to ensure the integrity
-- of the file at that location.
--
-- 'assetName', 'importAssetFromSignedUrlRequestDetails_assetName' - The name of the asset. When importing from Amazon S3, the S3 object key
-- is used as the asset name.
--
-- 'revisionId', 'importAssetFromSignedUrlRequestDetails_revisionId' - The unique identifier for the revision associated with this import
-- request.
newImportAssetFromSignedUrlRequestDetails ::
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'md5Hash'
  Prelude.Text ->
  -- | 'assetName'
  Prelude.Text ->
  -- | 'revisionId'
  Prelude.Text ->
  ImportAssetFromSignedUrlRequestDetails
newImportAssetFromSignedUrlRequestDetails
  pDataSetId_
  pMd5Hash_
  pAssetName_
  pRevisionId_ =
    ImportAssetFromSignedUrlRequestDetails'
      { dataSetId =
          pDataSetId_,
        md5Hash = pMd5Hash_,
        assetName = pAssetName_,
        revisionId = pRevisionId_
      }

-- | The unique identifier for the data set associated with this import job.
importAssetFromSignedUrlRequestDetails_dataSetId :: Lens.Lens' ImportAssetFromSignedUrlRequestDetails Prelude.Text
importAssetFromSignedUrlRequestDetails_dataSetId = Lens.lens (\ImportAssetFromSignedUrlRequestDetails' {dataSetId} -> dataSetId) (\s@ImportAssetFromSignedUrlRequestDetails' {} a -> s {dataSetId = a} :: ImportAssetFromSignedUrlRequestDetails)

-- | The Base64-encoded Md5 hash for the asset, used to ensure the integrity
-- of the file at that location.
importAssetFromSignedUrlRequestDetails_md5Hash :: Lens.Lens' ImportAssetFromSignedUrlRequestDetails Prelude.Text
importAssetFromSignedUrlRequestDetails_md5Hash = Lens.lens (\ImportAssetFromSignedUrlRequestDetails' {md5Hash} -> md5Hash) (\s@ImportAssetFromSignedUrlRequestDetails' {} a -> s {md5Hash = a} :: ImportAssetFromSignedUrlRequestDetails)

-- | The name of the asset. When importing from Amazon S3, the S3 object key
-- is used as the asset name.
importAssetFromSignedUrlRequestDetails_assetName :: Lens.Lens' ImportAssetFromSignedUrlRequestDetails Prelude.Text
importAssetFromSignedUrlRequestDetails_assetName = Lens.lens (\ImportAssetFromSignedUrlRequestDetails' {assetName} -> assetName) (\s@ImportAssetFromSignedUrlRequestDetails' {} a -> s {assetName = a} :: ImportAssetFromSignedUrlRequestDetails)

-- | The unique identifier for the revision associated with this import
-- request.
importAssetFromSignedUrlRequestDetails_revisionId :: Lens.Lens' ImportAssetFromSignedUrlRequestDetails Prelude.Text
importAssetFromSignedUrlRequestDetails_revisionId = Lens.lens (\ImportAssetFromSignedUrlRequestDetails' {revisionId} -> revisionId) (\s@ImportAssetFromSignedUrlRequestDetails' {} a -> s {revisionId = a} :: ImportAssetFromSignedUrlRequestDetails)

instance
  Prelude.Hashable
    ImportAssetFromSignedUrlRequestDetails
  where
  hashWithSalt
    salt'
    ImportAssetFromSignedUrlRequestDetails' {..} =
      salt' `Prelude.hashWithSalt` revisionId
        `Prelude.hashWithSalt` assetName
        `Prelude.hashWithSalt` md5Hash
        `Prelude.hashWithSalt` dataSetId

instance
  Prelude.NFData
    ImportAssetFromSignedUrlRequestDetails
  where
  rnf ImportAssetFromSignedUrlRequestDetails' {..} =
    Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf assetName
      `Prelude.seq` Prelude.rnf md5Hash

instance
  Core.ToJSON
    ImportAssetFromSignedUrlRequestDetails
  where
  toJSON ImportAssetFromSignedUrlRequestDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DataSetId" Core..= dataSetId),
            Prelude.Just ("Md5Hash" Core..= md5Hash),
            Prelude.Just ("AssetName" Core..= assetName),
            Prelude.Just ("RevisionId" Core..= revisionId)
          ]
      )
