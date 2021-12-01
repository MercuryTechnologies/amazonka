{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DataExchange.UpdateRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation updates a revision.
module Amazonka.DataExchange.UpdateRevision
  ( -- * Creating a Request
    UpdateRevision (..),
    newUpdateRevision,

    -- * Request Lenses
    updateRevision_finalized,
    updateRevision_comment,
    updateRevision_revisionId,
    updateRevision_dataSetId,

    -- * Destructuring the Response
    UpdateRevisionResponse (..),
    newUpdateRevisionResponse,

    -- * Response Lenses
    updateRevisionResponse_arn,
    updateRevisionResponse_createdAt,
    updateRevisionResponse_sourceId,
    updateRevisionResponse_finalized,
    updateRevisionResponse_dataSetId,
    updateRevisionResponse_id,
    updateRevisionResponse_updatedAt,
    updateRevisionResponse_comment,
    updateRevisionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The request body for UpdateRevision.
--
-- /See:/ 'newUpdateRevision' smart constructor.
data UpdateRevision = UpdateRevision'
  { -- | Finalizing a revision tells AWS Data Exchange that your changes to the
    -- assets in the revision are complete. After it\'s in this read-only
    -- state, you can publish the revision to your products.
    finalized :: Prelude.Maybe Prelude.Bool,
    -- | An optional comment about the revision.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for a revision.
    revisionId :: Prelude.Text,
    -- | The unique identifier for a data set.
    dataSetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'finalized', 'updateRevision_finalized' - Finalizing a revision tells AWS Data Exchange that your changes to the
-- assets in the revision are complete. After it\'s in this read-only
-- state, you can publish the revision to your products.
--
-- 'comment', 'updateRevision_comment' - An optional comment about the revision.
--
-- 'revisionId', 'updateRevision_revisionId' - The unique identifier for a revision.
--
-- 'dataSetId', 'updateRevision_dataSetId' - The unique identifier for a data set.
newUpdateRevision ::
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  UpdateRevision
newUpdateRevision pRevisionId_ pDataSetId_ =
  UpdateRevision'
    { finalized = Prelude.Nothing,
      comment = Prelude.Nothing,
      revisionId = pRevisionId_,
      dataSetId = pDataSetId_
    }

-- | Finalizing a revision tells AWS Data Exchange that your changes to the
-- assets in the revision are complete. After it\'s in this read-only
-- state, you can publish the revision to your products.
updateRevision_finalized :: Lens.Lens' UpdateRevision (Prelude.Maybe Prelude.Bool)
updateRevision_finalized = Lens.lens (\UpdateRevision' {finalized} -> finalized) (\s@UpdateRevision' {} a -> s {finalized = a} :: UpdateRevision)

-- | An optional comment about the revision.
updateRevision_comment :: Lens.Lens' UpdateRevision (Prelude.Maybe Prelude.Text)
updateRevision_comment = Lens.lens (\UpdateRevision' {comment} -> comment) (\s@UpdateRevision' {} a -> s {comment = a} :: UpdateRevision)

-- | The unique identifier for a revision.
updateRevision_revisionId :: Lens.Lens' UpdateRevision Prelude.Text
updateRevision_revisionId = Lens.lens (\UpdateRevision' {revisionId} -> revisionId) (\s@UpdateRevision' {} a -> s {revisionId = a} :: UpdateRevision)

-- | The unique identifier for a data set.
updateRevision_dataSetId :: Lens.Lens' UpdateRevision Prelude.Text
updateRevision_dataSetId = Lens.lens (\UpdateRevision' {dataSetId} -> dataSetId) (\s@UpdateRevision' {} a -> s {dataSetId = a} :: UpdateRevision)

instance Core.AWSRequest UpdateRevision where
  type
    AWSResponse UpdateRevision =
      UpdateRevisionResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRevisionResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "CreatedAt")
            Prelude.<*> (x Core..?> "SourceId")
            Prelude.<*> (x Core..?> "Finalized")
            Prelude.<*> (x Core..?> "DataSetId")
            Prelude.<*> (x Core..?> "Id")
            Prelude.<*> (x Core..?> "UpdatedAt")
            Prelude.<*> (x Core..?> "Comment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRevision where
  hashWithSalt salt' UpdateRevision' {..} =
    salt' `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` finalized

instance Prelude.NFData UpdateRevision where
  rnf UpdateRevision' {..} =
    Prelude.rnf finalized
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf comment

instance Core.ToHeaders UpdateRevision where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRevision where
  toJSON UpdateRevision' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Finalized" Core..=) Prelude.<$> finalized,
            ("Comment" Core..=) Prelude.<$> comment
          ]
      )

instance Core.ToPath UpdateRevision where
  toPath UpdateRevision' {..} =
    Prelude.mconcat
      [ "/v1/data-sets/",
        Core.toBS dataSetId,
        "/revisions/",
        Core.toBS revisionId
      ]

instance Core.ToQuery UpdateRevision where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRevisionResponse' smart constructor.
data UpdateRevisionResponse = UpdateRevisionResponse'
  { -- | The ARN for the revision.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the revision was created, in ISO 8601 format.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The revision ID of the owned revision corresponding to the entitled
    -- revision being viewed. This parameter is returned when a revision owner
    -- is viewing the entitled copy of its owned revision.
    sourceId :: Prelude.Maybe Prelude.Text,
    -- | To publish a revision to a data set in a product, the revision must
    -- first be finalized. Finalizing a revision tells AWS Data Exchange that
    -- changes to the assets in the revision are complete. After it\'s in this
    -- read-only state, you can publish the revision to your products.
    --
    -- Finalized revisions can be published through the AWS Data Exchange
    -- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
    -- Marketplace Catalog API action. When using the API, revisions are
    -- uniquely identified by their ARN.
    finalized :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the data set associated with this revision.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the revision.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the revision was last updated, in ISO 8601
    -- format.
    updatedAt :: Prelude.Maybe Core.POSIX,
    -- | An optional comment about the revision.
    comment :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRevisionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateRevisionResponse_arn' - The ARN for the revision.
--
-- 'createdAt', 'updateRevisionResponse_createdAt' - The date and time that the revision was created, in ISO 8601 format.
--
-- 'sourceId', 'updateRevisionResponse_sourceId' - The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
--
-- 'finalized', 'updateRevisionResponse_finalized' - To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- changes to the assets in the revision are complete. After it\'s in this
-- read-only state, you can publish the revision to your products.
--
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
--
-- 'dataSetId', 'updateRevisionResponse_dataSetId' - The unique identifier for the data set associated with this revision.
--
-- 'id', 'updateRevisionResponse_id' - The unique identifier for the revision.
--
-- 'updatedAt', 'updateRevisionResponse_updatedAt' - The date and time that the revision was last updated, in ISO 8601
-- format.
--
-- 'comment', 'updateRevisionResponse_comment' - An optional comment about the revision.
--
-- 'httpStatus', 'updateRevisionResponse_httpStatus' - The response's http status code.
newUpdateRevisionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRevisionResponse
newUpdateRevisionResponse pHttpStatus_ =
  UpdateRevisionResponse'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      sourceId = Prelude.Nothing,
      finalized = Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      id = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      comment = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN for the revision.
updateRevisionResponse_arn :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Text)
updateRevisionResponse_arn = Lens.lens (\UpdateRevisionResponse' {arn} -> arn) (\s@UpdateRevisionResponse' {} a -> s {arn = a} :: UpdateRevisionResponse)

-- | The date and time that the revision was created, in ISO 8601 format.
updateRevisionResponse_createdAt :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.UTCTime)
updateRevisionResponse_createdAt = Lens.lens (\UpdateRevisionResponse' {createdAt} -> createdAt) (\s@UpdateRevisionResponse' {} a -> s {createdAt = a} :: UpdateRevisionResponse) Prelude.. Lens.mapping Core._Time

-- | The revision ID of the owned revision corresponding to the entitled
-- revision being viewed. This parameter is returned when a revision owner
-- is viewing the entitled copy of its owned revision.
updateRevisionResponse_sourceId :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Text)
updateRevisionResponse_sourceId = Lens.lens (\UpdateRevisionResponse' {sourceId} -> sourceId) (\s@UpdateRevisionResponse' {} a -> s {sourceId = a} :: UpdateRevisionResponse)

-- | To publish a revision to a data set in a product, the revision must
-- first be finalized. Finalizing a revision tells AWS Data Exchange that
-- changes to the assets in the revision are complete. After it\'s in this
-- read-only state, you can publish the revision to your products.
--
-- Finalized revisions can be published through the AWS Data Exchange
-- console or the AWS Marketplace Catalog API, using the StartChangeSet AWS
-- Marketplace Catalog API action. When using the API, revisions are
-- uniquely identified by their ARN.
updateRevisionResponse_finalized :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Bool)
updateRevisionResponse_finalized = Lens.lens (\UpdateRevisionResponse' {finalized} -> finalized) (\s@UpdateRevisionResponse' {} a -> s {finalized = a} :: UpdateRevisionResponse)

-- | The unique identifier for the data set associated with this revision.
updateRevisionResponse_dataSetId :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Text)
updateRevisionResponse_dataSetId = Lens.lens (\UpdateRevisionResponse' {dataSetId} -> dataSetId) (\s@UpdateRevisionResponse' {} a -> s {dataSetId = a} :: UpdateRevisionResponse)

-- | The unique identifier for the revision.
updateRevisionResponse_id :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Text)
updateRevisionResponse_id = Lens.lens (\UpdateRevisionResponse' {id} -> id) (\s@UpdateRevisionResponse' {} a -> s {id = a} :: UpdateRevisionResponse)

-- | The date and time that the revision was last updated, in ISO 8601
-- format.
updateRevisionResponse_updatedAt :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.UTCTime)
updateRevisionResponse_updatedAt = Lens.lens (\UpdateRevisionResponse' {updatedAt} -> updatedAt) (\s@UpdateRevisionResponse' {} a -> s {updatedAt = a} :: UpdateRevisionResponse) Prelude.. Lens.mapping Core._Time

-- | An optional comment about the revision.
updateRevisionResponse_comment :: Lens.Lens' UpdateRevisionResponse (Prelude.Maybe Prelude.Text)
updateRevisionResponse_comment = Lens.lens (\UpdateRevisionResponse' {comment} -> comment) (\s@UpdateRevisionResponse' {} a -> s {comment = a} :: UpdateRevisionResponse)

-- | The response's http status code.
updateRevisionResponse_httpStatus :: Lens.Lens' UpdateRevisionResponse Prelude.Int
updateRevisionResponse_httpStatus = Lens.lens (\UpdateRevisionResponse' {httpStatus} -> httpStatus) (\s@UpdateRevisionResponse' {} a -> s {httpStatus = a} :: UpdateRevisionResponse)

instance Prelude.NFData UpdateRevisionResponse where
  rnf UpdateRevisionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf comment
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf finalized
      `Prelude.seq` Prelude.rnf sourceId
      `Prelude.seq` Prelude.rnf createdAt
