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
-- Module      : Amazonka.ECR.BatchGetImage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets detailed information for an image. Images are specified with either
-- an @imageTag@ or @imageDigest@.
--
-- When an image is pulled, the BatchGetImage API is called once to
-- retrieve the image manifest.
module Amazonka.ECR.BatchGetImage
  ( -- * Creating a Request
    BatchGetImage (..),
    newBatchGetImage,

    -- * Request Lenses
    batchGetImage_registryId,
    batchGetImage_acceptedMediaTypes,
    batchGetImage_repositoryName,
    batchGetImage_imageIds,

    -- * Destructuring the Response
    BatchGetImageResponse (..),
    newBatchGetImageResponse,

    -- * Response Lenses
    batchGetImageResponse_images,
    batchGetImageResponse_failures,
    batchGetImageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ECR.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchGetImage' smart constructor.
data BatchGetImage = BatchGetImage'
  { -- | The Amazon Web Services account ID associated with the registry that
    -- contains the images to describe. If you do not specify a registry, the
    -- default registry is assumed.
    registryId :: Prelude.Maybe Prelude.Text,
    -- | The accepted media types for the request.
    --
    -- Valid values: @application\/vnd.docker.distribution.manifest.v1+json@ |
    -- @application\/vnd.docker.distribution.manifest.v2+json@ |
    -- @application\/vnd.oci.image.manifest.v1+json@
    acceptedMediaTypes :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The repository that contains the images to describe.
    repositoryName :: Prelude.Text,
    -- | A list of image ID references that correspond to images to describe. The
    -- format of the @imageIds@ reference is @imageTag=tag@ or
    -- @imageDigest=digest@.
    imageIds :: [ImageIdentifier]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetImage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'registryId', 'batchGetImage_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the images to describe. If you do not specify a registry, the
-- default registry is assumed.
--
-- 'acceptedMediaTypes', 'batchGetImage_acceptedMediaTypes' - The accepted media types for the request.
--
-- Valid values: @application\/vnd.docker.distribution.manifest.v1+json@ |
-- @application\/vnd.docker.distribution.manifest.v2+json@ |
-- @application\/vnd.oci.image.manifest.v1+json@
--
-- 'repositoryName', 'batchGetImage_repositoryName' - The repository that contains the images to describe.
--
-- 'imageIds', 'batchGetImage_imageIds' - A list of image ID references that correspond to images to describe. The
-- format of the @imageIds@ reference is @imageTag=tag@ or
-- @imageDigest=digest@.
newBatchGetImage ::
  -- | 'repositoryName'
  Prelude.Text ->
  BatchGetImage
newBatchGetImage pRepositoryName_ =
  BatchGetImage'
    { registryId = Prelude.Nothing,
      acceptedMediaTypes = Prelude.Nothing,
      repositoryName = pRepositoryName_,
      imageIds = Prelude.mempty
    }

-- | The Amazon Web Services account ID associated with the registry that
-- contains the images to describe. If you do not specify a registry, the
-- default registry is assumed.
batchGetImage_registryId :: Lens.Lens' BatchGetImage (Prelude.Maybe Prelude.Text)
batchGetImage_registryId = Lens.lens (\BatchGetImage' {registryId} -> registryId) (\s@BatchGetImage' {} a -> s {registryId = a} :: BatchGetImage)

-- | The accepted media types for the request.
--
-- Valid values: @application\/vnd.docker.distribution.manifest.v1+json@ |
-- @application\/vnd.docker.distribution.manifest.v2+json@ |
-- @application\/vnd.oci.image.manifest.v1+json@
batchGetImage_acceptedMediaTypes :: Lens.Lens' BatchGetImage (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
batchGetImage_acceptedMediaTypes = Lens.lens (\BatchGetImage' {acceptedMediaTypes} -> acceptedMediaTypes) (\s@BatchGetImage' {} a -> s {acceptedMediaTypes = a} :: BatchGetImage) Prelude.. Lens.mapping Lens.coerced

-- | The repository that contains the images to describe.
batchGetImage_repositoryName :: Lens.Lens' BatchGetImage Prelude.Text
batchGetImage_repositoryName = Lens.lens (\BatchGetImage' {repositoryName} -> repositoryName) (\s@BatchGetImage' {} a -> s {repositoryName = a} :: BatchGetImage)

-- | A list of image ID references that correspond to images to describe. The
-- format of the @imageIds@ reference is @imageTag=tag@ or
-- @imageDigest=digest@.
batchGetImage_imageIds :: Lens.Lens' BatchGetImage [ImageIdentifier]
batchGetImage_imageIds = Lens.lens (\BatchGetImage' {imageIds} -> imageIds) (\s@BatchGetImage' {} a -> s {imageIds = a} :: BatchGetImage) Prelude.. Lens.coerced

instance Core.AWSRequest BatchGetImage where
  type
    AWSResponse BatchGetImage =
      BatchGetImageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchGetImageResponse'
            Prelude.<$> (x Core..?> "images" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "failures" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchGetImage where
  hashWithSalt salt' BatchGetImage' {..} =
    salt' `Prelude.hashWithSalt` imageIds
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` acceptedMediaTypes
      `Prelude.hashWithSalt` registryId

instance Prelude.NFData BatchGetImage where
  rnf BatchGetImage' {..} =
    Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf imageIds
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf acceptedMediaTypes

instance Core.ToHeaders BatchGetImage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonEC2ContainerRegistry_V20150921.BatchGetImage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON BatchGetImage where
  toJSON BatchGetImage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("registryId" Core..=) Prelude.<$> registryId,
            ("acceptedMediaTypes" Core..=)
              Prelude.<$> acceptedMediaTypes,
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just ("imageIds" Core..= imageIds)
          ]
      )

instance Core.ToPath BatchGetImage where
  toPath = Prelude.const "/"

instance Core.ToQuery BatchGetImage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchGetImageResponse' smart constructor.
data BatchGetImageResponse = BatchGetImageResponse'
  { -- | A list of image objects corresponding to the image references in the
    -- request.
    images :: Prelude.Maybe [Image],
    -- | Any failures associated with the call.
    failures :: Prelude.Maybe [ImageFailure],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchGetImageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'images', 'batchGetImageResponse_images' - A list of image objects corresponding to the image references in the
-- request.
--
-- 'failures', 'batchGetImageResponse_failures' - Any failures associated with the call.
--
-- 'httpStatus', 'batchGetImageResponse_httpStatus' - The response's http status code.
newBatchGetImageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchGetImageResponse
newBatchGetImageResponse pHttpStatus_ =
  BatchGetImageResponse'
    { images = Prelude.Nothing,
      failures = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of image objects corresponding to the image references in the
-- request.
batchGetImageResponse_images :: Lens.Lens' BatchGetImageResponse (Prelude.Maybe [Image])
batchGetImageResponse_images = Lens.lens (\BatchGetImageResponse' {images} -> images) (\s@BatchGetImageResponse' {} a -> s {images = a} :: BatchGetImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | Any failures associated with the call.
batchGetImageResponse_failures :: Lens.Lens' BatchGetImageResponse (Prelude.Maybe [ImageFailure])
batchGetImageResponse_failures = Lens.lens (\BatchGetImageResponse' {failures} -> failures) (\s@BatchGetImageResponse' {} a -> s {failures = a} :: BatchGetImageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchGetImageResponse_httpStatus :: Lens.Lens' BatchGetImageResponse Prelude.Int
batchGetImageResponse_httpStatus = Lens.lens (\BatchGetImageResponse' {httpStatus} -> httpStatus) (\s@BatchGetImageResponse' {} a -> s {httpStatus = a} :: BatchGetImageResponse)

instance Prelude.NFData BatchGetImageResponse where
  rnf BatchGetImageResponse' {..} =
    Prelude.rnf images
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf failures
