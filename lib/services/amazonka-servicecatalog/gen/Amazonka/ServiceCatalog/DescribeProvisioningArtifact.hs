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
-- Module      : Amazonka.ServiceCatalog.DescribeProvisioningArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified provisioning artifact (also known
-- as a version) for the specified product.
module Amazonka.ServiceCatalog.DescribeProvisioningArtifact
  ( -- * Creating a Request
    DescribeProvisioningArtifact (..),
    newDescribeProvisioningArtifact,

    -- * Request Lenses
    describeProvisioningArtifact_productName,
    describeProvisioningArtifact_provisioningArtifactId,
    describeProvisioningArtifact_verbose,
    describeProvisioningArtifact_provisioningArtifactName,
    describeProvisioningArtifact_acceptLanguage,
    describeProvisioningArtifact_productId,

    -- * Destructuring the Response
    DescribeProvisioningArtifactResponse (..),
    newDescribeProvisioningArtifactResponse,

    -- * Response Lenses
    describeProvisioningArtifactResponse_status,
    describeProvisioningArtifactResponse_info,
    describeProvisioningArtifactResponse_provisioningArtifactDetail,
    describeProvisioningArtifactResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newDescribeProvisioningArtifact' smart constructor.
data DescribeProvisioningArtifact = DescribeProvisioningArtifact'
  { -- | The product name.
    productName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioning artifact.
    provisioningArtifactId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a verbose level of detail is enabled.
    verbose :: Prelude.Maybe Prelude.Bool,
    -- | The provisioning artifact name.
    provisioningArtifactName :: Prelude.Maybe Prelude.Text,
    -- | The language code.
    --
    -- -   @en@ - English (default)
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisioningArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productName', 'describeProvisioningArtifact_productName' - The product name.
--
-- 'provisioningArtifactId', 'describeProvisioningArtifact_provisioningArtifactId' - The identifier of the provisioning artifact.
--
-- 'verbose', 'describeProvisioningArtifact_verbose' - Indicates whether a verbose level of detail is enabled.
--
-- 'provisioningArtifactName', 'describeProvisioningArtifact_provisioningArtifactName' - The provisioning artifact name.
--
-- 'acceptLanguage', 'describeProvisioningArtifact_acceptLanguage' - The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'productId', 'describeProvisioningArtifact_productId' - The product identifier.
newDescribeProvisioningArtifact ::
  DescribeProvisioningArtifact
newDescribeProvisioningArtifact =
  DescribeProvisioningArtifact'
    { productName =
        Prelude.Nothing,
      provisioningArtifactId = Prelude.Nothing,
      verbose = Prelude.Nothing,
      provisioningArtifactName = Prelude.Nothing,
      acceptLanguage = Prelude.Nothing,
      productId = Prelude.Nothing
    }

-- | The product name.
describeProvisioningArtifact_productName :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Text)
describeProvisioningArtifact_productName = Lens.lens (\DescribeProvisioningArtifact' {productName} -> productName) (\s@DescribeProvisioningArtifact' {} a -> s {productName = a} :: DescribeProvisioningArtifact)

-- | The identifier of the provisioning artifact.
describeProvisioningArtifact_provisioningArtifactId :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Text)
describeProvisioningArtifact_provisioningArtifactId = Lens.lens (\DescribeProvisioningArtifact' {provisioningArtifactId} -> provisioningArtifactId) (\s@DescribeProvisioningArtifact' {} a -> s {provisioningArtifactId = a} :: DescribeProvisioningArtifact)

-- | Indicates whether a verbose level of detail is enabled.
describeProvisioningArtifact_verbose :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Bool)
describeProvisioningArtifact_verbose = Lens.lens (\DescribeProvisioningArtifact' {verbose} -> verbose) (\s@DescribeProvisioningArtifact' {} a -> s {verbose = a} :: DescribeProvisioningArtifact)

-- | The provisioning artifact name.
describeProvisioningArtifact_provisioningArtifactName :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Text)
describeProvisioningArtifact_provisioningArtifactName = Lens.lens (\DescribeProvisioningArtifact' {provisioningArtifactName} -> provisioningArtifactName) (\s@DescribeProvisioningArtifact' {} a -> s {provisioningArtifactName = a} :: DescribeProvisioningArtifact)

-- | The language code.
--
-- -   @en@ - English (default)
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
describeProvisioningArtifact_acceptLanguage :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Text)
describeProvisioningArtifact_acceptLanguage = Lens.lens (\DescribeProvisioningArtifact' {acceptLanguage} -> acceptLanguage) (\s@DescribeProvisioningArtifact' {} a -> s {acceptLanguage = a} :: DescribeProvisioningArtifact)

-- | The product identifier.
describeProvisioningArtifact_productId :: Lens.Lens' DescribeProvisioningArtifact (Prelude.Maybe Prelude.Text)
describeProvisioningArtifact_productId = Lens.lens (\DescribeProvisioningArtifact' {productId} -> productId) (\s@DescribeProvisioningArtifact' {} a -> s {productId = a} :: DescribeProvisioningArtifact)

instance Core.AWSRequest DescribeProvisioningArtifact where
  type
    AWSResponse DescribeProvisioningArtifact =
      DescribeProvisioningArtifactResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeProvisioningArtifactResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Info" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "ProvisioningArtifactDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeProvisioningArtifact
  where
  hashWithSalt salt' DescribeProvisioningArtifact' {..} =
    salt' `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` provisioningArtifactName
      `Prelude.hashWithSalt` verbose
      `Prelude.hashWithSalt` provisioningArtifactId
      `Prelude.hashWithSalt` productName

instance Prelude.NFData DescribeProvisioningArtifact where
  rnf DescribeProvisioningArtifact' {..} =
    Prelude.rnf productName
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf provisioningArtifactName
      `Prelude.seq` Prelude.rnf verbose
      `Prelude.seq` Prelude.rnf provisioningArtifactId

instance Core.ToHeaders DescribeProvisioningArtifact where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWS242ServiceCatalogService.DescribeProvisioningArtifact" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeProvisioningArtifact where
  toJSON DescribeProvisioningArtifact' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ProductName" Core..=) Prelude.<$> productName,
            ("ProvisioningArtifactId" Core..=)
              Prelude.<$> provisioningArtifactId,
            ("Verbose" Core..=) Prelude.<$> verbose,
            ("ProvisioningArtifactName" Core..=)
              Prelude.<$> provisioningArtifactName,
            ("AcceptLanguage" Core..=)
              Prelude.<$> acceptLanguage,
            ("ProductId" Core..=) Prelude.<$> productId
          ]
      )

instance Core.ToPath DescribeProvisioningArtifact where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeProvisioningArtifact where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeProvisioningArtifactResponse' smart constructor.
data DescribeProvisioningArtifactResponse = DescribeProvisioningArtifactResponse'
  { -- | The status of the current request.
    status :: Prelude.Maybe RequestStatus,
    -- | The URL of the CloudFormation template in Amazon S3.
    info :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Information about the provisioning artifact.
    provisioningArtifactDetail :: Prelude.Maybe ProvisioningArtifactDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeProvisioningArtifactResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeProvisioningArtifactResponse_status' - The status of the current request.
--
-- 'info', 'describeProvisioningArtifactResponse_info' - The URL of the CloudFormation template in Amazon S3.
--
-- 'provisioningArtifactDetail', 'describeProvisioningArtifactResponse_provisioningArtifactDetail' - Information about the provisioning artifact.
--
-- 'httpStatus', 'describeProvisioningArtifactResponse_httpStatus' - The response's http status code.
newDescribeProvisioningArtifactResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeProvisioningArtifactResponse
newDescribeProvisioningArtifactResponse pHttpStatus_ =
  DescribeProvisioningArtifactResponse'
    { status =
        Prelude.Nothing,
      info = Prelude.Nothing,
      provisioningArtifactDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status of the current request.
describeProvisioningArtifactResponse_status :: Lens.Lens' DescribeProvisioningArtifactResponse (Prelude.Maybe RequestStatus)
describeProvisioningArtifactResponse_status = Lens.lens (\DescribeProvisioningArtifactResponse' {status} -> status) (\s@DescribeProvisioningArtifactResponse' {} a -> s {status = a} :: DescribeProvisioningArtifactResponse)

-- | The URL of the CloudFormation template in Amazon S3.
describeProvisioningArtifactResponse_info :: Lens.Lens' DescribeProvisioningArtifactResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeProvisioningArtifactResponse_info = Lens.lens (\DescribeProvisioningArtifactResponse' {info} -> info) (\s@DescribeProvisioningArtifactResponse' {} a -> s {info = a} :: DescribeProvisioningArtifactResponse) Prelude.. Lens.mapping Lens.coerced

-- | Information about the provisioning artifact.
describeProvisioningArtifactResponse_provisioningArtifactDetail :: Lens.Lens' DescribeProvisioningArtifactResponse (Prelude.Maybe ProvisioningArtifactDetail)
describeProvisioningArtifactResponse_provisioningArtifactDetail = Lens.lens (\DescribeProvisioningArtifactResponse' {provisioningArtifactDetail} -> provisioningArtifactDetail) (\s@DescribeProvisioningArtifactResponse' {} a -> s {provisioningArtifactDetail = a} :: DescribeProvisioningArtifactResponse)

-- | The response's http status code.
describeProvisioningArtifactResponse_httpStatus :: Lens.Lens' DescribeProvisioningArtifactResponse Prelude.Int
describeProvisioningArtifactResponse_httpStatus = Lens.lens (\DescribeProvisioningArtifactResponse' {httpStatus} -> httpStatus) (\s@DescribeProvisioningArtifactResponse' {} a -> s {httpStatus = a} :: DescribeProvisioningArtifactResponse)

instance
  Prelude.NFData
    DescribeProvisioningArtifactResponse
  where
  rnf DescribeProvisioningArtifactResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf provisioningArtifactDetail
      `Prelude.seq` Prelude.rnf info
