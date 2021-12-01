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
-- Module      : Amazonka.LicenseManager.CreateLicenseVersion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new version of the specified license.
module Amazonka.LicenseManager.CreateLicenseVersion
  ( -- * Creating a Request
    CreateLicenseVersion (..),
    newCreateLicenseVersion,

    -- * Request Lenses
    createLicenseVersion_sourceVersion,
    createLicenseVersion_licenseMetadata,
    createLicenseVersion_licenseArn,
    createLicenseVersion_licenseName,
    createLicenseVersion_productName,
    createLicenseVersion_issuer,
    createLicenseVersion_homeRegion,
    createLicenseVersion_validity,
    createLicenseVersion_entitlements,
    createLicenseVersion_consumptionConfiguration,
    createLicenseVersion_status,
    createLicenseVersion_clientToken,

    -- * Destructuring the Response
    CreateLicenseVersionResponse (..),
    newCreateLicenseVersionResponse,

    -- * Response Lenses
    createLicenseVersionResponse_status,
    createLicenseVersionResponse_version,
    createLicenseVersionResponse_licenseArn,
    createLicenseVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLicenseVersion' smart constructor.
data CreateLicenseVersion = CreateLicenseVersion'
  { -- | Current version of the license.
    sourceVersion :: Prelude.Maybe Prelude.Text,
    -- | Information about the license.
    licenseMetadata :: Prelude.Maybe [Metadata],
    -- | Amazon Resource Name (ARN) of the license.
    licenseArn :: Prelude.Text,
    -- | License name.
    licenseName :: Prelude.Text,
    -- | Product name.
    productName :: Prelude.Text,
    -- | License issuer.
    issuer :: Issuer,
    -- | Home Region of the license.
    homeRegion :: Prelude.Text,
    -- | Date and time range during which the license is valid, in ISO8601-UTC
    -- format.
    validity :: DatetimeRange,
    -- | License entitlements.
    entitlements :: [Entitlement],
    -- | Configuration for consumption of the license. Choose a provisional
    -- configuration for workloads running with continuous connectivity. Choose
    -- a borrow configuration for workloads with offline usage.
    consumptionConfiguration :: ConsumptionConfiguration,
    -- | License status.
    status :: LicenseStatus,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLicenseVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceVersion', 'createLicenseVersion_sourceVersion' - Current version of the license.
--
-- 'licenseMetadata', 'createLicenseVersion_licenseMetadata' - Information about the license.
--
-- 'licenseArn', 'createLicenseVersion_licenseArn' - Amazon Resource Name (ARN) of the license.
--
-- 'licenseName', 'createLicenseVersion_licenseName' - License name.
--
-- 'productName', 'createLicenseVersion_productName' - Product name.
--
-- 'issuer', 'createLicenseVersion_issuer' - License issuer.
--
-- 'homeRegion', 'createLicenseVersion_homeRegion' - Home Region of the license.
--
-- 'validity', 'createLicenseVersion_validity' - Date and time range during which the license is valid, in ISO8601-UTC
-- format.
--
-- 'entitlements', 'createLicenseVersion_entitlements' - License entitlements.
--
-- 'consumptionConfiguration', 'createLicenseVersion_consumptionConfiguration' - Configuration for consumption of the license. Choose a provisional
-- configuration for workloads running with continuous connectivity. Choose
-- a borrow configuration for workloads with offline usage.
--
-- 'status', 'createLicenseVersion_status' - License status.
--
-- 'clientToken', 'createLicenseVersion_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
newCreateLicenseVersion ::
  -- | 'licenseArn'
  Prelude.Text ->
  -- | 'licenseName'
  Prelude.Text ->
  -- | 'productName'
  Prelude.Text ->
  -- | 'issuer'
  Issuer ->
  -- | 'homeRegion'
  Prelude.Text ->
  -- | 'validity'
  DatetimeRange ->
  -- | 'consumptionConfiguration'
  ConsumptionConfiguration ->
  -- | 'status'
  LicenseStatus ->
  -- | 'clientToken'
  Prelude.Text ->
  CreateLicenseVersion
newCreateLicenseVersion
  pLicenseArn_
  pLicenseName_
  pProductName_
  pIssuer_
  pHomeRegion_
  pValidity_
  pConsumptionConfiguration_
  pStatus_
  pClientToken_ =
    CreateLicenseVersion'
      { sourceVersion =
          Prelude.Nothing,
        licenseMetadata = Prelude.Nothing,
        licenseArn = pLicenseArn_,
        licenseName = pLicenseName_,
        productName = pProductName_,
        issuer = pIssuer_,
        homeRegion = pHomeRegion_,
        validity = pValidity_,
        entitlements = Prelude.mempty,
        consumptionConfiguration =
          pConsumptionConfiguration_,
        status = pStatus_,
        clientToken = pClientToken_
      }

-- | Current version of the license.
createLicenseVersion_sourceVersion :: Lens.Lens' CreateLicenseVersion (Prelude.Maybe Prelude.Text)
createLicenseVersion_sourceVersion = Lens.lens (\CreateLicenseVersion' {sourceVersion} -> sourceVersion) (\s@CreateLicenseVersion' {} a -> s {sourceVersion = a} :: CreateLicenseVersion)

-- | Information about the license.
createLicenseVersion_licenseMetadata :: Lens.Lens' CreateLicenseVersion (Prelude.Maybe [Metadata])
createLicenseVersion_licenseMetadata = Lens.lens (\CreateLicenseVersion' {licenseMetadata} -> licenseMetadata) (\s@CreateLicenseVersion' {} a -> s {licenseMetadata = a} :: CreateLicenseVersion) Prelude.. Lens.mapping Lens.coerced

-- | Amazon Resource Name (ARN) of the license.
createLicenseVersion_licenseArn :: Lens.Lens' CreateLicenseVersion Prelude.Text
createLicenseVersion_licenseArn = Lens.lens (\CreateLicenseVersion' {licenseArn} -> licenseArn) (\s@CreateLicenseVersion' {} a -> s {licenseArn = a} :: CreateLicenseVersion)

-- | License name.
createLicenseVersion_licenseName :: Lens.Lens' CreateLicenseVersion Prelude.Text
createLicenseVersion_licenseName = Lens.lens (\CreateLicenseVersion' {licenseName} -> licenseName) (\s@CreateLicenseVersion' {} a -> s {licenseName = a} :: CreateLicenseVersion)

-- | Product name.
createLicenseVersion_productName :: Lens.Lens' CreateLicenseVersion Prelude.Text
createLicenseVersion_productName = Lens.lens (\CreateLicenseVersion' {productName} -> productName) (\s@CreateLicenseVersion' {} a -> s {productName = a} :: CreateLicenseVersion)

-- | License issuer.
createLicenseVersion_issuer :: Lens.Lens' CreateLicenseVersion Issuer
createLicenseVersion_issuer = Lens.lens (\CreateLicenseVersion' {issuer} -> issuer) (\s@CreateLicenseVersion' {} a -> s {issuer = a} :: CreateLicenseVersion)

-- | Home Region of the license.
createLicenseVersion_homeRegion :: Lens.Lens' CreateLicenseVersion Prelude.Text
createLicenseVersion_homeRegion = Lens.lens (\CreateLicenseVersion' {homeRegion} -> homeRegion) (\s@CreateLicenseVersion' {} a -> s {homeRegion = a} :: CreateLicenseVersion)

-- | Date and time range during which the license is valid, in ISO8601-UTC
-- format.
createLicenseVersion_validity :: Lens.Lens' CreateLicenseVersion DatetimeRange
createLicenseVersion_validity = Lens.lens (\CreateLicenseVersion' {validity} -> validity) (\s@CreateLicenseVersion' {} a -> s {validity = a} :: CreateLicenseVersion)

-- | License entitlements.
createLicenseVersion_entitlements :: Lens.Lens' CreateLicenseVersion [Entitlement]
createLicenseVersion_entitlements = Lens.lens (\CreateLicenseVersion' {entitlements} -> entitlements) (\s@CreateLicenseVersion' {} a -> s {entitlements = a} :: CreateLicenseVersion) Prelude.. Lens.coerced

-- | Configuration for consumption of the license. Choose a provisional
-- configuration for workloads running with continuous connectivity. Choose
-- a borrow configuration for workloads with offline usage.
createLicenseVersion_consumptionConfiguration :: Lens.Lens' CreateLicenseVersion ConsumptionConfiguration
createLicenseVersion_consumptionConfiguration = Lens.lens (\CreateLicenseVersion' {consumptionConfiguration} -> consumptionConfiguration) (\s@CreateLicenseVersion' {} a -> s {consumptionConfiguration = a} :: CreateLicenseVersion)

-- | License status.
createLicenseVersion_status :: Lens.Lens' CreateLicenseVersion LicenseStatus
createLicenseVersion_status = Lens.lens (\CreateLicenseVersion' {status} -> status) (\s@CreateLicenseVersion' {} a -> s {status = a} :: CreateLicenseVersion)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createLicenseVersion_clientToken :: Lens.Lens' CreateLicenseVersion Prelude.Text
createLicenseVersion_clientToken = Lens.lens (\CreateLicenseVersion' {clientToken} -> clientToken) (\s@CreateLicenseVersion' {} a -> s {clientToken = a} :: CreateLicenseVersion)

instance Core.AWSRequest CreateLicenseVersion where
  type
    AWSResponse CreateLicenseVersion =
      CreateLicenseVersionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLicenseVersionResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "Version")
            Prelude.<*> (x Core..?> "LicenseArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLicenseVersion where
  hashWithSalt salt' CreateLicenseVersion' {..} =
    salt' `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` consumptionConfiguration
      `Prelude.hashWithSalt` entitlements
      `Prelude.hashWithSalt` validity
      `Prelude.hashWithSalt` homeRegion
      `Prelude.hashWithSalt` issuer
      `Prelude.hashWithSalt` productName
      `Prelude.hashWithSalt` licenseName
      `Prelude.hashWithSalt` licenseArn
      `Prelude.hashWithSalt` licenseMetadata
      `Prelude.hashWithSalt` sourceVersion

instance Prelude.NFData CreateLicenseVersion where
  rnf CreateLicenseVersion' {..} =
    Prelude.rnf sourceVersion
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf consumptionConfiguration
      `Prelude.seq` Prelude.rnf entitlements
      `Prelude.seq` Prelude.rnf validity
      `Prelude.seq` Prelude.rnf homeRegion
      `Prelude.seq` Prelude.rnf issuer
      `Prelude.seq` Prelude.rnf productName
      `Prelude.seq` Prelude.rnf licenseName
      `Prelude.seq` Prelude.rnf licenseArn
      `Prelude.seq` Prelude.rnf licenseMetadata

instance Core.ToHeaders CreateLicenseVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.CreateLicenseVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLicenseVersion where
  toJSON CreateLicenseVersion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SourceVersion" Core..=) Prelude.<$> sourceVersion,
            ("LicenseMetadata" Core..=)
              Prelude.<$> licenseMetadata,
            Prelude.Just ("LicenseArn" Core..= licenseArn),
            Prelude.Just ("LicenseName" Core..= licenseName),
            Prelude.Just ("ProductName" Core..= productName),
            Prelude.Just ("Issuer" Core..= issuer),
            Prelude.Just ("HomeRegion" Core..= homeRegion),
            Prelude.Just ("Validity" Core..= validity),
            Prelude.Just ("Entitlements" Core..= entitlements),
            Prelude.Just
              ( "ConsumptionConfiguration"
                  Core..= consumptionConfiguration
              ),
            Prelude.Just ("Status" Core..= status),
            Prelude.Just ("ClientToken" Core..= clientToken)
          ]
      )

instance Core.ToPath CreateLicenseVersion where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLicenseVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLicenseVersionResponse' smart constructor.
data CreateLicenseVersionResponse = CreateLicenseVersionResponse'
  { -- | License status.
    status :: Prelude.Maybe LicenseStatus,
    -- | New version of the license.
    version :: Prelude.Maybe Prelude.Text,
    -- | License ARN.
    licenseArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLicenseVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createLicenseVersionResponse_status' - License status.
--
-- 'version', 'createLicenseVersionResponse_version' - New version of the license.
--
-- 'licenseArn', 'createLicenseVersionResponse_licenseArn' - License ARN.
--
-- 'httpStatus', 'createLicenseVersionResponse_httpStatus' - The response's http status code.
newCreateLicenseVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLicenseVersionResponse
newCreateLicenseVersionResponse pHttpStatus_ =
  CreateLicenseVersionResponse'
    { status =
        Prelude.Nothing,
      version = Prelude.Nothing,
      licenseArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | License status.
createLicenseVersionResponse_status :: Lens.Lens' CreateLicenseVersionResponse (Prelude.Maybe LicenseStatus)
createLicenseVersionResponse_status = Lens.lens (\CreateLicenseVersionResponse' {status} -> status) (\s@CreateLicenseVersionResponse' {} a -> s {status = a} :: CreateLicenseVersionResponse)

-- | New version of the license.
createLicenseVersionResponse_version :: Lens.Lens' CreateLicenseVersionResponse (Prelude.Maybe Prelude.Text)
createLicenseVersionResponse_version = Lens.lens (\CreateLicenseVersionResponse' {version} -> version) (\s@CreateLicenseVersionResponse' {} a -> s {version = a} :: CreateLicenseVersionResponse)

-- | License ARN.
createLicenseVersionResponse_licenseArn :: Lens.Lens' CreateLicenseVersionResponse (Prelude.Maybe Prelude.Text)
createLicenseVersionResponse_licenseArn = Lens.lens (\CreateLicenseVersionResponse' {licenseArn} -> licenseArn) (\s@CreateLicenseVersionResponse' {} a -> s {licenseArn = a} :: CreateLicenseVersionResponse)

-- | The response's http status code.
createLicenseVersionResponse_httpStatus :: Lens.Lens' CreateLicenseVersionResponse Prelude.Int
createLicenseVersionResponse_httpStatus = Lens.lens (\CreateLicenseVersionResponse' {httpStatus} -> httpStatus) (\s@CreateLicenseVersionResponse' {} a -> s {httpStatus = a} :: CreateLicenseVersionResponse)

instance Prelude.NFData CreateLicenseVersionResponse where
  rnf CreateLicenseVersionResponse' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf licenseArn
      `Prelude.seq` Prelude.rnf version
