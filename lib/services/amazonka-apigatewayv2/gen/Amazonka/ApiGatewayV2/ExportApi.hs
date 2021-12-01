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
-- Module      : Amazonka.ApiGatewayV2.ExportApi
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ApiGatewayV2.ExportApi
  ( -- * Creating a Request
    ExportApi (..),
    newExportApi,

    -- * Request Lenses
    exportApi_exportVersion,
    exportApi_stageName,
    exportApi_includeExtensions,
    exportApi_specification,
    exportApi_outputType,
    exportApi_apiId,

    -- * Destructuring the Response
    ExportApiResponse (..),
    newExportApiResponse,

    -- * Response Lenses
    exportApiResponse_body,
    exportApiResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportApi' smart constructor.
data ExportApi = ExportApi'
  { -- | The version of the API Gateway export algorithm. API Gateway uses the
    -- latest version by default. Currently, the only supported version is 1.0.
    exportVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the API stage to export. If you don\'t specify this
    -- property, a representation of the latest API configuration is exported.
    stageName :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to include
    -- <https://docs.aws.amazon.com//apigateway/latest/developerguide/api-gateway-swagger-extensions.html API Gateway extensions>
    -- in the exported API definition. API Gateway extensions are included by
    -- default.
    includeExtensions :: Prelude.Maybe Prelude.Bool,
    -- | The version of the API specification to use. OAS30, for OpenAPI 3.0, is
    -- the only supported value.
    specification :: Prelude.Text,
    -- | The output type of the exported definition file. Valid values are JSON
    -- and YAML.
    outputType :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportApi' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportVersion', 'exportApi_exportVersion' - The version of the API Gateway export algorithm. API Gateway uses the
-- latest version by default. Currently, the only supported version is 1.0.
--
-- 'stageName', 'exportApi_stageName' - The name of the API stage to export. If you don\'t specify this
-- property, a representation of the latest API configuration is exported.
--
-- 'includeExtensions', 'exportApi_includeExtensions' - Specifies whether to include
-- <https://docs.aws.amazon.com//apigateway/latest/developerguide/api-gateway-swagger-extensions.html API Gateway extensions>
-- in the exported API definition. API Gateway extensions are included by
-- default.
--
-- 'specification', 'exportApi_specification' - The version of the API specification to use. OAS30, for OpenAPI 3.0, is
-- the only supported value.
--
-- 'outputType', 'exportApi_outputType' - The output type of the exported definition file. Valid values are JSON
-- and YAML.
--
-- 'apiId', 'exportApi_apiId' - The API identifier.
newExportApi ::
  -- | 'specification'
  Prelude.Text ->
  -- | 'outputType'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  ExportApi
newExportApi pSpecification_ pOutputType_ pApiId_ =
  ExportApi'
    { exportVersion = Prelude.Nothing,
      stageName = Prelude.Nothing,
      includeExtensions = Prelude.Nothing,
      specification = pSpecification_,
      outputType = pOutputType_,
      apiId = pApiId_
    }

-- | The version of the API Gateway export algorithm. API Gateway uses the
-- latest version by default. Currently, the only supported version is 1.0.
exportApi_exportVersion :: Lens.Lens' ExportApi (Prelude.Maybe Prelude.Text)
exportApi_exportVersion = Lens.lens (\ExportApi' {exportVersion} -> exportVersion) (\s@ExportApi' {} a -> s {exportVersion = a} :: ExportApi)

-- | The name of the API stage to export. If you don\'t specify this
-- property, a representation of the latest API configuration is exported.
exportApi_stageName :: Lens.Lens' ExportApi (Prelude.Maybe Prelude.Text)
exportApi_stageName = Lens.lens (\ExportApi' {stageName} -> stageName) (\s@ExportApi' {} a -> s {stageName = a} :: ExportApi)

-- | Specifies whether to include
-- <https://docs.aws.amazon.com//apigateway/latest/developerguide/api-gateway-swagger-extensions.html API Gateway extensions>
-- in the exported API definition. API Gateway extensions are included by
-- default.
exportApi_includeExtensions :: Lens.Lens' ExportApi (Prelude.Maybe Prelude.Bool)
exportApi_includeExtensions = Lens.lens (\ExportApi' {includeExtensions} -> includeExtensions) (\s@ExportApi' {} a -> s {includeExtensions = a} :: ExportApi)

-- | The version of the API specification to use. OAS30, for OpenAPI 3.0, is
-- the only supported value.
exportApi_specification :: Lens.Lens' ExportApi Prelude.Text
exportApi_specification = Lens.lens (\ExportApi' {specification} -> specification) (\s@ExportApi' {} a -> s {specification = a} :: ExportApi)

-- | The output type of the exported definition file. Valid values are JSON
-- and YAML.
exportApi_outputType :: Lens.Lens' ExportApi Prelude.Text
exportApi_outputType = Lens.lens (\ExportApi' {outputType} -> outputType) (\s@ExportApi' {} a -> s {outputType = a} :: ExportApi)

-- | The API identifier.
exportApi_apiId :: Lens.Lens' ExportApi Prelude.Text
exportApi_apiId = Lens.lens (\ExportApi' {apiId} -> apiId) (\s@ExportApi' {} a -> s {apiId = a} :: ExportApi)

instance Core.AWSRequest ExportApi where
  type AWSResponse ExportApi = ExportApiResponse
  request = Request.get defaultService
  response =
    Response.receiveBytes
      ( \s h x ->
          ExportApiResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportApi where
  hashWithSalt salt' ExportApi' {..} =
    salt' `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` outputType
      `Prelude.hashWithSalt` specification
      `Prelude.hashWithSalt` includeExtensions
      `Prelude.hashWithSalt` stageName
      `Prelude.hashWithSalt` exportVersion

instance Prelude.NFData ExportApi where
  rnf ExportApi' {..} =
    Prelude.rnf exportVersion
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf outputType
      `Prelude.seq` Prelude.rnf specification
      `Prelude.seq` Prelude.rnf includeExtensions
      `Prelude.seq` Prelude.rnf stageName

instance Core.ToHeaders ExportApi where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ExportApi where
  toPath ExportApi' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/exports/",
        Core.toBS specification
      ]

instance Core.ToQuery ExportApi where
  toQuery ExportApi' {..} =
    Prelude.mconcat
      [ "exportVersion" Core.=: exportVersion,
        "stageName" Core.=: stageName,
        "includeExtensions" Core.=: includeExtensions,
        "outputType" Core.=: outputType
      ]

-- | /See:/ 'newExportApiResponse' smart constructor.
data ExportApiResponse = ExportApiResponse'
  { body :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportApiResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'exportApiResponse_body' - Undocumented member.
--
-- 'httpStatus', 'exportApiResponse_httpStatus' - The response's http status code.
newExportApiResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportApiResponse
newExportApiResponse pHttpStatus_ =
  ExportApiResponse'
    { body = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
exportApiResponse_body :: Lens.Lens' ExportApiResponse (Prelude.Maybe Prelude.ByteString)
exportApiResponse_body = Lens.lens (\ExportApiResponse' {body} -> body) (\s@ExportApiResponse' {} a -> s {body = a} :: ExportApiResponse)

-- | The response's http status code.
exportApiResponse_httpStatus :: Lens.Lens' ExportApiResponse Prelude.Int
exportApiResponse_httpStatus = Lens.lens (\ExportApiResponse' {httpStatus} -> httpStatus) (\s@ExportApiResponse' {} a -> s {httpStatus = a} :: ExportApiResponse)

instance Prelude.NFData ExportApiResponse where
  rnf ExportApiResponse' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf httpStatus
