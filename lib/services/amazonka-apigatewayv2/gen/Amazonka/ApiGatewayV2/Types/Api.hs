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
-- Module      : Amazonka.ApiGatewayV2.Types.Api
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ApiGatewayV2.Types.Api where

import Amazonka.ApiGatewayV2.Types.Cors
import Amazonka.ApiGatewayV2.Types.ProtocolType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents an API.
--
-- /See:/ 'newApi' smart constructor.
data Api = Api'
  { -- | The API ID.
    apiId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether clients can invoke your API by using the default
    -- execute-api endpoint. By default, clients can invoke your API with the
    -- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
    -- To require that clients use a custom domain name to invoke your API,
    -- disable the default endpoint.
    disableExecuteApiEndpoint :: Prelude.Maybe Prelude.Bool,
    -- | The URI of the API, of the form
    -- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
    -- appended to this URI to form a complete path to a deployed API stage.
    apiEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The warning messages reported when failonwarnings is turned on during
    -- API import.
    warnings :: Prelude.Maybe [Prelude.Text],
    -- | The timestamp when the API was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | A version identifier for the API.
    version :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether an API is managed by API Gateway. You can\'t update or
    -- delete a managed API by using API Gateway. A managed API can be deleted
    -- only through the tooling or service that created it.
    apiGatewayManaged :: Prelude.Maybe Prelude.Bool,
    -- | An API key selection expression. Supported only for WebSocket APIs. See
    -- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
    apiKeySelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | A CORS configuration. Supported only for HTTP APIs.
    corsConfiguration :: Prelude.Maybe Cors,
    -- | The validation information during API import. This may include
    -- particular properties of your OpenAPI definition which are ignored
    -- during import. Supported only for HTTP APIs.
    importInfo :: Prelude.Maybe [Prelude.Text],
    -- | Avoid validating models when creating a deployment. Supported only for
    -- WebSocket APIs.
    disableSchemaValidation :: Prelude.Maybe Prelude.Bool,
    -- | The description of the API.
    description :: Prelude.Maybe Prelude.Text,
    -- | A collection of tags associated with the API.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The route selection expression for the API. For HTTP APIs, the
    -- routeSelectionExpression must be ${request.method} ${request.path}. If
    -- not provided, this will be the default for HTTP APIs. This property is
    -- required for WebSocket APIs.
    routeSelectionExpression :: Prelude.Text,
    -- | The name of the API.
    name :: Prelude.Text,
    -- | The API protocol.
    protocolType :: ProtocolType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Api' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiId', 'api_apiId' - The API ID.
--
-- 'disableExecuteApiEndpoint', 'api_disableExecuteApiEndpoint' - Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
--
-- 'apiEndpoint', 'api_apiEndpoint' - The URI of the API, of the form
-- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
-- appended to this URI to form a complete path to a deployed API stage.
--
-- 'warnings', 'api_warnings' - The warning messages reported when failonwarnings is turned on during
-- API import.
--
-- 'createdDate', 'api_createdDate' - The timestamp when the API was created.
--
-- 'version', 'api_version' - A version identifier for the API.
--
-- 'apiGatewayManaged', 'api_apiGatewayManaged' - Specifies whether an API is managed by API Gateway. You can\'t update or
-- delete a managed API by using API Gateway. A managed API can be deleted
-- only through the tooling or service that created it.
--
-- 'apiKeySelectionExpression', 'api_apiKeySelectionExpression' - An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
--
-- 'corsConfiguration', 'api_corsConfiguration' - A CORS configuration. Supported only for HTTP APIs.
--
-- 'importInfo', 'api_importInfo' - The validation information during API import. This may include
-- particular properties of your OpenAPI definition which are ignored
-- during import. Supported only for HTTP APIs.
--
-- 'disableSchemaValidation', 'api_disableSchemaValidation' - Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
--
-- 'description', 'api_description' - The description of the API.
--
-- 'tags', 'api_tags' - A collection of tags associated with the API.
--
-- 'routeSelectionExpression', 'api_routeSelectionExpression' - The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
--
-- 'name', 'api_name' - The name of the API.
--
-- 'protocolType', 'api_protocolType' - The API protocol.
newApi ::
  -- | 'routeSelectionExpression'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'protocolType'
  ProtocolType ->
  Api
newApi
  pRouteSelectionExpression_
  pName_
  pProtocolType_ =
    Api'
      { apiId = Prelude.Nothing,
        disableExecuteApiEndpoint = Prelude.Nothing,
        apiEndpoint = Prelude.Nothing,
        warnings = Prelude.Nothing,
        createdDate = Prelude.Nothing,
        version = Prelude.Nothing,
        apiGatewayManaged = Prelude.Nothing,
        apiKeySelectionExpression = Prelude.Nothing,
        corsConfiguration = Prelude.Nothing,
        importInfo = Prelude.Nothing,
        disableSchemaValidation = Prelude.Nothing,
        description = Prelude.Nothing,
        tags = Prelude.Nothing,
        routeSelectionExpression =
          pRouteSelectionExpression_,
        name = pName_,
        protocolType = pProtocolType_
      }

-- | The API ID.
api_apiId :: Lens.Lens' Api (Prelude.Maybe Prelude.Text)
api_apiId = Lens.lens (\Api' {apiId} -> apiId) (\s@Api' {} a -> s {apiId = a} :: Api)

-- | Specifies whether clients can invoke your API by using the default
-- execute-api endpoint. By default, clients can invoke your API with the
-- default https:\/\/{api_id}.execute-api.{region}.amazonaws.com endpoint.
-- To require that clients use a custom domain name to invoke your API,
-- disable the default endpoint.
api_disableExecuteApiEndpoint :: Lens.Lens' Api (Prelude.Maybe Prelude.Bool)
api_disableExecuteApiEndpoint = Lens.lens (\Api' {disableExecuteApiEndpoint} -> disableExecuteApiEndpoint) (\s@Api' {} a -> s {disableExecuteApiEndpoint = a} :: Api)

-- | The URI of the API, of the form
-- {api-id}.execute-api.{region}.amazonaws.com. The stage name is typically
-- appended to this URI to form a complete path to a deployed API stage.
api_apiEndpoint :: Lens.Lens' Api (Prelude.Maybe Prelude.Text)
api_apiEndpoint = Lens.lens (\Api' {apiEndpoint} -> apiEndpoint) (\s@Api' {} a -> s {apiEndpoint = a} :: Api)

-- | The warning messages reported when failonwarnings is turned on during
-- API import.
api_warnings :: Lens.Lens' Api (Prelude.Maybe [Prelude.Text])
api_warnings = Lens.lens (\Api' {warnings} -> warnings) (\s@Api' {} a -> s {warnings = a} :: Api) Prelude.. Lens.mapping Lens.coerced

-- | The timestamp when the API was created.
api_createdDate :: Lens.Lens' Api (Prelude.Maybe Prelude.UTCTime)
api_createdDate = Lens.lens (\Api' {createdDate} -> createdDate) (\s@Api' {} a -> s {createdDate = a} :: Api) Prelude.. Lens.mapping Core._Time

-- | A version identifier for the API.
api_version :: Lens.Lens' Api (Prelude.Maybe Prelude.Text)
api_version = Lens.lens (\Api' {version} -> version) (\s@Api' {} a -> s {version = a} :: Api)

-- | Specifies whether an API is managed by API Gateway. You can\'t update or
-- delete a managed API by using API Gateway. A managed API can be deleted
-- only through the tooling or service that created it.
api_apiGatewayManaged :: Lens.Lens' Api (Prelude.Maybe Prelude.Bool)
api_apiGatewayManaged = Lens.lens (\Api' {apiGatewayManaged} -> apiGatewayManaged) (\s@Api' {} a -> s {apiGatewayManaged = a} :: Api)

-- | An API key selection expression. Supported only for WebSocket APIs. See
-- <https://docs.aws.amazon.com/apigateway/latest/developerguide/apigateway-websocket-api-selection-expressions.html#apigateway-websocket-api-apikey-selection-expressions API Key Selection Expressions>.
api_apiKeySelectionExpression :: Lens.Lens' Api (Prelude.Maybe Prelude.Text)
api_apiKeySelectionExpression = Lens.lens (\Api' {apiKeySelectionExpression} -> apiKeySelectionExpression) (\s@Api' {} a -> s {apiKeySelectionExpression = a} :: Api)

-- | A CORS configuration. Supported only for HTTP APIs.
api_corsConfiguration :: Lens.Lens' Api (Prelude.Maybe Cors)
api_corsConfiguration = Lens.lens (\Api' {corsConfiguration} -> corsConfiguration) (\s@Api' {} a -> s {corsConfiguration = a} :: Api)

-- | The validation information during API import. This may include
-- particular properties of your OpenAPI definition which are ignored
-- during import. Supported only for HTTP APIs.
api_importInfo :: Lens.Lens' Api (Prelude.Maybe [Prelude.Text])
api_importInfo = Lens.lens (\Api' {importInfo} -> importInfo) (\s@Api' {} a -> s {importInfo = a} :: Api) Prelude.. Lens.mapping Lens.coerced

-- | Avoid validating models when creating a deployment. Supported only for
-- WebSocket APIs.
api_disableSchemaValidation :: Lens.Lens' Api (Prelude.Maybe Prelude.Bool)
api_disableSchemaValidation = Lens.lens (\Api' {disableSchemaValidation} -> disableSchemaValidation) (\s@Api' {} a -> s {disableSchemaValidation = a} :: Api)

-- | The description of the API.
api_description :: Lens.Lens' Api (Prelude.Maybe Prelude.Text)
api_description = Lens.lens (\Api' {description} -> description) (\s@Api' {} a -> s {description = a} :: Api)

-- | A collection of tags associated with the API.
api_tags :: Lens.Lens' Api (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
api_tags = Lens.lens (\Api' {tags} -> tags) (\s@Api' {} a -> s {tags = a} :: Api) Prelude.. Lens.mapping Lens.coerced

-- | The route selection expression for the API. For HTTP APIs, the
-- routeSelectionExpression must be ${request.method} ${request.path}. If
-- not provided, this will be the default for HTTP APIs. This property is
-- required for WebSocket APIs.
api_routeSelectionExpression :: Lens.Lens' Api Prelude.Text
api_routeSelectionExpression = Lens.lens (\Api' {routeSelectionExpression} -> routeSelectionExpression) (\s@Api' {} a -> s {routeSelectionExpression = a} :: Api)

-- | The name of the API.
api_name :: Lens.Lens' Api Prelude.Text
api_name = Lens.lens (\Api' {name} -> name) (\s@Api' {} a -> s {name = a} :: Api)

-- | The API protocol.
api_protocolType :: Lens.Lens' Api ProtocolType
api_protocolType = Lens.lens (\Api' {protocolType} -> protocolType) (\s@Api' {} a -> s {protocolType = a} :: Api)

instance Core.FromJSON Api where
  parseJSON =
    Core.withObject
      "Api"
      ( \x ->
          Api'
            Prelude.<$> (x Core..:? "apiId")
            Prelude.<*> (x Core..:? "disableExecuteApiEndpoint")
            Prelude.<*> (x Core..:? "apiEndpoint")
            Prelude.<*> (x Core..:? "warnings" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "apiGatewayManaged")
            Prelude.<*> (x Core..:? "apiKeySelectionExpression")
            Prelude.<*> (x Core..:? "corsConfiguration")
            Prelude.<*> (x Core..:? "importInfo" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "disableSchemaValidation")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "routeSelectionExpression")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "protocolType")
      )

instance Prelude.Hashable Api where
  hashWithSalt salt' Api' {..} =
    salt' `Prelude.hashWithSalt` protocolType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` routeSelectionExpression
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` disableSchemaValidation
      `Prelude.hashWithSalt` importInfo
      `Prelude.hashWithSalt` corsConfiguration
      `Prelude.hashWithSalt` apiKeySelectionExpression
      `Prelude.hashWithSalt` apiGatewayManaged
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` warnings
      `Prelude.hashWithSalt` apiEndpoint
      `Prelude.hashWithSalt` disableExecuteApiEndpoint
      `Prelude.hashWithSalt` apiId

instance Prelude.NFData Api where
  rnf Api' {..} =
    Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf protocolType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf routeSelectionExpression
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf disableSchemaValidation
      `Prelude.seq` Prelude.rnf importInfo
      `Prelude.seq` Prelude.rnf corsConfiguration
      `Prelude.seq` Prelude.rnf apiKeySelectionExpression
      `Prelude.seq` Prelude.rnf apiGatewayManaged
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf warnings
      `Prelude.seq` Prelude.rnf apiEndpoint
      `Prelude.seq` Prelude.rnf disableExecuteApiEndpoint
