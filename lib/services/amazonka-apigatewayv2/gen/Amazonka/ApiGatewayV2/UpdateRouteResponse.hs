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
-- Module      : Amazonka.ApiGatewayV2.UpdateRouteResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a RouteResponse.
module Amazonka.ApiGatewayV2.UpdateRouteResponse
  ( -- * Creating a Request
    UpdateRouteResponse (..),
    newUpdateRouteResponse,

    -- * Request Lenses
    updateRouteResponse_modelSelectionExpression,
    updateRouteResponse_responseModels,
    updateRouteResponse_routeResponseKey,
    updateRouteResponse_responseParameters,
    updateRouteResponse_routeResponseId,
    updateRouteResponse_apiId,
    updateRouteResponse_routeId,

    -- * Destructuring the Response
    UpdateRouteResponseResponse (..),
    newUpdateRouteResponseResponse,

    -- * Response Lenses
    updateRouteResponseResponse_modelSelectionExpression,
    updateRouteResponseResponse_responseModels,
    updateRouteResponseResponse_routeResponseId,
    updateRouteResponseResponse_routeResponseKey,
    updateRouteResponseResponse_responseParameters,
    updateRouteResponseResponse_httpStatus,
  )
where

import Amazonka.ApiGatewayV2.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates a RouteResponse.
--
-- /See:/ 'newUpdateRouteResponse' smart constructor.
data UpdateRouteResponse = UpdateRouteResponse'
  { -- | The model selection expression for the route response. Supported only
    -- for WebSocket APIs.
    modelSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | The response models for the route response.
    responseModels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The route response key.
    routeResponseKey :: Prelude.Maybe Prelude.Text,
    -- | The route response parameters.
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text ParameterConstraints),
    -- | The route response ID.
    routeResponseId :: Prelude.Text,
    -- | The API identifier.
    apiId :: Prelude.Text,
    -- | The route ID.
    routeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRouteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelSelectionExpression', 'updateRouteResponse_modelSelectionExpression' - The model selection expression for the route response. Supported only
-- for WebSocket APIs.
--
-- 'responseModels', 'updateRouteResponse_responseModels' - The response models for the route response.
--
-- 'routeResponseKey', 'updateRouteResponse_routeResponseKey' - The route response key.
--
-- 'responseParameters', 'updateRouteResponse_responseParameters' - The route response parameters.
--
-- 'routeResponseId', 'updateRouteResponse_routeResponseId' - The route response ID.
--
-- 'apiId', 'updateRouteResponse_apiId' - The API identifier.
--
-- 'routeId', 'updateRouteResponse_routeId' - The route ID.
newUpdateRouteResponse ::
  -- | 'routeResponseId'
  Prelude.Text ->
  -- | 'apiId'
  Prelude.Text ->
  -- | 'routeId'
  Prelude.Text ->
  UpdateRouteResponse
newUpdateRouteResponse
  pRouteResponseId_
  pApiId_
  pRouteId_ =
    UpdateRouteResponse'
      { modelSelectionExpression =
          Prelude.Nothing,
        responseModels = Prelude.Nothing,
        routeResponseKey = Prelude.Nothing,
        responseParameters = Prelude.Nothing,
        routeResponseId = pRouteResponseId_,
        apiId = pApiId_,
        routeId = pRouteId_
      }

-- | The model selection expression for the route response. Supported only
-- for WebSocket APIs.
updateRouteResponse_modelSelectionExpression :: Lens.Lens' UpdateRouteResponse (Prelude.Maybe Prelude.Text)
updateRouteResponse_modelSelectionExpression = Lens.lens (\UpdateRouteResponse' {modelSelectionExpression} -> modelSelectionExpression) (\s@UpdateRouteResponse' {} a -> s {modelSelectionExpression = a} :: UpdateRouteResponse)

-- | The response models for the route response.
updateRouteResponse_responseModels :: Lens.Lens' UpdateRouteResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateRouteResponse_responseModels = Lens.lens (\UpdateRouteResponse' {responseModels} -> responseModels) (\s@UpdateRouteResponse' {} a -> s {responseModels = a} :: UpdateRouteResponse) Prelude.. Lens.mapping Lens.coerced

-- | The route response key.
updateRouteResponse_routeResponseKey :: Lens.Lens' UpdateRouteResponse (Prelude.Maybe Prelude.Text)
updateRouteResponse_routeResponseKey = Lens.lens (\UpdateRouteResponse' {routeResponseKey} -> routeResponseKey) (\s@UpdateRouteResponse' {} a -> s {routeResponseKey = a} :: UpdateRouteResponse)

-- | The route response parameters.
updateRouteResponse_responseParameters :: Lens.Lens' UpdateRouteResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ParameterConstraints))
updateRouteResponse_responseParameters = Lens.lens (\UpdateRouteResponse' {responseParameters} -> responseParameters) (\s@UpdateRouteResponse' {} a -> s {responseParameters = a} :: UpdateRouteResponse) Prelude.. Lens.mapping Lens.coerced

-- | The route response ID.
updateRouteResponse_routeResponseId :: Lens.Lens' UpdateRouteResponse Prelude.Text
updateRouteResponse_routeResponseId = Lens.lens (\UpdateRouteResponse' {routeResponseId} -> routeResponseId) (\s@UpdateRouteResponse' {} a -> s {routeResponseId = a} :: UpdateRouteResponse)

-- | The API identifier.
updateRouteResponse_apiId :: Lens.Lens' UpdateRouteResponse Prelude.Text
updateRouteResponse_apiId = Lens.lens (\UpdateRouteResponse' {apiId} -> apiId) (\s@UpdateRouteResponse' {} a -> s {apiId = a} :: UpdateRouteResponse)

-- | The route ID.
updateRouteResponse_routeId :: Lens.Lens' UpdateRouteResponse Prelude.Text
updateRouteResponse_routeId = Lens.lens (\UpdateRouteResponse' {routeId} -> routeId) (\s@UpdateRouteResponse' {} a -> s {routeId = a} :: UpdateRouteResponse)

instance Core.AWSRequest UpdateRouteResponse where
  type
    AWSResponse UpdateRouteResponse =
      UpdateRouteResponseResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRouteResponseResponse'
            Prelude.<$> (x Core..?> "modelSelectionExpression")
            Prelude.<*> (x Core..?> "responseModels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "routeResponseId")
            Prelude.<*> (x Core..?> "routeResponseKey")
            Prelude.<*> ( x Core..?> "responseParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRouteResponse where
  hashWithSalt salt' UpdateRouteResponse' {..} =
    salt' `Prelude.hashWithSalt` routeId
      `Prelude.hashWithSalt` apiId
      `Prelude.hashWithSalt` routeResponseId
      `Prelude.hashWithSalt` responseParameters
      `Prelude.hashWithSalt` routeResponseKey
      `Prelude.hashWithSalt` responseModels
      `Prelude.hashWithSalt` modelSelectionExpression

instance Prelude.NFData UpdateRouteResponse where
  rnf UpdateRouteResponse' {..} =
    Prelude.rnf modelSelectionExpression
      `Prelude.seq` Prelude.rnf routeId
      `Prelude.seq` Prelude.rnf apiId
      `Prelude.seq` Prelude.rnf routeResponseId
      `Prelude.seq` Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf routeResponseKey
      `Prelude.seq` Prelude.rnf responseModels

instance Core.ToHeaders UpdateRouteResponse where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRouteResponse where
  toJSON UpdateRouteResponse' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("modelSelectionExpression" Core..=)
              Prelude.<$> modelSelectionExpression,
            ("responseModels" Core..=)
              Prelude.<$> responseModels,
            ("routeResponseKey" Core..=)
              Prelude.<$> routeResponseKey,
            ("responseParameters" Core..=)
              Prelude.<$> responseParameters
          ]
      )

instance Core.ToPath UpdateRouteResponse where
  toPath UpdateRouteResponse' {..} =
    Prelude.mconcat
      [ "/v2/apis/",
        Core.toBS apiId,
        "/routes/",
        Core.toBS routeId,
        "/routeresponses/",
        Core.toBS routeResponseId
      ]

instance Core.ToQuery UpdateRouteResponse where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRouteResponseResponse' smart constructor.
data UpdateRouteResponseResponse = UpdateRouteResponseResponse'
  { -- | Represents the model selection expression of a route response. Supported
    -- only for WebSocket APIs.
    modelSelectionExpression :: Prelude.Maybe Prelude.Text,
    -- | Represents the response models of a route response.
    responseModels :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Represents the identifier of a route response.
    routeResponseId :: Prelude.Maybe Prelude.Text,
    -- | Represents the route response key of a route response.
    routeResponseKey :: Prelude.Maybe Prelude.Text,
    -- | Represents the response parameters of a route response.
    responseParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text ParameterConstraints),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRouteResponseResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelSelectionExpression', 'updateRouteResponseResponse_modelSelectionExpression' - Represents the model selection expression of a route response. Supported
-- only for WebSocket APIs.
--
-- 'responseModels', 'updateRouteResponseResponse_responseModels' - Represents the response models of a route response.
--
-- 'routeResponseId', 'updateRouteResponseResponse_routeResponseId' - Represents the identifier of a route response.
--
-- 'routeResponseKey', 'updateRouteResponseResponse_routeResponseKey' - Represents the route response key of a route response.
--
-- 'responseParameters', 'updateRouteResponseResponse_responseParameters' - Represents the response parameters of a route response.
--
-- 'httpStatus', 'updateRouteResponseResponse_httpStatus' - The response's http status code.
newUpdateRouteResponseResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRouteResponseResponse
newUpdateRouteResponseResponse pHttpStatus_ =
  UpdateRouteResponseResponse'
    { modelSelectionExpression =
        Prelude.Nothing,
      responseModels = Prelude.Nothing,
      routeResponseId = Prelude.Nothing,
      routeResponseKey = Prelude.Nothing,
      responseParameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the model selection expression of a route response. Supported
-- only for WebSocket APIs.
updateRouteResponseResponse_modelSelectionExpression :: Lens.Lens' UpdateRouteResponseResponse (Prelude.Maybe Prelude.Text)
updateRouteResponseResponse_modelSelectionExpression = Lens.lens (\UpdateRouteResponseResponse' {modelSelectionExpression} -> modelSelectionExpression) (\s@UpdateRouteResponseResponse' {} a -> s {modelSelectionExpression = a} :: UpdateRouteResponseResponse)

-- | Represents the response models of a route response.
updateRouteResponseResponse_responseModels :: Lens.Lens' UpdateRouteResponseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateRouteResponseResponse_responseModels = Lens.lens (\UpdateRouteResponseResponse' {responseModels} -> responseModels) (\s@UpdateRouteResponseResponse' {} a -> s {responseModels = a} :: UpdateRouteResponseResponse) Prelude.. Lens.mapping Lens.coerced

-- | Represents the identifier of a route response.
updateRouteResponseResponse_routeResponseId :: Lens.Lens' UpdateRouteResponseResponse (Prelude.Maybe Prelude.Text)
updateRouteResponseResponse_routeResponseId = Lens.lens (\UpdateRouteResponseResponse' {routeResponseId} -> routeResponseId) (\s@UpdateRouteResponseResponse' {} a -> s {routeResponseId = a} :: UpdateRouteResponseResponse)

-- | Represents the route response key of a route response.
updateRouteResponseResponse_routeResponseKey :: Lens.Lens' UpdateRouteResponseResponse (Prelude.Maybe Prelude.Text)
updateRouteResponseResponse_routeResponseKey = Lens.lens (\UpdateRouteResponseResponse' {routeResponseKey} -> routeResponseKey) (\s@UpdateRouteResponseResponse' {} a -> s {routeResponseKey = a} :: UpdateRouteResponseResponse)

-- | Represents the response parameters of a route response.
updateRouteResponseResponse_responseParameters :: Lens.Lens' UpdateRouteResponseResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text ParameterConstraints))
updateRouteResponseResponse_responseParameters = Lens.lens (\UpdateRouteResponseResponse' {responseParameters} -> responseParameters) (\s@UpdateRouteResponseResponse' {} a -> s {responseParameters = a} :: UpdateRouteResponseResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateRouteResponseResponse_httpStatus :: Lens.Lens' UpdateRouteResponseResponse Prelude.Int
updateRouteResponseResponse_httpStatus = Lens.lens (\UpdateRouteResponseResponse' {httpStatus} -> httpStatus) (\s@UpdateRouteResponseResponse' {} a -> s {httpStatus = a} :: UpdateRouteResponseResponse)

instance Prelude.NFData UpdateRouteResponseResponse where
  rnf UpdateRouteResponseResponse' {..} =
    Prelude.rnf modelSelectionExpression
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf responseParameters
      `Prelude.seq` Prelude.rnf routeResponseKey
      `Prelude.seq` Prelude.rnf routeResponseId
      `Prelude.seq` Prelude.rnf responseModels
