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
-- Module      : Amazonka.IoTWireless.GetServiceEndpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the account-specific endpoint for Configuration and Update Server
-- (CUPS) protocol or LoRaWAN Network Server (LNS) connections.
module Amazonka.IoTWireless.GetServiceEndpoint
  ( -- * Creating a Request
    GetServiceEndpoint (..),
    newGetServiceEndpoint,

    -- * Request Lenses
    getServiceEndpoint_serviceType,

    -- * Destructuring the Response
    GetServiceEndpointResponse (..),
    newGetServiceEndpointResponse,

    -- * Response Lenses
    getServiceEndpointResponse_serviceEndpoint,
    getServiceEndpointResponse_serverTrust,
    getServiceEndpointResponse_serviceType,
    getServiceEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServiceEndpoint' smart constructor.
data GetServiceEndpoint = GetServiceEndpoint'
  { -- | The service type for which to get endpoint information about. Can be
    -- @CUPS@ for the Configuration and Update Server endpoint, or @LNS@ for
    -- the LoRaWAN Network Server endpoint.
    serviceType :: Prelude.Maybe WirelessGatewayServiceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceType', 'getServiceEndpoint_serviceType' - The service type for which to get endpoint information about. Can be
-- @CUPS@ for the Configuration and Update Server endpoint, or @LNS@ for
-- the LoRaWAN Network Server endpoint.
newGetServiceEndpoint ::
  GetServiceEndpoint
newGetServiceEndpoint =
  GetServiceEndpoint' {serviceType = Prelude.Nothing}

-- | The service type for which to get endpoint information about. Can be
-- @CUPS@ for the Configuration and Update Server endpoint, or @LNS@ for
-- the LoRaWAN Network Server endpoint.
getServiceEndpoint_serviceType :: Lens.Lens' GetServiceEndpoint (Prelude.Maybe WirelessGatewayServiceType)
getServiceEndpoint_serviceType = Lens.lens (\GetServiceEndpoint' {serviceType} -> serviceType) (\s@GetServiceEndpoint' {} a -> s {serviceType = a} :: GetServiceEndpoint)

instance Core.AWSRequest GetServiceEndpoint where
  type
    AWSResponse GetServiceEndpoint =
      GetServiceEndpointResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServiceEndpointResponse'
            Prelude.<$> (x Core..?> "ServiceEndpoint")
            Prelude.<*> (x Core..?> "ServerTrust")
            Prelude.<*> (x Core..?> "ServiceType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServiceEndpoint where
  hashWithSalt salt' GetServiceEndpoint' {..} =
    salt' `Prelude.hashWithSalt` serviceType

instance Prelude.NFData GetServiceEndpoint where
  rnf GetServiceEndpoint' {..} = Prelude.rnf serviceType

instance Core.ToHeaders GetServiceEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetServiceEndpoint where
  toPath = Prelude.const "/service-endpoint"

instance Core.ToQuery GetServiceEndpoint where
  toQuery GetServiceEndpoint' {..} =
    Prelude.mconcat ["serviceType" Core.=: serviceType]

-- | /See:/ 'newGetServiceEndpointResponse' smart constructor.
data GetServiceEndpointResponse = GetServiceEndpointResponse'
  { -- | The service endpoint value.
    serviceEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The Root CA of the server trust certificate.
    serverTrust :: Prelude.Maybe Prelude.Text,
    -- | The endpoint\'s service type.
    serviceType :: Prelude.Maybe WirelessGatewayServiceType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServiceEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceEndpoint', 'getServiceEndpointResponse_serviceEndpoint' - The service endpoint value.
--
-- 'serverTrust', 'getServiceEndpointResponse_serverTrust' - The Root CA of the server trust certificate.
--
-- 'serviceType', 'getServiceEndpointResponse_serviceType' - The endpoint\'s service type.
--
-- 'httpStatus', 'getServiceEndpointResponse_httpStatus' - The response's http status code.
newGetServiceEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServiceEndpointResponse
newGetServiceEndpointResponse pHttpStatus_ =
  GetServiceEndpointResponse'
    { serviceEndpoint =
        Prelude.Nothing,
      serverTrust = Prelude.Nothing,
      serviceType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The service endpoint value.
getServiceEndpointResponse_serviceEndpoint :: Lens.Lens' GetServiceEndpointResponse (Prelude.Maybe Prelude.Text)
getServiceEndpointResponse_serviceEndpoint = Lens.lens (\GetServiceEndpointResponse' {serviceEndpoint} -> serviceEndpoint) (\s@GetServiceEndpointResponse' {} a -> s {serviceEndpoint = a} :: GetServiceEndpointResponse)

-- | The Root CA of the server trust certificate.
getServiceEndpointResponse_serverTrust :: Lens.Lens' GetServiceEndpointResponse (Prelude.Maybe Prelude.Text)
getServiceEndpointResponse_serverTrust = Lens.lens (\GetServiceEndpointResponse' {serverTrust} -> serverTrust) (\s@GetServiceEndpointResponse' {} a -> s {serverTrust = a} :: GetServiceEndpointResponse)

-- | The endpoint\'s service type.
getServiceEndpointResponse_serviceType :: Lens.Lens' GetServiceEndpointResponse (Prelude.Maybe WirelessGatewayServiceType)
getServiceEndpointResponse_serviceType = Lens.lens (\GetServiceEndpointResponse' {serviceType} -> serviceType) (\s@GetServiceEndpointResponse' {} a -> s {serviceType = a} :: GetServiceEndpointResponse)

-- | The response's http status code.
getServiceEndpointResponse_httpStatus :: Lens.Lens' GetServiceEndpointResponse Prelude.Int
getServiceEndpointResponse_httpStatus = Lens.lens (\GetServiceEndpointResponse' {httpStatus} -> httpStatus) (\s@GetServiceEndpointResponse' {} a -> s {httpStatus = a} :: GetServiceEndpointResponse)

instance Prelude.NFData GetServiceEndpointResponse where
  rnf GetServiceEndpointResponse' {..} =
    Prelude.rnf serviceEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serviceType
      `Prelude.seq` Prelude.rnf serverTrust
