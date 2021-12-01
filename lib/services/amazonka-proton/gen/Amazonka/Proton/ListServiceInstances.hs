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
-- Module      : Amazonka.Proton.ListServiceInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List service instances with summaries of detail data.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListServiceInstances
  ( -- * Creating a Request
    ListServiceInstances (..),
    newListServiceInstances,

    -- * Request Lenses
    listServiceInstances_nextToken,
    listServiceInstances_serviceName,
    listServiceInstances_maxResults,

    -- * Destructuring the Response
    ListServiceInstancesResponse (..),
    newListServiceInstancesResponse,

    -- * Response Lenses
    listServiceInstancesResponse_nextToken,
    listServiceInstancesResponse_httpStatus,
    listServiceInstancesResponse_serviceInstances,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListServiceInstances' smart constructor.
data ListServiceInstances = ListServiceInstances'
  { -- | A token to indicate the location of the next service in the array of
    -- service instances, after the list of service instances that was
    -- previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the service that the service instance belongs to.
    serviceName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of service instances to list.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceInstances_nextToken' - A token to indicate the location of the next service in the array of
-- service instances, after the list of service instances that was
-- previously requested.
--
-- 'serviceName', 'listServiceInstances_serviceName' - The name of the service that the service instance belongs to.
--
-- 'maxResults', 'listServiceInstances_maxResults' - The maximum number of service instances to list.
newListServiceInstances ::
  ListServiceInstances
newListServiceInstances =
  ListServiceInstances'
    { nextToken = Prelude.Nothing,
      serviceName = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A token to indicate the location of the next service in the array of
-- service instances, after the list of service instances that was
-- previously requested.
listServiceInstances_nextToken :: Lens.Lens' ListServiceInstances (Prelude.Maybe Prelude.Text)
listServiceInstances_nextToken = Lens.lens (\ListServiceInstances' {nextToken} -> nextToken) (\s@ListServiceInstances' {} a -> s {nextToken = a} :: ListServiceInstances)

-- | The name of the service that the service instance belongs to.
listServiceInstances_serviceName :: Lens.Lens' ListServiceInstances (Prelude.Maybe Prelude.Text)
listServiceInstances_serviceName = Lens.lens (\ListServiceInstances' {serviceName} -> serviceName) (\s@ListServiceInstances' {} a -> s {serviceName = a} :: ListServiceInstances)

-- | The maximum number of service instances to list.
listServiceInstances_maxResults :: Lens.Lens' ListServiceInstances (Prelude.Maybe Prelude.Natural)
listServiceInstances_maxResults = Lens.lens (\ListServiceInstances' {maxResults} -> maxResults) (\s@ListServiceInstances' {} a -> s {maxResults = a} :: ListServiceInstances)

instance Core.AWSPager ListServiceInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServiceInstancesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listServiceInstancesResponse_serviceInstances
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listServiceInstances_nextToken
          Lens..~ rs
          Lens.^? listServiceInstancesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListServiceInstances where
  type
    AWSResponse ListServiceInstances =
      ListServiceInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceInstancesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "serviceInstances"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListServiceInstances where
  hashWithSalt salt' ListServiceInstances' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` serviceName
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListServiceInstances where
  rnf ListServiceInstances' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf serviceName

instance Core.ToHeaders ListServiceInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.ListServiceInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListServiceInstances where
  toJSON ListServiceInstances' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("serviceName" Core..=) Prelude.<$> serviceName,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListServiceInstances where
  toPath = Prelude.const "/"

instance Core.ToQuery ListServiceInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServiceInstancesResponse' smart constructor.
data ListServiceInstancesResponse = ListServiceInstancesResponse'
  { -- | A token to indicate the location of the next service instance in the
    -- array of service instances, after the current requested list of service
    -- instances.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of service instances with summaries of detail data.
    serviceInstances :: [ServiceInstanceSummary]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceInstancesResponse_nextToken' - A token to indicate the location of the next service instance in the
-- array of service instances, after the current requested list of service
-- instances.
--
-- 'httpStatus', 'listServiceInstancesResponse_httpStatus' - The response's http status code.
--
-- 'serviceInstances', 'listServiceInstancesResponse_serviceInstances' - An array of service instances with summaries of detail data.
newListServiceInstancesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceInstancesResponse
newListServiceInstancesResponse pHttpStatus_ =
  ListServiceInstancesResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      serviceInstances = Prelude.mempty
    }

-- | A token to indicate the location of the next service instance in the
-- array of service instances, after the current requested list of service
-- instances.
listServiceInstancesResponse_nextToken :: Lens.Lens' ListServiceInstancesResponse (Prelude.Maybe Prelude.Text)
listServiceInstancesResponse_nextToken = Lens.lens (\ListServiceInstancesResponse' {nextToken} -> nextToken) (\s@ListServiceInstancesResponse' {} a -> s {nextToken = a} :: ListServiceInstancesResponse)

-- | The response's http status code.
listServiceInstancesResponse_httpStatus :: Lens.Lens' ListServiceInstancesResponse Prelude.Int
listServiceInstancesResponse_httpStatus = Lens.lens (\ListServiceInstancesResponse' {httpStatus} -> httpStatus) (\s@ListServiceInstancesResponse' {} a -> s {httpStatus = a} :: ListServiceInstancesResponse)

-- | An array of service instances with summaries of detail data.
listServiceInstancesResponse_serviceInstances :: Lens.Lens' ListServiceInstancesResponse [ServiceInstanceSummary]
listServiceInstancesResponse_serviceInstances = Lens.lens (\ListServiceInstancesResponse' {serviceInstances} -> serviceInstances) (\s@ListServiceInstancesResponse' {} a -> s {serviceInstances = a} :: ListServiceInstancesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListServiceInstancesResponse where
  rnf ListServiceInstancesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceInstances
      `Prelude.seq` Prelude.rnf httpStatus
