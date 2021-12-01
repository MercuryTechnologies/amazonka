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
-- Module      : Amazonka.GreengrassV2.ListCoreDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a paginated list of Greengrass core devices.
--
-- This operation returns paginated results.
module Amazonka.GreengrassV2.ListCoreDevices
  ( -- * Creating a Request
    ListCoreDevices (..),
    newListCoreDevices,

    -- * Request Lenses
    listCoreDevices_status,
    listCoreDevices_thingGroupArn,
    listCoreDevices_nextToken,
    listCoreDevices_maxResults,

    -- * Destructuring the Response
    ListCoreDevicesResponse (..),
    newListCoreDevicesResponse,

    -- * Response Lenses
    listCoreDevicesResponse_nextToken,
    listCoreDevicesResponse_coreDevices,
    listCoreDevicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.GreengrassV2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCoreDevices' smart constructor.
data ListCoreDevices = ListCoreDevices'
  { -- | The core device status by which to filter. If you specify this
    -- parameter, the list includes only core devices that have this status.
    -- Choose one of the following options:
    --
    -- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
    --     on the core device without issue.
    --
    -- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
    --     a failed state on the core device.
    status :: Prelude.Maybe CoreDeviceStatus,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the IoT thing group by which to filter. If you specify this
    -- parameter, the list includes only core devices that are members of this
    -- thing group.
    thingGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The token to be used for the next set of paginated results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to be returned per paginated request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoreDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listCoreDevices_status' - The core device status by which to filter. If you specify this
-- parameter, the list includes only core devices that have this status.
-- Choose one of the following options:
--
-- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
--     on the core device without issue.
--
-- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
--     a failed state on the core device.
--
-- 'thingGroupArn', 'listCoreDevices_thingGroupArn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT thing group by which to filter. If you specify this
-- parameter, the list includes only core devices that are members of this
-- thing group.
--
-- 'nextToken', 'listCoreDevices_nextToken' - The token to be used for the next set of paginated results.
--
-- 'maxResults', 'listCoreDevices_maxResults' - The maximum number of results to be returned per paginated request.
newListCoreDevices ::
  ListCoreDevices
newListCoreDevices =
  ListCoreDevices'
    { status = Prelude.Nothing,
      thingGroupArn = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The core device status by which to filter. If you specify this
-- parameter, the list includes only core devices that have this status.
-- Choose one of the following options:
--
-- -   @HEALTHY@ – The IoT Greengrass Core software and all components run
--     on the core device without issue.
--
-- -   @UNHEALTHY@ – The IoT Greengrass Core software or a component is in
--     a failed state on the core device.
listCoreDevices_status :: Lens.Lens' ListCoreDevices (Prelude.Maybe CoreDeviceStatus)
listCoreDevices_status = Lens.lens (\ListCoreDevices' {status} -> status) (\s@ListCoreDevices' {} a -> s {status = a} :: ListCoreDevices)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the IoT thing group by which to filter. If you specify this
-- parameter, the list includes only core devices that are members of this
-- thing group.
listCoreDevices_thingGroupArn :: Lens.Lens' ListCoreDevices (Prelude.Maybe Prelude.Text)
listCoreDevices_thingGroupArn = Lens.lens (\ListCoreDevices' {thingGroupArn} -> thingGroupArn) (\s@ListCoreDevices' {} a -> s {thingGroupArn = a} :: ListCoreDevices)

-- | The token to be used for the next set of paginated results.
listCoreDevices_nextToken :: Lens.Lens' ListCoreDevices (Prelude.Maybe Prelude.Text)
listCoreDevices_nextToken = Lens.lens (\ListCoreDevices' {nextToken} -> nextToken) (\s@ListCoreDevices' {} a -> s {nextToken = a} :: ListCoreDevices)

-- | The maximum number of results to be returned per paginated request.
listCoreDevices_maxResults :: Lens.Lens' ListCoreDevices (Prelude.Maybe Prelude.Natural)
listCoreDevices_maxResults = Lens.lens (\ListCoreDevices' {maxResults} -> maxResults) (\s@ListCoreDevices' {} a -> s {maxResults = a} :: ListCoreDevices)

instance Core.AWSPager ListCoreDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCoreDevicesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCoreDevicesResponse_coreDevices
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCoreDevices_nextToken
          Lens..~ rs
          Lens.^? listCoreDevicesResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCoreDevices where
  type
    AWSResponse ListCoreDevices =
      ListCoreDevicesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCoreDevicesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "coreDevices" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListCoreDevices where
  hashWithSalt salt' ListCoreDevices' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` thingGroupArn
      `Prelude.hashWithSalt` status

instance Prelude.NFData ListCoreDevices where
  rnf ListCoreDevices' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf thingGroupArn

instance Core.ToHeaders ListCoreDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListCoreDevices where
  toPath = Prelude.const "/greengrass/v2/coreDevices"

instance Core.ToQuery ListCoreDevices where
  toQuery ListCoreDevices' {..} =
    Prelude.mconcat
      [ "status" Core.=: status,
        "thingGroupArn" Core.=: thingGroupArn,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListCoreDevicesResponse' smart constructor.
data ListCoreDevicesResponse = ListCoreDevicesResponse'
  { -- | The token for the next set of results, or null if there are no
    -- additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list that summarizes each core device.
    coreDevices :: Prelude.Maybe [CoreDevice],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoreDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listCoreDevicesResponse_nextToken' - The token for the next set of results, or null if there are no
-- additional results.
--
-- 'coreDevices', 'listCoreDevicesResponse_coreDevices' - A list that summarizes each core device.
--
-- 'httpStatus', 'listCoreDevicesResponse_httpStatus' - The response's http status code.
newListCoreDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListCoreDevicesResponse
newListCoreDevicesResponse pHttpStatus_ =
  ListCoreDevicesResponse'
    { nextToken =
        Prelude.Nothing,
      coreDevices = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no
-- additional results.
listCoreDevicesResponse_nextToken :: Lens.Lens' ListCoreDevicesResponse (Prelude.Maybe Prelude.Text)
listCoreDevicesResponse_nextToken = Lens.lens (\ListCoreDevicesResponse' {nextToken} -> nextToken) (\s@ListCoreDevicesResponse' {} a -> s {nextToken = a} :: ListCoreDevicesResponse)

-- | A list that summarizes each core device.
listCoreDevicesResponse_coreDevices :: Lens.Lens' ListCoreDevicesResponse (Prelude.Maybe [CoreDevice])
listCoreDevicesResponse_coreDevices = Lens.lens (\ListCoreDevicesResponse' {coreDevices} -> coreDevices) (\s@ListCoreDevicesResponse' {} a -> s {coreDevices = a} :: ListCoreDevicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listCoreDevicesResponse_httpStatus :: Lens.Lens' ListCoreDevicesResponse Prelude.Int
listCoreDevicesResponse_httpStatus = Lens.lens (\ListCoreDevicesResponse' {httpStatus} -> httpStatus) (\s@ListCoreDevicesResponse' {} a -> s {httpStatus = a} :: ListCoreDevicesResponse)

instance Prelude.NFData ListCoreDevicesResponse where
  rnf ListCoreDevicesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf coreDevices
