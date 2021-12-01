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
-- Module      : Amazonka.IoTSiteWise.GetAssetPropertyValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an asset property\'s current value. For more information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/query-industrial-data.html#current-values Querying current values>
-- in the /IoT SiteWise User Guide/.
--
-- To identify an asset property, you must specify one of the following:
--
-- -   The @assetId@ and @propertyId@ of an asset property.
--
-- -   A @propertyAlias@, which is a data stream alias (for example,
--     @\/company\/windfarm\/3\/turbine\/7\/temperature@). To define an
--     asset property\'s alias, see
--     <https://docs.aws.amazon.com/iot-sitewise/latest/APIReference/API_UpdateAssetProperty.html UpdateAssetProperty>.
module Amazonka.IoTSiteWise.GetAssetPropertyValue
  ( -- * Creating a Request
    GetAssetPropertyValue (..),
    newGetAssetPropertyValue,

    -- * Request Lenses
    getAssetPropertyValue_propertyAlias,
    getAssetPropertyValue_propertyId,
    getAssetPropertyValue_assetId,

    -- * Destructuring the Response
    GetAssetPropertyValueResponse (..),
    newGetAssetPropertyValueResponse,

    -- * Response Lenses
    getAssetPropertyValueResponse_propertyValue,
    getAssetPropertyValueResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAssetPropertyValue' smart constructor.
data GetAssetPropertyValue = GetAssetPropertyValue'
  { -- | The alias that identifies the property, such as an OPC-UA server data
    -- stream path (for example,
    -- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
    -- in the /IoT SiteWise User Guide/.
    propertyAlias :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset property.
    propertyId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset.
    assetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssetPropertyValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyAlias', 'getAssetPropertyValue_propertyAlias' - The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
--
-- 'propertyId', 'getAssetPropertyValue_propertyId' - The ID of the asset property.
--
-- 'assetId', 'getAssetPropertyValue_assetId' - The ID of the asset.
newGetAssetPropertyValue ::
  GetAssetPropertyValue
newGetAssetPropertyValue =
  GetAssetPropertyValue'
    { propertyAlias =
        Prelude.Nothing,
      propertyId = Prelude.Nothing,
      assetId = Prelude.Nothing
    }

-- | The alias that identifies the property, such as an OPC-UA server data
-- stream path (for example,
-- @\/company\/windfarm\/3\/turbine\/7\/temperature@). For more
-- information, see
-- <https://docs.aws.amazon.com/iot-sitewise/latest/userguide/connect-data-streams.html Mapping industrial data streams to asset properties>
-- in the /IoT SiteWise User Guide/.
getAssetPropertyValue_propertyAlias :: Lens.Lens' GetAssetPropertyValue (Prelude.Maybe Prelude.Text)
getAssetPropertyValue_propertyAlias = Lens.lens (\GetAssetPropertyValue' {propertyAlias} -> propertyAlias) (\s@GetAssetPropertyValue' {} a -> s {propertyAlias = a} :: GetAssetPropertyValue)

-- | The ID of the asset property.
getAssetPropertyValue_propertyId :: Lens.Lens' GetAssetPropertyValue (Prelude.Maybe Prelude.Text)
getAssetPropertyValue_propertyId = Lens.lens (\GetAssetPropertyValue' {propertyId} -> propertyId) (\s@GetAssetPropertyValue' {} a -> s {propertyId = a} :: GetAssetPropertyValue)

-- | The ID of the asset.
getAssetPropertyValue_assetId :: Lens.Lens' GetAssetPropertyValue (Prelude.Maybe Prelude.Text)
getAssetPropertyValue_assetId = Lens.lens (\GetAssetPropertyValue' {assetId} -> assetId) (\s@GetAssetPropertyValue' {} a -> s {assetId = a} :: GetAssetPropertyValue)

instance Core.AWSRequest GetAssetPropertyValue where
  type
    AWSResponse GetAssetPropertyValue =
      GetAssetPropertyValueResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAssetPropertyValueResponse'
            Prelude.<$> (x Core..?> "propertyValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAssetPropertyValue where
  hashWithSalt salt' GetAssetPropertyValue' {..} =
    salt' `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` propertyId
      `Prelude.hashWithSalt` propertyAlias

instance Prelude.NFData GetAssetPropertyValue where
  rnf GetAssetPropertyValue' {..} =
    Prelude.rnf propertyAlias
      `Prelude.seq` Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf propertyId

instance Core.ToHeaders GetAssetPropertyValue where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetAssetPropertyValue where
  toPath = Prelude.const "/properties/latest"

instance Core.ToQuery GetAssetPropertyValue where
  toQuery GetAssetPropertyValue' {..} =
    Prelude.mconcat
      [ "propertyAlias" Core.=: propertyAlias,
        "propertyId" Core.=: propertyId,
        "assetId" Core.=: assetId
      ]

-- | /See:/ 'newGetAssetPropertyValueResponse' smart constructor.
data GetAssetPropertyValueResponse = GetAssetPropertyValueResponse'
  { -- | The current asset property value.
    propertyValue :: Prelude.Maybe AssetPropertyValue,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAssetPropertyValueResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyValue', 'getAssetPropertyValueResponse_propertyValue' - The current asset property value.
--
-- 'httpStatus', 'getAssetPropertyValueResponse_httpStatus' - The response's http status code.
newGetAssetPropertyValueResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAssetPropertyValueResponse
newGetAssetPropertyValueResponse pHttpStatus_ =
  GetAssetPropertyValueResponse'
    { propertyValue =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current asset property value.
getAssetPropertyValueResponse_propertyValue :: Lens.Lens' GetAssetPropertyValueResponse (Prelude.Maybe AssetPropertyValue)
getAssetPropertyValueResponse_propertyValue = Lens.lens (\GetAssetPropertyValueResponse' {propertyValue} -> propertyValue) (\s@GetAssetPropertyValueResponse' {} a -> s {propertyValue = a} :: GetAssetPropertyValueResponse)

-- | The response's http status code.
getAssetPropertyValueResponse_httpStatus :: Lens.Lens' GetAssetPropertyValueResponse Prelude.Int
getAssetPropertyValueResponse_httpStatus = Lens.lens (\GetAssetPropertyValueResponse' {httpStatus} -> httpStatus) (\s@GetAssetPropertyValueResponse' {} a -> s {httpStatus = a} :: GetAssetPropertyValueResponse)

instance Prelude.NFData GetAssetPropertyValueResponse where
  rnf GetAssetPropertyValueResponse' {..} =
    Prelude.rnf propertyValue
      `Prelude.seq` Prelude.rnf httpStatus
