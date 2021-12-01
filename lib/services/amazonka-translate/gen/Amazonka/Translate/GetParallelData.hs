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
-- Module      : Amazonka.Translate.GetParallelData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a parallel data resource.
module Amazonka.Translate.GetParallelData
  ( -- * Creating a Request
    GetParallelData (..),
    newGetParallelData,

    -- * Request Lenses
    getParallelData_name,

    -- * Destructuring the Response
    GetParallelDataResponse (..),
    newGetParallelDataResponse,

    -- * Response Lenses
    getParallelDataResponse_parallelDataProperties,
    getParallelDataResponse_dataLocation,
    getParallelDataResponse_auxiliaryDataLocation,
    getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation,
    getParallelDataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newGetParallelData' smart constructor.
data GetParallelData = GetParallelData'
  { -- | The name of the parallel data resource that is being retrieved.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParallelData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getParallelData_name' - The name of the parallel data resource that is being retrieved.
newGetParallelData ::
  -- | 'name'
  Prelude.Text ->
  GetParallelData
newGetParallelData pName_ =
  GetParallelData' {name = pName_}

-- | The name of the parallel data resource that is being retrieved.
getParallelData_name :: Lens.Lens' GetParallelData Prelude.Text
getParallelData_name = Lens.lens (\GetParallelData' {name} -> name) (\s@GetParallelData' {} a -> s {name = a} :: GetParallelData)

instance Core.AWSRequest GetParallelData where
  type
    AWSResponse GetParallelData =
      GetParallelDataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParallelDataResponse'
            Prelude.<$> (x Core..?> "ParallelDataProperties")
            Prelude.<*> (x Core..?> "DataLocation")
            Prelude.<*> (x Core..?> "AuxiliaryDataLocation")
            Prelude.<*> ( x
                            Core..?> "LatestUpdateAttemptAuxiliaryDataLocation"
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetParallelData where
  hashWithSalt salt' GetParallelData' {..} =
    salt' `Prelude.hashWithSalt` name

instance Prelude.NFData GetParallelData where
  rnf GetParallelData' {..} = Prelude.rnf name

instance Core.ToHeaders GetParallelData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.GetParallelData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetParallelData where
  toJSON GetParallelData' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Core..= name)]
      )

instance Core.ToPath GetParallelData where
  toPath = Prelude.const "/"

instance Core.ToQuery GetParallelData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetParallelDataResponse' smart constructor.
data GetParallelDataResponse = GetParallelDataResponse'
  { -- | The properties of the parallel data resource that is being retrieved.
    parallelDataProperties :: Prelude.Maybe ParallelDataProperties,
    -- | The location of the most recent parallel data input file that was
    -- successfully imported into Amazon Translate. The location is returned as
    -- a presigned URL that has a 30 minute expiration.
    dataLocation :: Prelude.Maybe ParallelDataDataLocation,
    -- | The Amazon S3 location of a file that provides any errors or warnings
    -- that were produced by your input file. This file was created when Amazon
    -- Translate attempted to create a parallel data resource. The location is
    -- returned as a presigned URL to that has a 30 minute expiration.
    auxiliaryDataLocation :: Prelude.Maybe ParallelDataDataLocation,
    -- | The Amazon S3 location of a file that provides any errors or warnings
    -- that were produced by your input file. This file was created when Amazon
    -- Translate attempted to update a parallel data resource. The location is
    -- returned as a presigned URL to that has a 30 minute expiration.
    latestUpdateAttemptAuxiliaryDataLocation :: Prelude.Maybe ParallelDataDataLocation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParallelDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parallelDataProperties', 'getParallelDataResponse_parallelDataProperties' - The properties of the parallel data resource that is being retrieved.
--
-- 'dataLocation', 'getParallelDataResponse_dataLocation' - The location of the most recent parallel data input file that was
-- successfully imported into Amazon Translate. The location is returned as
-- a presigned URL that has a 30 minute expiration.
--
-- 'auxiliaryDataLocation', 'getParallelDataResponse_auxiliaryDataLocation' - The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to create a parallel data resource. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
--
-- 'latestUpdateAttemptAuxiliaryDataLocation', 'getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation' - The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to update a parallel data resource. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
--
-- 'httpStatus', 'getParallelDataResponse_httpStatus' - The response's http status code.
newGetParallelDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetParallelDataResponse
newGetParallelDataResponse pHttpStatus_ =
  GetParallelDataResponse'
    { parallelDataProperties =
        Prelude.Nothing,
      dataLocation = Prelude.Nothing,
      auxiliaryDataLocation = Prelude.Nothing,
      latestUpdateAttemptAuxiliaryDataLocation =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The properties of the parallel data resource that is being retrieved.
getParallelDataResponse_parallelDataProperties :: Lens.Lens' GetParallelDataResponse (Prelude.Maybe ParallelDataProperties)
getParallelDataResponse_parallelDataProperties = Lens.lens (\GetParallelDataResponse' {parallelDataProperties} -> parallelDataProperties) (\s@GetParallelDataResponse' {} a -> s {parallelDataProperties = a} :: GetParallelDataResponse)

-- | The location of the most recent parallel data input file that was
-- successfully imported into Amazon Translate. The location is returned as
-- a presigned URL that has a 30 minute expiration.
getParallelDataResponse_dataLocation :: Lens.Lens' GetParallelDataResponse (Prelude.Maybe ParallelDataDataLocation)
getParallelDataResponse_dataLocation = Lens.lens (\GetParallelDataResponse' {dataLocation} -> dataLocation) (\s@GetParallelDataResponse' {} a -> s {dataLocation = a} :: GetParallelDataResponse)

-- | The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to create a parallel data resource. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
getParallelDataResponse_auxiliaryDataLocation :: Lens.Lens' GetParallelDataResponse (Prelude.Maybe ParallelDataDataLocation)
getParallelDataResponse_auxiliaryDataLocation = Lens.lens (\GetParallelDataResponse' {auxiliaryDataLocation} -> auxiliaryDataLocation) (\s@GetParallelDataResponse' {} a -> s {auxiliaryDataLocation = a} :: GetParallelDataResponse)

-- | The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to update a parallel data resource. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation :: Lens.Lens' GetParallelDataResponse (Prelude.Maybe ParallelDataDataLocation)
getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation = Lens.lens (\GetParallelDataResponse' {latestUpdateAttemptAuxiliaryDataLocation} -> latestUpdateAttemptAuxiliaryDataLocation) (\s@GetParallelDataResponse' {} a -> s {latestUpdateAttemptAuxiliaryDataLocation = a} :: GetParallelDataResponse)

-- | The response's http status code.
getParallelDataResponse_httpStatus :: Lens.Lens' GetParallelDataResponse Prelude.Int
getParallelDataResponse_httpStatus = Lens.lens (\GetParallelDataResponse' {httpStatus} -> httpStatus) (\s@GetParallelDataResponse' {} a -> s {httpStatus = a} :: GetParallelDataResponse)

instance Prelude.NFData GetParallelDataResponse where
  rnf GetParallelDataResponse' {..} =
    Prelude.rnf parallelDataProperties
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf latestUpdateAttemptAuxiliaryDataLocation
      `Prelude.seq` Prelude.rnf auxiliaryDataLocation
      `Prelude.seq` Prelude.rnf dataLocation
