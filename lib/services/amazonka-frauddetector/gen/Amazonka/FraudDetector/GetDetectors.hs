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
-- Module      : Amazonka.FraudDetector.GetDetectors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all detectors or a single detector if a @detectorId@ is specified.
-- This is a paginated API. If you provide a null @maxResults@, this action
-- retrieves a maximum of 10 records per page. If you provide a
-- @maxResults@, the value must be between 5 and 10. To get the next page
-- results, provide the pagination token from the @GetDetectorsResponse@ as
-- part of your request. A null pagination token fetches the records from
-- the beginning.
module Amazonka.FraudDetector.GetDetectors
  ( -- * Creating a Request
    GetDetectors (..),
    newGetDetectors,

    -- * Request Lenses
    getDetectors_nextToken,
    getDetectors_detectorId,
    getDetectors_maxResults,

    -- * Destructuring the Response
    GetDetectorsResponse (..),
    newGetDetectorsResponse,

    -- * Response Lenses
    getDetectorsResponse_nextToken,
    getDetectorsResponse_detectors,
    getDetectorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDetectors' smart constructor.
data GetDetectors = GetDetectors'
  { -- | The next token for the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The detector ID.
    detectorId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDetectors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDetectors_nextToken' - The next token for the subsequent request.
--
-- 'detectorId', 'getDetectors_detectorId' - The detector ID.
--
-- 'maxResults', 'getDetectors_maxResults' - The maximum number of objects to return for the request.
newGetDetectors ::
  GetDetectors
newGetDetectors =
  GetDetectors'
    { nextToken = Prelude.Nothing,
      detectorId = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The next token for the subsequent request.
getDetectors_nextToken :: Lens.Lens' GetDetectors (Prelude.Maybe Prelude.Text)
getDetectors_nextToken = Lens.lens (\GetDetectors' {nextToken} -> nextToken) (\s@GetDetectors' {} a -> s {nextToken = a} :: GetDetectors)

-- | The detector ID.
getDetectors_detectorId :: Lens.Lens' GetDetectors (Prelude.Maybe Prelude.Text)
getDetectors_detectorId = Lens.lens (\GetDetectors' {detectorId} -> detectorId) (\s@GetDetectors' {} a -> s {detectorId = a} :: GetDetectors)

-- | The maximum number of objects to return for the request.
getDetectors_maxResults :: Lens.Lens' GetDetectors (Prelude.Maybe Prelude.Natural)
getDetectors_maxResults = Lens.lens (\GetDetectors' {maxResults} -> maxResults) (\s@GetDetectors' {} a -> s {maxResults = a} :: GetDetectors)

instance Core.AWSRequest GetDetectors where
  type AWSResponse GetDetectors = GetDetectorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDetectorsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "detectors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDetectors where
  hashWithSalt salt' GetDetectors' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` detectorId
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetDetectors where
  rnf GetDetectors' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf detectorId

instance Core.ToHeaders GetDetectors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.GetDetectors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetDetectors where
  toJSON GetDetectors' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("detectorId" Core..=) Prelude.<$> detectorId,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetDetectors where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDetectors where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDetectorsResponse' smart constructor.
data GetDetectorsResponse = GetDetectorsResponse'
  { -- | The next page token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The detectors.
    detectors :: Prelude.Maybe [Detector],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDetectorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDetectorsResponse_nextToken' - The next page token.
--
-- 'detectors', 'getDetectorsResponse_detectors' - The detectors.
--
-- 'httpStatus', 'getDetectorsResponse_httpStatus' - The response's http status code.
newGetDetectorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDetectorsResponse
newGetDetectorsResponse pHttpStatus_ =
  GetDetectorsResponse'
    { nextToken = Prelude.Nothing,
      detectors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The next page token.
getDetectorsResponse_nextToken :: Lens.Lens' GetDetectorsResponse (Prelude.Maybe Prelude.Text)
getDetectorsResponse_nextToken = Lens.lens (\GetDetectorsResponse' {nextToken} -> nextToken) (\s@GetDetectorsResponse' {} a -> s {nextToken = a} :: GetDetectorsResponse)

-- | The detectors.
getDetectorsResponse_detectors :: Lens.Lens' GetDetectorsResponse (Prelude.Maybe [Detector])
getDetectorsResponse_detectors = Lens.lens (\GetDetectorsResponse' {detectors} -> detectors) (\s@GetDetectorsResponse' {} a -> s {detectors = a} :: GetDetectorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getDetectorsResponse_httpStatus :: Lens.Lens' GetDetectorsResponse Prelude.Int
getDetectorsResponse_httpStatus = Lens.lens (\GetDetectorsResponse' {httpStatus} -> httpStatus) (\s@GetDetectorsResponse' {} a -> s {httpStatus = a} :: GetDetectorsResponse)

instance Prelude.NFData GetDetectorsResponse where
  rnf GetDetectorsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf detectors
