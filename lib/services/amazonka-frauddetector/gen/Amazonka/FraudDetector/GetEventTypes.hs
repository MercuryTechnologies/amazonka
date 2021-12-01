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
-- Module      : Amazonka.FraudDetector.GetEventTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets all event types or a specific event type if name is provided. This
-- is a paginated API. If you provide a null @maxResults@, this action
-- retrieves a maximum of 10 records per page. If you provide a
-- @maxResults@, the value must be between 5 and 10. To get the next page
-- results, provide the pagination token from the @GetEventTypesResponse@
-- as part of your request. A null pagination token fetches the records
-- from the beginning.
module Amazonka.FraudDetector.GetEventTypes
  ( -- * Creating a Request
    GetEventTypes (..),
    newGetEventTypes,

    -- * Request Lenses
    getEventTypes_nextToken,
    getEventTypes_name,
    getEventTypes_maxResults,

    -- * Destructuring the Response
    GetEventTypesResponse (..),
    newGetEventTypesResponse,

    -- * Response Lenses
    getEventTypesResponse_eventTypes,
    getEventTypesResponse_nextToken,
    getEventTypesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.FraudDetector.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEventTypes' smart constructor.
data GetEventTypes = GetEventTypes'
  { -- | The next token for the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of objects to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getEventTypes_nextToken' - The next token for the subsequent request.
--
-- 'name', 'getEventTypes_name' - The name.
--
-- 'maxResults', 'getEventTypes_maxResults' - The maximum number of objects to return for the request.
newGetEventTypes ::
  GetEventTypes
newGetEventTypes =
  GetEventTypes'
    { nextToken = Prelude.Nothing,
      name = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The next token for the subsequent request.
getEventTypes_nextToken :: Lens.Lens' GetEventTypes (Prelude.Maybe Prelude.Text)
getEventTypes_nextToken = Lens.lens (\GetEventTypes' {nextToken} -> nextToken) (\s@GetEventTypes' {} a -> s {nextToken = a} :: GetEventTypes)

-- | The name.
getEventTypes_name :: Lens.Lens' GetEventTypes (Prelude.Maybe Prelude.Text)
getEventTypes_name = Lens.lens (\GetEventTypes' {name} -> name) (\s@GetEventTypes' {} a -> s {name = a} :: GetEventTypes)

-- | The maximum number of objects to return for the request.
getEventTypes_maxResults :: Lens.Lens' GetEventTypes (Prelude.Maybe Prelude.Natural)
getEventTypes_maxResults = Lens.lens (\GetEventTypes' {maxResults} -> maxResults) (\s@GetEventTypes' {} a -> s {maxResults = a} :: GetEventTypes)

instance Core.AWSRequest GetEventTypes where
  type
    AWSResponse GetEventTypes =
      GetEventTypesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEventTypesResponse'
            Prelude.<$> (x Core..?> "eventTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEventTypes where
  hashWithSalt salt' GetEventTypes' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetEventTypes where
  rnf GetEventTypes' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders GetEventTypes where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSHawksNestServiceFacade.GetEventTypes" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetEventTypes where
  toJSON GetEventTypes' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("name" Core..=) Prelude.<$> name,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetEventTypes where
  toPath = Prelude.const "/"

instance Core.ToQuery GetEventTypes where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEventTypesResponse' smart constructor.
data GetEventTypesResponse = GetEventTypesResponse'
  { -- | An array of event types.
    eventTypes :: Prelude.Maybe [Core.Sensitive EventType],
    -- | The next page token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEventTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventTypes', 'getEventTypesResponse_eventTypes' - An array of event types.
--
-- 'nextToken', 'getEventTypesResponse_nextToken' - The next page token.
--
-- 'httpStatus', 'getEventTypesResponse_httpStatus' - The response's http status code.
newGetEventTypesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEventTypesResponse
newGetEventTypesResponse pHttpStatus_ =
  GetEventTypesResponse'
    { eventTypes =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of event types.
getEventTypesResponse_eventTypes :: Lens.Lens' GetEventTypesResponse (Prelude.Maybe [EventType])
getEventTypesResponse_eventTypes = Lens.lens (\GetEventTypesResponse' {eventTypes} -> eventTypes) (\s@GetEventTypesResponse' {} a -> s {eventTypes = a} :: GetEventTypesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page token.
getEventTypesResponse_nextToken :: Lens.Lens' GetEventTypesResponse (Prelude.Maybe Prelude.Text)
getEventTypesResponse_nextToken = Lens.lens (\GetEventTypesResponse' {nextToken} -> nextToken) (\s@GetEventTypesResponse' {} a -> s {nextToken = a} :: GetEventTypesResponse)

-- | The response's http status code.
getEventTypesResponse_httpStatus :: Lens.Lens' GetEventTypesResponse Prelude.Int
getEventTypesResponse_httpStatus = Lens.lens (\GetEventTypesResponse' {httpStatus} -> httpStatus) (\s@GetEventTypesResponse' {} a -> s {httpStatus = a} :: GetEventTypesResponse)

instance Prelude.NFData GetEventTypesResponse where
  rnf GetEventTypesResponse' {..} =
    Prelude.rnf eventTypes
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
