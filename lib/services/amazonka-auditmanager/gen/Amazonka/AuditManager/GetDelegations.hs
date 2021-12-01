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
-- Module      : Amazonka.AuditManager.GetDelegations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of delegations from an audit owner to a delegate.
module Amazonka.AuditManager.GetDelegations
  ( -- * Creating a Request
    GetDelegations (..),
    newGetDelegations,

    -- * Request Lenses
    getDelegations_nextToken,
    getDelegations_maxResults,

    -- * Destructuring the Response
    GetDelegationsResponse (..),
    newGetDelegationsResponse,

    -- * Response Lenses
    getDelegationsResponse_delegations,
    getDelegationsResponse_nextToken,
    getDelegationsResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDelegations' smart constructor.
data GetDelegations = GetDelegations'
  { -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Represents the maximum number of results per page, or per API request
    -- call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDelegations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getDelegations_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'maxResults', 'getDelegations_maxResults' - Represents the maximum number of results per page, or per API request
-- call.
newGetDelegations ::
  GetDelegations
newGetDelegations =
  GetDelegations'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The pagination token used to fetch the next set of results.
getDelegations_nextToken :: Lens.Lens' GetDelegations (Prelude.Maybe Prelude.Text)
getDelegations_nextToken = Lens.lens (\GetDelegations' {nextToken} -> nextToken) (\s@GetDelegations' {} a -> s {nextToken = a} :: GetDelegations)

-- | Represents the maximum number of results per page, or per API request
-- call.
getDelegations_maxResults :: Lens.Lens' GetDelegations (Prelude.Maybe Prelude.Natural)
getDelegations_maxResults = Lens.lens (\GetDelegations' {maxResults} -> maxResults) (\s@GetDelegations' {} a -> s {maxResults = a} :: GetDelegations)

instance Core.AWSRequest GetDelegations where
  type
    AWSResponse GetDelegations =
      GetDelegationsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDelegationsResponse'
            Prelude.<$> (x Core..?> "delegations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDelegations where
  hashWithSalt salt' GetDelegations' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetDelegations where
  rnf GetDelegations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders GetDelegations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetDelegations where
  toPath = Prelude.const "/delegations"

instance Core.ToQuery GetDelegations where
  toQuery GetDelegations' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newGetDelegationsResponse' smart constructor.
data GetDelegationsResponse = GetDelegationsResponse'
  { -- | The list of delegations returned by the @GetDelegations@ API.
    delegations :: Prelude.Maybe [DelegationMetadata],
    -- | The pagination token used to fetch the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDelegationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'delegations', 'getDelegationsResponse_delegations' - The list of delegations returned by the @GetDelegations@ API.
--
-- 'nextToken', 'getDelegationsResponse_nextToken' - The pagination token used to fetch the next set of results.
--
-- 'httpStatus', 'getDelegationsResponse_httpStatus' - The response's http status code.
newGetDelegationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDelegationsResponse
newGetDelegationsResponse pHttpStatus_ =
  GetDelegationsResponse'
    { delegations =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of delegations returned by the @GetDelegations@ API.
getDelegationsResponse_delegations :: Lens.Lens' GetDelegationsResponse (Prelude.Maybe [DelegationMetadata])
getDelegationsResponse_delegations = Lens.lens (\GetDelegationsResponse' {delegations} -> delegations) (\s@GetDelegationsResponse' {} a -> s {delegations = a} :: GetDelegationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The pagination token used to fetch the next set of results.
getDelegationsResponse_nextToken :: Lens.Lens' GetDelegationsResponse (Prelude.Maybe Prelude.Text)
getDelegationsResponse_nextToken = Lens.lens (\GetDelegationsResponse' {nextToken} -> nextToken) (\s@GetDelegationsResponse' {} a -> s {nextToken = a} :: GetDelegationsResponse)

-- | The response's http status code.
getDelegationsResponse_httpStatus :: Lens.Lens' GetDelegationsResponse Prelude.Int
getDelegationsResponse_httpStatus = Lens.lens (\GetDelegationsResponse' {httpStatus} -> httpStatus) (\s@GetDelegationsResponse' {} a -> s {httpStatus = a} :: GetDelegationsResponse)

instance Prelude.NFData GetDelegationsResponse where
  rnf GetDelegationsResponse' {..} =
    Prelude.rnf delegations
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
