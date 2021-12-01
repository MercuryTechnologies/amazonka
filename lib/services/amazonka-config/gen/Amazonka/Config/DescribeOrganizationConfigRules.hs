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
-- Module      : Amazonka.Config.DescribeOrganizationConfigRules
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of organization config rules.
--
-- When you specify the limit and the next token, you receive a paginated
-- response. Limit and next token are not applicable if you specify
-- organization config rule names. It is only applicable, when you request
-- all the organization config rules.
--
-- This operation returns paginated results.
module Amazonka.Config.DescribeOrganizationConfigRules
  ( -- * Creating a Request
    DescribeOrganizationConfigRules (..),
    newDescribeOrganizationConfigRules,

    -- * Request Lenses
    describeOrganizationConfigRules_organizationConfigRuleNames,
    describeOrganizationConfigRules_nextToken,
    describeOrganizationConfigRules_limit,

    -- * Destructuring the Response
    DescribeOrganizationConfigRulesResponse (..),
    newDescribeOrganizationConfigRulesResponse,

    -- * Response Lenses
    describeOrganizationConfigRulesResponse_organizationConfigRules,
    describeOrganizationConfigRulesResponse_nextToken,
    describeOrganizationConfigRulesResponse_httpStatus,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeOrganizationConfigRules' smart constructor.
data DescribeOrganizationConfigRules = DescribeOrganizationConfigRules'
  { -- | The names of organization config rules for which you want details. If
    -- you do not specify any names, Config returns details for all your
    -- organization config rules.
    organizationConfigRuleNames :: Prelude.Maybe [Prelude.Text],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of organization config rules returned on each page.
    -- If you do no specify a number, Config uses the default. The default is
    -- 100.
    limit :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConfigRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationConfigRuleNames', 'describeOrganizationConfigRules_organizationConfigRuleNames' - The names of organization config rules for which you want details. If
-- you do not specify any names, Config returns details for all your
-- organization config rules.
--
-- 'nextToken', 'describeOrganizationConfigRules_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'limit', 'describeOrganizationConfigRules_limit' - The maximum number of organization config rules returned on each page.
-- If you do no specify a number, Config uses the default. The default is
-- 100.
newDescribeOrganizationConfigRules ::
  DescribeOrganizationConfigRules
newDescribeOrganizationConfigRules =
  DescribeOrganizationConfigRules'
    { organizationConfigRuleNames =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      limit = Prelude.Nothing
    }

-- | The names of organization config rules for which you want details. If
-- you do not specify any names, Config returns details for all your
-- organization config rules.
describeOrganizationConfigRules_organizationConfigRuleNames :: Lens.Lens' DescribeOrganizationConfigRules (Prelude.Maybe [Prelude.Text])
describeOrganizationConfigRules_organizationConfigRuleNames = Lens.lens (\DescribeOrganizationConfigRules' {organizationConfigRuleNames} -> organizationConfigRuleNames) (\s@DescribeOrganizationConfigRules' {} a -> s {organizationConfigRuleNames = a} :: DescribeOrganizationConfigRules) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeOrganizationConfigRules_nextToken :: Lens.Lens' DescribeOrganizationConfigRules (Prelude.Maybe Prelude.Text)
describeOrganizationConfigRules_nextToken = Lens.lens (\DescribeOrganizationConfigRules' {nextToken} -> nextToken) (\s@DescribeOrganizationConfigRules' {} a -> s {nextToken = a} :: DescribeOrganizationConfigRules)

-- | The maximum number of organization config rules returned on each page.
-- If you do no specify a number, Config uses the default. The default is
-- 100.
describeOrganizationConfigRules_limit :: Lens.Lens' DescribeOrganizationConfigRules (Prelude.Maybe Prelude.Natural)
describeOrganizationConfigRules_limit = Lens.lens (\DescribeOrganizationConfigRules' {limit} -> limit) (\s@DescribeOrganizationConfigRules' {} a -> s {limit = a} :: DescribeOrganizationConfigRules)

instance
  Core.AWSPager
    DescribeOrganizationConfigRules
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeOrganizationConfigRulesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? describeOrganizationConfigRulesResponse_organizationConfigRules
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& describeOrganizationConfigRules_nextToken
          Lens..~ rs
          Lens.^? describeOrganizationConfigRulesResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    DescribeOrganizationConfigRules
  where
  type
    AWSResponse DescribeOrganizationConfigRules =
      DescribeOrganizationConfigRulesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOrganizationConfigRulesResponse'
            Prelude.<$> ( x Core..?> "OrganizationConfigRules"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOrganizationConfigRules
  where
  hashWithSalt
    salt'
    DescribeOrganizationConfigRules' {..} =
      salt' `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` organizationConfigRuleNames

instance
  Prelude.NFData
    DescribeOrganizationConfigRules
  where
  rnf DescribeOrganizationConfigRules' {..} =
    Prelude.rnf organizationConfigRuleNames
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken

instance
  Core.ToHeaders
    DescribeOrganizationConfigRules
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeOrganizationConfigRules" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeOrganizationConfigRules where
  toJSON DescribeOrganizationConfigRules' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("OrganizationConfigRuleNames" Core..=)
              Prelude.<$> organizationConfigRuleNames,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit
          ]
      )

instance Core.ToPath DescribeOrganizationConfigRules where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeOrganizationConfigRules where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeOrganizationConfigRulesResponse' smart constructor.
data DescribeOrganizationConfigRulesResponse = DescribeOrganizationConfigRulesResponse'
  { -- | Returns a list of @OrganizationConfigRule@ objects.
    organizationConfigRules :: Prelude.Maybe [OrganizationConfigRule],
    -- | The @nextToken@ string returned on a previous page that you use to get
    -- the next page of results in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOrganizationConfigRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'organizationConfigRules', 'describeOrganizationConfigRulesResponse_organizationConfigRules' - Returns a list of @OrganizationConfigRule@ objects.
--
-- 'nextToken', 'describeOrganizationConfigRulesResponse_nextToken' - The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
--
-- 'httpStatus', 'describeOrganizationConfigRulesResponse_httpStatus' - The response's http status code.
newDescribeOrganizationConfigRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOrganizationConfigRulesResponse
newDescribeOrganizationConfigRulesResponse
  pHttpStatus_ =
    DescribeOrganizationConfigRulesResponse'
      { organizationConfigRules =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns a list of @OrganizationConfigRule@ objects.
describeOrganizationConfigRulesResponse_organizationConfigRules :: Lens.Lens' DescribeOrganizationConfigRulesResponse (Prelude.Maybe [OrganizationConfigRule])
describeOrganizationConfigRulesResponse_organizationConfigRules = Lens.lens (\DescribeOrganizationConfigRulesResponse' {organizationConfigRules} -> organizationConfigRules) (\s@DescribeOrganizationConfigRulesResponse' {} a -> s {organizationConfigRules = a} :: DescribeOrganizationConfigRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The @nextToken@ string returned on a previous page that you use to get
-- the next page of results in a paginated response.
describeOrganizationConfigRulesResponse_nextToken :: Lens.Lens' DescribeOrganizationConfigRulesResponse (Prelude.Maybe Prelude.Text)
describeOrganizationConfigRulesResponse_nextToken = Lens.lens (\DescribeOrganizationConfigRulesResponse' {nextToken} -> nextToken) (\s@DescribeOrganizationConfigRulesResponse' {} a -> s {nextToken = a} :: DescribeOrganizationConfigRulesResponse)

-- | The response's http status code.
describeOrganizationConfigRulesResponse_httpStatus :: Lens.Lens' DescribeOrganizationConfigRulesResponse Prelude.Int
describeOrganizationConfigRulesResponse_httpStatus = Lens.lens (\DescribeOrganizationConfigRulesResponse' {httpStatus} -> httpStatus) (\s@DescribeOrganizationConfigRulesResponse' {} a -> s {httpStatus = a} :: DescribeOrganizationConfigRulesResponse)

instance
  Prelude.NFData
    DescribeOrganizationConfigRulesResponse
  where
  rnf DescribeOrganizationConfigRulesResponse' {..} =
    Prelude.rnf organizationConfigRules
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
