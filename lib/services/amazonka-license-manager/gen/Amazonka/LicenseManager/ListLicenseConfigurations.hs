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
-- Module      : Amazonka.LicenseManager.ListLicenseConfigurations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the license configurations for your account.
--
-- This operation returns paginated results.
module Amazonka.LicenseManager.ListLicenseConfigurations
  ( -- * Creating a Request
    ListLicenseConfigurations (..),
    newListLicenseConfigurations,

    -- * Request Lenses
    listLicenseConfigurations_filters,
    listLicenseConfigurations_nextToken,
    listLicenseConfigurations_licenseConfigurationArns,
    listLicenseConfigurations_maxResults,

    -- * Destructuring the Response
    ListLicenseConfigurationsResponse (..),
    newListLicenseConfigurationsResponse,

    -- * Response Lenses
    listLicenseConfigurationsResponse_nextToken,
    listLicenseConfigurationsResponse_licenseConfigurations,
    listLicenseConfigurationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LicenseManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLicenseConfigurations' smart constructor.
data ListLicenseConfigurations = ListLicenseConfigurations'
  { -- | Filters to scope the results. The following filters and logical
    -- operators are supported:
    --
    -- -   @licenseCountingType@ - The dimension for which licenses are
    --     counted. Possible values are @vCPU@ | @Instance@ | @Core@ |
    --     @Socket@. Logical operators are @EQUALS@ | @NOT_EQUALS@.
    --
    -- -   @enforceLicenseCount@ - A Boolean value that indicates whether hard
    --     license enforcement is used. Logical operators are @EQUALS@ |
    --     @NOT_EQUALS@.
    --
    -- -   @usagelimitExceeded@ - A Boolean value that indicates whether the
    --     available licenses have been exceeded. Logical operators are
    --     @EQUALS@ | @NOT_EQUALS@.
    filters :: Prelude.Maybe [Filter],
    -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Amazon Resource Names (ARN) of the license configurations.
    licenseConfigurationArns :: Prelude.Maybe [Prelude.Text],
    -- | Maximum number of results to return in a single call.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLicenseConfigurations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listLicenseConfigurations_filters' - Filters to scope the results. The following filters and logical
-- operators are supported:
--
-- -   @licenseCountingType@ - The dimension for which licenses are
--     counted. Possible values are @vCPU@ | @Instance@ | @Core@ |
--     @Socket@. Logical operators are @EQUALS@ | @NOT_EQUALS@.
--
-- -   @enforceLicenseCount@ - A Boolean value that indicates whether hard
--     license enforcement is used. Logical operators are @EQUALS@ |
--     @NOT_EQUALS@.
--
-- -   @usagelimitExceeded@ - A Boolean value that indicates whether the
--     available licenses have been exceeded. Logical operators are
--     @EQUALS@ | @NOT_EQUALS@.
--
-- 'nextToken', 'listLicenseConfigurations_nextToken' - Token for the next set of results.
--
-- 'licenseConfigurationArns', 'listLicenseConfigurations_licenseConfigurationArns' - Amazon Resource Names (ARN) of the license configurations.
--
-- 'maxResults', 'listLicenseConfigurations_maxResults' - Maximum number of results to return in a single call.
newListLicenseConfigurations ::
  ListLicenseConfigurations
newListLicenseConfigurations =
  ListLicenseConfigurations'
    { filters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      licenseConfigurationArns = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Filters to scope the results. The following filters and logical
-- operators are supported:
--
-- -   @licenseCountingType@ - The dimension for which licenses are
--     counted. Possible values are @vCPU@ | @Instance@ | @Core@ |
--     @Socket@. Logical operators are @EQUALS@ | @NOT_EQUALS@.
--
-- -   @enforceLicenseCount@ - A Boolean value that indicates whether hard
--     license enforcement is used. Logical operators are @EQUALS@ |
--     @NOT_EQUALS@.
--
-- -   @usagelimitExceeded@ - A Boolean value that indicates whether the
--     available licenses have been exceeded. Logical operators are
--     @EQUALS@ | @NOT_EQUALS@.
listLicenseConfigurations_filters :: Lens.Lens' ListLicenseConfigurations (Prelude.Maybe [Filter])
listLicenseConfigurations_filters = Lens.lens (\ListLicenseConfigurations' {filters} -> filters) (\s@ListLicenseConfigurations' {} a -> s {filters = a} :: ListLicenseConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | Token for the next set of results.
listLicenseConfigurations_nextToken :: Lens.Lens' ListLicenseConfigurations (Prelude.Maybe Prelude.Text)
listLicenseConfigurations_nextToken = Lens.lens (\ListLicenseConfigurations' {nextToken} -> nextToken) (\s@ListLicenseConfigurations' {} a -> s {nextToken = a} :: ListLicenseConfigurations)

-- | Amazon Resource Names (ARN) of the license configurations.
listLicenseConfigurations_licenseConfigurationArns :: Lens.Lens' ListLicenseConfigurations (Prelude.Maybe [Prelude.Text])
listLicenseConfigurations_licenseConfigurationArns = Lens.lens (\ListLicenseConfigurations' {licenseConfigurationArns} -> licenseConfigurationArns) (\s@ListLicenseConfigurations' {} a -> s {licenseConfigurationArns = a} :: ListLicenseConfigurations) Prelude.. Lens.mapping Lens.coerced

-- | Maximum number of results to return in a single call.
listLicenseConfigurations_maxResults :: Lens.Lens' ListLicenseConfigurations (Prelude.Maybe Prelude.Int)
listLicenseConfigurations_maxResults = Lens.lens (\ListLicenseConfigurations' {maxResults} -> maxResults) (\s@ListLicenseConfigurations' {} a -> s {maxResults = a} :: ListLicenseConfigurations)

instance Core.AWSPager ListLicenseConfigurations where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLicenseConfigurationsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLicenseConfigurationsResponse_licenseConfigurations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLicenseConfigurations_nextToken
          Lens..~ rs
          Lens.^? listLicenseConfigurationsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLicenseConfigurations where
  type
    AWSResponse ListLicenseConfigurations =
      ListLicenseConfigurationsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLicenseConfigurationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "LicenseConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLicenseConfigurations where
  hashWithSalt salt' ListLicenseConfigurations' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` licenseConfigurationArns
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` filters

instance Prelude.NFData ListLicenseConfigurations where
  rnf ListLicenseConfigurations' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf licenseConfigurationArns
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders ListLicenseConfigurations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSLicenseManager.ListLicenseConfigurations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListLicenseConfigurations where
  toJSON ListLicenseConfigurations' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Filters" Core..=) Prelude.<$> filters,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("LicenseConfigurationArns" Core..=)
              Prelude.<$> licenseConfigurationArns,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath ListLicenseConfigurations where
  toPath = Prelude.const "/"

instance Core.ToQuery ListLicenseConfigurations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLicenseConfigurationsResponse' smart constructor.
data ListLicenseConfigurationsResponse = ListLicenseConfigurationsResponse'
  { -- | Token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the license configurations.
    licenseConfigurations :: Prelude.Maybe [LicenseConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLicenseConfigurationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLicenseConfigurationsResponse_nextToken' - Token for the next set of results.
--
-- 'licenseConfigurations', 'listLicenseConfigurationsResponse_licenseConfigurations' - Information about the license configurations.
--
-- 'httpStatus', 'listLicenseConfigurationsResponse_httpStatus' - The response's http status code.
newListLicenseConfigurationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLicenseConfigurationsResponse
newListLicenseConfigurationsResponse pHttpStatus_ =
  ListLicenseConfigurationsResponse'
    { nextToken =
        Prelude.Nothing,
      licenseConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Token for the next set of results.
listLicenseConfigurationsResponse_nextToken :: Lens.Lens' ListLicenseConfigurationsResponse (Prelude.Maybe Prelude.Text)
listLicenseConfigurationsResponse_nextToken = Lens.lens (\ListLicenseConfigurationsResponse' {nextToken} -> nextToken) (\s@ListLicenseConfigurationsResponse' {} a -> s {nextToken = a} :: ListLicenseConfigurationsResponse)

-- | Information about the license configurations.
listLicenseConfigurationsResponse_licenseConfigurations :: Lens.Lens' ListLicenseConfigurationsResponse (Prelude.Maybe [LicenseConfiguration])
listLicenseConfigurationsResponse_licenseConfigurations = Lens.lens (\ListLicenseConfigurationsResponse' {licenseConfigurations} -> licenseConfigurations) (\s@ListLicenseConfigurationsResponse' {} a -> s {licenseConfigurations = a} :: ListLicenseConfigurationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listLicenseConfigurationsResponse_httpStatus :: Lens.Lens' ListLicenseConfigurationsResponse Prelude.Int
listLicenseConfigurationsResponse_httpStatus = Lens.lens (\ListLicenseConfigurationsResponse' {httpStatus} -> httpStatus) (\s@ListLicenseConfigurationsResponse' {} a -> s {httpStatus = a} :: ListLicenseConfigurationsResponse)

instance
  Prelude.NFData
    ListLicenseConfigurationsResponse
  where
  rnf ListLicenseConfigurationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf licenseConfigurations
