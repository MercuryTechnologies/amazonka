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
-- Module      : Amazonka.CodeArtifact.UpdateRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the properties of a repository.
module Amazonka.CodeArtifact.UpdateRepository
  ( -- * Creating a Request
    UpdateRepository (..),
    newUpdateRepository,

    -- * Request Lenses
    updateRepository_upstreams,
    updateRepository_domainOwner,
    updateRepository_description,
    updateRepository_domain,
    updateRepository_repository,

    -- * Destructuring the Response
    UpdateRepositoryResponse (..),
    newUpdateRepositoryResponse,

    -- * Response Lenses
    updateRepositoryResponse_repository,
    updateRepositoryResponse_httpStatus,
  )
where

import Amazonka.CodeArtifact.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRepository' smart constructor.
data UpdateRepository = UpdateRepository'
  { -- | A list of upstream repositories to associate with the repository. The
    -- order of the upstream repositories in the list determines their priority
    -- order when AWS CodeArtifact looks for a requested package version. For
    -- more information, see
    -- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
    upstreams :: Prelude.Maybe [UpstreamRepository],
    -- | The 12-digit account number of the AWS account that owns the domain. It
    -- does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | An updated repository description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain associated with the repository to update.
    domain :: Prelude.Text,
    -- | The name of the repository to update.
    repository :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'upstreams', 'updateRepository_upstreams' - A list of upstream repositories to associate with the repository. The
-- order of the upstream repositories in the list determines their priority
-- order when AWS CodeArtifact looks for a requested package version. For
-- more information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
--
-- 'domainOwner', 'updateRepository_domainOwner' - The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
--
-- 'description', 'updateRepository_description' - An updated repository description.
--
-- 'domain', 'updateRepository_domain' - The name of the domain associated with the repository to update.
--
-- 'repository', 'updateRepository_repository' - The name of the repository to update.
newUpdateRepository ::
  -- | 'domain'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  UpdateRepository
newUpdateRepository pDomain_ pRepository_ =
  UpdateRepository'
    { upstreams = Prelude.Nothing,
      domainOwner = Prelude.Nothing,
      description = Prelude.Nothing,
      domain = pDomain_,
      repository = pRepository_
    }

-- | A list of upstream repositories to associate with the repository. The
-- order of the upstream repositories in the list determines their priority
-- order when AWS CodeArtifact looks for a requested package version. For
-- more information, see
-- <https://docs.aws.amazon.com/codeartifact/latest/ug/repos-upstream.html Working with upstream repositories>.
updateRepository_upstreams :: Lens.Lens' UpdateRepository (Prelude.Maybe [UpstreamRepository])
updateRepository_upstreams = Lens.lens (\UpdateRepository' {upstreams} -> upstreams) (\s@UpdateRepository' {} a -> s {upstreams = a} :: UpdateRepository) Prelude.. Lens.mapping Lens.coerced

-- | The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
updateRepository_domainOwner :: Lens.Lens' UpdateRepository (Prelude.Maybe Prelude.Text)
updateRepository_domainOwner = Lens.lens (\UpdateRepository' {domainOwner} -> domainOwner) (\s@UpdateRepository' {} a -> s {domainOwner = a} :: UpdateRepository)

-- | An updated repository description.
updateRepository_description :: Lens.Lens' UpdateRepository (Prelude.Maybe Prelude.Text)
updateRepository_description = Lens.lens (\UpdateRepository' {description} -> description) (\s@UpdateRepository' {} a -> s {description = a} :: UpdateRepository)

-- | The name of the domain associated with the repository to update.
updateRepository_domain :: Lens.Lens' UpdateRepository Prelude.Text
updateRepository_domain = Lens.lens (\UpdateRepository' {domain} -> domain) (\s@UpdateRepository' {} a -> s {domain = a} :: UpdateRepository)

-- | The name of the repository to update.
updateRepository_repository :: Lens.Lens' UpdateRepository Prelude.Text
updateRepository_repository = Lens.lens (\UpdateRepository' {repository} -> repository) (\s@UpdateRepository' {} a -> s {repository = a} :: UpdateRepository)

instance Core.AWSRequest UpdateRepository where
  type
    AWSResponse UpdateRepository =
      UpdateRepositoryResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRepositoryResponse'
            Prelude.<$> (x Core..?> "repository")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRepository where
  hashWithSalt salt' UpdateRepository' {..} =
    salt' `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` upstreams

instance Prelude.NFData UpdateRepository where
  rnf UpdateRepository' {..} =
    Prelude.rnf upstreams
      `Prelude.seq` Prelude.rnf repository
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf domainOwner

instance Core.ToHeaders UpdateRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRepository where
  toJSON UpdateRepository' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("upstreams" Core..=) Prelude.<$> upstreams,
            ("description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateRepository where
  toPath = Prelude.const "/v1/repository"

instance Core.ToQuery UpdateRepository where
  toQuery UpdateRepository' {..} =
    Prelude.mconcat
      [ "domain-owner" Core.=: domainOwner,
        "domain" Core.=: domain,
        "repository" Core.=: repository
      ]

-- | /See:/ 'newUpdateRepositoryResponse' smart constructor.
data UpdateRepositoryResponse = UpdateRepositoryResponse'
  { -- | The updated repository.
    repository :: Prelude.Maybe RepositoryDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repository', 'updateRepositoryResponse_repository' - The updated repository.
--
-- 'httpStatus', 'updateRepositoryResponse_httpStatus' - The response's http status code.
newUpdateRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRepositoryResponse
newUpdateRepositoryResponse pHttpStatus_ =
  UpdateRepositoryResponse'
    { repository =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The updated repository.
updateRepositoryResponse_repository :: Lens.Lens' UpdateRepositoryResponse (Prelude.Maybe RepositoryDescription)
updateRepositoryResponse_repository = Lens.lens (\UpdateRepositoryResponse' {repository} -> repository) (\s@UpdateRepositoryResponse' {} a -> s {repository = a} :: UpdateRepositoryResponse)

-- | The response's http status code.
updateRepositoryResponse_httpStatus :: Lens.Lens' UpdateRepositoryResponse Prelude.Int
updateRepositoryResponse_httpStatus = Lens.lens (\UpdateRepositoryResponse' {httpStatus} -> httpStatus) (\s@UpdateRepositoryResponse' {} a -> s {httpStatus = a} :: UpdateRepositoryResponse)

instance Prelude.NFData UpdateRepositoryResponse where
  rnf UpdateRepositoryResponse' {..} =
    Prelude.rnf repository
      `Prelude.seq` Prelude.rnf httpStatus
