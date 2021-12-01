{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CodeArtifact.Types.RepositorySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.RepositorySummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a repository, including its Amazon Resource Name (ARN),
-- description, and domain information. The
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_ListRepositories.html ListRepositories>
-- operation returns a list of @RepositorySummary@ objects.
--
-- /See:/ 'newRepositorySummary' smart constructor.
data RepositorySummary = RepositorySummary'
  { -- | The ARN of the repository.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account number of the AWS account that owns the domain. It
    -- does not include dashes or spaces.
    domainOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the domain that contains the repository.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The name of the repository.
    name :: Prelude.Maybe Prelude.Text,
    -- | The AWS account ID that manages the repository.
    administratorAccount :: Prelude.Maybe Prelude.Text,
    -- | The description of the repository.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositorySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'repositorySummary_arn' - The ARN of the repository.
--
-- 'domainOwner', 'repositorySummary_domainOwner' - The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
--
-- 'domainName', 'repositorySummary_domainName' - The name of the domain that contains the repository.
--
-- 'name', 'repositorySummary_name' - The name of the repository.
--
-- 'administratorAccount', 'repositorySummary_administratorAccount' - The AWS account ID that manages the repository.
--
-- 'description', 'repositorySummary_description' - The description of the repository.
newRepositorySummary ::
  RepositorySummary
newRepositorySummary =
  RepositorySummary'
    { arn = Prelude.Nothing,
      domainOwner = Prelude.Nothing,
      domainName = Prelude.Nothing,
      name = Prelude.Nothing,
      administratorAccount = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The ARN of the repository.
repositorySummary_arn :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_arn = Lens.lens (\RepositorySummary' {arn} -> arn) (\s@RepositorySummary' {} a -> s {arn = a} :: RepositorySummary)

-- | The 12-digit account number of the AWS account that owns the domain. It
-- does not include dashes or spaces.
repositorySummary_domainOwner :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_domainOwner = Lens.lens (\RepositorySummary' {domainOwner} -> domainOwner) (\s@RepositorySummary' {} a -> s {domainOwner = a} :: RepositorySummary)

-- | The name of the domain that contains the repository.
repositorySummary_domainName :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_domainName = Lens.lens (\RepositorySummary' {domainName} -> domainName) (\s@RepositorySummary' {} a -> s {domainName = a} :: RepositorySummary)

-- | The name of the repository.
repositorySummary_name :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_name = Lens.lens (\RepositorySummary' {name} -> name) (\s@RepositorySummary' {} a -> s {name = a} :: RepositorySummary)

-- | The AWS account ID that manages the repository.
repositorySummary_administratorAccount :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_administratorAccount = Lens.lens (\RepositorySummary' {administratorAccount} -> administratorAccount) (\s@RepositorySummary' {} a -> s {administratorAccount = a} :: RepositorySummary)

-- | The description of the repository.
repositorySummary_description :: Lens.Lens' RepositorySummary (Prelude.Maybe Prelude.Text)
repositorySummary_description = Lens.lens (\RepositorySummary' {description} -> description) (\s@RepositorySummary' {} a -> s {description = a} :: RepositorySummary)

instance Core.FromJSON RepositorySummary where
  parseJSON =
    Core.withObject
      "RepositorySummary"
      ( \x ->
          RepositorySummary'
            Prelude.<$> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "domainOwner")
            Prelude.<*> (x Core..:? "domainName")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "administratorAccount")
            Prelude.<*> (x Core..:? "description")
      )

instance Prelude.Hashable RepositorySummary where
  hashWithSalt salt' RepositorySummary' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` administratorAccount
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` domainOwner
      `Prelude.hashWithSalt` arn

instance Prelude.NFData RepositorySummary where
  rnf RepositorySummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf administratorAccount
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf domainOwner
