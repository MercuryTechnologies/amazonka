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
-- Module      : Amazonka.CodeCommit.Types.RepositoryMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.RepositoryMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a repository.
--
-- /See:/ 'newRepositoryMetadata' smart constructor.
data RepositoryMetadata = RepositoryMetadata'
  { -- | A comment or description about the repository.
    repositoryDescription :: Prelude.Maybe Prelude.Text,
    -- | The date and time the repository was last modified, in timestamp format.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the repository.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The URL to use for cloning the repository over HTTPS.
    cloneUrlHttp :: Prelude.Maybe Prelude.Text,
    -- | The ID of the AWS account associated with the repository.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The repository\'s default branch name.
    defaultBranch :: Prelude.Maybe Prelude.Text,
    -- | The ID of the repository.
    repositoryId :: Prelude.Maybe Prelude.Text,
    -- | The repository\'s name.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The date and time the repository was created, in timestamp format.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The URL to use for cloning the repository over SSH.
    cloneUrlSsh :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryDescription', 'repositoryMetadata_repositoryDescription' - A comment or description about the repository.
--
-- 'lastModifiedDate', 'repositoryMetadata_lastModifiedDate' - The date and time the repository was last modified, in timestamp format.
--
-- 'arn', 'repositoryMetadata_arn' - The Amazon Resource Name (ARN) of the repository.
--
-- 'cloneUrlHttp', 'repositoryMetadata_cloneUrlHttp' - The URL to use for cloning the repository over HTTPS.
--
-- 'accountId', 'repositoryMetadata_accountId' - The ID of the AWS account associated with the repository.
--
-- 'defaultBranch', 'repositoryMetadata_defaultBranch' - The repository\'s default branch name.
--
-- 'repositoryId', 'repositoryMetadata_repositoryId' - The ID of the repository.
--
-- 'repositoryName', 'repositoryMetadata_repositoryName' - The repository\'s name.
--
-- 'creationDate', 'repositoryMetadata_creationDate' - The date and time the repository was created, in timestamp format.
--
-- 'cloneUrlSsh', 'repositoryMetadata_cloneUrlSsh' - The URL to use for cloning the repository over SSH.
newRepositoryMetadata ::
  RepositoryMetadata
newRepositoryMetadata =
  RepositoryMetadata'
    { repositoryDescription =
        Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      arn = Prelude.Nothing,
      cloneUrlHttp = Prelude.Nothing,
      accountId = Prelude.Nothing,
      defaultBranch = Prelude.Nothing,
      repositoryId = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      cloneUrlSsh = Prelude.Nothing
    }

-- | A comment or description about the repository.
repositoryMetadata_repositoryDescription :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_repositoryDescription = Lens.lens (\RepositoryMetadata' {repositoryDescription} -> repositoryDescription) (\s@RepositoryMetadata' {} a -> s {repositoryDescription = a} :: RepositoryMetadata)

-- | The date and time the repository was last modified, in timestamp format.
repositoryMetadata_lastModifiedDate :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.UTCTime)
repositoryMetadata_lastModifiedDate = Lens.lens (\RepositoryMetadata' {lastModifiedDate} -> lastModifiedDate) (\s@RepositoryMetadata' {} a -> s {lastModifiedDate = a} :: RepositoryMetadata) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the repository.
repositoryMetadata_arn :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_arn = Lens.lens (\RepositoryMetadata' {arn} -> arn) (\s@RepositoryMetadata' {} a -> s {arn = a} :: RepositoryMetadata)

-- | The URL to use for cloning the repository over HTTPS.
repositoryMetadata_cloneUrlHttp :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_cloneUrlHttp = Lens.lens (\RepositoryMetadata' {cloneUrlHttp} -> cloneUrlHttp) (\s@RepositoryMetadata' {} a -> s {cloneUrlHttp = a} :: RepositoryMetadata)

-- | The ID of the AWS account associated with the repository.
repositoryMetadata_accountId :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_accountId = Lens.lens (\RepositoryMetadata' {accountId} -> accountId) (\s@RepositoryMetadata' {} a -> s {accountId = a} :: RepositoryMetadata)

-- | The repository\'s default branch name.
repositoryMetadata_defaultBranch :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_defaultBranch = Lens.lens (\RepositoryMetadata' {defaultBranch} -> defaultBranch) (\s@RepositoryMetadata' {} a -> s {defaultBranch = a} :: RepositoryMetadata)

-- | The ID of the repository.
repositoryMetadata_repositoryId :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_repositoryId = Lens.lens (\RepositoryMetadata' {repositoryId} -> repositoryId) (\s@RepositoryMetadata' {} a -> s {repositoryId = a} :: RepositoryMetadata)

-- | The repository\'s name.
repositoryMetadata_repositoryName :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_repositoryName = Lens.lens (\RepositoryMetadata' {repositoryName} -> repositoryName) (\s@RepositoryMetadata' {} a -> s {repositoryName = a} :: RepositoryMetadata)

-- | The date and time the repository was created, in timestamp format.
repositoryMetadata_creationDate :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.UTCTime)
repositoryMetadata_creationDate = Lens.lens (\RepositoryMetadata' {creationDate} -> creationDate) (\s@RepositoryMetadata' {} a -> s {creationDate = a} :: RepositoryMetadata) Prelude.. Lens.mapping Core._Time

-- | The URL to use for cloning the repository over SSH.
repositoryMetadata_cloneUrlSsh :: Lens.Lens' RepositoryMetadata (Prelude.Maybe Prelude.Text)
repositoryMetadata_cloneUrlSsh = Lens.lens (\RepositoryMetadata' {cloneUrlSsh} -> cloneUrlSsh) (\s@RepositoryMetadata' {} a -> s {cloneUrlSsh = a} :: RepositoryMetadata)

instance Core.FromJSON RepositoryMetadata where
  parseJSON =
    Core.withObject
      "RepositoryMetadata"
      ( \x ->
          RepositoryMetadata'
            Prelude.<$> (x Core..:? "repositoryDescription")
            Prelude.<*> (x Core..:? "lastModifiedDate")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "cloneUrlHttp")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "defaultBranch")
            Prelude.<*> (x Core..:? "repositoryId")
            Prelude.<*> (x Core..:? "repositoryName")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "cloneUrlSsh")
      )

instance Prelude.Hashable RepositoryMetadata where
  hashWithSalt salt' RepositoryMetadata' {..} =
    salt' `Prelude.hashWithSalt` cloneUrlSsh
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` repositoryId
      `Prelude.hashWithSalt` defaultBranch
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` cloneUrlHttp
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` repositoryDescription

instance Prelude.NFData RepositoryMetadata where
  rnf RepositoryMetadata' {..} =
    Prelude.rnf repositoryDescription
      `Prelude.seq` Prelude.rnf cloneUrlSsh
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf repositoryId
      `Prelude.seq` Prelude.rnf defaultBranch
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf cloneUrlHttp
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastModifiedDate
