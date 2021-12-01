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
-- Module      : Amazonka.ECR.Types.Repository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.Repository where

import qualified Amazonka.Core as Core
import Amazonka.ECR.Types.EncryptionConfiguration
import Amazonka.ECR.Types.ImageScanningConfiguration
import Amazonka.ECR.Types.ImageTagMutability
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object representing a repository.
--
-- /See:/ 'newRepository' smart constructor.
data Repository = Repository'
  { -- | The Amazon Resource Name (ARN) that identifies the repository. The ARN
    -- contains the @arn:aws:ecr@ namespace, followed by the region of the
    -- repository, Amazon Web Services account ID of the repository owner,
    -- repository namespace, and repository name. For example,
    -- @arn:aws:ecr:region:012345678910:repository\/test@.
    repositoryArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in JavaScript date format, when the repository was
    -- created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Web Services account ID associated with the registry that
    -- contains the repository.
    registryId :: Prelude.Maybe Prelude.Text,
    imageScanningConfiguration :: Prelude.Maybe ImageScanningConfiguration,
    -- | The URI for the repository. You can use this URI for container image
    -- @push@ and @pull@ operations.
    repositoryUri :: Prelude.Maybe Prelude.Text,
    -- | The encryption configuration for the repository. This determines how the
    -- contents of your repository are encrypted at rest.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | The name of the repository.
    repositoryName :: Prelude.Maybe Prelude.Text,
    -- | The tag mutability setting for the repository.
    imageTagMutability :: Prelude.Maybe ImageTagMutability
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Repository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryArn', 'repository_repositoryArn' - The Amazon Resource Name (ARN) that identifies the repository. The ARN
-- contains the @arn:aws:ecr@ namespace, followed by the region of the
-- repository, Amazon Web Services account ID of the repository owner,
-- repository namespace, and repository name. For example,
-- @arn:aws:ecr:region:012345678910:repository\/test@.
--
-- 'createdAt', 'repository_createdAt' - The date and time, in JavaScript date format, when the repository was
-- created.
--
-- 'registryId', 'repository_registryId' - The Amazon Web Services account ID associated with the registry that
-- contains the repository.
--
-- 'imageScanningConfiguration', 'repository_imageScanningConfiguration' - Undocumented member.
--
-- 'repositoryUri', 'repository_repositoryUri' - The URI for the repository. You can use this URI for container image
-- @push@ and @pull@ operations.
--
-- 'encryptionConfiguration', 'repository_encryptionConfiguration' - The encryption configuration for the repository. This determines how the
-- contents of your repository are encrypted at rest.
--
-- 'repositoryName', 'repository_repositoryName' - The name of the repository.
--
-- 'imageTagMutability', 'repository_imageTagMutability' - The tag mutability setting for the repository.
newRepository ::
  Repository
newRepository =
  Repository'
    { repositoryArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      registryId = Prelude.Nothing,
      imageScanningConfiguration = Prelude.Nothing,
      repositoryUri = Prelude.Nothing,
      encryptionConfiguration = Prelude.Nothing,
      repositoryName = Prelude.Nothing,
      imageTagMutability = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) that identifies the repository. The ARN
-- contains the @arn:aws:ecr@ namespace, followed by the region of the
-- repository, Amazon Web Services account ID of the repository owner,
-- repository namespace, and repository name. For example,
-- @arn:aws:ecr:region:012345678910:repository\/test@.
repository_repositoryArn :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_repositoryArn = Lens.lens (\Repository' {repositoryArn} -> repositoryArn) (\s@Repository' {} a -> s {repositoryArn = a} :: Repository)

-- | The date and time, in JavaScript date format, when the repository was
-- created.
repository_createdAt :: Lens.Lens' Repository (Prelude.Maybe Prelude.UTCTime)
repository_createdAt = Lens.lens (\Repository' {createdAt} -> createdAt) (\s@Repository' {} a -> s {createdAt = a} :: Repository) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services account ID associated with the registry that
-- contains the repository.
repository_registryId :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_registryId = Lens.lens (\Repository' {registryId} -> registryId) (\s@Repository' {} a -> s {registryId = a} :: Repository)

-- | Undocumented member.
repository_imageScanningConfiguration :: Lens.Lens' Repository (Prelude.Maybe ImageScanningConfiguration)
repository_imageScanningConfiguration = Lens.lens (\Repository' {imageScanningConfiguration} -> imageScanningConfiguration) (\s@Repository' {} a -> s {imageScanningConfiguration = a} :: Repository)

-- | The URI for the repository. You can use this URI for container image
-- @push@ and @pull@ operations.
repository_repositoryUri :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_repositoryUri = Lens.lens (\Repository' {repositoryUri} -> repositoryUri) (\s@Repository' {} a -> s {repositoryUri = a} :: Repository)

-- | The encryption configuration for the repository. This determines how the
-- contents of your repository are encrypted at rest.
repository_encryptionConfiguration :: Lens.Lens' Repository (Prelude.Maybe EncryptionConfiguration)
repository_encryptionConfiguration = Lens.lens (\Repository' {encryptionConfiguration} -> encryptionConfiguration) (\s@Repository' {} a -> s {encryptionConfiguration = a} :: Repository)

-- | The name of the repository.
repository_repositoryName :: Lens.Lens' Repository (Prelude.Maybe Prelude.Text)
repository_repositoryName = Lens.lens (\Repository' {repositoryName} -> repositoryName) (\s@Repository' {} a -> s {repositoryName = a} :: Repository)

-- | The tag mutability setting for the repository.
repository_imageTagMutability :: Lens.Lens' Repository (Prelude.Maybe ImageTagMutability)
repository_imageTagMutability = Lens.lens (\Repository' {imageTagMutability} -> imageTagMutability) (\s@Repository' {} a -> s {imageTagMutability = a} :: Repository)

instance Core.FromJSON Repository where
  parseJSON =
    Core.withObject
      "Repository"
      ( \x ->
          Repository'
            Prelude.<$> (x Core..:? "repositoryArn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "registryId")
            Prelude.<*> (x Core..:? "imageScanningConfiguration")
            Prelude.<*> (x Core..:? "repositoryUri")
            Prelude.<*> (x Core..:? "encryptionConfiguration")
            Prelude.<*> (x Core..:? "repositoryName")
            Prelude.<*> (x Core..:? "imageTagMutability")
      )

instance Prelude.Hashable Repository where
  hashWithSalt salt' Repository' {..} =
    salt' `Prelude.hashWithSalt` imageTagMutability
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` encryptionConfiguration
      `Prelude.hashWithSalt` repositoryUri
      `Prelude.hashWithSalt` imageScanningConfiguration
      `Prelude.hashWithSalt` registryId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` repositoryArn

instance Prelude.NFData Repository where
  rnf Repository' {..} =
    Prelude.rnf repositoryArn
      `Prelude.seq` Prelude.rnf imageTagMutability
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf encryptionConfiguration
      `Prelude.seq` Prelude.rnf repositoryUri
      `Prelude.seq` Prelude.rnf imageScanningConfiguration
      `Prelude.seq` Prelude.rnf registryId
      `Prelude.seq` Prelude.rnf createdAt
