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
-- Module      : Amazonka.Lambda.Types.FunctionCodeLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lambda.Types.FunctionCodeLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a function\'s deployment package.
--
-- /See:/ 'newFunctionCodeLocation' smart constructor.
data FunctionCodeLocation = FunctionCodeLocation'
  { -- | A presigned URL that you can use to download the deployment package.
    location :: Prelude.Maybe Prelude.Text,
    -- | The resolved URI for the image.
    resolvedImageUri :: Prelude.Maybe Prelude.Text,
    -- | URI of a container image in the Amazon ECR registry.
    imageUri :: Prelude.Maybe Prelude.Text,
    -- | The service that\'s hosting the file.
    repositoryType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FunctionCodeLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'functionCodeLocation_location' - A presigned URL that you can use to download the deployment package.
--
-- 'resolvedImageUri', 'functionCodeLocation_resolvedImageUri' - The resolved URI for the image.
--
-- 'imageUri', 'functionCodeLocation_imageUri' - URI of a container image in the Amazon ECR registry.
--
-- 'repositoryType', 'functionCodeLocation_repositoryType' - The service that\'s hosting the file.
newFunctionCodeLocation ::
  FunctionCodeLocation
newFunctionCodeLocation =
  FunctionCodeLocation'
    { location = Prelude.Nothing,
      resolvedImageUri = Prelude.Nothing,
      imageUri = Prelude.Nothing,
      repositoryType = Prelude.Nothing
    }

-- | A presigned URL that you can use to download the deployment package.
functionCodeLocation_location :: Lens.Lens' FunctionCodeLocation (Prelude.Maybe Prelude.Text)
functionCodeLocation_location = Lens.lens (\FunctionCodeLocation' {location} -> location) (\s@FunctionCodeLocation' {} a -> s {location = a} :: FunctionCodeLocation)

-- | The resolved URI for the image.
functionCodeLocation_resolvedImageUri :: Lens.Lens' FunctionCodeLocation (Prelude.Maybe Prelude.Text)
functionCodeLocation_resolvedImageUri = Lens.lens (\FunctionCodeLocation' {resolvedImageUri} -> resolvedImageUri) (\s@FunctionCodeLocation' {} a -> s {resolvedImageUri = a} :: FunctionCodeLocation)

-- | URI of a container image in the Amazon ECR registry.
functionCodeLocation_imageUri :: Lens.Lens' FunctionCodeLocation (Prelude.Maybe Prelude.Text)
functionCodeLocation_imageUri = Lens.lens (\FunctionCodeLocation' {imageUri} -> imageUri) (\s@FunctionCodeLocation' {} a -> s {imageUri = a} :: FunctionCodeLocation)

-- | The service that\'s hosting the file.
functionCodeLocation_repositoryType :: Lens.Lens' FunctionCodeLocation (Prelude.Maybe Prelude.Text)
functionCodeLocation_repositoryType = Lens.lens (\FunctionCodeLocation' {repositoryType} -> repositoryType) (\s@FunctionCodeLocation' {} a -> s {repositoryType = a} :: FunctionCodeLocation)

instance Core.FromJSON FunctionCodeLocation where
  parseJSON =
    Core.withObject
      "FunctionCodeLocation"
      ( \x ->
          FunctionCodeLocation'
            Prelude.<$> (x Core..:? "Location")
            Prelude.<*> (x Core..:? "ResolvedImageUri")
            Prelude.<*> (x Core..:? "ImageUri")
            Prelude.<*> (x Core..:? "RepositoryType")
      )

instance Prelude.Hashable FunctionCodeLocation where
  hashWithSalt salt' FunctionCodeLocation' {..} =
    salt' `Prelude.hashWithSalt` repositoryType
      `Prelude.hashWithSalt` imageUri
      `Prelude.hashWithSalt` resolvedImageUri
      `Prelude.hashWithSalt` location

instance Prelude.NFData FunctionCodeLocation where
  rnf FunctionCodeLocation' {..} =
    Prelude.rnf location
      `Prelude.seq` Prelude.rnf repositoryType
      `Prelude.seq` Prelude.rnf imageUri
      `Prelude.seq` Prelude.rnf resolvedImageUri
