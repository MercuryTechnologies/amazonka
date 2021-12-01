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
-- Module      : Amazonka.CodeArtifact.Types.PackageVersionSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageVersionSummary where

import Amazonka.CodeArtifact.Types.PackageVersionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a package version, including its status, version, and
-- revision. The
-- <https://docs.aws.amazon.com/codeartifact/latest/APIReference/API_ListPackageVersions.html ListPackageVersions>
-- operation returns a list of @PackageVersionSummary@ objects.
--
-- /See:/ 'newPackageVersionSummary' smart constructor.
data PackageVersionSummary = PackageVersionSummary'
  { -- | The revision associated with a package version.
    revision :: Prelude.Maybe Prelude.Text,
    -- | Information about a package version.
    version :: Prelude.Text,
    -- | A string that contains the status of the package version. It can be one
    -- of the following:
    --
    -- -   @Published@
    --
    -- -   @Unfinished@
    --
    -- -   @Unlisted@
    --
    -- -   @Archived@
    --
    -- -   @Disposed@
    status :: PackageVersionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageVersionSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revision', 'packageVersionSummary_revision' - The revision associated with a package version.
--
-- 'version', 'packageVersionSummary_version' - Information about a package version.
--
-- 'status', 'packageVersionSummary_status' - A string that contains the status of the package version. It can be one
-- of the following:
--
-- -   @Published@
--
-- -   @Unfinished@
--
-- -   @Unlisted@
--
-- -   @Archived@
--
-- -   @Disposed@
newPackageVersionSummary ::
  -- | 'version'
  Prelude.Text ->
  -- | 'status'
  PackageVersionStatus ->
  PackageVersionSummary
newPackageVersionSummary pVersion_ pStatus_ =
  PackageVersionSummary'
    { revision = Prelude.Nothing,
      version = pVersion_,
      status = pStatus_
    }

-- | The revision associated with a package version.
packageVersionSummary_revision :: Lens.Lens' PackageVersionSummary (Prelude.Maybe Prelude.Text)
packageVersionSummary_revision = Lens.lens (\PackageVersionSummary' {revision} -> revision) (\s@PackageVersionSummary' {} a -> s {revision = a} :: PackageVersionSummary)

-- | Information about a package version.
packageVersionSummary_version :: Lens.Lens' PackageVersionSummary Prelude.Text
packageVersionSummary_version = Lens.lens (\PackageVersionSummary' {version} -> version) (\s@PackageVersionSummary' {} a -> s {version = a} :: PackageVersionSummary)

-- | A string that contains the status of the package version. It can be one
-- of the following:
--
-- -   @Published@
--
-- -   @Unfinished@
--
-- -   @Unlisted@
--
-- -   @Archived@
--
-- -   @Disposed@
packageVersionSummary_status :: Lens.Lens' PackageVersionSummary PackageVersionStatus
packageVersionSummary_status = Lens.lens (\PackageVersionSummary' {status} -> status) (\s@PackageVersionSummary' {} a -> s {status = a} :: PackageVersionSummary)

instance Core.FromJSON PackageVersionSummary where
  parseJSON =
    Core.withObject
      "PackageVersionSummary"
      ( \x ->
          PackageVersionSummary'
            Prelude.<$> (x Core..:? "revision")
            Prelude.<*> (x Core..: "version")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable PackageVersionSummary where
  hashWithSalt salt' PackageVersionSummary' {..} =
    salt' `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` revision

instance Prelude.NFData PackageVersionSummary where
  rnf PackageVersionSummary' {..} =
    Prelude.rnf revision
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf version
