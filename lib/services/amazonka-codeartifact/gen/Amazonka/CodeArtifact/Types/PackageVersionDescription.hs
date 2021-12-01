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
-- Module      : Amazonka.CodeArtifact.Types.PackageVersionDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.PackageVersionDescription where

import Amazonka.CodeArtifact.Types.LicenseInfo
import Amazonka.CodeArtifact.Types.PackageFormat
import Amazonka.CodeArtifact.Types.PackageVersionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about a package version.
--
-- /See:/ 'newPackageVersionDescription' smart constructor.
data PackageVersionDescription = PackageVersionDescription'
  { -- | A summary of the package version. The summary is extracted from the
    -- package. The information in and detail level of the summary depends on
    -- the package version\'s format.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The homepage associated with the package.
    homePage :: Prelude.Maybe Prelude.Text,
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
    status :: Prelude.Maybe PackageVersionStatus,
    -- | The repository for the source code in the package version, or the source
    -- code used to build it.
    sourceCodeRepository :: Prelude.Maybe Prelude.Text,
    -- | The name of the requested package.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | A timestamp that contains the date and time the package version was
    -- published.
    publishedTime :: Prelude.Maybe Core.POSIX,
    -- | The format of the package version. The valid package formats are:
    --
    -- -   @npm@: A Node Package Manager (npm) package.
    --
    -- -   @pypi@: A Python Package Index (PyPI) package.
    --
    -- -   @maven@: A Maven package that contains compiled code in a
    --     distributable format, such as a JAR file.
    format :: Prelude.Maybe PackageFormat,
    -- | The namespace of the package. The package component that specifies its
    -- namespace depends on its type. For example:
    --
    -- -   The namespace of a Maven package is its @groupId@.
    --
    -- -   The namespace of an npm package is its @scope@.
    --
    -- -   A Python package does not contain a corresponding component, so
    --     Python packages do not have a namespace.
    namespace :: Prelude.Maybe Prelude.Text,
    -- | The version of the package.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the package that is displayed. The @displayName@ varies
    -- depending on the package version\'s format. For example, if an npm
    -- package is named @ui@, is in the namespace @vue@, and has the format
    -- @npm@, then the @displayName@ is @\@vue\/ui@.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Information about licenses associated with the package version.
    licenses :: Prelude.Maybe [LicenseInfo],
    -- | The revision of the package version.
    revision :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageVersionDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'packageVersionDescription_summary' - A summary of the package version. The summary is extracted from the
-- package. The information in and detail level of the summary depends on
-- the package version\'s format.
--
-- 'homePage', 'packageVersionDescription_homePage' - The homepage associated with the package.
--
-- 'status', 'packageVersionDescription_status' - A string that contains the status of the package version. It can be one
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
--
-- 'sourceCodeRepository', 'packageVersionDescription_sourceCodeRepository' - The repository for the source code in the package version, or the source
-- code used to build it.
--
-- 'packageName', 'packageVersionDescription_packageName' - The name of the requested package.
--
-- 'publishedTime', 'packageVersionDescription_publishedTime' - A timestamp that contains the date and time the package version was
-- published.
--
-- 'format', 'packageVersionDescription_format' - The format of the package version. The valid package formats are:
--
-- -   @npm@: A Node Package Manager (npm) package.
--
-- -   @pypi@: A Python Package Index (PyPI) package.
--
-- -   @maven@: A Maven package that contains compiled code in a
--     distributable format, such as a JAR file.
--
-- 'namespace', 'packageVersionDescription_namespace' - The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
--
-- 'version', 'packageVersionDescription_version' - The version of the package.
--
-- 'displayName', 'packageVersionDescription_displayName' - The name of the package that is displayed. The @displayName@ varies
-- depending on the package version\'s format. For example, if an npm
-- package is named @ui@, is in the namespace @vue@, and has the format
-- @npm@, then the @displayName@ is @\@vue\/ui@.
--
-- 'licenses', 'packageVersionDescription_licenses' - Information about licenses associated with the package version.
--
-- 'revision', 'packageVersionDescription_revision' - The revision of the package version.
newPackageVersionDescription ::
  PackageVersionDescription
newPackageVersionDescription =
  PackageVersionDescription'
    { summary =
        Prelude.Nothing,
      homePage = Prelude.Nothing,
      status = Prelude.Nothing,
      sourceCodeRepository = Prelude.Nothing,
      packageName = Prelude.Nothing,
      publishedTime = Prelude.Nothing,
      format = Prelude.Nothing,
      namespace = Prelude.Nothing,
      version = Prelude.Nothing,
      displayName = Prelude.Nothing,
      licenses = Prelude.Nothing,
      revision = Prelude.Nothing
    }

-- | A summary of the package version. The summary is extracted from the
-- package. The information in and detail level of the summary depends on
-- the package version\'s format.
packageVersionDescription_summary :: Lens.Lens' PackageVersionDescription (Prelude.Maybe Prelude.Text)
packageVersionDescription_summary = Lens.lens (\PackageVersionDescription' {summary} -> summary) (\s@PackageVersionDescription' {} a -> s {summary = a} :: PackageVersionDescription)

-- | The homepage associated with the package.
packageVersionDescription_homePage :: Lens.Lens' PackageVersionDescription (Prelude.Maybe Prelude.Text)
packageVersionDescription_homePage = Lens.lens (\PackageVersionDescription' {homePage} -> homePage) (\s@PackageVersionDescription' {} a -> s {homePage = a} :: PackageVersionDescription)

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
packageVersionDescription_status :: Lens.Lens' PackageVersionDescription (Prelude.Maybe PackageVersionStatus)
packageVersionDescription_status = Lens.lens (\PackageVersionDescription' {status} -> status) (\s@PackageVersionDescription' {} a -> s {status = a} :: PackageVersionDescription)

-- | The repository for the source code in the package version, or the source
-- code used to build it.
packageVersionDescription_sourceCodeRepository :: Lens.Lens' PackageVersionDescription (Prelude.Maybe Prelude.Text)
packageVersionDescription_sourceCodeRepository = Lens.lens (\PackageVersionDescription' {sourceCodeRepository} -> sourceCodeRepository) (\s@PackageVersionDescription' {} a -> s {sourceCodeRepository = a} :: PackageVersionDescription)

-- | The name of the requested package.
packageVersionDescription_packageName :: Lens.Lens' PackageVersionDescription (Prelude.Maybe Prelude.Text)
packageVersionDescription_packageName = Lens.lens (\PackageVersionDescription' {packageName} -> packageName) (\s@PackageVersionDescription' {} a -> s {packageName = a} :: PackageVersionDescription)

-- | A timestamp that contains the date and time the package version was
-- published.
packageVersionDescription_publishedTime :: Lens.Lens' PackageVersionDescription (Prelude.Maybe Prelude.UTCTime)
packageVersionDescription_publishedTime = Lens.lens (\PackageVersionDescription' {publishedTime} -> publishedTime) (\s@PackageVersionDescription' {} a -> s {publishedTime = a} :: PackageVersionDescription) Prelude.. Lens.mapping Core._Time

-- | The format of the package version. The valid package formats are:
--
-- -   @npm@: A Node Package Manager (npm) package.
--
-- -   @pypi@: A Python Package Index (PyPI) package.
--
-- -   @maven@: A Maven package that contains compiled code in a
--     distributable format, such as a JAR file.
packageVersionDescription_format :: Lens.Lens' PackageVersionDescription (Prelude.Maybe PackageFormat)
packageVersionDescription_format = Lens.lens (\PackageVersionDescription' {format} -> format) (\s@PackageVersionDescription' {} a -> s {format = a} :: PackageVersionDescription)

-- | The namespace of the package. The package component that specifies its
-- namespace depends on its type. For example:
--
-- -   The namespace of a Maven package is its @groupId@.
--
-- -   The namespace of an npm package is its @scope@.
--
-- -   A Python package does not contain a corresponding component, so
--     Python packages do not have a namespace.
packageVersionDescription_namespace :: Lens.Lens' PackageVersionDescription (Prelude.Maybe Prelude.Text)
packageVersionDescription_namespace = Lens.lens (\PackageVersionDescription' {namespace} -> namespace) (\s@PackageVersionDescription' {} a -> s {namespace = a} :: PackageVersionDescription)

-- | The version of the package.
packageVersionDescription_version :: Lens.Lens' PackageVersionDescription (Prelude.Maybe Prelude.Text)
packageVersionDescription_version = Lens.lens (\PackageVersionDescription' {version} -> version) (\s@PackageVersionDescription' {} a -> s {version = a} :: PackageVersionDescription)

-- | The name of the package that is displayed. The @displayName@ varies
-- depending on the package version\'s format. For example, if an npm
-- package is named @ui@, is in the namespace @vue@, and has the format
-- @npm@, then the @displayName@ is @\@vue\/ui@.
packageVersionDescription_displayName :: Lens.Lens' PackageVersionDescription (Prelude.Maybe Prelude.Text)
packageVersionDescription_displayName = Lens.lens (\PackageVersionDescription' {displayName} -> displayName) (\s@PackageVersionDescription' {} a -> s {displayName = a} :: PackageVersionDescription)

-- | Information about licenses associated with the package version.
packageVersionDescription_licenses :: Lens.Lens' PackageVersionDescription (Prelude.Maybe [LicenseInfo])
packageVersionDescription_licenses = Lens.lens (\PackageVersionDescription' {licenses} -> licenses) (\s@PackageVersionDescription' {} a -> s {licenses = a} :: PackageVersionDescription) Prelude.. Lens.mapping Lens.coerced

-- | The revision of the package version.
packageVersionDescription_revision :: Lens.Lens' PackageVersionDescription (Prelude.Maybe Prelude.Text)
packageVersionDescription_revision = Lens.lens (\PackageVersionDescription' {revision} -> revision) (\s@PackageVersionDescription' {} a -> s {revision = a} :: PackageVersionDescription)

instance Core.FromJSON PackageVersionDescription where
  parseJSON =
    Core.withObject
      "PackageVersionDescription"
      ( \x ->
          PackageVersionDescription'
            Prelude.<$> (x Core..:? "summary")
            Prelude.<*> (x Core..:? "homePage")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "sourceCodeRepository")
            Prelude.<*> (x Core..:? "packageName")
            Prelude.<*> (x Core..:? "publishedTime")
            Prelude.<*> (x Core..:? "format")
            Prelude.<*> (x Core..:? "namespace")
            Prelude.<*> (x Core..:? "version")
            Prelude.<*> (x Core..:? "displayName")
            Prelude.<*> (x Core..:? "licenses" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "revision")
      )

instance Prelude.Hashable PackageVersionDescription where
  hashWithSalt salt' PackageVersionDescription' {..} =
    salt' `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` licenses
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` namespace
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` publishedTime
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` sourceCodeRepository
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` homePage
      `Prelude.hashWithSalt` summary

instance Prelude.NFData PackageVersionDescription where
  rnf PackageVersionDescription' {..} =
    Prelude.rnf summary
      `Prelude.seq` Prelude.rnf revision
      `Prelude.seq` Prelude.rnf licenses
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf namespace
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf publishedTime
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf sourceCodeRepository
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf homePage
