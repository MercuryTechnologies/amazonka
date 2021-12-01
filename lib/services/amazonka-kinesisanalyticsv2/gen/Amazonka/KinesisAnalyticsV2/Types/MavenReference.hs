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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.MavenReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.MavenReference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The information required to specify a Maven reference. You can use Maven
-- references to specify dependency JAR files.
--
-- /See:/ 'newMavenReference' smart constructor.
data MavenReference = MavenReference'
  { -- | The group ID of the Maven reference.
    groupId :: Prelude.Text,
    -- | The artifact ID of the Maven reference.
    artifactId :: Prelude.Text,
    -- | The version of the Maven reference.
    version :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MavenReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupId', 'mavenReference_groupId' - The group ID of the Maven reference.
--
-- 'artifactId', 'mavenReference_artifactId' - The artifact ID of the Maven reference.
--
-- 'version', 'mavenReference_version' - The version of the Maven reference.
newMavenReference ::
  -- | 'groupId'
  Prelude.Text ->
  -- | 'artifactId'
  Prelude.Text ->
  -- | 'version'
  Prelude.Text ->
  MavenReference
newMavenReference pGroupId_ pArtifactId_ pVersion_ =
  MavenReference'
    { groupId = pGroupId_,
      artifactId = pArtifactId_,
      version = pVersion_
    }

-- | The group ID of the Maven reference.
mavenReference_groupId :: Lens.Lens' MavenReference Prelude.Text
mavenReference_groupId = Lens.lens (\MavenReference' {groupId} -> groupId) (\s@MavenReference' {} a -> s {groupId = a} :: MavenReference)

-- | The artifact ID of the Maven reference.
mavenReference_artifactId :: Lens.Lens' MavenReference Prelude.Text
mavenReference_artifactId = Lens.lens (\MavenReference' {artifactId} -> artifactId) (\s@MavenReference' {} a -> s {artifactId = a} :: MavenReference)

-- | The version of the Maven reference.
mavenReference_version :: Lens.Lens' MavenReference Prelude.Text
mavenReference_version = Lens.lens (\MavenReference' {version} -> version) (\s@MavenReference' {} a -> s {version = a} :: MavenReference)

instance Core.FromJSON MavenReference where
  parseJSON =
    Core.withObject
      "MavenReference"
      ( \x ->
          MavenReference'
            Prelude.<$> (x Core..: "GroupId")
            Prelude.<*> (x Core..: "ArtifactId")
            Prelude.<*> (x Core..: "Version")
      )

instance Prelude.Hashable MavenReference where
  hashWithSalt salt' MavenReference' {..} =
    salt' `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` artifactId
      `Prelude.hashWithSalt` groupId

instance Prelude.NFData MavenReference where
  rnf MavenReference' {..} =
    Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf artifactId

instance Core.ToJSON MavenReference where
  toJSON MavenReference' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GroupId" Core..= groupId),
            Prelude.Just ("ArtifactId" Core..= artifactId),
            Prelude.Just ("Version" Core..= version)
          ]
      )
