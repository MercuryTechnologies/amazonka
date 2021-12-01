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
-- Module      : Amazonka.ImportExport.Types.Artifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImportExport.Types.Artifact where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A discrete item that contains the description and URL of an artifact
-- (such as a PDF).
--
-- /See:/ 'newArtifact' smart constructor.
data Artifact = Artifact'
  { url :: Prelude.Maybe Prelude.Text,
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Artifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'artifact_url' - Undocumented member.
--
-- 'description', 'artifact_description' - Undocumented member.
newArtifact ::
  Artifact
newArtifact =
  Artifact'
    { url = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | Undocumented member.
artifact_url :: Lens.Lens' Artifact (Prelude.Maybe Prelude.Text)
artifact_url = Lens.lens (\Artifact' {url} -> url) (\s@Artifact' {} a -> s {url = a} :: Artifact)

-- | Undocumented member.
artifact_description :: Lens.Lens' Artifact (Prelude.Maybe Prelude.Text)
artifact_description = Lens.lens (\Artifact' {description} -> description) (\s@Artifact' {} a -> s {description = a} :: Artifact)

instance Core.FromXML Artifact where
  parseXML x =
    Artifact'
      Prelude.<$> (x Core..@? "URL")
      Prelude.<*> (x Core..@? "Description")

instance Prelude.Hashable Artifact where
  hashWithSalt salt' Artifact' {..} =
    salt' `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` url

instance Prelude.NFData Artifact where
  rnf Artifact' {..} =
    Prelude.rnf url
      `Prelude.seq` Prelude.rnf description
