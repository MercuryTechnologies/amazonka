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
-- Module      : Amazonka.DeviceFarm.Types.TestGridSessionArtifact
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.TestGridSessionArtifact where

import qualified Amazonka.Core as Core
import Amazonka.DeviceFarm.Types.TestGridSessionArtifactType
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Artifacts are video and other files that are produced in the process of
-- running a browser in an automated context.
--
-- Video elements might be broken up into multiple artifacts as they grow
-- in size during creation.
--
-- /See:/ 'newTestGridSessionArtifact' smart constructor.
data TestGridSessionArtifact = TestGridSessionArtifact'
  { -- | A semi-stable URL to the content of the object.
    url :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The kind of artifact.
    type' :: Prelude.Maybe TestGridSessionArtifactType,
    -- | The file name of the artifact.
    filename :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestGridSessionArtifact' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'testGridSessionArtifact_url' - A semi-stable URL to the content of the object.
--
-- 'type'', 'testGridSessionArtifact_type' - The kind of artifact.
--
-- 'filename', 'testGridSessionArtifact_filename' - The file name of the artifact.
newTestGridSessionArtifact ::
  TestGridSessionArtifact
newTestGridSessionArtifact =
  TestGridSessionArtifact'
    { url = Prelude.Nothing,
      type' = Prelude.Nothing,
      filename = Prelude.Nothing
    }

-- | A semi-stable URL to the content of the object.
testGridSessionArtifact_url :: Lens.Lens' TestGridSessionArtifact (Prelude.Maybe Prelude.Text)
testGridSessionArtifact_url = Lens.lens (\TestGridSessionArtifact' {url} -> url) (\s@TestGridSessionArtifact' {} a -> s {url = a} :: TestGridSessionArtifact) Prelude.. Lens.mapping Core._Sensitive

-- | The kind of artifact.
testGridSessionArtifact_type :: Lens.Lens' TestGridSessionArtifact (Prelude.Maybe TestGridSessionArtifactType)
testGridSessionArtifact_type = Lens.lens (\TestGridSessionArtifact' {type'} -> type') (\s@TestGridSessionArtifact' {} a -> s {type' = a} :: TestGridSessionArtifact)

-- | The file name of the artifact.
testGridSessionArtifact_filename :: Lens.Lens' TestGridSessionArtifact (Prelude.Maybe Prelude.Text)
testGridSessionArtifact_filename = Lens.lens (\TestGridSessionArtifact' {filename} -> filename) (\s@TestGridSessionArtifact' {} a -> s {filename = a} :: TestGridSessionArtifact)

instance Core.FromJSON TestGridSessionArtifact where
  parseJSON =
    Core.withObject
      "TestGridSessionArtifact"
      ( \x ->
          TestGridSessionArtifact'
            Prelude.<$> (x Core..:? "url")
            Prelude.<*> (x Core..:? "type")
            Prelude.<*> (x Core..:? "filename")
      )

instance Prelude.Hashable TestGridSessionArtifact where
  hashWithSalt salt' TestGridSessionArtifact' {..} =
    salt' `Prelude.hashWithSalt` filename
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` url

instance Prelude.NFData TestGridSessionArtifact where
  rnf TestGridSessionArtifact' {..} =
    Prelude.rnf url `Prelude.seq` Prelude.rnf filename
      `Prelude.seq` Prelude.rnf type'
