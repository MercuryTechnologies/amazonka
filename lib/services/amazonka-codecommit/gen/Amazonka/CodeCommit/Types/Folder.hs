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
-- Module      : Amazonka.CodeCommit.Types.Folder
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.Folder where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a folder in a repository.
--
-- /See:/ 'newFolder' smart constructor.
data Folder = Folder'
  { -- | The fully qualified path of the folder in the repository.
    absolutePath :: Prelude.Maybe Prelude.Text,
    -- | The full SHA-1 pointer of the tree information for the commit that
    -- contains the folder.
    treeId :: Prelude.Maybe Prelude.Text,
    -- | The relative path of the specified folder from the folder where the
    -- query originated.
    relativePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Folder' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'absolutePath', 'folder_absolutePath' - The fully qualified path of the folder in the repository.
--
-- 'treeId', 'folder_treeId' - The full SHA-1 pointer of the tree information for the commit that
-- contains the folder.
--
-- 'relativePath', 'folder_relativePath' - The relative path of the specified folder from the folder where the
-- query originated.
newFolder ::
  Folder
newFolder =
  Folder'
    { absolutePath = Prelude.Nothing,
      treeId = Prelude.Nothing,
      relativePath = Prelude.Nothing
    }

-- | The fully qualified path of the folder in the repository.
folder_absolutePath :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_absolutePath = Lens.lens (\Folder' {absolutePath} -> absolutePath) (\s@Folder' {} a -> s {absolutePath = a} :: Folder)

-- | The full SHA-1 pointer of the tree information for the commit that
-- contains the folder.
folder_treeId :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_treeId = Lens.lens (\Folder' {treeId} -> treeId) (\s@Folder' {} a -> s {treeId = a} :: Folder)

-- | The relative path of the specified folder from the folder where the
-- query originated.
folder_relativePath :: Lens.Lens' Folder (Prelude.Maybe Prelude.Text)
folder_relativePath = Lens.lens (\Folder' {relativePath} -> relativePath) (\s@Folder' {} a -> s {relativePath = a} :: Folder)

instance Core.FromJSON Folder where
  parseJSON =
    Core.withObject
      "Folder"
      ( \x ->
          Folder'
            Prelude.<$> (x Core..:? "absolutePath")
            Prelude.<*> (x Core..:? "treeId")
            Prelude.<*> (x Core..:? "relativePath")
      )

instance Prelude.Hashable Folder where
  hashWithSalt salt' Folder' {..} =
    salt' `Prelude.hashWithSalt` relativePath
      `Prelude.hashWithSalt` treeId
      `Prelude.hashWithSalt` absolutePath

instance Prelude.NFData Folder where
  rnf Folder' {..} =
    Prelude.rnf absolutePath
      `Prelude.seq` Prelude.rnf relativePath
      `Prelude.seq` Prelude.rnf treeId
