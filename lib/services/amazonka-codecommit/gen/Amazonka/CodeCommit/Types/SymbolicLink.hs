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
-- Module      : Amazonka.CodeCommit.Types.SymbolicLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.SymbolicLink where

import Amazonka.CodeCommit.Types.FileModeTypeEnum
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a symbolic link in a repository folder.
--
-- /See:/ 'newSymbolicLink' smart constructor.
data SymbolicLink = SymbolicLink'
  { -- | The fully qualified path to the folder that contains the symbolic link.
    absolutePath :: Prelude.Maybe Prelude.Text,
    -- | The file mode permissions of the blob that cotains information about the
    -- symbolic link.
    fileMode :: Prelude.Maybe FileModeTypeEnum,
    -- | The blob ID that contains the information about the symbolic link.
    blobId :: Prelude.Maybe Prelude.Text,
    -- | The relative path of the symbolic link from the folder where the query
    -- originated.
    relativePath :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SymbolicLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'absolutePath', 'symbolicLink_absolutePath' - The fully qualified path to the folder that contains the symbolic link.
--
-- 'fileMode', 'symbolicLink_fileMode' - The file mode permissions of the blob that cotains information about the
-- symbolic link.
--
-- 'blobId', 'symbolicLink_blobId' - The blob ID that contains the information about the symbolic link.
--
-- 'relativePath', 'symbolicLink_relativePath' - The relative path of the symbolic link from the folder where the query
-- originated.
newSymbolicLink ::
  SymbolicLink
newSymbolicLink =
  SymbolicLink'
    { absolutePath = Prelude.Nothing,
      fileMode = Prelude.Nothing,
      blobId = Prelude.Nothing,
      relativePath = Prelude.Nothing
    }

-- | The fully qualified path to the folder that contains the symbolic link.
symbolicLink_absolutePath :: Lens.Lens' SymbolicLink (Prelude.Maybe Prelude.Text)
symbolicLink_absolutePath = Lens.lens (\SymbolicLink' {absolutePath} -> absolutePath) (\s@SymbolicLink' {} a -> s {absolutePath = a} :: SymbolicLink)

-- | The file mode permissions of the blob that cotains information about the
-- symbolic link.
symbolicLink_fileMode :: Lens.Lens' SymbolicLink (Prelude.Maybe FileModeTypeEnum)
symbolicLink_fileMode = Lens.lens (\SymbolicLink' {fileMode} -> fileMode) (\s@SymbolicLink' {} a -> s {fileMode = a} :: SymbolicLink)

-- | The blob ID that contains the information about the symbolic link.
symbolicLink_blobId :: Lens.Lens' SymbolicLink (Prelude.Maybe Prelude.Text)
symbolicLink_blobId = Lens.lens (\SymbolicLink' {blobId} -> blobId) (\s@SymbolicLink' {} a -> s {blobId = a} :: SymbolicLink)

-- | The relative path of the symbolic link from the folder where the query
-- originated.
symbolicLink_relativePath :: Lens.Lens' SymbolicLink (Prelude.Maybe Prelude.Text)
symbolicLink_relativePath = Lens.lens (\SymbolicLink' {relativePath} -> relativePath) (\s@SymbolicLink' {} a -> s {relativePath = a} :: SymbolicLink)

instance Core.FromJSON SymbolicLink where
  parseJSON =
    Core.withObject
      "SymbolicLink"
      ( \x ->
          SymbolicLink'
            Prelude.<$> (x Core..:? "absolutePath")
            Prelude.<*> (x Core..:? "fileMode")
            Prelude.<*> (x Core..:? "blobId")
            Prelude.<*> (x Core..:? "relativePath")
      )

instance Prelude.Hashable SymbolicLink where
  hashWithSalt salt' SymbolicLink' {..} =
    salt' `Prelude.hashWithSalt` relativePath
      `Prelude.hashWithSalt` blobId
      `Prelude.hashWithSalt` fileMode
      `Prelude.hashWithSalt` absolutePath

instance Prelude.NFData SymbolicLink where
  rnf SymbolicLink' {..} =
    Prelude.rnf absolutePath
      `Prelude.seq` Prelude.rnf relativePath
      `Prelude.seq` Prelude.rnf blobId
      `Prelude.seq` Prelude.rnf fileMode
