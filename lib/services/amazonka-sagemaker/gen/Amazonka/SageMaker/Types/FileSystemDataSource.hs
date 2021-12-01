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
-- Module      : Amazonka.SageMaker.Types.FileSystemDataSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FileSystemDataSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FileSystemAccessMode
import Amazonka.SageMaker.Types.FileSystemType

-- | Specifies a file system data source for a channel.
--
-- /See:/ 'newFileSystemDataSource' smart constructor.
data FileSystemDataSource = FileSystemDataSource'
  { -- | The file system id.
    fileSystemId :: Prelude.Text,
    -- | The access mode of the mount of the directory associated with the
    -- channel. A directory can be mounted either in @ro@ (read-only) or @rw@
    -- (read-write) mode.
    fileSystemAccessMode :: FileSystemAccessMode,
    -- | The file system type.
    fileSystemType :: FileSystemType,
    -- | The full path to the directory to associate with the channel.
    directoryPath :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FileSystemDataSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'fileSystemDataSource_fileSystemId' - The file system id.
--
-- 'fileSystemAccessMode', 'fileSystemDataSource_fileSystemAccessMode' - The access mode of the mount of the directory associated with the
-- channel. A directory can be mounted either in @ro@ (read-only) or @rw@
-- (read-write) mode.
--
-- 'fileSystemType', 'fileSystemDataSource_fileSystemType' - The file system type.
--
-- 'directoryPath', 'fileSystemDataSource_directoryPath' - The full path to the directory to associate with the channel.
newFileSystemDataSource ::
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'fileSystemAccessMode'
  FileSystemAccessMode ->
  -- | 'fileSystemType'
  FileSystemType ->
  -- | 'directoryPath'
  Prelude.Text ->
  FileSystemDataSource
newFileSystemDataSource
  pFileSystemId_
  pFileSystemAccessMode_
  pFileSystemType_
  pDirectoryPath_ =
    FileSystemDataSource'
      { fileSystemId =
          pFileSystemId_,
        fileSystemAccessMode = pFileSystemAccessMode_,
        fileSystemType = pFileSystemType_,
        directoryPath = pDirectoryPath_
      }

-- | The file system id.
fileSystemDataSource_fileSystemId :: Lens.Lens' FileSystemDataSource Prelude.Text
fileSystemDataSource_fileSystemId = Lens.lens (\FileSystemDataSource' {fileSystemId} -> fileSystemId) (\s@FileSystemDataSource' {} a -> s {fileSystemId = a} :: FileSystemDataSource)

-- | The access mode of the mount of the directory associated with the
-- channel. A directory can be mounted either in @ro@ (read-only) or @rw@
-- (read-write) mode.
fileSystemDataSource_fileSystemAccessMode :: Lens.Lens' FileSystemDataSource FileSystemAccessMode
fileSystemDataSource_fileSystemAccessMode = Lens.lens (\FileSystemDataSource' {fileSystemAccessMode} -> fileSystemAccessMode) (\s@FileSystemDataSource' {} a -> s {fileSystemAccessMode = a} :: FileSystemDataSource)

-- | The file system type.
fileSystemDataSource_fileSystemType :: Lens.Lens' FileSystemDataSource FileSystemType
fileSystemDataSource_fileSystemType = Lens.lens (\FileSystemDataSource' {fileSystemType} -> fileSystemType) (\s@FileSystemDataSource' {} a -> s {fileSystemType = a} :: FileSystemDataSource)

-- | The full path to the directory to associate with the channel.
fileSystemDataSource_directoryPath :: Lens.Lens' FileSystemDataSource Prelude.Text
fileSystemDataSource_directoryPath = Lens.lens (\FileSystemDataSource' {directoryPath} -> directoryPath) (\s@FileSystemDataSource' {} a -> s {directoryPath = a} :: FileSystemDataSource)

instance Core.FromJSON FileSystemDataSource where
  parseJSON =
    Core.withObject
      "FileSystemDataSource"
      ( \x ->
          FileSystemDataSource'
            Prelude.<$> (x Core..: "FileSystemId")
            Prelude.<*> (x Core..: "FileSystemAccessMode")
            Prelude.<*> (x Core..: "FileSystemType")
            Prelude.<*> (x Core..: "DirectoryPath")
      )

instance Prelude.Hashable FileSystemDataSource where
  hashWithSalt salt' FileSystemDataSource' {..} =
    salt' `Prelude.hashWithSalt` directoryPath
      `Prelude.hashWithSalt` fileSystemType
      `Prelude.hashWithSalt` fileSystemAccessMode
      `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData FileSystemDataSource where
  rnf FileSystemDataSource' {..} =
    Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf directoryPath
      `Prelude.seq` Prelude.rnf fileSystemType
      `Prelude.seq` Prelude.rnf fileSystemAccessMode

instance Core.ToJSON FileSystemDataSource where
  toJSON FileSystemDataSource' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("FileSystemId" Core..= fileSystemId),
            Prelude.Just
              ( "FileSystemAccessMode"
                  Core..= fileSystemAccessMode
              ),
            Prelude.Just
              ("FileSystemType" Core..= fileSystemType),
            Prelude.Just
              ("DirectoryPath" Core..= directoryPath)
          ]
      )
