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
-- Module      : Amazonka.CodeCommit.Types.Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.Location where

import Amazonka.CodeCommit.Types.RelativeFileVersionEnum
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about the location of a change or comment in the
-- comparison between two commits or a pull request.
--
-- /See:/ 'newLocation' smart constructor.
data Location = Location'
  { -- | In a comparison of commits or a pull request, whether the change is in
    -- the before or after of that comparison.
    relativeFileVersion :: Prelude.Maybe RelativeFileVersionEnum,
    -- | The name of the file being compared, including its extension and
    -- subdirectory, if any.
    filePath :: Prelude.Maybe Prelude.Text,
    -- | The position of a change in a compared file, in line number format.
    filePosition :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relativeFileVersion', 'location_relativeFileVersion' - In a comparison of commits or a pull request, whether the change is in
-- the before or after of that comparison.
--
-- 'filePath', 'location_filePath' - The name of the file being compared, including its extension and
-- subdirectory, if any.
--
-- 'filePosition', 'location_filePosition' - The position of a change in a compared file, in line number format.
newLocation ::
  Location
newLocation =
  Location'
    { relativeFileVersion = Prelude.Nothing,
      filePath = Prelude.Nothing,
      filePosition = Prelude.Nothing
    }

-- | In a comparison of commits or a pull request, whether the change is in
-- the before or after of that comparison.
location_relativeFileVersion :: Lens.Lens' Location (Prelude.Maybe RelativeFileVersionEnum)
location_relativeFileVersion = Lens.lens (\Location' {relativeFileVersion} -> relativeFileVersion) (\s@Location' {} a -> s {relativeFileVersion = a} :: Location)

-- | The name of the file being compared, including its extension and
-- subdirectory, if any.
location_filePath :: Lens.Lens' Location (Prelude.Maybe Prelude.Text)
location_filePath = Lens.lens (\Location' {filePath} -> filePath) (\s@Location' {} a -> s {filePath = a} :: Location)

-- | The position of a change in a compared file, in line number format.
location_filePosition :: Lens.Lens' Location (Prelude.Maybe Prelude.Integer)
location_filePosition = Lens.lens (\Location' {filePosition} -> filePosition) (\s@Location' {} a -> s {filePosition = a} :: Location)

instance Core.FromJSON Location where
  parseJSON =
    Core.withObject
      "Location"
      ( \x ->
          Location'
            Prelude.<$> (x Core..:? "relativeFileVersion")
            Prelude.<*> (x Core..:? "filePath")
            Prelude.<*> (x Core..:? "filePosition")
      )

instance Prelude.Hashable Location where
  hashWithSalt salt' Location' {..} =
    salt' `Prelude.hashWithSalt` filePosition
      `Prelude.hashWithSalt` filePath
      `Prelude.hashWithSalt` relativeFileVersion

instance Prelude.NFData Location where
  rnf Location' {..} =
    Prelude.rnf relativeFileVersion
      `Prelude.seq` Prelude.rnf filePosition
      `Prelude.seq` Prelude.rnf filePath

instance Core.ToJSON Location where
  toJSON Location' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("relativeFileVersion" Core..=)
              Prelude.<$> relativeFileVersion,
            ("filePath" Core..=) Prelude.<$> filePath,
            ("filePosition" Core..=) Prelude.<$> filePosition
          ]
      )
