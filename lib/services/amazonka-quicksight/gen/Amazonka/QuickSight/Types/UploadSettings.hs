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
-- Module      : Amazonka.QuickSight.Types.UploadSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.UploadSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FileFormat
import Amazonka.QuickSight.Types.TextQualifier

-- | Information about the format for a source file or files.
--
-- /See:/ 'newUploadSettings' smart constructor.
data UploadSettings = UploadSettings'
  { -- | A row number to start reading data from.
    startFromRow :: Prelude.Maybe Prelude.Natural,
    -- | Whether the file has a header row, or the files each have a header row.
    containsHeader :: Prelude.Maybe Prelude.Bool,
    -- | Text qualifier.
    textQualifier :: Prelude.Maybe TextQualifier,
    -- | File format.
    format :: Prelude.Maybe FileFormat,
    -- | The delimiter between values in the file.
    delimiter :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UploadSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startFromRow', 'uploadSettings_startFromRow' - A row number to start reading data from.
--
-- 'containsHeader', 'uploadSettings_containsHeader' - Whether the file has a header row, or the files each have a header row.
--
-- 'textQualifier', 'uploadSettings_textQualifier' - Text qualifier.
--
-- 'format', 'uploadSettings_format' - File format.
--
-- 'delimiter', 'uploadSettings_delimiter' - The delimiter between values in the file.
newUploadSettings ::
  UploadSettings
newUploadSettings =
  UploadSettings'
    { startFromRow = Prelude.Nothing,
      containsHeader = Prelude.Nothing,
      textQualifier = Prelude.Nothing,
      format = Prelude.Nothing,
      delimiter = Prelude.Nothing
    }

-- | A row number to start reading data from.
uploadSettings_startFromRow :: Lens.Lens' UploadSettings (Prelude.Maybe Prelude.Natural)
uploadSettings_startFromRow = Lens.lens (\UploadSettings' {startFromRow} -> startFromRow) (\s@UploadSettings' {} a -> s {startFromRow = a} :: UploadSettings)

-- | Whether the file has a header row, or the files each have a header row.
uploadSettings_containsHeader :: Lens.Lens' UploadSettings (Prelude.Maybe Prelude.Bool)
uploadSettings_containsHeader = Lens.lens (\UploadSettings' {containsHeader} -> containsHeader) (\s@UploadSettings' {} a -> s {containsHeader = a} :: UploadSettings)

-- | Text qualifier.
uploadSettings_textQualifier :: Lens.Lens' UploadSettings (Prelude.Maybe TextQualifier)
uploadSettings_textQualifier = Lens.lens (\UploadSettings' {textQualifier} -> textQualifier) (\s@UploadSettings' {} a -> s {textQualifier = a} :: UploadSettings)

-- | File format.
uploadSettings_format :: Lens.Lens' UploadSettings (Prelude.Maybe FileFormat)
uploadSettings_format = Lens.lens (\UploadSettings' {format} -> format) (\s@UploadSettings' {} a -> s {format = a} :: UploadSettings)

-- | The delimiter between values in the file.
uploadSettings_delimiter :: Lens.Lens' UploadSettings (Prelude.Maybe Prelude.Text)
uploadSettings_delimiter = Lens.lens (\UploadSettings' {delimiter} -> delimiter) (\s@UploadSettings' {} a -> s {delimiter = a} :: UploadSettings)

instance Core.FromJSON UploadSettings where
  parseJSON =
    Core.withObject
      "UploadSettings"
      ( \x ->
          UploadSettings'
            Prelude.<$> (x Core..:? "StartFromRow")
            Prelude.<*> (x Core..:? "ContainsHeader")
            Prelude.<*> (x Core..:? "TextQualifier")
            Prelude.<*> (x Core..:? "Format")
            Prelude.<*> (x Core..:? "Delimiter")
      )

instance Prelude.Hashable UploadSettings where
  hashWithSalt salt' UploadSettings' {..} =
    salt' `Prelude.hashWithSalt` delimiter
      `Prelude.hashWithSalt` format
      `Prelude.hashWithSalt` textQualifier
      `Prelude.hashWithSalt` containsHeader
      `Prelude.hashWithSalt` startFromRow

instance Prelude.NFData UploadSettings where
  rnf UploadSettings' {..} =
    Prelude.rnf startFromRow
      `Prelude.seq` Prelude.rnf delimiter
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf textQualifier
      `Prelude.seq` Prelude.rnf containsHeader

instance Core.ToJSON UploadSettings where
  toJSON UploadSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("StartFromRow" Core..=) Prelude.<$> startFromRow,
            ("ContainsHeader" Core..=)
              Prelude.<$> containsHeader,
            ("TextQualifier" Core..=) Prelude.<$> textQualifier,
            ("Format" Core..=) Prelude.<$> format,
            ("Delimiter" Core..=) Prelude.<$> delimiter
          ]
      )
