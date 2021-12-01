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
-- Module      : Amazonka.MediaConvert.Types.WebvttHlsSourceSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.WebvttHlsSourceSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConvert.Types.LanguageCode
import qualified Amazonka.Prelude as Prelude

-- | Settings specific to WebVTT sources in HLS alternative rendition group.
-- Specify the properties (renditionGroupId, renditionName or
-- renditionLanguageCode) to identify the unique subtitle track among the
-- alternative rendition groups present in the HLS manifest. If no unique
-- track is found, or multiple tracks match the specified properties, the
-- job fails. If there is only one subtitle track in the rendition group,
-- the settings can be left empty and the default subtitle track will be
-- chosen. If your caption source is a sidecar file, use FileSourceSettings
-- instead of WebvttHlsSourceSettings.
--
-- /See:/ 'newWebvttHlsSourceSettings' smart constructor.
data WebvttHlsSourceSettings = WebvttHlsSourceSettings'
  { -- | Optional. Specify media name
    renditionName :: Prelude.Maybe Prelude.Text,
    -- | Optional. Specify alternative group ID
    renditionGroupId :: Prelude.Maybe Prelude.Text,
    -- | Optional. Specify ISO 639-2 or ISO 639-3 code in the language property
    renditionLanguageCode :: Prelude.Maybe LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WebvttHlsSourceSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'renditionName', 'webvttHlsSourceSettings_renditionName' - Optional. Specify media name
--
-- 'renditionGroupId', 'webvttHlsSourceSettings_renditionGroupId' - Optional. Specify alternative group ID
--
-- 'renditionLanguageCode', 'webvttHlsSourceSettings_renditionLanguageCode' - Optional. Specify ISO 639-2 or ISO 639-3 code in the language property
newWebvttHlsSourceSettings ::
  WebvttHlsSourceSettings
newWebvttHlsSourceSettings =
  WebvttHlsSourceSettings'
    { renditionName =
        Prelude.Nothing,
      renditionGroupId = Prelude.Nothing,
      renditionLanguageCode = Prelude.Nothing
    }

-- | Optional. Specify media name
webvttHlsSourceSettings_renditionName :: Lens.Lens' WebvttHlsSourceSettings (Prelude.Maybe Prelude.Text)
webvttHlsSourceSettings_renditionName = Lens.lens (\WebvttHlsSourceSettings' {renditionName} -> renditionName) (\s@WebvttHlsSourceSettings' {} a -> s {renditionName = a} :: WebvttHlsSourceSettings)

-- | Optional. Specify alternative group ID
webvttHlsSourceSettings_renditionGroupId :: Lens.Lens' WebvttHlsSourceSettings (Prelude.Maybe Prelude.Text)
webvttHlsSourceSettings_renditionGroupId = Lens.lens (\WebvttHlsSourceSettings' {renditionGroupId} -> renditionGroupId) (\s@WebvttHlsSourceSettings' {} a -> s {renditionGroupId = a} :: WebvttHlsSourceSettings)

-- | Optional. Specify ISO 639-2 or ISO 639-3 code in the language property
webvttHlsSourceSettings_renditionLanguageCode :: Lens.Lens' WebvttHlsSourceSettings (Prelude.Maybe LanguageCode)
webvttHlsSourceSettings_renditionLanguageCode = Lens.lens (\WebvttHlsSourceSettings' {renditionLanguageCode} -> renditionLanguageCode) (\s@WebvttHlsSourceSettings' {} a -> s {renditionLanguageCode = a} :: WebvttHlsSourceSettings)

instance Core.FromJSON WebvttHlsSourceSettings where
  parseJSON =
    Core.withObject
      "WebvttHlsSourceSettings"
      ( \x ->
          WebvttHlsSourceSettings'
            Prelude.<$> (x Core..:? "renditionName")
            Prelude.<*> (x Core..:? "renditionGroupId")
            Prelude.<*> (x Core..:? "renditionLanguageCode")
      )

instance Prelude.Hashable WebvttHlsSourceSettings where
  hashWithSalt salt' WebvttHlsSourceSettings' {..} =
    salt' `Prelude.hashWithSalt` renditionLanguageCode
      `Prelude.hashWithSalt` renditionGroupId
      `Prelude.hashWithSalt` renditionName

instance Prelude.NFData WebvttHlsSourceSettings where
  rnf WebvttHlsSourceSettings' {..} =
    Prelude.rnf renditionName
      `Prelude.seq` Prelude.rnf renditionLanguageCode
      `Prelude.seq` Prelude.rnf renditionGroupId

instance Core.ToJSON WebvttHlsSourceSettings where
  toJSON WebvttHlsSourceSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("renditionName" Core..=) Prelude.<$> renditionName,
            ("renditionGroupId" Core..=)
              Prelude.<$> renditionGroupId,
            ("renditionLanguageCode" Core..=)
              Prelude.<$> renditionLanguageCode
          ]
      )
