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
-- Module      : Amazonka.MediaLive.Types.CaptionLanguageMapping
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.CaptionLanguageMapping where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Maps a caption channel to an ISO 693-2 language code
-- (http:\/\/www.loc.gov\/standards\/iso639-2), with an optional
-- description.
--
-- /See:/ 'newCaptionLanguageMapping' smart constructor.
data CaptionLanguageMapping = CaptionLanguageMapping'
  { -- | Three character ISO 639-2 language code (see
    -- http:\/\/www.loc.gov\/standards\/iso639-2)
    languageCode :: Prelude.Text,
    -- | Textual description of language
    languageDescription :: Prelude.Text,
    -- | The closed caption channel being described by this
    -- CaptionLanguageMapping. Each channel mapping must have a unique channel
    -- number (maximum of 4)
    captionChannel :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaptionLanguageMapping' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'languageCode', 'captionLanguageMapping_languageCode' - Three character ISO 639-2 language code (see
-- http:\/\/www.loc.gov\/standards\/iso639-2)
--
-- 'languageDescription', 'captionLanguageMapping_languageDescription' - Textual description of language
--
-- 'captionChannel', 'captionLanguageMapping_captionChannel' - The closed caption channel being described by this
-- CaptionLanguageMapping. Each channel mapping must have a unique channel
-- number (maximum of 4)
newCaptionLanguageMapping ::
  -- | 'languageCode'
  Prelude.Text ->
  -- | 'languageDescription'
  Prelude.Text ->
  -- | 'captionChannel'
  Prelude.Natural ->
  CaptionLanguageMapping
newCaptionLanguageMapping
  pLanguageCode_
  pLanguageDescription_
  pCaptionChannel_ =
    CaptionLanguageMapping'
      { languageCode =
          pLanguageCode_,
        languageDescription = pLanguageDescription_,
        captionChannel = pCaptionChannel_
      }

-- | Three character ISO 639-2 language code (see
-- http:\/\/www.loc.gov\/standards\/iso639-2)
captionLanguageMapping_languageCode :: Lens.Lens' CaptionLanguageMapping Prelude.Text
captionLanguageMapping_languageCode = Lens.lens (\CaptionLanguageMapping' {languageCode} -> languageCode) (\s@CaptionLanguageMapping' {} a -> s {languageCode = a} :: CaptionLanguageMapping)

-- | Textual description of language
captionLanguageMapping_languageDescription :: Lens.Lens' CaptionLanguageMapping Prelude.Text
captionLanguageMapping_languageDescription = Lens.lens (\CaptionLanguageMapping' {languageDescription} -> languageDescription) (\s@CaptionLanguageMapping' {} a -> s {languageDescription = a} :: CaptionLanguageMapping)

-- | The closed caption channel being described by this
-- CaptionLanguageMapping. Each channel mapping must have a unique channel
-- number (maximum of 4)
captionLanguageMapping_captionChannel :: Lens.Lens' CaptionLanguageMapping Prelude.Natural
captionLanguageMapping_captionChannel = Lens.lens (\CaptionLanguageMapping' {captionChannel} -> captionChannel) (\s@CaptionLanguageMapping' {} a -> s {captionChannel = a} :: CaptionLanguageMapping)

instance Core.FromJSON CaptionLanguageMapping where
  parseJSON =
    Core.withObject
      "CaptionLanguageMapping"
      ( \x ->
          CaptionLanguageMapping'
            Prelude.<$> (x Core..: "languageCode")
            Prelude.<*> (x Core..: "languageDescription")
            Prelude.<*> (x Core..: "captionChannel")
      )

instance Prelude.Hashable CaptionLanguageMapping where
  hashWithSalt salt' CaptionLanguageMapping' {..} =
    salt' `Prelude.hashWithSalt` captionChannel
      `Prelude.hashWithSalt` languageDescription
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData CaptionLanguageMapping where
  rnf CaptionLanguageMapping' {..} =
    Prelude.rnf languageCode
      `Prelude.seq` Prelude.rnf captionChannel
      `Prelude.seq` Prelude.rnf languageDescription

instance Core.ToJSON CaptionLanguageMapping where
  toJSON CaptionLanguageMapping' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("languageCode" Core..= languageCode),
            Prelude.Just
              ("languageDescription" Core..= languageDescription),
            Prelude.Just
              ("captionChannel" Core..= captionChannel)
          ]
      )
