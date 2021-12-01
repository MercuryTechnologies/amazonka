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
-- Module      : Amazonka.MediaConnect.Types.MediaStreamAttributesRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConnect.Types.MediaStreamAttributesRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaConnect.Types.FmtpRequest
import qualified Amazonka.Prelude as Prelude

-- | Attributes that are related to the media stream.
--
-- /See:/ 'newMediaStreamAttributesRequest' smart constructor.
data MediaStreamAttributesRequest = MediaStreamAttributesRequest'
  { -- | The audio language, in a format that is recognized by the receiver.
    lang :: Prelude.Maybe Prelude.Text,
    -- | The settings that you want to use to define the media stream.
    fmtp :: Prelude.Maybe FmtpRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MediaStreamAttributesRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lang', 'mediaStreamAttributesRequest_lang' - The audio language, in a format that is recognized by the receiver.
--
-- 'fmtp', 'mediaStreamAttributesRequest_fmtp' - The settings that you want to use to define the media stream.
newMediaStreamAttributesRequest ::
  MediaStreamAttributesRequest
newMediaStreamAttributesRequest =
  MediaStreamAttributesRequest'
    { lang =
        Prelude.Nothing,
      fmtp = Prelude.Nothing
    }

-- | The audio language, in a format that is recognized by the receiver.
mediaStreamAttributesRequest_lang :: Lens.Lens' MediaStreamAttributesRequest (Prelude.Maybe Prelude.Text)
mediaStreamAttributesRequest_lang = Lens.lens (\MediaStreamAttributesRequest' {lang} -> lang) (\s@MediaStreamAttributesRequest' {} a -> s {lang = a} :: MediaStreamAttributesRequest)

-- | The settings that you want to use to define the media stream.
mediaStreamAttributesRequest_fmtp :: Lens.Lens' MediaStreamAttributesRequest (Prelude.Maybe FmtpRequest)
mediaStreamAttributesRequest_fmtp = Lens.lens (\MediaStreamAttributesRequest' {fmtp} -> fmtp) (\s@MediaStreamAttributesRequest' {} a -> s {fmtp = a} :: MediaStreamAttributesRequest)

instance
  Prelude.Hashable
    MediaStreamAttributesRequest
  where
  hashWithSalt salt' MediaStreamAttributesRequest' {..} =
    salt' `Prelude.hashWithSalt` fmtp
      `Prelude.hashWithSalt` lang

instance Prelude.NFData MediaStreamAttributesRequest where
  rnf MediaStreamAttributesRequest' {..} =
    Prelude.rnf lang `Prelude.seq` Prelude.rnf fmtp

instance Core.ToJSON MediaStreamAttributesRequest where
  toJSON MediaStreamAttributesRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("lang" Core..=) Prelude.<$> lang,
            ("fmtp" Core..=) Prelude.<$> fmtp
          ]
      )
