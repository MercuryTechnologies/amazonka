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
-- Module      : Amazonka.Pinpoint.Types.InAppMessageHeaderConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.InAppMessageHeaderConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.Alignment
import qualified Amazonka.Prelude as Prelude

-- | Text config for Message Header.
--
-- /See:/ 'newInAppMessageHeaderConfig' smart constructor.
data InAppMessageHeaderConfig = InAppMessageHeaderConfig'
  { -- | The alignment of the text. Valid values: LEFT, CENTER, RIGHT.
    alignment :: Alignment,
    -- | Message Header.
    header :: Prelude.Text,
    -- | The text color.
    textColor :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InAppMessageHeaderConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'alignment', 'inAppMessageHeaderConfig_alignment' - The alignment of the text. Valid values: LEFT, CENTER, RIGHT.
--
-- 'header', 'inAppMessageHeaderConfig_header' - Message Header.
--
-- 'textColor', 'inAppMessageHeaderConfig_textColor' - The text color.
newInAppMessageHeaderConfig ::
  -- | 'alignment'
  Alignment ->
  -- | 'header'
  Prelude.Text ->
  -- | 'textColor'
  Prelude.Text ->
  InAppMessageHeaderConfig
newInAppMessageHeaderConfig
  pAlignment_
  pHeader_
  pTextColor_ =
    InAppMessageHeaderConfig'
      { alignment = pAlignment_,
        header = pHeader_,
        textColor = pTextColor_
      }

-- | The alignment of the text. Valid values: LEFT, CENTER, RIGHT.
inAppMessageHeaderConfig_alignment :: Lens.Lens' InAppMessageHeaderConfig Alignment
inAppMessageHeaderConfig_alignment = Lens.lens (\InAppMessageHeaderConfig' {alignment} -> alignment) (\s@InAppMessageHeaderConfig' {} a -> s {alignment = a} :: InAppMessageHeaderConfig)

-- | Message Header.
inAppMessageHeaderConfig_header :: Lens.Lens' InAppMessageHeaderConfig Prelude.Text
inAppMessageHeaderConfig_header = Lens.lens (\InAppMessageHeaderConfig' {header} -> header) (\s@InAppMessageHeaderConfig' {} a -> s {header = a} :: InAppMessageHeaderConfig)

-- | The text color.
inAppMessageHeaderConfig_textColor :: Lens.Lens' InAppMessageHeaderConfig Prelude.Text
inAppMessageHeaderConfig_textColor = Lens.lens (\InAppMessageHeaderConfig' {textColor} -> textColor) (\s@InAppMessageHeaderConfig' {} a -> s {textColor = a} :: InAppMessageHeaderConfig)

instance Core.FromJSON InAppMessageHeaderConfig where
  parseJSON =
    Core.withObject
      "InAppMessageHeaderConfig"
      ( \x ->
          InAppMessageHeaderConfig'
            Prelude.<$> (x Core..: "Alignment")
            Prelude.<*> (x Core..: "Header")
            Prelude.<*> (x Core..: "TextColor")
      )

instance Prelude.Hashable InAppMessageHeaderConfig where
  hashWithSalt salt' InAppMessageHeaderConfig' {..} =
    salt' `Prelude.hashWithSalt` textColor
      `Prelude.hashWithSalt` header
      `Prelude.hashWithSalt` alignment

instance Prelude.NFData InAppMessageHeaderConfig where
  rnf InAppMessageHeaderConfig' {..} =
    Prelude.rnf alignment
      `Prelude.seq` Prelude.rnf textColor
      `Prelude.seq` Prelude.rnf header

instance Core.ToJSON InAppMessageHeaderConfig where
  toJSON InAppMessageHeaderConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Alignment" Core..= alignment),
            Prelude.Just ("Header" Core..= header),
            Prelude.Just ("TextColor" Core..= textColor)
          ]
      )
