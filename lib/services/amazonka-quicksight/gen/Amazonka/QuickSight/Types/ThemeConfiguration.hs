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
-- Module      : Amazonka.QuickSight.Types.ThemeConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ThemeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataColorPalette
import Amazonka.QuickSight.Types.SheetStyle
import Amazonka.QuickSight.Types.UIColorPalette

-- | The theme configuration. This configuration contains all of the display
-- properties for a theme.
--
-- /See:/ 'newThemeConfiguration' smart constructor.
data ThemeConfiguration = ThemeConfiguration'
  { -- | Color properties that apply to the UI and to charts, excluding the
    -- colors that apply to data.
    uIColorPalette :: Prelude.Maybe UIColorPalette,
    -- | Display options related to sheets.
    sheet :: Prelude.Maybe SheetStyle,
    -- | Color properties that apply to chart data colors.
    dataColorPalette :: Prelude.Maybe DataColorPalette
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThemeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'uIColorPalette', 'themeConfiguration_uIColorPalette' - Color properties that apply to the UI and to charts, excluding the
-- colors that apply to data.
--
-- 'sheet', 'themeConfiguration_sheet' - Display options related to sheets.
--
-- 'dataColorPalette', 'themeConfiguration_dataColorPalette' - Color properties that apply to chart data colors.
newThemeConfiguration ::
  ThemeConfiguration
newThemeConfiguration =
  ThemeConfiguration'
    { uIColorPalette =
        Prelude.Nothing,
      sheet = Prelude.Nothing,
      dataColorPalette = Prelude.Nothing
    }

-- | Color properties that apply to the UI and to charts, excluding the
-- colors that apply to data.
themeConfiguration_uIColorPalette :: Lens.Lens' ThemeConfiguration (Prelude.Maybe UIColorPalette)
themeConfiguration_uIColorPalette = Lens.lens (\ThemeConfiguration' {uIColorPalette} -> uIColorPalette) (\s@ThemeConfiguration' {} a -> s {uIColorPalette = a} :: ThemeConfiguration)

-- | Display options related to sheets.
themeConfiguration_sheet :: Lens.Lens' ThemeConfiguration (Prelude.Maybe SheetStyle)
themeConfiguration_sheet = Lens.lens (\ThemeConfiguration' {sheet} -> sheet) (\s@ThemeConfiguration' {} a -> s {sheet = a} :: ThemeConfiguration)

-- | Color properties that apply to chart data colors.
themeConfiguration_dataColorPalette :: Lens.Lens' ThemeConfiguration (Prelude.Maybe DataColorPalette)
themeConfiguration_dataColorPalette = Lens.lens (\ThemeConfiguration' {dataColorPalette} -> dataColorPalette) (\s@ThemeConfiguration' {} a -> s {dataColorPalette = a} :: ThemeConfiguration)

instance Core.FromJSON ThemeConfiguration where
  parseJSON =
    Core.withObject
      "ThemeConfiguration"
      ( \x ->
          ThemeConfiguration'
            Prelude.<$> (x Core..:? "UIColorPalette")
            Prelude.<*> (x Core..:? "Sheet")
            Prelude.<*> (x Core..:? "DataColorPalette")
      )

instance Prelude.Hashable ThemeConfiguration where
  hashWithSalt salt' ThemeConfiguration' {..} =
    salt' `Prelude.hashWithSalt` dataColorPalette
      `Prelude.hashWithSalt` sheet
      `Prelude.hashWithSalt` uIColorPalette

instance Prelude.NFData ThemeConfiguration where
  rnf ThemeConfiguration' {..} =
    Prelude.rnf uIColorPalette
      `Prelude.seq` Prelude.rnf dataColorPalette
      `Prelude.seq` Prelude.rnf sheet

instance Core.ToJSON ThemeConfiguration where
  toJSON ThemeConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UIColorPalette" Core..=)
              Prelude.<$> uIColorPalette,
            ("Sheet" Core..=) Prelude.<$> sheet,
            ("DataColorPalette" Core..=)
              Prelude.<$> dataColorPalette
          ]
      )
