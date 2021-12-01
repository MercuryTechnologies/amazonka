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
-- Module      : Amazonka.QuickSight.Types.Theme
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.Theme where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ThemeType
import Amazonka.QuickSight.Types.ThemeVersion

-- | Summary information about a theme.
--
-- /See:/ 'newTheme' smart constructor.
data Theme = Theme'
  { -- | The identifier that the user gives to the theme.
    themeId :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the theme was last updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the theme.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the theme was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The name that the user gives to the theme.
    name :: Prelude.Maybe Prelude.Text,
    version :: Prelude.Maybe ThemeVersion,
    -- | The type of theme, based on how it was created. Valid values include:
    -- @QUICKSIGHT@ and @CUSTOM@.
    type' :: Prelude.Maybe ThemeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Theme' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'themeId', 'theme_themeId' - The identifier that the user gives to the theme.
--
-- 'lastUpdatedTime', 'theme_lastUpdatedTime' - The date and time that the theme was last updated.
--
-- 'arn', 'theme_arn' - The Amazon Resource Name (ARN) of the theme.
--
-- 'createdTime', 'theme_createdTime' - The date and time that the theme was created.
--
-- 'name', 'theme_name' - The name that the user gives to the theme.
--
-- 'version', 'theme_version' - Undocumented member.
--
-- 'type'', 'theme_type' - The type of theme, based on how it was created. Valid values include:
-- @QUICKSIGHT@ and @CUSTOM@.
newTheme ::
  Theme
newTheme =
  Theme'
    { themeId = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      name = Prelude.Nothing,
      version = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The identifier that the user gives to the theme.
theme_themeId :: Lens.Lens' Theme (Prelude.Maybe Prelude.Text)
theme_themeId = Lens.lens (\Theme' {themeId} -> themeId) (\s@Theme' {} a -> s {themeId = a} :: Theme)

-- | The date and time that the theme was last updated.
theme_lastUpdatedTime :: Lens.Lens' Theme (Prelude.Maybe Prelude.UTCTime)
theme_lastUpdatedTime = Lens.lens (\Theme' {lastUpdatedTime} -> lastUpdatedTime) (\s@Theme' {} a -> s {lastUpdatedTime = a} :: Theme) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the theme.
theme_arn :: Lens.Lens' Theme (Prelude.Maybe Prelude.Text)
theme_arn = Lens.lens (\Theme' {arn} -> arn) (\s@Theme' {} a -> s {arn = a} :: Theme)

-- | The date and time that the theme was created.
theme_createdTime :: Lens.Lens' Theme (Prelude.Maybe Prelude.UTCTime)
theme_createdTime = Lens.lens (\Theme' {createdTime} -> createdTime) (\s@Theme' {} a -> s {createdTime = a} :: Theme) Prelude.. Lens.mapping Core._Time

-- | The name that the user gives to the theme.
theme_name :: Lens.Lens' Theme (Prelude.Maybe Prelude.Text)
theme_name = Lens.lens (\Theme' {name} -> name) (\s@Theme' {} a -> s {name = a} :: Theme)

-- | Undocumented member.
theme_version :: Lens.Lens' Theme (Prelude.Maybe ThemeVersion)
theme_version = Lens.lens (\Theme' {version} -> version) (\s@Theme' {} a -> s {version = a} :: Theme)

-- | The type of theme, based on how it was created. Valid values include:
-- @QUICKSIGHT@ and @CUSTOM@.
theme_type :: Lens.Lens' Theme (Prelude.Maybe ThemeType)
theme_type = Lens.lens (\Theme' {type'} -> type') (\s@Theme' {} a -> s {type' = a} :: Theme)

instance Core.FromJSON Theme where
  parseJSON =
    Core.withObject
      "Theme"
      ( \x ->
          Theme'
            Prelude.<$> (x Core..:? "ThemeId")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable Theme where
  hashWithSalt salt' Theme' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` themeId

instance Prelude.NFData Theme where
  rnf Theme' {..} =
    Prelude.rnf themeId `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf lastUpdatedTime
