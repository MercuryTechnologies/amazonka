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
-- Module      : Amazonka.QuickSight.Types.SheetControlsOption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetControlsOption where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DashboardUIState

-- | Sheet controls option.
--
-- /See:/ 'newSheetControlsOption' smart constructor.
data SheetControlsOption = SheetControlsOption'
  { -- | Visibility state.
    visibilityState :: Prelude.Maybe DashboardUIState
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SheetControlsOption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'visibilityState', 'sheetControlsOption_visibilityState' - Visibility state.
newSheetControlsOption ::
  SheetControlsOption
newSheetControlsOption =
  SheetControlsOption'
    { visibilityState =
        Prelude.Nothing
    }

-- | Visibility state.
sheetControlsOption_visibilityState :: Lens.Lens' SheetControlsOption (Prelude.Maybe DashboardUIState)
sheetControlsOption_visibilityState = Lens.lens (\SheetControlsOption' {visibilityState} -> visibilityState) (\s@SheetControlsOption' {} a -> s {visibilityState = a} :: SheetControlsOption)

instance Prelude.Hashable SheetControlsOption where
  hashWithSalt salt' SheetControlsOption' {..} =
    salt' `Prelude.hashWithSalt` visibilityState

instance Prelude.NFData SheetControlsOption where
  rnf SheetControlsOption' {..} =
    Prelude.rnf visibilityState

instance Core.ToJSON SheetControlsOption where
  toJSON SheetControlsOption' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("VisibilityState" Core..=)
              Prelude.<$> visibilityState
          ]
      )
