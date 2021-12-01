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
-- Module      : Amazonka.ServiceCatalog.Types.ListTagOptionsFilters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ListTagOptionsFilters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Filters to use when listing TagOptions.
--
-- /See:/ 'newListTagOptionsFilters' smart constructor.
data ListTagOptionsFilters = ListTagOptionsFilters'
  { -- | The TagOption value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The active state.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The TagOption key.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTagOptionsFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'listTagOptionsFilters_value' - The TagOption value.
--
-- 'active', 'listTagOptionsFilters_active' - The active state.
--
-- 'key', 'listTagOptionsFilters_key' - The TagOption key.
newListTagOptionsFilters ::
  ListTagOptionsFilters
newListTagOptionsFilters =
  ListTagOptionsFilters'
    { value = Prelude.Nothing,
      active = Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The TagOption value.
listTagOptionsFilters_value :: Lens.Lens' ListTagOptionsFilters (Prelude.Maybe Prelude.Text)
listTagOptionsFilters_value = Lens.lens (\ListTagOptionsFilters' {value} -> value) (\s@ListTagOptionsFilters' {} a -> s {value = a} :: ListTagOptionsFilters)

-- | The active state.
listTagOptionsFilters_active :: Lens.Lens' ListTagOptionsFilters (Prelude.Maybe Prelude.Bool)
listTagOptionsFilters_active = Lens.lens (\ListTagOptionsFilters' {active} -> active) (\s@ListTagOptionsFilters' {} a -> s {active = a} :: ListTagOptionsFilters)

-- | The TagOption key.
listTagOptionsFilters_key :: Lens.Lens' ListTagOptionsFilters (Prelude.Maybe Prelude.Text)
listTagOptionsFilters_key = Lens.lens (\ListTagOptionsFilters' {key} -> key) (\s@ListTagOptionsFilters' {} a -> s {key = a} :: ListTagOptionsFilters)

instance Prelude.Hashable ListTagOptionsFilters where
  hashWithSalt salt' ListTagOptionsFilters' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` value

instance Prelude.NFData ListTagOptionsFilters where
  rnf ListTagOptionsFilters' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf active

instance Core.ToJSON ListTagOptionsFilters where
  toJSON ListTagOptionsFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Active" Core..=) Prelude.<$> active,
            ("Key" Core..=) Prelude.<$> key
          ]
      )
