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
-- Module      : Amazonka.CodeDeploy.Types.TagFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeDeploy.Types.TagFilter where

import Amazonka.CodeDeploy.Types.TagFilterType
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an on-premises instance tag filter.
--
-- /See:/ 'newTagFilter' smart constructor.
data TagFilter = TagFilter'
  { -- | The on-premises instance tag filter value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The on-premises instance tag filter key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The on-premises instance tag filter type:
    --
    -- -   KEY_ONLY: Key only.
    --
    -- -   VALUE_ONLY: Value only.
    --
    -- -   KEY_AND_VALUE: Key and value.
    type' :: Prelude.Maybe TagFilterType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'tagFilter_value' - The on-premises instance tag filter value.
--
-- 'key', 'tagFilter_key' - The on-premises instance tag filter key.
--
-- 'type'', 'tagFilter_type' - The on-premises instance tag filter type:
--
-- -   KEY_ONLY: Key only.
--
-- -   VALUE_ONLY: Value only.
--
-- -   KEY_AND_VALUE: Key and value.
newTagFilter ::
  TagFilter
newTagFilter =
  TagFilter'
    { value = Prelude.Nothing,
      key = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The on-premises instance tag filter value.
tagFilter_value :: Lens.Lens' TagFilter (Prelude.Maybe Prelude.Text)
tagFilter_value = Lens.lens (\TagFilter' {value} -> value) (\s@TagFilter' {} a -> s {value = a} :: TagFilter)

-- | The on-premises instance tag filter key.
tagFilter_key :: Lens.Lens' TagFilter (Prelude.Maybe Prelude.Text)
tagFilter_key = Lens.lens (\TagFilter' {key} -> key) (\s@TagFilter' {} a -> s {key = a} :: TagFilter)

-- | The on-premises instance tag filter type:
--
-- -   KEY_ONLY: Key only.
--
-- -   VALUE_ONLY: Value only.
--
-- -   KEY_AND_VALUE: Key and value.
tagFilter_type :: Lens.Lens' TagFilter (Prelude.Maybe TagFilterType)
tagFilter_type = Lens.lens (\TagFilter' {type'} -> type') (\s@TagFilter' {} a -> s {type' = a} :: TagFilter)

instance Core.FromJSON TagFilter where
  parseJSON =
    Core.withObject
      "TagFilter"
      ( \x ->
          TagFilter'
            Prelude.<$> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Key")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable TagFilter where
  hashWithSalt salt' TagFilter' {..} =
    salt' `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData TagFilter where
  rnf TagFilter' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf key

instance Core.ToJSON TagFilter where
  toJSON TagFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Key" Core..=) Prelude.<$> key,
            ("Type" Core..=) Prelude.<$> type'
          ]
      )
