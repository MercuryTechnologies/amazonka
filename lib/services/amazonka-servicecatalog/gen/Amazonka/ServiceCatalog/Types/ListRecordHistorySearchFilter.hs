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
-- Module      : Amazonka.ServiceCatalog.Types.ListRecordHistorySearchFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ListRecordHistorySearchFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The search filter to use when listing history records.
--
-- /See:/ 'newListRecordHistorySearchFilter' smart constructor.
data ListRecordHistorySearchFilter = ListRecordHistorySearchFilter'
  { -- | The filter value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The filter key.
    --
    -- -   @product@ - Filter results based on the specified product
    --     identifier.
    --
    -- -   @provisionedproduct@ - Filter results based on the provisioned
    --     product identifier.
    key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRecordHistorySearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'listRecordHistorySearchFilter_value' - The filter value.
--
-- 'key', 'listRecordHistorySearchFilter_key' - The filter key.
--
-- -   @product@ - Filter results based on the specified product
--     identifier.
--
-- -   @provisionedproduct@ - Filter results based on the provisioned
--     product identifier.
newListRecordHistorySearchFilter ::
  ListRecordHistorySearchFilter
newListRecordHistorySearchFilter =
  ListRecordHistorySearchFilter'
    { value =
        Prelude.Nothing,
      key = Prelude.Nothing
    }

-- | The filter value.
listRecordHistorySearchFilter_value :: Lens.Lens' ListRecordHistorySearchFilter (Prelude.Maybe Prelude.Text)
listRecordHistorySearchFilter_value = Lens.lens (\ListRecordHistorySearchFilter' {value} -> value) (\s@ListRecordHistorySearchFilter' {} a -> s {value = a} :: ListRecordHistorySearchFilter)

-- | The filter key.
--
-- -   @product@ - Filter results based on the specified product
--     identifier.
--
-- -   @provisionedproduct@ - Filter results based on the provisioned
--     product identifier.
listRecordHistorySearchFilter_key :: Lens.Lens' ListRecordHistorySearchFilter (Prelude.Maybe Prelude.Text)
listRecordHistorySearchFilter_key = Lens.lens (\ListRecordHistorySearchFilter' {key} -> key) (\s@ListRecordHistorySearchFilter' {} a -> s {key = a} :: ListRecordHistorySearchFilter)

instance
  Prelude.Hashable
    ListRecordHistorySearchFilter
  where
  hashWithSalt salt' ListRecordHistorySearchFilter' {..} =
    salt' `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData ListRecordHistorySearchFilter where
  rnf ListRecordHistorySearchFilter' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key

instance Core.ToJSON ListRecordHistorySearchFilter where
  toJSON ListRecordHistorySearchFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Value" Core..=) Prelude.<$> value,
            ("Key" Core..=) Prelude.<$> key
          ]
      )
