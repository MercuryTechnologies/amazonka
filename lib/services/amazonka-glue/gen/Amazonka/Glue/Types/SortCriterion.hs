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
-- Module      : Amazonka.Glue.Types.SortCriterion
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.SortCriterion where

import qualified Amazonka.Core as Core
import Amazonka.Glue.Types.Sort
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a field to sort by and a sort order.
--
-- /See:/ 'newSortCriterion' smart constructor.
data SortCriterion = SortCriterion'
  { -- | An ascending or descending sort.
    sort :: Prelude.Maybe Sort,
    -- | The name of the field on which to sort.
    fieldName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SortCriterion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sort', 'sortCriterion_sort' - An ascending or descending sort.
--
-- 'fieldName', 'sortCriterion_fieldName' - The name of the field on which to sort.
newSortCriterion ::
  SortCriterion
newSortCriterion =
  SortCriterion'
    { sort = Prelude.Nothing,
      fieldName = Prelude.Nothing
    }

-- | An ascending or descending sort.
sortCriterion_sort :: Lens.Lens' SortCriterion (Prelude.Maybe Sort)
sortCriterion_sort = Lens.lens (\SortCriterion' {sort} -> sort) (\s@SortCriterion' {} a -> s {sort = a} :: SortCriterion)

-- | The name of the field on which to sort.
sortCriterion_fieldName :: Lens.Lens' SortCriterion (Prelude.Maybe Prelude.Text)
sortCriterion_fieldName = Lens.lens (\SortCriterion' {fieldName} -> fieldName) (\s@SortCriterion' {} a -> s {fieldName = a} :: SortCriterion)

instance Prelude.Hashable SortCriterion where
  hashWithSalt salt' SortCriterion' {..} =
    salt' `Prelude.hashWithSalt` fieldName
      `Prelude.hashWithSalt` sort

instance Prelude.NFData SortCriterion where
  rnf SortCriterion' {..} =
    Prelude.rnf sort
      `Prelude.seq` Prelude.rnf fieldName

instance Core.ToJSON SortCriterion where
  toJSON SortCriterion' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Sort" Core..=) Prelude.<$> sort,
            ("FieldName" Core..=) Prelude.<$> fieldName
          ]
      )
