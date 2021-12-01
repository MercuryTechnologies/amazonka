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
-- Module      : Amazonka.LakeFormation.Types.FilterCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LakeFormation.Types.FilterCondition where

import qualified Amazonka.Core as Core
import Amazonka.LakeFormation.Types.ComparisonOperator
import Amazonka.LakeFormation.Types.FieldNameString
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | This structure describes the filtering of columns in a table based on a
-- filter condition.
--
-- /See:/ 'newFilterCondition' smart constructor.
data FilterCondition = FilterCondition'
  { -- | The field to filter in the filter condition.
    field :: Prelude.Maybe FieldNameString,
    -- | The comparison operator used in the filter condition.
    comparisonOperator :: Prelude.Maybe ComparisonOperator,
    -- | A string with values used in evaluating the filter condition.
    stringValueList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'field', 'filterCondition_field' - The field to filter in the filter condition.
--
-- 'comparisonOperator', 'filterCondition_comparisonOperator' - The comparison operator used in the filter condition.
--
-- 'stringValueList', 'filterCondition_stringValueList' - A string with values used in evaluating the filter condition.
newFilterCondition ::
  FilterCondition
newFilterCondition =
  FilterCondition'
    { field = Prelude.Nothing,
      comparisonOperator = Prelude.Nothing,
      stringValueList = Prelude.Nothing
    }

-- | The field to filter in the filter condition.
filterCondition_field :: Lens.Lens' FilterCondition (Prelude.Maybe FieldNameString)
filterCondition_field = Lens.lens (\FilterCondition' {field} -> field) (\s@FilterCondition' {} a -> s {field = a} :: FilterCondition)

-- | The comparison operator used in the filter condition.
filterCondition_comparisonOperator :: Lens.Lens' FilterCondition (Prelude.Maybe ComparisonOperator)
filterCondition_comparisonOperator = Lens.lens (\FilterCondition' {comparisonOperator} -> comparisonOperator) (\s@FilterCondition' {} a -> s {comparisonOperator = a} :: FilterCondition)

-- | A string with values used in evaluating the filter condition.
filterCondition_stringValueList :: Lens.Lens' FilterCondition (Prelude.Maybe [Prelude.Text])
filterCondition_stringValueList = Lens.lens (\FilterCondition' {stringValueList} -> stringValueList) (\s@FilterCondition' {} a -> s {stringValueList = a} :: FilterCondition) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable FilterCondition where
  hashWithSalt salt' FilterCondition' {..} =
    salt' `Prelude.hashWithSalt` stringValueList
      `Prelude.hashWithSalt` comparisonOperator
      `Prelude.hashWithSalt` field

instance Prelude.NFData FilterCondition where
  rnf FilterCondition' {..} =
    Prelude.rnf field
      `Prelude.seq` Prelude.rnf stringValueList
      `Prelude.seq` Prelude.rnf comparisonOperator

instance Core.ToJSON FilterCondition where
  toJSON FilterCondition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Field" Core..=) Prelude.<$> field,
            ("ComparisonOperator" Core..=)
              Prelude.<$> comparisonOperator,
            ("StringValueList" Core..=)
              Prelude.<$> stringValueList
          ]
      )
