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
-- Module      : Amazonka.MacieV2.Types.FindingStatisticsSortCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.FindingStatisticsSortCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types.FindingStatisticsSortAttributeName
import Amazonka.MacieV2.Types.OrderBy
import qualified Amazonka.Prelude as Prelude

-- | Specifies criteria for sorting the results of a query that retrieves
-- aggregated statistical data about findings.
--
-- /See:/ 'newFindingStatisticsSortCriteria' smart constructor.
data FindingStatisticsSortCriteria = FindingStatisticsSortCriteria'
  { -- | The sort order to apply to the results, based on the value for the
    -- property specified by the attributeName property. Valid values are: ASC,
    -- sort the results in ascending order; and, DESC, sort the results in
    -- descending order.
    orderBy :: Prelude.Maybe OrderBy,
    -- | The grouping to sort the results by. Valid values are: count, sort the
    -- results by the number of findings in each group of results; and,
    -- groupKey, sort the results by the name of each group of results.
    attributeName :: Prelude.Maybe FindingStatisticsSortAttributeName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FindingStatisticsSortCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'orderBy', 'findingStatisticsSortCriteria_orderBy' - The sort order to apply to the results, based on the value for the
-- property specified by the attributeName property. Valid values are: ASC,
-- sort the results in ascending order; and, DESC, sort the results in
-- descending order.
--
-- 'attributeName', 'findingStatisticsSortCriteria_attributeName' - The grouping to sort the results by. Valid values are: count, sort the
-- results by the number of findings in each group of results; and,
-- groupKey, sort the results by the name of each group of results.
newFindingStatisticsSortCriteria ::
  FindingStatisticsSortCriteria
newFindingStatisticsSortCriteria =
  FindingStatisticsSortCriteria'
    { orderBy =
        Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | The sort order to apply to the results, based on the value for the
-- property specified by the attributeName property. Valid values are: ASC,
-- sort the results in ascending order; and, DESC, sort the results in
-- descending order.
findingStatisticsSortCriteria_orderBy :: Lens.Lens' FindingStatisticsSortCriteria (Prelude.Maybe OrderBy)
findingStatisticsSortCriteria_orderBy = Lens.lens (\FindingStatisticsSortCriteria' {orderBy} -> orderBy) (\s@FindingStatisticsSortCriteria' {} a -> s {orderBy = a} :: FindingStatisticsSortCriteria)

-- | The grouping to sort the results by. Valid values are: count, sort the
-- results by the number of findings in each group of results; and,
-- groupKey, sort the results by the name of each group of results.
findingStatisticsSortCriteria_attributeName :: Lens.Lens' FindingStatisticsSortCriteria (Prelude.Maybe FindingStatisticsSortAttributeName)
findingStatisticsSortCriteria_attributeName = Lens.lens (\FindingStatisticsSortCriteria' {attributeName} -> attributeName) (\s@FindingStatisticsSortCriteria' {} a -> s {attributeName = a} :: FindingStatisticsSortCriteria)

instance
  Prelude.Hashable
    FindingStatisticsSortCriteria
  where
  hashWithSalt salt' FindingStatisticsSortCriteria' {..} =
    salt' `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` orderBy

instance Prelude.NFData FindingStatisticsSortCriteria where
  rnf FindingStatisticsSortCriteria' {..} =
    Prelude.rnf orderBy
      `Prelude.seq` Prelude.rnf attributeName

instance Core.ToJSON FindingStatisticsSortCriteria where
  toJSON FindingStatisticsSortCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("orderBy" Core..=) Prelude.<$> orderBy,
            ("attributeName" Core..=) Prelude.<$> attributeName
          ]
      )
