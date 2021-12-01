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
-- Module      : Amazonka.MacieV2.Types.SearchResourcesCriteria
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SearchResourcesCriteria where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types.SearchResourcesSimpleCriterion
import Amazonka.MacieV2.Types.SearchResourcesTagCriterion
import qualified Amazonka.Prelude as Prelude

-- | Specifies a property- or tag-based filter condition for including or
-- excluding Amazon Web Services resources from the query results.
--
-- /See:/ 'newSearchResourcesCriteria' smart constructor.
data SearchResourcesCriteria = SearchResourcesCriteria'
  { -- | A tag-based condition that defines an operator and tag keys, tag values,
    -- or tag key and value pairs for including or excluding resources from the
    -- results.
    tagCriterion :: Prelude.Maybe SearchResourcesTagCriterion,
    -- | A property-based condition that defines a property, operator, and one or
    -- more values for including or excluding resources from the results.
    simpleCriterion :: Prelude.Maybe SearchResourcesSimpleCriterion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SearchResourcesCriteria' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagCriterion', 'searchResourcesCriteria_tagCriterion' - A tag-based condition that defines an operator and tag keys, tag values,
-- or tag key and value pairs for including or excluding resources from the
-- results.
--
-- 'simpleCriterion', 'searchResourcesCriteria_simpleCriterion' - A property-based condition that defines a property, operator, and one or
-- more values for including or excluding resources from the results.
newSearchResourcesCriteria ::
  SearchResourcesCriteria
newSearchResourcesCriteria =
  SearchResourcesCriteria'
    { tagCriterion =
        Prelude.Nothing,
      simpleCriterion = Prelude.Nothing
    }

-- | A tag-based condition that defines an operator and tag keys, tag values,
-- or tag key and value pairs for including or excluding resources from the
-- results.
searchResourcesCriteria_tagCriterion :: Lens.Lens' SearchResourcesCriteria (Prelude.Maybe SearchResourcesTagCriterion)
searchResourcesCriteria_tagCriterion = Lens.lens (\SearchResourcesCriteria' {tagCriterion} -> tagCriterion) (\s@SearchResourcesCriteria' {} a -> s {tagCriterion = a} :: SearchResourcesCriteria)

-- | A property-based condition that defines a property, operator, and one or
-- more values for including or excluding resources from the results.
searchResourcesCriteria_simpleCriterion :: Lens.Lens' SearchResourcesCriteria (Prelude.Maybe SearchResourcesSimpleCriterion)
searchResourcesCriteria_simpleCriterion = Lens.lens (\SearchResourcesCriteria' {simpleCriterion} -> simpleCriterion) (\s@SearchResourcesCriteria' {} a -> s {simpleCriterion = a} :: SearchResourcesCriteria)

instance Prelude.Hashable SearchResourcesCriteria where
  hashWithSalt salt' SearchResourcesCriteria' {..} =
    salt' `Prelude.hashWithSalt` simpleCriterion
      `Prelude.hashWithSalt` tagCriterion

instance Prelude.NFData SearchResourcesCriteria where
  rnf SearchResourcesCriteria' {..} =
    Prelude.rnf tagCriterion
      `Prelude.seq` Prelude.rnf simpleCriterion

instance Core.ToJSON SearchResourcesCriteria where
  toJSON SearchResourcesCriteria' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tagCriterion" Core..=) Prelude.<$> tagCriterion,
            ("simpleCriterion" Core..=)
              Prelude.<$> simpleCriterion
          ]
      )
