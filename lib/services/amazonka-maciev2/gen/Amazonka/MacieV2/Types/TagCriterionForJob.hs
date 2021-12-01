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
-- Module      : Amazonka.MacieV2.Types.TagCriterionForJob
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.TagCriterionForJob where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MacieV2.Types.JobComparator
import Amazonka.MacieV2.Types.TagCriterionPairForJob
import qualified Amazonka.Prelude as Prelude

-- | Specifies a tag-based condition that determines whether an S3 bucket is
-- included or excluded from a classification job.
--
-- /See:/ 'newTagCriterionForJob' smart constructor.
data TagCriterionForJob = TagCriterionForJob'
  { -- | The tag keys, tag values, or tag key and value pairs to use in the
    -- condition.
    tagValues :: Prelude.Maybe [TagCriterionPairForJob],
    -- | The operator to use in the condition. Valid values are EQ (equals) and
    -- NE (not equals).
    comparator :: Prelude.Maybe JobComparator
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagCriterionForJob' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagValues', 'tagCriterionForJob_tagValues' - The tag keys, tag values, or tag key and value pairs to use in the
-- condition.
--
-- 'comparator', 'tagCriterionForJob_comparator' - The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
newTagCriterionForJob ::
  TagCriterionForJob
newTagCriterionForJob =
  TagCriterionForJob'
    { tagValues = Prelude.Nothing,
      comparator = Prelude.Nothing
    }

-- | The tag keys, tag values, or tag key and value pairs to use in the
-- condition.
tagCriterionForJob_tagValues :: Lens.Lens' TagCriterionForJob (Prelude.Maybe [TagCriterionPairForJob])
tagCriterionForJob_tagValues = Lens.lens (\TagCriterionForJob' {tagValues} -> tagValues) (\s@TagCriterionForJob' {} a -> s {tagValues = a} :: TagCriterionForJob) Prelude.. Lens.mapping Lens.coerced

-- | The operator to use in the condition. Valid values are EQ (equals) and
-- NE (not equals).
tagCriterionForJob_comparator :: Lens.Lens' TagCriterionForJob (Prelude.Maybe JobComparator)
tagCriterionForJob_comparator = Lens.lens (\TagCriterionForJob' {comparator} -> comparator) (\s@TagCriterionForJob' {} a -> s {comparator = a} :: TagCriterionForJob)

instance Core.FromJSON TagCriterionForJob where
  parseJSON =
    Core.withObject
      "TagCriterionForJob"
      ( \x ->
          TagCriterionForJob'
            Prelude.<$> (x Core..:? "tagValues" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "comparator")
      )

instance Prelude.Hashable TagCriterionForJob where
  hashWithSalt salt' TagCriterionForJob' {..} =
    salt' `Prelude.hashWithSalt` comparator
      `Prelude.hashWithSalt` tagValues

instance Prelude.NFData TagCriterionForJob where
  rnf TagCriterionForJob' {..} =
    Prelude.rnf tagValues
      `Prelude.seq` Prelude.rnf comparator

instance Core.ToJSON TagCriterionForJob where
  toJSON TagCriterionForJob' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("tagValues" Core..=) Prelude.<$> tagValues,
            ("comparator" Core..=) Prelude.<$> comparator
          ]
      )
