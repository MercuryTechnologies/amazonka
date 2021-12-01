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
-- Module      : Amazonka.Personalize.Types.FilterSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.FilterSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A short summary of a filter\'s attributes.
--
-- /See:/ 'newFilterSummary' smart constructor.
data FilterSummary = FilterSummary'
  { -- | If the filter failed, the reason for the failure.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The status of the filter.
    status :: Prelude.Maybe Prelude.Text,
    -- | The time at which the filter was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the filter.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the filter.
    filterArn :: Prelude.Maybe Prelude.Text,
    -- | The time at which the filter was created.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The ARN of the dataset group to which the filter belongs.
    datasetGroupArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FilterSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'filterSummary_failureReason' - If the filter failed, the reason for the failure.
--
-- 'status', 'filterSummary_status' - The status of the filter.
--
-- 'lastUpdatedDateTime', 'filterSummary_lastUpdatedDateTime' - The time at which the filter was last updated.
--
-- 'name', 'filterSummary_name' - The name of the filter.
--
-- 'filterArn', 'filterSummary_filterArn' - The ARN of the filter.
--
-- 'creationDateTime', 'filterSummary_creationDateTime' - The time at which the filter was created.
--
-- 'datasetGroupArn', 'filterSummary_datasetGroupArn' - The ARN of the dataset group to which the filter belongs.
newFilterSummary ::
  FilterSummary
newFilterSummary =
  FilterSummary'
    { failureReason = Prelude.Nothing,
      status = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      filterArn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      datasetGroupArn = Prelude.Nothing
    }

-- | If the filter failed, the reason for the failure.
filterSummary_failureReason :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.Text)
filterSummary_failureReason = Lens.lens (\FilterSummary' {failureReason} -> failureReason) (\s@FilterSummary' {} a -> s {failureReason = a} :: FilterSummary)

-- | The status of the filter.
filterSummary_status :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.Text)
filterSummary_status = Lens.lens (\FilterSummary' {status} -> status) (\s@FilterSummary' {} a -> s {status = a} :: FilterSummary)

-- | The time at which the filter was last updated.
filterSummary_lastUpdatedDateTime :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.UTCTime)
filterSummary_lastUpdatedDateTime = Lens.lens (\FilterSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@FilterSummary' {} a -> s {lastUpdatedDateTime = a} :: FilterSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the filter.
filterSummary_name :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.Text)
filterSummary_name = Lens.lens (\FilterSummary' {name} -> name) (\s@FilterSummary' {} a -> s {name = a} :: FilterSummary)

-- | The ARN of the filter.
filterSummary_filterArn :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.Text)
filterSummary_filterArn = Lens.lens (\FilterSummary' {filterArn} -> filterArn) (\s@FilterSummary' {} a -> s {filterArn = a} :: FilterSummary)

-- | The time at which the filter was created.
filterSummary_creationDateTime :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.UTCTime)
filterSummary_creationDateTime = Lens.lens (\FilterSummary' {creationDateTime} -> creationDateTime) (\s@FilterSummary' {} a -> s {creationDateTime = a} :: FilterSummary) Prelude.. Lens.mapping Core._Time

-- | The ARN of the dataset group to which the filter belongs.
filterSummary_datasetGroupArn :: Lens.Lens' FilterSummary (Prelude.Maybe Prelude.Text)
filterSummary_datasetGroupArn = Lens.lens (\FilterSummary' {datasetGroupArn} -> datasetGroupArn) (\s@FilterSummary' {} a -> s {datasetGroupArn = a} :: FilterSummary)

instance Core.FromJSON FilterSummary where
  parseJSON =
    Core.withObject
      "FilterSummary"
      ( \x ->
          FilterSummary'
            Prelude.<$> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "filterArn")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "datasetGroupArn")
      )

instance Prelude.Hashable FilterSummary where
  hashWithSalt salt' FilterSummary' {..} =
    salt' `Prelude.hashWithSalt` datasetGroupArn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` filterArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` failureReason

instance Prelude.NFData FilterSummary where
  rnf FilterSummary' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf datasetGroupArn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf filterArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf status
