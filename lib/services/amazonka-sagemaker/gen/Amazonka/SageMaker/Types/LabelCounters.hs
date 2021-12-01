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
-- Module      : Amazonka.SageMaker.Types.LabelCounters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.LabelCounters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides a breakdown of the number of objects labeled.
--
-- /See:/ 'newLabelCounters' smart constructor.
data LabelCounters = LabelCounters'
  { -- | The total number of objects labeled by automated data labeling.
    machineLabeled :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects labeled.
    totalLabeled :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects that could not be labeled due to an error.
    failedNonRetryableError :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects not yet labeled.
    unlabeled :: Prelude.Maybe Prelude.Natural,
    -- | The total number of objects labeled by a human worker.
    humanLabeled :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LabelCounters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'machineLabeled', 'labelCounters_machineLabeled' - The total number of objects labeled by automated data labeling.
--
-- 'totalLabeled', 'labelCounters_totalLabeled' - The total number of objects labeled.
--
-- 'failedNonRetryableError', 'labelCounters_failedNonRetryableError' - The total number of objects that could not be labeled due to an error.
--
-- 'unlabeled', 'labelCounters_unlabeled' - The total number of objects not yet labeled.
--
-- 'humanLabeled', 'labelCounters_humanLabeled' - The total number of objects labeled by a human worker.
newLabelCounters ::
  LabelCounters
newLabelCounters =
  LabelCounters'
    { machineLabeled = Prelude.Nothing,
      totalLabeled = Prelude.Nothing,
      failedNonRetryableError = Prelude.Nothing,
      unlabeled = Prelude.Nothing,
      humanLabeled = Prelude.Nothing
    }

-- | The total number of objects labeled by automated data labeling.
labelCounters_machineLabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_machineLabeled = Lens.lens (\LabelCounters' {machineLabeled} -> machineLabeled) (\s@LabelCounters' {} a -> s {machineLabeled = a} :: LabelCounters)

-- | The total number of objects labeled.
labelCounters_totalLabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_totalLabeled = Lens.lens (\LabelCounters' {totalLabeled} -> totalLabeled) (\s@LabelCounters' {} a -> s {totalLabeled = a} :: LabelCounters)

-- | The total number of objects that could not be labeled due to an error.
labelCounters_failedNonRetryableError :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_failedNonRetryableError = Lens.lens (\LabelCounters' {failedNonRetryableError} -> failedNonRetryableError) (\s@LabelCounters' {} a -> s {failedNonRetryableError = a} :: LabelCounters)

-- | The total number of objects not yet labeled.
labelCounters_unlabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_unlabeled = Lens.lens (\LabelCounters' {unlabeled} -> unlabeled) (\s@LabelCounters' {} a -> s {unlabeled = a} :: LabelCounters)

-- | The total number of objects labeled by a human worker.
labelCounters_humanLabeled :: Lens.Lens' LabelCounters (Prelude.Maybe Prelude.Natural)
labelCounters_humanLabeled = Lens.lens (\LabelCounters' {humanLabeled} -> humanLabeled) (\s@LabelCounters' {} a -> s {humanLabeled = a} :: LabelCounters)

instance Core.FromJSON LabelCounters where
  parseJSON =
    Core.withObject
      "LabelCounters"
      ( \x ->
          LabelCounters'
            Prelude.<$> (x Core..:? "MachineLabeled")
            Prelude.<*> (x Core..:? "TotalLabeled")
            Prelude.<*> (x Core..:? "FailedNonRetryableError")
            Prelude.<*> (x Core..:? "Unlabeled")
            Prelude.<*> (x Core..:? "HumanLabeled")
      )

instance Prelude.Hashable LabelCounters where
  hashWithSalt salt' LabelCounters' {..} =
    salt' `Prelude.hashWithSalt` humanLabeled
      `Prelude.hashWithSalt` unlabeled
      `Prelude.hashWithSalt` failedNonRetryableError
      `Prelude.hashWithSalt` totalLabeled
      `Prelude.hashWithSalt` machineLabeled

instance Prelude.NFData LabelCounters where
  rnf LabelCounters' {..} =
    Prelude.rnf machineLabeled
      `Prelude.seq` Prelude.rnf humanLabeled
      `Prelude.seq` Prelude.rnf unlabeled
      `Prelude.seq` Prelude.rnf failedNonRetryableError
      `Prelude.seq` Prelude.rnf totalLabeled
