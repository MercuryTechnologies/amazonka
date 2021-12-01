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
-- Module      : Amazonka.WellArchitected.Types.Milestone
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.Milestone where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.Workload

-- | A milestone return object.
--
-- /See:/ 'newMilestone' smart constructor.
data Milestone = Milestone'
  { workload :: Prelude.Maybe Workload,
    milestoneNumber :: Prelude.Maybe Prelude.Natural,
    milestoneName :: Prelude.Maybe Prelude.Text,
    recordedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Milestone' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workload', 'milestone_workload' - Undocumented member.
--
-- 'milestoneNumber', 'milestone_milestoneNumber' - Undocumented member.
--
-- 'milestoneName', 'milestone_milestoneName' - Undocumented member.
--
-- 'recordedAt', 'milestone_recordedAt' - Undocumented member.
newMilestone ::
  Milestone
newMilestone =
  Milestone'
    { workload = Prelude.Nothing,
      milestoneNumber = Prelude.Nothing,
      milestoneName = Prelude.Nothing,
      recordedAt = Prelude.Nothing
    }

-- | Undocumented member.
milestone_workload :: Lens.Lens' Milestone (Prelude.Maybe Workload)
milestone_workload = Lens.lens (\Milestone' {workload} -> workload) (\s@Milestone' {} a -> s {workload = a} :: Milestone)

-- | Undocumented member.
milestone_milestoneNumber :: Lens.Lens' Milestone (Prelude.Maybe Prelude.Natural)
milestone_milestoneNumber = Lens.lens (\Milestone' {milestoneNumber} -> milestoneNumber) (\s@Milestone' {} a -> s {milestoneNumber = a} :: Milestone)

-- | Undocumented member.
milestone_milestoneName :: Lens.Lens' Milestone (Prelude.Maybe Prelude.Text)
milestone_milestoneName = Lens.lens (\Milestone' {milestoneName} -> milestoneName) (\s@Milestone' {} a -> s {milestoneName = a} :: Milestone)

-- | Undocumented member.
milestone_recordedAt :: Lens.Lens' Milestone (Prelude.Maybe Prelude.UTCTime)
milestone_recordedAt = Lens.lens (\Milestone' {recordedAt} -> recordedAt) (\s@Milestone' {} a -> s {recordedAt = a} :: Milestone) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Milestone where
  parseJSON =
    Core.withObject
      "Milestone"
      ( \x ->
          Milestone'
            Prelude.<$> (x Core..:? "Workload")
            Prelude.<*> (x Core..:? "MilestoneNumber")
            Prelude.<*> (x Core..:? "MilestoneName")
            Prelude.<*> (x Core..:? "RecordedAt")
      )

instance Prelude.Hashable Milestone where
  hashWithSalt salt' Milestone' {..} =
    salt' `Prelude.hashWithSalt` recordedAt
      `Prelude.hashWithSalt` milestoneName
      `Prelude.hashWithSalt` milestoneNumber
      `Prelude.hashWithSalt` workload

instance Prelude.NFData Milestone where
  rnf Milestone' {..} =
    Prelude.rnf workload
      `Prelude.seq` Prelude.rnf recordedAt
      `Prelude.seq` Prelude.rnf milestoneName
      `Prelude.seq` Prelude.rnf milestoneNumber
