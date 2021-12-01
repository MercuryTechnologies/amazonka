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
-- Module      : Amazonka.WellArchitected.Types.PillarReviewSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.PillarReviewSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.Risk

-- | A pillar review summary of a lens review.
--
-- /See:/ 'newPillarReviewSummary' smart constructor.
data PillarReviewSummary = PillarReviewSummary'
  { pillarId :: Prelude.Maybe Prelude.Text,
    pillarName :: Prelude.Maybe Prelude.Text,
    riskCounts :: Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural),
    notes :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PillarReviewSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pillarId', 'pillarReviewSummary_pillarId' - Undocumented member.
--
-- 'pillarName', 'pillarReviewSummary_pillarName' - Undocumented member.
--
-- 'riskCounts', 'pillarReviewSummary_riskCounts' - Undocumented member.
--
-- 'notes', 'pillarReviewSummary_notes' - Undocumented member.
newPillarReviewSummary ::
  PillarReviewSummary
newPillarReviewSummary =
  PillarReviewSummary'
    { pillarId = Prelude.Nothing,
      pillarName = Prelude.Nothing,
      riskCounts = Prelude.Nothing,
      notes = Prelude.Nothing
    }

-- | Undocumented member.
pillarReviewSummary_pillarId :: Lens.Lens' PillarReviewSummary (Prelude.Maybe Prelude.Text)
pillarReviewSummary_pillarId = Lens.lens (\PillarReviewSummary' {pillarId} -> pillarId) (\s@PillarReviewSummary' {} a -> s {pillarId = a} :: PillarReviewSummary)

-- | Undocumented member.
pillarReviewSummary_pillarName :: Lens.Lens' PillarReviewSummary (Prelude.Maybe Prelude.Text)
pillarReviewSummary_pillarName = Lens.lens (\PillarReviewSummary' {pillarName} -> pillarName) (\s@PillarReviewSummary' {} a -> s {pillarName = a} :: PillarReviewSummary)

-- | Undocumented member.
pillarReviewSummary_riskCounts :: Lens.Lens' PillarReviewSummary (Prelude.Maybe (Prelude.HashMap Risk Prelude.Natural))
pillarReviewSummary_riskCounts = Lens.lens (\PillarReviewSummary' {riskCounts} -> riskCounts) (\s@PillarReviewSummary' {} a -> s {riskCounts = a} :: PillarReviewSummary) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
pillarReviewSummary_notes :: Lens.Lens' PillarReviewSummary (Prelude.Maybe Prelude.Text)
pillarReviewSummary_notes = Lens.lens (\PillarReviewSummary' {notes} -> notes) (\s@PillarReviewSummary' {} a -> s {notes = a} :: PillarReviewSummary)

instance Core.FromJSON PillarReviewSummary where
  parseJSON =
    Core.withObject
      "PillarReviewSummary"
      ( \x ->
          PillarReviewSummary'
            Prelude.<$> (x Core..:? "PillarId")
            Prelude.<*> (x Core..:? "PillarName")
            Prelude.<*> (x Core..:? "RiskCounts" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Notes")
      )

instance Prelude.Hashable PillarReviewSummary where
  hashWithSalt salt' PillarReviewSummary' {..} =
    salt' `Prelude.hashWithSalt` notes
      `Prelude.hashWithSalt` riskCounts
      `Prelude.hashWithSalt` pillarName
      `Prelude.hashWithSalt` pillarId

instance Prelude.NFData PillarReviewSummary where
  rnf PillarReviewSummary' {..} =
    Prelude.rnf pillarId
      `Prelude.seq` Prelude.rnf notes
      `Prelude.seq` Prelude.rnf riskCounts
      `Prelude.seq` Prelude.rnf pillarName
