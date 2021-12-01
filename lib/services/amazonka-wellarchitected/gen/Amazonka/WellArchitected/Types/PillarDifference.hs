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
-- Module      : Amazonka.WellArchitected.Types.PillarDifference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.PillarDifference where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.DifferenceStatus
import Amazonka.WellArchitected.Types.QuestionDifference

-- | A pillar difference return object.
--
-- /See:/ 'newPillarDifference' smart constructor.
data PillarDifference = PillarDifference'
  { pillarId :: Prelude.Maybe Prelude.Text,
    -- | List of question differences.
    questionDifferences :: Prelude.Maybe [QuestionDifference],
    -- | Indicates the type of change to the pillar.
    differenceStatus :: Prelude.Maybe DifferenceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PillarDifference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pillarId', 'pillarDifference_pillarId' - Undocumented member.
--
-- 'questionDifferences', 'pillarDifference_questionDifferences' - List of question differences.
--
-- 'differenceStatus', 'pillarDifference_differenceStatus' - Indicates the type of change to the pillar.
newPillarDifference ::
  PillarDifference
newPillarDifference =
  PillarDifference'
    { pillarId = Prelude.Nothing,
      questionDifferences = Prelude.Nothing,
      differenceStatus = Prelude.Nothing
    }

-- | Undocumented member.
pillarDifference_pillarId :: Lens.Lens' PillarDifference (Prelude.Maybe Prelude.Text)
pillarDifference_pillarId = Lens.lens (\PillarDifference' {pillarId} -> pillarId) (\s@PillarDifference' {} a -> s {pillarId = a} :: PillarDifference)

-- | List of question differences.
pillarDifference_questionDifferences :: Lens.Lens' PillarDifference (Prelude.Maybe [QuestionDifference])
pillarDifference_questionDifferences = Lens.lens (\PillarDifference' {questionDifferences} -> questionDifferences) (\s@PillarDifference' {} a -> s {questionDifferences = a} :: PillarDifference) Prelude.. Lens.mapping Lens.coerced

-- | Indicates the type of change to the pillar.
pillarDifference_differenceStatus :: Lens.Lens' PillarDifference (Prelude.Maybe DifferenceStatus)
pillarDifference_differenceStatus = Lens.lens (\PillarDifference' {differenceStatus} -> differenceStatus) (\s@PillarDifference' {} a -> s {differenceStatus = a} :: PillarDifference)

instance Core.FromJSON PillarDifference where
  parseJSON =
    Core.withObject
      "PillarDifference"
      ( \x ->
          PillarDifference'
            Prelude.<$> (x Core..:? "PillarId")
            Prelude.<*> ( x Core..:? "QuestionDifferences"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DifferenceStatus")
      )

instance Prelude.Hashable PillarDifference where
  hashWithSalt salt' PillarDifference' {..} =
    salt' `Prelude.hashWithSalt` differenceStatus
      `Prelude.hashWithSalt` questionDifferences
      `Prelude.hashWithSalt` pillarId

instance Prelude.NFData PillarDifference where
  rnf PillarDifference' {..} =
    Prelude.rnf pillarId
      `Prelude.seq` Prelude.rnf differenceStatus
      `Prelude.seq` Prelude.rnf questionDifferences
