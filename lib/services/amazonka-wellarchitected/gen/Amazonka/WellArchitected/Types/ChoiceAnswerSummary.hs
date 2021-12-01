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
-- Module      : Amazonka.WellArchitected.Types.ChoiceAnswerSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.ChoiceAnswerSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.ChoiceReason
import Amazonka.WellArchitected.Types.ChoiceStatus

-- | A choice summary that has been answered on a question in your workload.
--
-- /See:/ 'newChoiceAnswerSummary' smart constructor.
data ChoiceAnswerSummary = ChoiceAnswerSummary'
  { -- | The status of a choice.
    status :: Prelude.Maybe ChoiceStatus,
    -- | The reason why a choice is non-applicable to a question in your
    -- workload.
    reason :: Prelude.Maybe ChoiceReason,
    choiceId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChoiceAnswerSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'choiceAnswerSummary_status' - The status of a choice.
--
-- 'reason', 'choiceAnswerSummary_reason' - The reason why a choice is non-applicable to a question in your
-- workload.
--
-- 'choiceId', 'choiceAnswerSummary_choiceId' - Undocumented member.
newChoiceAnswerSummary ::
  ChoiceAnswerSummary
newChoiceAnswerSummary =
  ChoiceAnswerSummary'
    { status = Prelude.Nothing,
      reason = Prelude.Nothing,
      choiceId = Prelude.Nothing
    }

-- | The status of a choice.
choiceAnswerSummary_status :: Lens.Lens' ChoiceAnswerSummary (Prelude.Maybe ChoiceStatus)
choiceAnswerSummary_status = Lens.lens (\ChoiceAnswerSummary' {status} -> status) (\s@ChoiceAnswerSummary' {} a -> s {status = a} :: ChoiceAnswerSummary)

-- | The reason why a choice is non-applicable to a question in your
-- workload.
choiceAnswerSummary_reason :: Lens.Lens' ChoiceAnswerSummary (Prelude.Maybe ChoiceReason)
choiceAnswerSummary_reason = Lens.lens (\ChoiceAnswerSummary' {reason} -> reason) (\s@ChoiceAnswerSummary' {} a -> s {reason = a} :: ChoiceAnswerSummary)

-- | Undocumented member.
choiceAnswerSummary_choiceId :: Lens.Lens' ChoiceAnswerSummary (Prelude.Maybe Prelude.Text)
choiceAnswerSummary_choiceId = Lens.lens (\ChoiceAnswerSummary' {choiceId} -> choiceId) (\s@ChoiceAnswerSummary' {} a -> s {choiceId = a} :: ChoiceAnswerSummary)

instance Core.FromJSON ChoiceAnswerSummary where
  parseJSON =
    Core.withObject
      "ChoiceAnswerSummary"
      ( \x ->
          ChoiceAnswerSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Reason")
            Prelude.<*> (x Core..:? "ChoiceId")
      )

instance Prelude.Hashable ChoiceAnswerSummary where
  hashWithSalt salt' ChoiceAnswerSummary' {..} =
    salt' `Prelude.hashWithSalt` choiceId
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` status

instance Prelude.NFData ChoiceAnswerSummary where
  rnf ChoiceAnswerSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf choiceId
      `Prelude.seq` Prelude.rnf reason
