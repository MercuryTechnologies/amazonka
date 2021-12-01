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
-- Module      : Amazonka.CodeCommit.Types.ApprovalRuleEventMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ApprovalRuleEventMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about an event for an approval rule.
--
-- /See:/ 'newApprovalRuleEventMetadata' smart constructor.
data ApprovalRuleEventMetadata = ApprovalRuleEventMetadata'
  { -- | The name of the approval rule.
    approvalRuleName :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID of the approval rule.
    approvalRuleId :: Prelude.Maybe Prelude.Text,
    -- | The content of the approval rule.
    approvalRuleContent :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApprovalRuleEventMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleName', 'approvalRuleEventMetadata_approvalRuleName' - The name of the approval rule.
--
-- 'approvalRuleId', 'approvalRuleEventMetadata_approvalRuleId' - The system-generated ID of the approval rule.
--
-- 'approvalRuleContent', 'approvalRuleEventMetadata_approvalRuleContent' - The content of the approval rule.
newApprovalRuleEventMetadata ::
  ApprovalRuleEventMetadata
newApprovalRuleEventMetadata =
  ApprovalRuleEventMetadata'
    { approvalRuleName =
        Prelude.Nothing,
      approvalRuleId = Prelude.Nothing,
      approvalRuleContent = Prelude.Nothing
    }

-- | The name of the approval rule.
approvalRuleEventMetadata_approvalRuleName :: Lens.Lens' ApprovalRuleEventMetadata (Prelude.Maybe Prelude.Text)
approvalRuleEventMetadata_approvalRuleName = Lens.lens (\ApprovalRuleEventMetadata' {approvalRuleName} -> approvalRuleName) (\s@ApprovalRuleEventMetadata' {} a -> s {approvalRuleName = a} :: ApprovalRuleEventMetadata)

-- | The system-generated ID of the approval rule.
approvalRuleEventMetadata_approvalRuleId :: Lens.Lens' ApprovalRuleEventMetadata (Prelude.Maybe Prelude.Text)
approvalRuleEventMetadata_approvalRuleId = Lens.lens (\ApprovalRuleEventMetadata' {approvalRuleId} -> approvalRuleId) (\s@ApprovalRuleEventMetadata' {} a -> s {approvalRuleId = a} :: ApprovalRuleEventMetadata)

-- | The content of the approval rule.
approvalRuleEventMetadata_approvalRuleContent :: Lens.Lens' ApprovalRuleEventMetadata (Prelude.Maybe Prelude.Text)
approvalRuleEventMetadata_approvalRuleContent = Lens.lens (\ApprovalRuleEventMetadata' {approvalRuleContent} -> approvalRuleContent) (\s@ApprovalRuleEventMetadata' {} a -> s {approvalRuleContent = a} :: ApprovalRuleEventMetadata)

instance Core.FromJSON ApprovalRuleEventMetadata where
  parseJSON =
    Core.withObject
      "ApprovalRuleEventMetadata"
      ( \x ->
          ApprovalRuleEventMetadata'
            Prelude.<$> (x Core..:? "approvalRuleName")
            Prelude.<*> (x Core..:? "approvalRuleId")
            Prelude.<*> (x Core..:? "approvalRuleContent")
      )

instance Prelude.Hashable ApprovalRuleEventMetadata where
  hashWithSalt salt' ApprovalRuleEventMetadata' {..} =
    salt' `Prelude.hashWithSalt` approvalRuleContent
      `Prelude.hashWithSalt` approvalRuleId
      `Prelude.hashWithSalt` approvalRuleName

instance Prelude.NFData ApprovalRuleEventMetadata where
  rnf ApprovalRuleEventMetadata' {..} =
    Prelude.rnf approvalRuleName
      `Prelude.seq` Prelude.rnf approvalRuleContent
      `Prelude.seq` Prelude.rnf approvalRuleId
