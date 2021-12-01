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
-- Module      : Amazonka.Config.Types.FailedRemediationBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.FailedRemediationBatch where

import Amazonka.Config.Types.RemediationConfiguration
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | List of each of the failed remediations with specific reasons.
--
-- /See:/ 'newFailedRemediationBatch' smart constructor.
data FailedRemediationBatch = FailedRemediationBatch'
  { -- | Returns a failure message. For example, the resource is already
    -- compliant.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | Returns remediation configurations of the failed items.
    failedItems :: Prelude.Maybe [RemediationConfiguration]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedRemediationBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureMessage', 'failedRemediationBatch_failureMessage' - Returns a failure message. For example, the resource is already
-- compliant.
--
-- 'failedItems', 'failedRemediationBatch_failedItems' - Returns remediation configurations of the failed items.
newFailedRemediationBatch ::
  FailedRemediationBatch
newFailedRemediationBatch =
  FailedRemediationBatch'
    { failureMessage =
        Prelude.Nothing,
      failedItems = Prelude.Nothing
    }

-- | Returns a failure message. For example, the resource is already
-- compliant.
failedRemediationBatch_failureMessage :: Lens.Lens' FailedRemediationBatch (Prelude.Maybe Prelude.Text)
failedRemediationBatch_failureMessage = Lens.lens (\FailedRemediationBatch' {failureMessage} -> failureMessage) (\s@FailedRemediationBatch' {} a -> s {failureMessage = a} :: FailedRemediationBatch)

-- | Returns remediation configurations of the failed items.
failedRemediationBatch_failedItems :: Lens.Lens' FailedRemediationBatch (Prelude.Maybe [RemediationConfiguration])
failedRemediationBatch_failedItems = Lens.lens (\FailedRemediationBatch' {failedItems} -> failedItems) (\s@FailedRemediationBatch' {} a -> s {failedItems = a} :: FailedRemediationBatch) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON FailedRemediationBatch where
  parseJSON =
    Core.withObject
      "FailedRemediationBatch"
      ( \x ->
          FailedRemediationBatch'
            Prelude.<$> (x Core..:? "FailureMessage")
            Prelude.<*> (x Core..:? "FailedItems" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable FailedRemediationBatch where
  hashWithSalt salt' FailedRemediationBatch' {..} =
    salt' `Prelude.hashWithSalt` failedItems
      `Prelude.hashWithSalt` failureMessage

instance Prelude.NFData FailedRemediationBatch where
  rnf FailedRemediationBatch' {..} =
    Prelude.rnf failureMessage
      `Prelude.seq` Prelude.rnf failedItems
