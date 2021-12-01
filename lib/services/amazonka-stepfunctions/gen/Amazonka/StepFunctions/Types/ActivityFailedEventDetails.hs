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
-- Module      : Amazonka.StepFunctions.Types.ActivityFailedEventDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StepFunctions.Types.ActivityFailedEventDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an activity that failed during an execution.
--
-- /See:/ 'newActivityFailedEventDetails' smart constructor.
data ActivityFailedEventDetails = ActivityFailedEventDetails'
  { -- | The error code of the failure.
    error :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A more detailed explanation of the cause of the failure.
    cause :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivityFailedEventDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'error', 'activityFailedEventDetails_error' - The error code of the failure.
--
-- 'cause', 'activityFailedEventDetails_cause' - A more detailed explanation of the cause of the failure.
newActivityFailedEventDetails ::
  ActivityFailedEventDetails
newActivityFailedEventDetails =
  ActivityFailedEventDetails'
    { error =
        Prelude.Nothing,
      cause = Prelude.Nothing
    }

-- | The error code of the failure.
activityFailedEventDetails_error :: Lens.Lens' ActivityFailedEventDetails (Prelude.Maybe Prelude.Text)
activityFailedEventDetails_error = Lens.lens (\ActivityFailedEventDetails' {error} -> error) (\s@ActivityFailedEventDetails' {} a -> s {error = a} :: ActivityFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

-- | A more detailed explanation of the cause of the failure.
activityFailedEventDetails_cause :: Lens.Lens' ActivityFailedEventDetails (Prelude.Maybe Prelude.Text)
activityFailedEventDetails_cause = Lens.lens (\ActivityFailedEventDetails' {cause} -> cause) (\s@ActivityFailedEventDetails' {} a -> s {cause = a} :: ActivityFailedEventDetails) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON ActivityFailedEventDetails where
  parseJSON =
    Core.withObject
      "ActivityFailedEventDetails"
      ( \x ->
          ActivityFailedEventDetails'
            Prelude.<$> (x Core..:? "error")
            Prelude.<*> (x Core..:? "cause")
      )

instance Prelude.Hashable ActivityFailedEventDetails where
  hashWithSalt salt' ActivityFailedEventDetails' {..} =
    salt' `Prelude.hashWithSalt` cause
      `Prelude.hashWithSalt` error

instance Prelude.NFData ActivityFailedEventDetails where
  rnf ActivityFailedEventDetails' {..} =
    Prelude.rnf error `Prelude.seq` Prelude.rnf cause
