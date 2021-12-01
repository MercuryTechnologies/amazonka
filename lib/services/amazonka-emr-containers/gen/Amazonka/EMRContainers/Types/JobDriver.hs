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
-- Module      : Amazonka.EMRContainers.Types.JobDriver
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMRContainers.Types.JobDriver where

import qualified Amazonka.Core as Core
import Amazonka.EMRContainers.Types.SparkSubmitJobDriver
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specify the driver that the job runs on.
--
-- /See:/ 'newJobDriver' smart constructor.
data JobDriver = JobDriver'
  { -- | The job driver parameters specified for spark submit.
    sparkSubmitJobDriver :: Prelude.Maybe SparkSubmitJobDriver
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobDriver' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sparkSubmitJobDriver', 'jobDriver_sparkSubmitJobDriver' - The job driver parameters specified for spark submit.
newJobDriver ::
  JobDriver
newJobDriver =
  JobDriver' {sparkSubmitJobDriver = Prelude.Nothing}

-- | The job driver parameters specified for spark submit.
jobDriver_sparkSubmitJobDriver :: Lens.Lens' JobDriver (Prelude.Maybe SparkSubmitJobDriver)
jobDriver_sparkSubmitJobDriver = Lens.lens (\JobDriver' {sparkSubmitJobDriver} -> sparkSubmitJobDriver) (\s@JobDriver' {} a -> s {sparkSubmitJobDriver = a} :: JobDriver)

instance Core.FromJSON JobDriver where
  parseJSON =
    Core.withObject
      "JobDriver"
      ( \x ->
          JobDriver'
            Prelude.<$> (x Core..:? "sparkSubmitJobDriver")
      )

instance Prelude.Hashable JobDriver where
  hashWithSalt salt' JobDriver' {..} =
    salt' `Prelude.hashWithSalt` sparkSubmitJobDriver

instance Prelude.NFData JobDriver where
  rnf JobDriver' {..} = Prelude.rnf sparkSubmitJobDriver

instance Core.ToJSON JobDriver where
  toJSON JobDriver' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("sparkSubmitJobDriver" Core..=)
              Prelude.<$> sparkSubmitJobDriver
          ]
      )
