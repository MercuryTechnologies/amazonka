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
-- Module      : Amazonka.SageMaker.Types.StoppingCondition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.StoppingCondition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies a limit to how long a model training job or model compilation
-- job can run. It also specifies how long a managed spot training job has
-- to complete. When the job reaches the time limit, Amazon SageMaker ends
-- the training or compilation job. Use this API to cap model training
-- costs.
--
-- To stop a training job, Amazon SageMaker sends the algorithm the
-- @SIGTERM@ signal, which delays job termination for 120 seconds.
-- Algorithms can use this 120-second window to save the model artifacts,
-- so the results of training are not lost.
--
-- The training algorithms provided by Amazon SageMaker automatically save
-- the intermediate results of a model training job when possible. This
-- attempt to save artifacts is only a best effort case as model might not
-- be in a state from which it can be saved. For example, if training has
-- just started, the model might not be ready to save. When saved, this
-- intermediate data is a valid model artifact. You can use it to create a
-- model with @CreateModel@.
--
-- The Neural Topic Model (NTM) currently does not support saving
-- intermediate model artifacts. When training NTMs, make sure that the
-- maximum runtime is sufficient for the training job to complete.
--
-- /See:/ 'newStoppingCondition' smart constructor.
data StoppingCondition = StoppingCondition'
  { -- | The maximum length of time, in seconds, that a managed Spot training job
    -- has to complete. It is the amount of time spent waiting for Spot
    -- capacity plus the amount of time the job can run. It must be equal to or
    -- greater than @MaxRuntimeInSeconds@. If the job does not complete during
    -- this time, Amazon SageMaker ends the job.
    --
    -- When @RetryStrategy@ is specified in the job request,
    -- @MaxWaitTimeInSeconds@ specifies the maximum time for all of the
    -- attempts in total, not each individual attempt.
    maxWaitTimeInSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The maximum length of time, in seconds, that a training or compilation
    -- job can run.
    --
    -- For compilation jobs, if the job does not complete during this time, you
    -- will receive a @TimeOut@ error. We recommend starting with 900 seconds
    -- and increase as necessary based on your model.
    --
    -- For all other jobs, if the job does not complete during this time,
    -- Amazon SageMaker ends the job. When @RetryStrategy@ is specified in the
    -- job request, @MaxRuntimeInSeconds@ specifies the maximum time for all of
    -- the attempts in total, not each individual attempt. The default value is
    -- 1 day. The maximum value is 28 days.
    maxRuntimeInSeconds :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StoppingCondition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxWaitTimeInSeconds', 'stoppingCondition_maxWaitTimeInSeconds' - The maximum length of time, in seconds, that a managed Spot training job
-- has to complete. It is the amount of time spent waiting for Spot
-- capacity plus the amount of time the job can run. It must be equal to or
-- greater than @MaxRuntimeInSeconds@. If the job does not complete during
-- this time, Amazon SageMaker ends the job.
--
-- When @RetryStrategy@ is specified in the job request,
-- @MaxWaitTimeInSeconds@ specifies the maximum time for all of the
-- attempts in total, not each individual attempt.
--
-- 'maxRuntimeInSeconds', 'stoppingCondition_maxRuntimeInSeconds' - The maximum length of time, in seconds, that a training or compilation
-- job can run.
--
-- For compilation jobs, if the job does not complete during this time, you
-- will receive a @TimeOut@ error. We recommend starting with 900 seconds
-- and increase as necessary based on your model.
--
-- For all other jobs, if the job does not complete during this time,
-- Amazon SageMaker ends the job. When @RetryStrategy@ is specified in the
-- job request, @MaxRuntimeInSeconds@ specifies the maximum time for all of
-- the attempts in total, not each individual attempt. The default value is
-- 1 day. The maximum value is 28 days.
newStoppingCondition ::
  StoppingCondition
newStoppingCondition =
  StoppingCondition'
    { maxWaitTimeInSeconds =
        Prelude.Nothing,
      maxRuntimeInSeconds = Prelude.Nothing
    }

-- | The maximum length of time, in seconds, that a managed Spot training job
-- has to complete. It is the amount of time spent waiting for Spot
-- capacity plus the amount of time the job can run. It must be equal to or
-- greater than @MaxRuntimeInSeconds@. If the job does not complete during
-- this time, Amazon SageMaker ends the job.
--
-- When @RetryStrategy@ is specified in the job request,
-- @MaxWaitTimeInSeconds@ specifies the maximum time for all of the
-- attempts in total, not each individual attempt.
stoppingCondition_maxWaitTimeInSeconds :: Lens.Lens' StoppingCondition (Prelude.Maybe Prelude.Natural)
stoppingCondition_maxWaitTimeInSeconds = Lens.lens (\StoppingCondition' {maxWaitTimeInSeconds} -> maxWaitTimeInSeconds) (\s@StoppingCondition' {} a -> s {maxWaitTimeInSeconds = a} :: StoppingCondition)

-- | The maximum length of time, in seconds, that a training or compilation
-- job can run.
--
-- For compilation jobs, if the job does not complete during this time, you
-- will receive a @TimeOut@ error. We recommend starting with 900 seconds
-- and increase as necessary based on your model.
--
-- For all other jobs, if the job does not complete during this time,
-- Amazon SageMaker ends the job. When @RetryStrategy@ is specified in the
-- job request, @MaxRuntimeInSeconds@ specifies the maximum time for all of
-- the attempts in total, not each individual attempt. The default value is
-- 1 day. The maximum value is 28 days.
stoppingCondition_maxRuntimeInSeconds :: Lens.Lens' StoppingCondition (Prelude.Maybe Prelude.Natural)
stoppingCondition_maxRuntimeInSeconds = Lens.lens (\StoppingCondition' {maxRuntimeInSeconds} -> maxRuntimeInSeconds) (\s@StoppingCondition' {} a -> s {maxRuntimeInSeconds = a} :: StoppingCondition)

instance Core.FromJSON StoppingCondition where
  parseJSON =
    Core.withObject
      "StoppingCondition"
      ( \x ->
          StoppingCondition'
            Prelude.<$> (x Core..:? "MaxWaitTimeInSeconds")
            Prelude.<*> (x Core..:? "MaxRuntimeInSeconds")
      )

instance Prelude.Hashable StoppingCondition where
  hashWithSalt salt' StoppingCondition' {..} =
    salt' `Prelude.hashWithSalt` maxRuntimeInSeconds
      `Prelude.hashWithSalt` maxWaitTimeInSeconds

instance Prelude.NFData StoppingCondition where
  rnf StoppingCondition' {..} =
    Prelude.rnf maxWaitTimeInSeconds
      `Prelude.seq` Prelude.rnf maxRuntimeInSeconds

instance Core.ToJSON StoppingCondition where
  toJSON StoppingCondition' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxWaitTimeInSeconds" Core..=)
              Prelude.<$> maxWaitTimeInSeconds,
            ("MaxRuntimeInSeconds" Core..=)
              Prelude.<$> maxRuntimeInSeconds
          ]
      )
