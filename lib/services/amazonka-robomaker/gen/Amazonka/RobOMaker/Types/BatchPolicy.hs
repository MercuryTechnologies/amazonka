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
-- Module      : Amazonka.RobOMaker.Types.BatchPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.BatchPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the batch policy.
--
-- /See:/ 'newBatchPolicy' smart constructor.
data BatchPolicy = BatchPolicy'
  { -- | The amount of time, in seconds, to wait for the batch to complete.
    --
    -- If a batch times out, and there are pending requests that were failing
    -- due to an internal failure (like @InternalServiceError@), they will be
    -- moved to the failed list and the batch status will be @Failed@. If the
    -- pending requests were failing for any other reason, the failed pending
    -- requests will be moved to the failed list and the batch status will be
    -- @TimedOut@.
    timeoutInSeconds :: Prelude.Maybe Prelude.Integer,
    -- | The number of active simulation jobs create as part of the batch that
    -- can be in an active state at the same time.
    --
    -- Active states include: @Pending@,@Preparing@, @Running@, @Restarting@,
    -- @RunningFailed@ and @Terminating@. All other states are terminal states.
    maxConcurrency :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'timeoutInSeconds', 'batchPolicy_timeoutInSeconds' - The amount of time, in seconds, to wait for the batch to complete.
--
-- If a batch times out, and there are pending requests that were failing
-- due to an internal failure (like @InternalServiceError@), they will be
-- moved to the failed list and the batch status will be @Failed@. If the
-- pending requests were failing for any other reason, the failed pending
-- requests will be moved to the failed list and the batch status will be
-- @TimedOut@.
--
-- 'maxConcurrency', 'batchPolicy_maxConcurrency' - The number of active simulation jobs create as part of the batch that
-- can be in an active state at the same time.
--
-- Active states include: @Pending@,@Preparing@, @Running@, @Restarting@,
-- @RunningFailed@ and @Terminating@. All other states are terminal states.
newBatchPolicy ::
  BatchPolicy
newBatchPolicy =
  BatchPolicy'
    { timeoutInSeconds = Prelude.Nothing,
      maxConcurrency = Prelude.Nothing
    }

-- | The amount of time, in seconds, to wait for the batch to complete.
--
-- If a batch times out, and there are pending requests that were failing
-- due to an internal failure (like @InternalServiceError@), they will be
-- moved to the failed list and the batch status will be @Failed@. If the
-- pending requests were failing for any other reason, the failed pending
-- requests will be moved to the failed list and the batch status will be
-- @TimedOut@.
batchPolicy_timeoutInSeconds :: Lens.Lens' BatchPolicy (Prelude.Maybe Prelude.Integer)
batchPolicy_timeoutInSeconds = Lens.lens (\BatchPolicy' {timeoutInSeconds} -> timeoutInSeconds) (\s@BatchPolicy' {} a -> s {timeoutInSeconds = a} :: BatchPolicy)

-- | The number of active simulation jobs create as part of the batch that
-- can be in an active state at the same time.
--
-- Active states include: @Pending@,@Preparing@, @Running@, @Restarting@,
-- @RunningFailed@ and @Terminating@. All other states are terminal states.
batchPolicy_maxConcurrency :: Lens.Lens' BatchPolicy (Prelude.Maybe Prelude.Int)
batchPolicy_maxConcurrency = Lens.lens (\BatchPolicy' {maxConcurrency} -> maxConcurrency) (\s@BatchPolicy' {} a -> s {maxConcurrency = a} :: BatchPolicy)

instance Core.FromJSON BatchPolicy where
  parseJSON =
    Core.withObject
      "BatchPolicy"
      ( \x ->
          BatchPolicy'
            Prelude.<$> (x Core..:? "timeoutInSeconds")
            Prelude.<*> (x Core..:? "maxConcurrency")
      )

instance Prelude.Hashable BatchPolicy where
  hashWithSalt salt' BatchPolicy' {..} =
    salt' `Prelude.hashWithSalt` maxConcurrency
      `Prelude.hashWithSalt` timeoutInSeconds

instance Prelude.NFData BatchPolicy where
  rnf BatchPolicy' {..} =
    Prelude.rnf timeoutInSeconds
      `Prelude.seq` Prelude.rnf maxConcurrency

instance Core.ToJSON BatchPolicy where
  toJSON BatchPolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("timeoutInSeconds" Core..=)
              Prelude.<$> timeoutInSeconds,
            ("maxConcurrency" Core..=)
              Prelude.<$> maxConcurrency
          ]
      )
