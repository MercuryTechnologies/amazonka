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
-- Module      : Amazonka.RobOMaker.Types.FailedCreateSimulationJobRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.FailedCreateSimulationJobRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.SimulationJobErrorCode
import Amazonka.RobOMaker.Types.SimulationJobRequest

-- | Information about a failed create simulation job request.
--
-- /See:/ 'newFailedCreateSimulationJobRequest' smart constructor.
data FailedCreateSimulationJobRequest = FailedCreateSimulationJobRequest'
  { -- | The failure reason of the simulation job request.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The failure code.
    failureCode :: Prelude.Maybe SimulationJobErrorCode,
    -- | The time, in milliseconds since the epoch, when the simulation job batch
    -- failed.
    failedAt :: Prelude.Maybe Core.POSIX,
    -- | The simulation job request.
    request :: Prelude.Maybe SimulationJobRequest
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedCreateSimulationJobRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureReason', 'failedCreateSimulationJobRequest_failureReason' - The failure reason of the simulation job request.
--
-- 'failureCode', 'failedCreateSimulationJobRequest_failureCode' - The failure code.
--
-- 'failedAt', 'failedCreateSimulationJobRequest_failedAt' - The time, in milliseconds since the epoch, when the simulation job batch
-- failed.
--
-- 'request', 'failedCreateSimulationJobRequest_request' - The simulation job request.
newFailedCreateSimulationJobRequest ::
  FailedCreateSimulationJobRequest
newFailedCreateSimulationJobRequest =
  FailedCreateSimulationJobRequest'
    { failureReason =
        Prelude.Nothing,
      failureCode = Prelude.Nothing,
      failedAt = Prelude.Nothing,
      request = Prelude.Nothing
    }

-- | The failure reason of the simulation job request.
failedCreateSimulationJobRequest_failureReason :: Lens.Lens' FailedCreateSimulationJobRequest (Prelude.Maybe Prelude.Text)
failedCreateSimulationJobRequest_failureReason = Lens.lens (\FailedCreateSimulationJobRequest' {failureReason} -> failureReason) (\s@FailedCreateSimulationJobRequest' {} a -> s {failureReason = a} :: FailedCreateSimulationJobRequest)

-- | The failure code.
failedCreateSimulationJobRequest_failureCode :: Lens.Lens' FailedCreateSimulationJobRequest (Prelude.Maybe SimulationJobErrorCode)
failedCreateSimulationJobRequest_failureCode = Lens.lens (\FailedCreateSimulationJobRequest' {failureCode} -> failureCode) (\s@FailedCreateSimulationJobRequest' {} a -> s {failureCode = a} :: FailedCreateSimulationJobRequest)

-- | The time, in milliseconds since the epoch, when the simulation job batch
-- failed.
failedCreateSimulationJobRequest_failedAt :: Lens.Lens' FailedCreateSimulationJobRequest (Prelude.Maybe Prelude.UTCTime)
failedCreateSimulationJobRequest_failedAt = Lens.lens (\FailedCreateSimulationJobRequest' {failedAt} -> failedAt) (\s@FailedCreateSimulationJobRequest' {} a -> s {failedAt = a} :: FailedCreateSimulationJobRequest) Prelude.. Lens.mapping Core._Time

-- | The simulation job request.
failedCreateSimulationJobRequest_request :: Lens.Lens' FailedCreateSimulationJobRequest (Prelude.Maybe SimulationJobRequest)
failedCreateSimulationJobRequest_request = Lens.lens (\FailedCreateSimulationJobRequest' {request} -> request) (\s@FailedCreateSimulationJobRequest' {} a -> s {request = a} :: FailedCreateSimulationJobRequest)

instance
  Core.FromJSON
    FailedCreateSimulationJobRequest
  where
  parseJSON =
    Core.withObject
      "FailedCreateSimulationJobRequest"
      ( \x ->
          FailedCreateSimulationJobRequest'
            Prelude.<$> (x Core..:? "failureReason")
            Prelude.<*> (x Core..:? "failureCode")
            Prelude.<*> (x Core..:? "failedAt")
            Prelude.<*> (x Core..:? "request")
      )

instance
  Prelude.Hashable
    FailedCreateSimulationJobRequest
  where
  hashWithSalt
    salt'
    FailedCreateSimulationJobRequest' {..} =
      salt' `Prelude.hashWithSalt` request
        `Prelude.hashWithSalt` failedAt
        `Prelude.hashWithSalt` failureCode
        `Prelude.hashWithSalt` failureReason

instance
  Prelude.NFData
    FailedCreateSimulationJobRequest
  where
  rnf FailedCreateSimulationJobRequest' {..} =
    Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf request
      `Prelude.seq` Prelude.rnf failedAt
      `Prelude.seq` Prelude.rnf failureCode
