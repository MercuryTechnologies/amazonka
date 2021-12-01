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
-- Module      : Amazonka.RobOMaker.Types.WorldFailure
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.WorldFailure where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.WorldGenerationJobErrorCode

-- | Information about a failed world.
--
-- /See:/ 'newWorldFailure' smart constructor.
data WorldFailure = WorldFailure'
  { -- | The sample reason why the world failed. World errors are aggregated. A
    -- sample is used as the @sampleFailureReason@.
    sampleFailureReason :: Prelude.Maybe Prelude.Text,
    -- | The failure code of the world export job if it failed:
    --
    -- [InternalServiceError]
    --     Internal service error.
    --
    -- [LimitExceeded]
    --     The requested resource exceeds the maximum number allowed, or the
    --     number of concurrent stream requests exceeds the maximum number
    --     allowed.
    --
    -- [ResourceNotFound]
    --     The specified resource could not be found.
    --
    -- [RequestThrottled]
    --     The request was throttled.
    --
    -- [InvalidInput]
    --     An input parameter in the request is not valid.
    failureCode :: Prelude.Maybe WorldGenerationJobErrorCode,
    -- | The number of failed worlds.
    failureCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorldFailure' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sampleFailureReason', 'worldFailure_sampleFailureReason' - The sample reason why the world failed. World errors are aggregated. A
-- sample is used as the @sampleFailureReason@.
--
-- 'failureCode', 'worldFailure_failureCode' - The failure code of the world export job if it failed:
--
-- [InternalServiceError]
--     Internal service error.
--
-- [LimitExceeded]
--     The requested resource exceeds the maximum number allowed, or the
--     number of concurrent stream requests exceeds the maximum number
--     allowed.
--
-- [ResourceNotFound]
--     The specified resource could not be found.
--
-- [RequestThrottled]
--     The request was throttled.
--
-- [InvalidInput]
--     An input parameter in the request is not valid.
--
-- 'failureCount', 'worldFailure_failureCount' - The number of failed worlds.
newWorldFailure ::
  WorldFailure
newWorldFailure =
  WorldFailure'
    { sampleFailureReason =
        Prelude.Nothing,
      failureCode = Prelude.Nothing,
      failureCount = Prelude.Nothing
    }

-- | The sample reason why the world failed. World errors are aggregated. A
-- sample is used as the @sampleFailureReason@.
worldFailure_sampleFailureReason :: Lens.Lens' WorldFailure (Prelude.Maybe Prelude.Text)
worldFailure_sampleFailureReason = Lens.lens (\WorldFailure' {sampleFailureReason} -> sampleFailureReason) (\s@WorldFailure' {} a -> s {sampleFailureReason = a} :: WorldFailure)

-- | The failure code of the world export job if it failed:
--
-- [InternalServiceError]
--     Internal service error.
--
-- [LimitExceeded]
--     The requested resource exceeds the maximum number allowed, or the
--     number of concurrent stream requests exceeds the maximum number
--     allowed.
--
-- [ResourceNotFound]
--     The specified resource could not be found.
--
-- [RequestThrottled]
--     The request was throttled.
--
-- [InvalidInput]
--     An input parameter in the request is not valid.
worldFailure_failureCode :: Lens.Lens' WorldFailure (Prelude.Maybe WorldGenerationJobErrorCode)
worldFailure_failureCode = Lens.lens (\WorldFailure' {failureCode} -> failureCode) (\s@WorldFailure' {} a -> s {failureCode = a} :: WorldFailure)

-- | The number of failed worlds.
worldFailure_failureCount :: Lens.Lens' WorldFailure (Prelude.Maybe Prelude.Int)
worldFailure_failureCount = Lens.lens (\WorldFailure' {failureCount} -> failureCount) (\s@WorldFailure' {} a -> s {failureCount = a} :: WorldFailure)

instance Core.FromJSON WorldFailure where
  parseJSON =
    Core.withObject
      "WorldFailure"
      ( \x ->
          WorldFailure'
            Prelude.<$> (x Core..:? "sampleFailureReason")
            Prelude.<*> (x Core..:? "failureCode")
            Prelude.<*> (x Core..:? "failureCount")
      )

instance Prelude.Hashable WorldFailure where
  hashWithSalt salt' WorldFailure' {..} =
    salt' `Prelude.hashWithSalt` failureCount
      `Prelude.hashWithSalt` failureCode
      `Prelude.hashWithSalt` sampleFailureReason

instance Prelude.NFData WorldFailure where
  rnf WorldFailure' {..} =
    Prelude.rnf sampleFailureReason
      `Prelude.seq` Prelude.rnf failureCount
      `Prelude.seq` Prelude.rnf failureCode
