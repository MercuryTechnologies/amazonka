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
-- Module      : Amazonka.RobOMaker.Types.WorldGenerationJobSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.WorldGenerationJobSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.RobOMaker.Types.WorldCount
import Amazonka.RobOMaker.Types.WorldGenerationJobStatus

-- | Information about a world generator job.
--
-- /See:/ 'newWorldGenerationJobSummary' smart constructor.
data WorldGenerationJobSummary = WorldGenerationJobSummary'
  { -- | The status of the world generator job:
    --
    -- [Pending]
    --     The world generator job request is pending.
    --
    -- [Running]
    --     The world generator job is running.
    --
    -- [Completed]
    --     The world generator job completed.
    --
    -- [Failed]
    --     The world generator job failed. See @failureCode@ for more
    --     information.
    --
    -- [PartialFailed]
    --     Some worlds did not generate.
    --
    -- [Canceled]
    --     The world generator job was cancelled.
    --
    -- [Canceling]
    --     The world generator job is being cancelled.
    status :: Prelude.Maybe WorldGenerationJobStatus,
    -- | The Amazon Resource Name (ARN) of the world generator job.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time, in milliseconds since the epoch, when the world generator job
    -- was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | Information about the world count.
    worldCount :: Prelude.Maybe WorldCount,
    -- | The number of worlds that were generated.
    succeededWorldCount :: Prelude.Maybe Prelude.Int,
    -- | The number of worlds that failed.
    failedWorldCount :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (arn) of the world template.
    template :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorldGenerationJobSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'worldGenerationJobSummary_status' - The status of the world generator job:
--
-- [Pending]
--     The world generator job request is pending.
--
-- [Running]
--     The world generator job is running.
--
-- [Completed]
--     The world generator job completed.
--
-- [Failed]
--     The world generator job failed. See @failureCode@ for more
--     information.
--
-- [PartialFailed]
--     Some worlds did not generate.
--
-- [Canceled]
--     The world generator job was cancelled.
--
-- [Canceling]
--     The world generator job is being cancelled.
--
-- 'arn', 'worldGenerationJobSummary_arn' - The Amazon Resource Name (ARN) of the world generator job.
--
-- 'createdAt', 'worldGenerationJobSummary_createdAt' - The time, in milliseconds since the epoch, when the world generator job
-- was created.
--
-- 'worldCount', 'worldGenerationJobSummary_worldCount' - Information about the world count.
--
-- 'succeededWorldCount', 'worldGenerationJobSummary_succeededWorldCount' - The number of worlds that were generated.
--
-- 'failedWorldCount', 'worldGenerationJobSummary_failedWorldCount' - The number of worlds that failed.
--
-- 'template', 'worldGenerationJobSummary_template' - The Amazon Resource Name (arn) of the world template.
newWorldGenerationJobSummary ::
  WorldGenerationJobSummary
newWorldGenerationJobSummary =
  WorldGenerationJobSummary'
    { status =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      worldCount = Prelude.Nothing,
      succeededWorldCount = Prelude.Nothing,
      failedWorldCount = Prelude.Nothing,
      template = Prelude.Nothing
    }

-- | The status of the world generator job:
--
-- [Pending]
--     The world generator job request is pending.
--
-- [Running]
--     The world generator job is running.
--
-- [Completed]
--     The world generator job completed.
--
-- [Failed]
--     The world generator job failed. See @failureCode@ for more
--     information.
--
-- [PartialFailed]
--     Some worlds did not generate.
--
-- [Canceled]
--     The world generator job was cancelled.
--
-- [Canceling]
--     The world generator job is being cancelled.
worldGenerationJobSummary_status :: Lens.Lens' WorldGenerationJobSummary (Prelude.Maybe WorldGenerationJobStatus)
worldGenerationJobSummary_status = Lens.lens (\WorldGenerationJobSummary' {status} -> status) (\s@WorldGenerationJobSummary' {} a -> s {status = a} :: WorldGenerationJobSummary)

-- | The Amazon Resource Name (ARN) of the world generator job.
worldGenerationJobSummary_arn :: Lens.Lens' WorldGenerationJobSummary (Prelude.Maybe Prelude.Text)
worldGenerationJobSummary_arn = Lens.lens (\WorldGenerationJobSummary' {arn} -> arn) (\s@WorldGenerationJobSummary' {} a -> s {arn = a} :: WorldGenerationJobSummary)

-- | The time, in milliseconds since the epoch, when the world generator job
-- was created.
worldGenerationJobSummary_createdAt :: Lens.Lens' WorldGenerationJobSummary (Prelude.Maybe Prelude.UTCTime)
worldGenerationJobSummary_createdAt = Lens.lens (\WorldGenerationJobSummary' {createdAt} -> createdAt) (\s@WorldGenerationJobSummary' {} a -> s {createdAt = a} :: WorldGenerationJobSummary) Prelude.. Lens.mapping Core._Time

-- | Information about the world count.
worldGenerationJobSummary_worldCount :: Lens.Lens' WorldGenerationJobSummary (Prelude.Maybe WorldCount)
worldGenerationJobSummary_worldCount = Lens.lens (\WorldGenerationJobSummary' {worldCount} -> worldCount) (\s@WorldGenerationJobSummary' {} a -> s {worldCount = a} :: WorldGenerationJobSummary)

-- | The number of worlds that were generated.
worldGenerationJobSummary_succeededWorldCount :: Lens.Lens' WorldGenerationJobSummary (Prelude.Maybe Prelude.Int)
worldGenerationJobSummary_succeededWorldCount = Lens.lens (\WorldGenerationJobSummary' {succeededWorldCount} -> succeededWorldCount) (\s@WorldGenerationJobSummary' {} a -> s {succeededWorldCount = a} :: WorldGenerationJobSummary)

-- | The number of worlds that failed.
worldGenerationJobSummary_failedWorldCount :: Lens.Lens' WorldGenerationJobSummary (Prelude.Maybe Prelude.Int)
worldGenerationJobSummary_failedWorldCount = Lens.lens (\WorldGenerationJobSummary' {failedWorldCount} -> failedWorldCount) (\s@WorldGenerationJobSummary' {} a -> s {failedWorldCount = a} :: WorldGenerationJobSummary)

-- | The Amazon Resource Name (arn) of the world template.
worldGenerationJobSummary_template :: Lens.Lens' WorldGenerationJobSummary (Prelude.Maybe Prelude.Text)
worldGenerationJobSummary_template = Lens.lens (\WorldGenerationJobSummary' {template} -> template) (\s@WorldGenerationJobSummary' {} a -> s {template = a} :: WorldGenerationJobSummary)

instance Core.FromJSON WorldGenerationJobSummary where
  parseJSON =
    Core.withObject
      "WorldGenerationJobSummary"
      ( \x ->
          WorldGenerationJobSummary'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "worldCount")
            Prelude.<*> (x Core..:? "succeededWorldCount")
            Prelude.<*> (x Core..:? "failedWorldCount")
            Prelude.<*> (x Core..:? "template")
      )

instance Prelude.Hashable WorldGenerationJobSummary where
  hashWithSalt salt' WorldGenerationJobSummary' {..} =
    salt' `Prelude.hashWithSalt` template
      `Prelude.hashWithSalt` failedWorldCount
      `Prelude.hashWithSalt` succeededWorldCount
      `Prelude.hashWithSalt` worldCount
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status

instance Prelude.NFData WorldGenerationJobSummary where
  rnf WorldGenerationJobSummary' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf template
      `Prelude.seq` Prelude.rnf failedWorldCount
      `Prelude.seq` Prelude.rnf succeededWorldCount
      `Prelude.seq` Prelude.rnf worldCount
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf arn
