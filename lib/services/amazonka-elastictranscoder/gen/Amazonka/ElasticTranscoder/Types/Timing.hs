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
-- Module      : Amazonka.ElasticTranscoder.Types.Timing
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticTranscoder.Types.Timing where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Details about the timing of a job.
--
-- /See:/ 'newTiming' smart constructor.
data Timing = Timing'
  { -- | The time the job was submitted to Elastic Transcoder, in epoch
    -- milliseconds.
    submitTimeMillis :: Prelude.Maybe Prelude.Integer,
    -- | The time the job finished transcoding, in epoch milliseconds.
    finishTimeMillis :: Prelude.Maybe Prelude.Integer,
    -- | The time the job began transcoding, in epoch milliseconds.
    startTimeMillis :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Timing' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'submitTimeMillis', 'timing_submitTimeMillis' - The time the job was submitted to Elastic Transcoder, in epoch
-- milliseconds.
--
-- 'finishTimeMillis', 'timing_finishTimeMillis' - The time the job finished transcoding, in epoch milliseconds.
--
-- 'startTimeMillis', 'timing_startTimeMillis' - The time the job began transcoding, in epoch milliseconds.
newTiming ::
  Timing
newTiming =
  Timing'
    { submitTimeMillis = Prelude.Nothing,
      finishTimeMillis = Prelude.Nothing,
      startTimeMillis = Prelude.Nothing
    }

-- | The time the job was submitted to Elastic Transcoder, in epoch
-- milliseconds.
timing_submitTimeMillis :: Lens.Lens' Timing (Prelude.Maybe Prelude.Integer)
timing_submitTimeMillis = Lens.lens (\Timing' {submitTimeMillis} -> submitTimeMillis) (\s@Timing' {} a -> s {submitTimeMillis = a} :: Timing)

-- | The time the job finished transcoding, in epoch milliseconds.
timing_finishTimeMillis :: Lens.Lens' Timing (Prelude.Maybe Prelude.Integer)
timing_finishTimeMillis = Lens.lens (\Timing' {finishTimeMillis} -> finishTimeMillis) (\s@Timing' {} a -> s {finishTimeMillis = a} :: Timing)

-- | The time the job began transcoding, in epoch milliseconds.
timing_startTimeMillis :: Lens.Lens' Timing (Prelude.Maybe Prelude.Integer)
timing_startTimeMillis = Lens.lens (\Timing' {startTimeMillis} -> startTimeMillis) (\s@Timing' {} a -> s {startTimeMillis = a} :: Timing)

instance Core.FromJSON Timing where
  parseJSON =
    Core.withObject
      "Timing"
      ( \x ->
          Timing'
            Prelude.<$> (x Core..:? "SubmitTimeMillis")
            Prelude.<*> (x Core..:? "FinishTimeMillis")
            Prelude.<*> (x Core..:? "StartTimeMillis")
      )

instance Prelude.Hashable Timing where
  hashWithSalt salt' Timing' {..} =
    salt' `Prelude.hashWithSalt` startTimeMillis
      `Prelude.hashWithSalt` finishTimeMillis
      `Prelude.hashWithSalt` submitTimeMillis

instance Prelude.NFData Timing where
  rnf Timing' {..} =
    Prelude.rnf submitTimeMillis
      `Prelude.seq` Prelude.rnf startTimeMillis
      `Prelude.seq` Prelude.rnf finishTimeMillis
