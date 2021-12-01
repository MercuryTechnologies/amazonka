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
-- Module      : Amazonka.XRay.Types.Trace
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.Trace where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.Segment

-- | A collection of segment documents with matching trace IDs.
--
-- /See:/ 'newTrace' smart constructor.
data Trace = Trace'
  { -- | LimitExceeded is set to true when the trace has exceeded one of the
    -- defined quotas. For more information about quotas, see
    -- <https://docs.aws.amazon.com/general/latest/gr/xray.html Amazon Web Services X-Ray endpoints and quotas>.
    limitExceeded :: Prelude.Maybe Prelude.Bool,
    -- | The unique identifier for the request that generated the trace\'s
    -- segments and subsegments.
    id :: Prelude.Maybe Prelude.Text,
    -- | Segment documents for the segments and subsegments that comprise the
    -- trace.
    segments :: Prelude.Maybe [Segment],
    -- | The length of time in seconds between the start time of the root segment
    -- and the end time of the last segment that completed.
    duration :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Trace' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limitExceeded', 'trace_limitExceeded' - LimitExceeded is set to true when the trace has exceeded one of the
-- defined quotas. For more information about quotas, see
-- <https://docs.aws.amazon.com/general/latest/gr/xray.html Amazon Web Services X-Ray endpoints and quotas>.
--
-- 'id', 'trace_id' - The unique identifier for the request that generated the trace\'s
-- segments and subsegments.
--
-- 'segments', 'trace_segments' - Segment documents for the segments and subsegments that comprise the
-- trace.
--
-- 'duration', 'trace_duration' - The length of time in seconds between the start time of the root segment
-- and the end time of the last segment that completed.
newTrace ::
  Trace
newTrace =
  Trace'
    { limitExceeded = Prelude.Nothing,
      id = Prelude.Nothing,
      segments = Prelude.Nothing,
      duration = Prelude.Nothing
    }

-- | LimitExceeded is set to true when the trace has exceeded one of the
-- defined quotas. For more information about quotas, see
-- <https://docs.aws.amazon.com/general/latest/gr/xray.html Amazon Web Services X-Ray endpoints and quotas>.
trace_limitExceeded :: Lens.Lens' Trace (Prelude.Maybe Prelude.Bool)
trace_limitExceeded = Lens.lens (\Trace' {limitExceeded} -> limitExceeded) (\s@Trace' {} a -> s {limitExceeded = a} :: Trace)

-- | The unique identifier for the request that generated the trace\'s
-- segments and subsegments.
trace_id :: Lens.Lens' Trace (Prelude.Maybe Prelude.Text)
trace_id = Lens.lens (\Trace' {id} -> id) (\s@Trace' {} a -> s {id = a} :: Trace)

-- | Segment documents for the segments and subsegments that comprise the
-- trace.
trace_segments :: Lens.Lens' Trace (Prelude.Maybe [Segment])
trace_segments = Lens.lens (\Trace' {segments} -> segments) (\s@Trace' {} a -> s {segments = a} :: Trace) Prelude.. Lens.mapping Lens.coerced

-- | The length of time in seconds between the start time of the root segment
-- and the end time of the last segment that completed.
trace_duration :: Lens.Lens' Trace (Prelude.Maybe Prelude.Double)
trace_duration = Lens.lens (\Trace' {duration} -> duration) (\s@Trace' {} a -> s {duration = a} :: Trace)

instance Core.FromJSON Trace where
  parseJSON =
    Core.withObject
      "Trace"
      ( \x ->
          Trace'
            Prelude.<$> (x Core..:? "LimitExceeded")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Segments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Duration")
      )

instance Prelude.Hashable Trace where
  hashWithSalt salt' Trace' {..} =
    salt' `Prelude.hashWithSalt` duration
      `Prelude.hashWithSalt` segments
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` limitExceeded

instance Prelude.NFData Trace where
  rnf Trace' {..} =
    Prelude.rnf limitExceeded
      `Prelude.seq` Prelude.rnf duration
      `Prelude.seq` Prelude.rnf segments
      `Prelude.seq` Prelude.rnf id
