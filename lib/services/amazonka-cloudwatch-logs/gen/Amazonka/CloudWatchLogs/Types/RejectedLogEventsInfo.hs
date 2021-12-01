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
-- Module      : Amazonka.CloudWatchLogs.Types.RejectedLogEventsInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchLogs.Types.RejectedLogEventsInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the rejected events.
--
-- /See:/ 'newRejectedLogEventsInfo' smart constructor.
data RejectedLogEventsInfo = RejectedLogEventsInfo'
  { -- | The log events that are too old.
    tooOldLogEventEndIndex :: Prelude.Maybe Prelude.Int,
    -- | The log events that are too new.
    tooNewLogEventStartIndex :: Prelude.Maybe Prelude.Int,
    -- | The expired log events.
    expiredLogEventEndIndex :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectedLogEventsInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tooOldLogEventEndIndex', 'rejectedLogEventsInfo_tooOldLogEventEndIndex' - The log events that are too old.
--
-- 'tooNewLogEventStartIndex', 'rejectedLogEventsInfo_tooNewLogEventStartIndex' - The log events that are too new.
--
-- 'expiredLogEventEndIndex', 'rejectedLogEventsInfo_expiredLogEventEndIndex' - The expired log events.
newRejectedLogEventsInfo ::
  RejectedLogEventsInfo
newRejectedLogEventsInfo =
  RejectedLogEventsInfo'
    { tooOldLogEventEndIndex =
        Prelude.Nothing,
      tooNewLogEventStartIndex = Prelude.Nothing,
      expiredLogEventEndIndex = Prelude.Nothing
    }

-- | The log events that are too old.
rejectedLogEventsInfo_tooOldLogEventEndIndex :: Lens.Lens' RejectedLogEventsInfo (Prelude.Maybe Prelude.Int)
rejectedLogEventsInfo_tooOldLogEventEndIndex = Lens.lens (\RejectedLogEventsInfo' {tooOldLogEventEndIndex} -> tooOldLogEventEndIndex) (\s@RejectedLogEventsInfo' {} a -> s {tooOldLogEventEndIndex = a} :: RejectedLogEventsInfo)

-- | The log events that are too new.
rejectedLogEventsInfo_tooNewLogEventStartIndex :: Lens.Lens' RejectedLogEventsInfo (Prelude.Maybe Prelude.Int)
rejectedLogEventsInfo_tooNewLogEventStartIndex = Lens.lens (\RejectedLogEventsInfo' {tooNewLogEventStartIndex} -> tooNewLogEventStartIndex) (\s@RejectedLogEventsInfo' {} a -> s {tooNewLogEventStartIndex = a} :: RejectedLogEventsInfo)

-- | The expired log events.
rejectedLogEventsInfo_expiredLogEventEndIndex :: Lens.Lens' RejectedLogEventsInfo (Prelude.Maybe Prelude.Int)
rejectedLogEventsInfo_expiredLogEventEndIndex = Lens.lens (\RejectedLogEventsInfo' {expiredLogEventEndIndex} -> expiredLogEventEndIndex) (\s@RejectedLogEventsInfo' {} a -> s {expiredLogEventEndIndex = a} :: RejectedLogEventsInfo)

instance Core.FromJSON RejectedLogEventsInfo where
  parseJSON =
    Core.withObject
      "RejectedLogEventsInfo"
      ( \x ->
          RejectedLogEventsInfo'
            Prelude.<$> (x Core..:? "tooOldLogEventEndIndex")
            Prelude.<*> (x Core..:? "tooNewLogEventStartIndex")
            Prelude.<*> (x Core..:? "expiredLogEventEndIndex")
      )

instance Prelude.Hashable RejectedLogEventsInfo where
  hashWithSalt salt' RejectedLogEventsInfo' {..} =
    salt'
      `Prelude.hashWithSalt` expiredLogEventEndIndex
      `Prelude.hashWithSalt` tooNewLogEventStartIndex
      `Prelude.hashWithSalt` tooOldLogEventEndIndex

instance Prelude.NFData RejectedLogEventsInfo where
  rnf RejectedLogEventsInfo' {..} =
    Prelude.rnf tooOldLogEventEndIndex
      `Prelude.seq` Prelude.rnf expiredLogEventEndIndex
      `Prelude.seq` Prelude.rnf tooNewLogEventStartIndex
