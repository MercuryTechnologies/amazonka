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
-- Module      : Amazonka.CloudWatchEvents.Types.Replay
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudWatchEvents.Types.Replay where

import Amazonka.CloudWatchEvents.Types.ReplayState
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A @Replay@ object that contains details about a replay.
--
-- /See:/ 'newReplay' smart constructor.
data Replay = Replay'
  { -- | The ARN of the archive to replay event from.
    eventSourceArn :: Prelude.Maybe Prelude.Text,
    -- | The current state of the replay.
    state :: Prelude.Maybe ReplayState,
    -- | A time stamp for the time to start replaying events. Any event with a
    -- creation time prior to the @EventEndTime@ specified is replayed.
    eventEndTime :: Prelude.Maybe Core.POSIX,
    -- | A time stamp for the time that the replay started.
    replayStartTime :: Prelude.Maybe Core.POSIX,
    -- | A time stamp for the time that the replay completed.
    replayEndTime :: Prelude.Maybe Core.POSIX,
    -- | A time stamp for the time that the last event was replayed.
    eventLastReplayedTime :: Prelude.Maybe Core.POSIX,
    -- | A time stamp for the time to start replaying events. This is determined
    -- by the time in the event as described in
    -- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time>.
    eventStartTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the replay.
    replayName :: Prelude.Maybe Prelude.Text,
    -- | A description of why the replay is in the current state.
    stateReason :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Replay' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventSourceArn', 'replay_eventSourceArn' - The ARN of the archive to replay event from.
--
-- 'state', 'replay_state' - The current state of the replay.
--
-- 'eventEndTime', 'replay_eventEndTime' - A time stamp for the time to start replaying events. Any event with a
-- creation time prior to the @EventEndTime@ specified is replayed.
--
-- 'replayStartTime', 'replay_replayStartTime' - A time stamp for the time that the replay started.
--
-- 'replayEndTime', 'replay_replayEndTime' - A time stamp for the time that the replay completed.
--
-- 'eventLastReplayedTime', 'replay_eventLastReplayedTime' - A time stamp for the time that the last event was replayed.
--
-- 'eventStartTime', 'replay_eventStartTime' - A time stamp for the time to start replaying events. This is determined
-- by the time in the event as described in
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time>.
--
-- 'replayName', 'replay_replayName' - The name of the replay.
--
-- 'stateReason', 'replay_stateReason' - A description of why the replay is in the current state.
newReplay ::
  Replay
newReplay =
  Replay'
    { eventSourceArn = Prelude.Nothing,
      state = Prelude.Nothing,
      eventEndTime = Prelude.Nothing,
      replayStartTime = Prelude.Nothing,
      replayEndTime = Prelude.Nothing,
      eventLastReplayedTime = Prelude.Nothing,
      eventStartTime = Prelude.Nothing,
      replayName = Prelude.Nothing,
      stateReason = Prelude.Nothing
    }

-- | The ARN of the archive to replay event from.
replay_eventSourceArn :: Lens.Lens' Replay (Prelude.Maybe Prelude.Text)
replay_eventSourceArn = Lens.lens (\Replay' {eventSourceArn} -> eventSourceArn) (\s@Replay' {} a -> s {eventSourceArn = a} :: Replay)

-- | The current state of the replay.
replay_state :: Lens.Lens' Replay (Prelude.Maybe ReplayState)
replay_state = Lens.lens (\Replay' {state} -> state) (\s@Replay' {} a -> s {state = a} :: Replay)

-- | A time stamp for the time to start replaying events. Any event with a
-- creation time prior to the @EventEndTime@ specified is replayed.
replay_eventEndTime :: Lens.Lens' Replay (Prelude.Maybe Prelude.UTCTime)
replay_eventEndTime = Lens.lens (\Replay' {eventEndTime} -> eventEndTime) (\s@Replay' {} a -> s {eventEndTime = a} :: Replay) Prelude.. Lens.mapping Core._Time

-- | A time stamp for the time that the replay started.
replay_replayStartTime :: Lens.Lens' Replay (Prelude.Maybe Prelude.UTCTime)
replay_replayStartTime = Lens.lens (\Replay' {replayStartTime} -> replayStartTime) (\s@Replay' {} a -> s {replayStartTime = a} :: Replay) Prelude.. Lens.mapping Core._Time

-- | A time stamp for the time that the replay completed.
replay_replayEndTime :: Lens.Lens' Replay (Prelude.Maybe Prelude.UTCTime)
replay_replayEndTime = Lens.lens (\Replay' {replayEndTime} -> replayEndTime) (\s@Replay' {} a -> s {replayEndTime = a} :: Replay) Prelude.. Lens.mapping Core._Time

-- | A time stamp for the time that the last event was replayed.
replay_eventLastReplayedTime :: Lens.Lens' Replay (Prelude.Maybe Prelude.UTCTime)
replay_eventLastReplayedTime = Lens.lens (\Replay' {eventLastReplayedTime} -> eventLastReplayedTime) (\s@Replay' {} a -> s {eventLastReplayedTime = a} :: Replay) Prelude.. Lens.mapping Core._Time

-- | A time stamp for the time to start replaying events. This is determined
-- by the time in the event as described in
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutEventsRequestEntry.html#eventbridge-Type-PutEventsRequestEntry-Time Time>.
replay_eventStartTime :: Lens.Lens' Replay (Prelude.Maybe Prelude.UTCTime)
replay_eventStartTime = Lens.lens (\Replay' {eventStartTime} -> eventStartTime) (\s@Replay' {} a -> s {eventStartTime = a} :: Replay) Prelude.. Lens.mapping Core._Time

-- | The name of the replay.
replay_replayName :: Lens.Lens' Replay (Prelude.Maybe Prelude.Text)
replay_replayName = Lens.lens (\Replay' {replayName} -> replayName) (\s@Replay' {} a -> s {replayName = a} :: Replay)

-- | A description of why the replay is in the current state.
replay_stateReason :: Lens.Lens' Replay (Prelude.Maybe Prelude.Text)
replay_stateReason = Lens.lens (\Replay' {stateReason} -> stateReason) (\s@Replay' {} a -> s {stateReason = a} :: Replay)

instance Core.FromJSON Replay where
  parseJSON =
    Core.withObject
      "Replay"
      ( \x ->
          Replay'
            Prelude.<$> (x Core..:? "EventSourceArn")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "EventEndTime")
            Prelude.<*> (x Core..:? "ReplayStartTime")
            Prelude.<*> (x Core..:? "ReplayEndTime")
            Prelude.<*> (x Core..:? "EventLastReplayedTime")
            Prelude.<*> (x Core..:? "EventStartTime")
            Prelude.<*> (x Core..:? "ReplayName")
            Prelude.<*> (x Core..:? "StateReason")
      )

instance Prelude.Hashable Replay where
  hashWithSalt salt' Replay' {..} =
    salt' `Prelude.hashWithSalt` stateReason
      `Prelude.hashWithSalt` replayName
      `Prelude.hashWithSalt` eventStartTime
      `Prelude.hashWithSalt` eventLastReplayedTime
      `Prelude.hashWithSalt` replayEndTime
      `Prelude.hashWithSalt` replayStartTime
      `Prelude.hashWithSalt` eventEndTime
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` eventSourceArn

instance Prelude.NFData Replay where
  rnf Replay' {..} =
    Prelude.rnf eventSourceArn
      `Prelude.seq` Prelude.rnf stateReason
      `Prelude.seq` Prelude.rnf replayName
      `Prelude.seq` Prelude.rnf eventStartTime
      `Prelude.seq` Prelude.rnf eventLastReplayedTime
      `Prelude.seq` Prelude.rnf replayEndTime
      `Prelude.seq` Prelude.rnf replayStartTime
      `Prelude.seq` Prelude.rnf eventEndTime
      `Prelude.seq` Prelude.rnf state
