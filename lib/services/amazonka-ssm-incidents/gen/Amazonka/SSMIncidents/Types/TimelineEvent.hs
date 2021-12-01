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
-- Module      : Amazonka.SSMIncidents.Types.TimelineEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.TimelineEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A significant event that happened during the incident.
--
-- /See:/ 'newTimelineEvent' smart constructor.
data TimelineEvent = TimelineEvent'
  { -- | A short description of the event.
    eventData :: Prelude.Text,
    -- | The ID of the timeline event.
    eventId :: Prelude.Text,
    -- | The time that the event occurred.
    eventTime :: Core.POSIX,
    -- | The type of event that occurred. Currently Incident Manager supports
    -- only the @Custom Event@ type.
    eventType :: Prelude.Text,
    -- | The time that the timeline event was last updated.
    eventUpdatedTime :: Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the incident that the event occurred
    -- during.
    incidentRecordArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TimelineEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventData', 'timelineEvent_eventData' - A short description of the event.
--
-- 'eventId', 'timelineEvent_eventId' - The ID of the timeline event.
--
-- 'eventTime', 'timelineEvent_eventTime' - The time that the event occurred.
--
-- 'eventType', 'timelineEvent_eventType' - The type of event that occurred. Currently Incident Manager supports
-- only the @Custom Event@ type.
--
-- 'eventUpdatedTime', 'timelineEvent_eventUpdatedTime' - The time that the timeline event was last updated.
--
-- 'incidentRecordArn', 'timelineEvent_incidentRecordArn' - The Amazon Resource Name (ARN) of the incident that the event occurred
-- during.
newTimelineEvent ::
  -- | 'eventData'
  Prelude.Text ->
  -- | 'eventId'
  Prelude.Text ->
  -- | 'eventTime'
  Prelude.UTCTime ->
  -- | 'eventType'
  Prelude.Text ->
  -- | 'eventUpdatedTime'
  Prelude.UTCTime ->
  -- | 'incidentRecordArn'
  Prelude.Text ->
  TimelineEvent
newTimelineEvent
  pEventData_
  pEventId_
  pEventTime_
  pEventType_
  pEventUpdatedTime_
  pIncidentRecordArn_ =
    TimelineEvent'
      { eventData = pEventData_,
        eventId = pEventId_,
        eventTime = Core._Time Lens.# pEventTime_,
        eventType = pEventType_,
        eventUpdatedTime =
          Core._Time Lens.# pEventUpdatedTime_,
        incidentRecordArn = pIncidentRecordArn_
      }

-- | A short description of the event.
timelineEvent_eventData :: Lens.Lens' TimelineEvent Prelude.Text
timelineEvent_eventData = Lens.lens (\TimelineEvent' {eventData} -> eventData) (\s@TimelineEvent' {} a -> s {eventData = a} :: TimelineEvent)

-- | The ID of the timeline event.
timelineEvent_eventId :: Lens.Lens' TimelineEvent Prelude.Text
timelineEvent_eventId = Lens.lens (\TimelineEvent' {eventId} -> eventId) (\s@TimelineEvent' {} a -> s {eventId = a} :: TimelineEvent)

-- | The time that the event occurred.
timelineEvent_eventTime :: Lens.Lens' TimelineEvent Prelude.UTCTime
timelineEvent_eventTime = Lens.lens (\TimelineEvent' {eventTime} -> eventTime) (\s@TimelineEvent' {} a -> s {eventTime = a} :: TimelineEvent) Prelude.. Core._Time

-- | The type of event that occurred. Currently Incident Manager supports
-- only the @Custom Event@ type.
timelineEvent_eventType :: Lens.Lens' TimelineEvent Prelude.Text
timelineEvent_eventType = Lens.lens (\TimelineEvent' {eventType} -> eventType) (\s@TimelineEvent' {} a -> s {eventType = a} :: TimelineEvent)

-- | The time that the timeline event was last updated.
timelineEvent_eventUpdatedTime :: Lens.Lens' TimelineEvent Prelude.UTCTime
timelineEvent_eventUpdatedTime = Lens.lens (\TimelineEvent' {eventUpdatedTime} -> eventUpdatedTime) (\s@TimelineEvent' {} a -> s {eventUpdatedTime = a} :: TimelineEvent) Prelude.. Core._Time

-- | The Amazon Resource Name (ARN) of the incident that the event occurred
-- during.
timelineEvent_incidentRecordArn :: Lens.Lens' TimelineEvent Prelude.Text
timelineEvent_incidentRecordArn = Lens.lens (\TimelineEvent' {incidentRecordArn} -> incidentRecordArn) (\s@TimelineEvent' {} a -> s {incidentRecordArn = a} :: TimelineEvent)

instance Core.FromJSON TimelineEvent where
  parseJSON =
    Core.withObject
      "TimelineEvent"
      ( \x ->
          TimelineEvent'
            Prelude.<$> (x Core..: "eventData")
            Prelude.<*> (x Core..: "eventId")
            Prelude.<*> (x Core..: "eventTime")
            Prelude.<*> (x Core..: "eventType")
            Prelude.<*> (x Core..: "eventUpdatedTime")
            Prelude.<*> (x Core..: "incidentRecordArn")
      )

instance Prelude.Hashable TimelineEvent where
  hashWithSalt salt' TimelineEvent' {..} =
    salt' `Prelude.hashWithSalt` incidentRecordArn
      `Prelude.hashWithSalt` eventUpdatedTime
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` eventTime
      `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventData

instance Prelude.NFData TimelineEvent where
  rnf TimelineEvent' {..} =
    Prelude.rnf eventData
      `Prelude.seq` Prelude.rnf incidentRecordArn
      `Prelude.seq` Prelude.rnf eventUpdatedTime
      `Prelude.seq` Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf eventTime
      `Prelude.seq` Prelude.rnf eventId
