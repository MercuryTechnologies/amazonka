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
-- Module      : Amazonka.SWF.Types.ActivityTaskFailedEventAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.ActivityTaskFailedEventAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the details of the @ActivityTaskFailed@ event.
--
-- /See:/ 'newActivityTaskFailedEventAttributes' smart constructor.
data ActivityTaskFailedEventAttributes = ActivityTaskFailedEventAttributes'
  { -- | The reason provided for the failure.
    reason :: Prelude.Maybe Prelude.Text,
    -- | The details of the failure.
    details :: Prelude.Maybe Prelude.Text,
    -- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
    -- activity task was scheduled. This information can be useful for
    -- diagnosing problems by tracing back the chain of events leading up to
    -- this event.
    scheduledEventId :: Prelude.Integer,
    -- | The ID of the @ActivityTaskStarted@ event recorded when this activity
    -- task was started. This information can be useful for diagnosing problems
    -- by tracing back the chain of events leading up to this event.
    startedEventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActivityTaskFailedEventAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reason', 'activityTaskFailedEventAttributes_reason' - The reason provided for the failure.
--
-- 'details', 'activityTaskFailedEventAttributes_details' - The details of the failure.
--
-- 'scheduledEventId', 'activityTaskFailedEventAttributes_scheduledEventId' - The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
--
-- 'startedEventId', 'activityTaskFailedEventAttributes_startedEventId' - The ID of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
newActivityTaskFailedEventAttributes ::
  -- | 'scheduledEventId'
  Prelude.Integer ->
  -- | 'startedEventId'
  Prelude.Integer ->
  ActivityTaskFailedEventAttributes
newActivityTaskFailedEventAttributes
  pScheduledEventId_
  pStartedEventId_ =
    ActivityTaskFailedEventAttributes'
      { reason =
          Prelude.Nothing,
        details = Prelude.Nothing,
        scheduledEventId = pScheduledEventId_,
        startedEventId = pStartedEventId_
      }

-- | The reason provided for the failure.
activityTaskFailedEventAttributes_reason :: Lens.Lens' ActivityTaskFailedEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskFailedEventAttributes_reason = Lens.lens (\ActivityTaskFailedEventAttributes' {reason} -> reason) (\s@ActivityTaskFailedEventAttributes' {} a -> s {reason = a} :: ActivityTaskFailedEventAttributes)

-- | The details of the failure.
activityTaskFailedEventAttributes_details :: Lens.Lens' ActivityTaskFailedEventAttributes (Prelude.Maybe Prelude.Text)
activityTaskFailedEventAttributes_details = Lens.lens (\ActivityTaskFailedEventAttributes' {details} -> details) (\s@ActivityTaskFailedEventAttributes' {} a -> s {details = a} :: ActivityTaskFailedEventAttributes)

-- | The ID of the @ActivityTaskScheduled@ event that was recorded when this
-- activity task was scheduled. This information can be useful for
-- diagnosing problems by tracing back the chain of events leading up to
-- this event.
activityTaskFailedEventAttributes_scheduledEventId :: Lens.Lens' ActivityTaskFailedEventAttributes Prelude.Integer
activityTaskFailedEventAttributes_scheduledEventId = Lens.lens (\ActivityTaskFailedEventAttributes' {scheduledEventId} -> scheduledEventId) (\s@ActivityTaskFailedEventAttributes' {} a -> s {scheduledEventId = a} :: ActivityTaskFailedEventAttributes)

-- | The ID of the @ActivityTaskStarted@ event recorded when this activity
-- task was started. This information can be useful for diagnosing problems
-- by tracing back the chain of events leading up to this event.
activityTaskFailedEventAttributes_startedEventId :: Lens.Lens' ActivityTaskFailedEventAttributes Prelude.Integer
activityTaskFailedEventAttributes_startedEventId = Lens.lens (\ActivityTaskFailedEventAttributes' {startedEventId} -> startedEventId) (\s@ActivityTaskFailedEventAttributes' {} a -> s {startedEventId = a} :: ActivityTaskFailedEventAttributes)

instance
  Core.FromJSON
    ActivityTaskFailedEventAttributes
  where
  parseJSON =
    Core.withObject
      "ActivityTaskFailedEventAttributes"
      ( \x ->
          ActivityTaskFailedEventAttributes'
            Prelude.<$> (x Core..:? "reason")
            Prelude.<*> (x Core..:? "details")
            Prelude.<*> (x Core..: "scheduledEventId")
            Prelude.<*> (x Core..: "startedEventId")
      )

instance
  Prelude.Hashable
    ActivityTaskFailedEventAttributes
  where
  hashWithSalt
    salt'
    ActivityTaskFailedEventAttributes' {..} =
      salt' `Prelude.hashWithSalt` startedEventId
        `Prelude.hashWithSalt` scheduledEventId
        `Prelude.hashWithSalt` details
        `Prelude.hashWithSalt` reason

instance
  Prelude.NFData
    ActivityTaskFailedEventAttributes
  where
  rnf ActivityTaskFailedEventAttributes' {..} =
    Prelude.rnf reason
      `Prelude.seq` Prelude.rnf startedEventId
      `Prelude.seq` Prelude.rnf scheduledEventId
      `Prelude.seq` Prelude.rnf details
