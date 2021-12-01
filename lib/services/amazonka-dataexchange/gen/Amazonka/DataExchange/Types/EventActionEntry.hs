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
-- Module      : Amazonka.DataExchange.Types.EventActionEntry
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataExchange.Types.EventActionEntry where

import qualified Amazonka.Core as Core
import Amazonka.DataExchange.Types.Action
import Amazonka.DataExchange.Types.Event
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An event action is an object that defines the relationship between a
-- specific event and an automated action that will be taken on behalf of
-- the customer.
--
-- /See:/ 'newEventActionEntry' smart constructor.
data EventActionEntry = EventActionEntry'
  { -- | What occurs after a certain event.
    action :: Action,
    -- | The date and time that the event action was created, in ISO 8601 format.
    createdAt :: Core.POSIX,
    -- | What occurs to start an action.
    event :: Event,
    -- | The unique identifier for the event action.
    id :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the event action.
    arn :: Prelude.Text,
    -- | The date and time that the event action was last updated, in ISO 8601
    -- format.
    updatedAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventActionEntry' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'action', 'eventActionEntry_action' - What occurs after a certain event.
--
-- 'createdAt', 'eventActionEntry_createdAt' - The date and time that the event action was created, in ISO 8601 format.
--
-- 'event', 'eventActionEntry_event' - What occurs to start an action.
--
-- 'id', 'eventActionEntry_id' - The unique identifier for the event action.
--
-- 'arn', 'eventActionEntry_arn' - The Amazon Resource Name (ARN) for the event action.
--
-- 'updatedAt', 'eventActionEntry_updatedAt' - The date and time that the event action was last updated, in ISO 8601
-- format.
newEventActionEntry ::
  -- | 'action'
  Action ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'event'
  Event ->
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  EventActionEntry
newEventActionEntry
  pAction_
  pCreatedAt_
  pEvent_
  pId_
  pArn_
  pUpdatedAt_ =
    EventActionEntry'
      { action = pAction_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        event = pEvent_,
        id = pId_,
        arn = pArn_,
        updatedAt = Core._Time Lens.# pUpdatedAt_
      }

-- | What occurs after a certain event.
eventActionEntry_action :: Lens.Lens' EventActionEntry Action
eventActionEntry_action = Lens.lens (\EventActionEntry' {action} -> action) (\s@EventActionEntry' {} a -> s {action = a} :: EventActionEntry)

-- | The date and time that the event action was created, in ISO 8601 format.
eventActionEntry_createdAt :: Lens.Lens' EventActionEntry Prelude.UTCTime
eventActionEntry_createdAt = Lens.lens (\EventActionEntry' {createdAt} -> createdAt) (\s@EventActionEntry' {} a -> s {createdAt = a} :: EventActionEntry) Prelude.. Core._Time

-- | What occurs to start an action.
eventActionEntry_event :: Lens.Lens' EventActionEntry Event
eventActionEntry_event = Lens.lens (\EventActionEntry' {event} -> event) (\s@EventActionEntry' {} a -> s {event = a} :: EventActionEntry)

-- | The unique identifier for the event action.
eventActionEntry_id :: Lens.Lens' EventActionEntry Prelude.Text
eventActionEntry_id = Lens.lens (\EventActionEntry' {id} -> id) (\s@EventActionEntry' {} a -> s {id = a} :: EventActionEntry)

-- | The Amazon Resource Name (ARN) for the event action.
eventActionEntry_arn :: Lens.Lens' EventActionEntry Prelude.Text
eventActionEntry_arn = Lens.lens (\EventActionEntry' {arn} -> arn) (\s@EventActionEntry' {} a -> s {arn = a} :: EventActionEntry)

-- | The date and time that the event action was last updated, in ISO 8601
-- format.
eventActionEntry_updatedAt :: Lens.Lens' EventActionEntry Prelude.UTCTime
eventActionEntry_updatedAt = Lens.lens (\EventActionEntry' {updatedAt} -> updatedAt) (\s@EventActionEntry' {} a -> s {updatedAt = a} :: EventActionEntry) Prelude.. Core._Time

instance Core.FromJSON EventActionEntry where
  parseJSON =
    Core.withObject
      "EventActionEntry"
      ( \x ->
          EventActionEntry'
            Prelude.<$> (x Core..: "Action")
            Prelude.<*> (x Core..: "CreatedAt")
            Prelude.<*> (x Core..: "Event")
            Prelude.<*> (x Core..: "Id")
            Prelude.<*> (x Core..: "Arn")
            Prelude.<*> (x Core..: "UpdatedAt")
      )

instance Prelude.Hashable EventActionEntry where
  hashWithSalt salt' EventActionEntry' {..} =
    salt' `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` event
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` action

instance Prelude.NFData EventActionEntry where
  rnf EventActionEntry' {..} =
    Prelude.rnf action
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf event
      `Prelude.seq` Prelude.rnf createdAt
