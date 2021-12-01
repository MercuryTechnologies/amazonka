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
-- Module      : Amazonka.IoTEvents.Types.Event
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.Event where

import qualified Amazonka.Core as Core
import Amazonka.IoTEvents.Types.Action
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies the @actions@ to be performed when the @condition@ evaluates
-- to TRUE.
--
-- /See:/ 'newEvent' smart constructor.
data Event = Event'
  { -- | The actions to be performed.
    actions :: Prelude.Maybe [Action],
    -- | Optional. The Boolean expression that, when TRUE, causes the @actions@
    -- to be performed. If not present, the actions are performed (=TRUE). If
    -- the expression result is not a Boolean value, the actions are not
    -- performed (=FALSE).
    condition :: Prelude.Maybe Prelude.Text,
    -- | The name of the event.
    eventName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Event' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'event_actions' - The actions to be performed.
--
-- 'condition', 'event_condition' - Optional. The Boolean expression that, when TRUE, causes the @actions@
-- to be performed. If not present, the actions are performed (=TRUE). If
-- the expression result is not a Boolean value, the actions are not
-- performed (=FALSE).
--
-- 'eventName', 'event_eventName' - The name of the event.
newEvent ::
  -- | 'eventName'
  Prelude.Text ->
  Event
newEvent pEventName_ =
  Event'
    { actions = Prelude.Nothing,
      condition = Prelude.Nothing,
      eventName = pEventName_
    }

-- | The actions to be performed.
event_actions :: Lens.Lens' Event (Prelude.Maybe [Action])
event_actions = Lens.lens (\Event' {actions} -> actions) (\s@Event' {} a -> s {actions = a} :: Event) Prelude.. Lens.mapping Lens.coerced

-- | Optional. The Boolean expression that, when TRUE, causes the @actions@
-- to be performed. If not present, the actions are performed (=TRUE). If
-- the expression result is not a Boolean value, the actions are not
-- performed (=FALSE).
event_condition :: Lens.Lens' Event (Prelude.Maybe Prelude.Text)
event_condition = Lens.lens (\Event' {condition} -> condition) (\s@Event' {} a -> s {condition = a} :: Event)

-- | The name of the event.
event_eventName :: Lens.Lens' Event Prelude.Text
event_eventName = Lens.lens (\Event' {eventName} -> eventName) (\s@Event' {} a -> s {eventName = a} :: Event)

instance Core.FromJSON Event where
  parseJSON =
    Core.withObject
      "Event"
      ( \x ->
          Event'
            Prelude.<$> (x Core..:? "actions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "condition")
            Prelude.<*> (x Core..: "eventName")
      )

instance Prelude.Hashable Event where
  hashWithSalt salt' Event' {..} =
    salt' `Prelude.hashWithSalt` eventName
      `Prelude.hashWithSalt` condition
      `Prelude.hashWithSalt` actions

instance Prelude.NFData Event where
  rnf Event' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf eventName
      `Prelude.seq` Prelude.rnf condition

instance Core.ToJSON Event where
  toJSON Event' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("actions" Core..=) Prelude.<$> actions,
            ("condition" Core..=) Prelude.<$> condition,
            Prelude.Just ("eventName" Core..= eventName)
          ]
      )
