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
-- Module      : Amazonka.DLM.Types.EventParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DLM.Types.EventParameters where

import qualified Amazonka.Core as Core
import Amazonka.DLM.Types.EventTypeValues
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies an event that triggers an event-based policy.
--
-- /See:/ 'newEventParameters' smart constructor.
data EventParameters = EventParameters'
  { -- | The type of event. Currently, only snapshot sharing events are
    -- supported.
    eventType :: EventTypeValues,
    -- | The IDs of the Amazon Web Services accounts that can trigger policy by
    -- sharing snapshots with your account. The policy only runs if one of the
    -- specified Amazon Web Services accounts shares a snapshot with your
    -- account.
    snapshotOwner :: [Prelude.Text],
    -- | The snapshot description that can trigger the policy. The description
    -- pattern is specified using a regular expression. The policy runs only if
    -- a snapshot with a description that matches the specified pattern is
    -- shared with your account.
    --
    -- For example, specifying
    -- @^.*Created for policy: policy-1234567890abcdef0.*$@ configures the
    -- policy to run only if snapshots created by policy
    -- @policy-1234567890abcdef0@ are shared with your account.
    descriptionRegex :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EventParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventType', 'eventParameters_eventType' - The type of event. Currently, only snapshot sharing events are
-- supported.
--
-- 'snapshotOwner', 'eventParameters_snapshotOwner' - The IDs of the Amazon Web Services accounts that can trigger policy by
-- sharing snapshots with your account. The policy only runs if one of the
-- specified Amazon Web Services accounts shares a snapshot with your
-- account.
--
-- 'descriptionRegex', 'eventParameters_descriptionRegex' - The snapshot description that can trigger the policy. The description
-- pattern is specified using a regular expression. The policy runs only if
-- a snapshot with a description that matches the specified pattern is
-- shared with your account.
--
-- For example, specifying
-- @^.*Created for policy: policy-1234567890abcdef0.*$@ configures the
-- policy to run only if snapshots created by policy
-- @policy-1234567890abcdef0@ are shared with your account.
newEventParameters ::
  -- | 'eventType'
  EventTypeValues ->
  -- | 'descriptionRegex'
  Prelude.Text ->
  EventParameters
newEventParameters pEventType_ pDescriptionRegex_ =
  EventParameters'
    { eventType = pEventType_,
      snapshotOwner = Prelude.mempty,
      descriptionRegex = pDescriptionRegex_
    }

-- | The type of event. Currently, only snapshot sharing events are
-- supported.
eventParameters_eventType :: Lens.Lens' EventParameters EventTypeValues
eventParameters_eventType = Lens.lens (\EventParameters' {eventType} -> eventType) (\s@EventParameters' {} a -> s {eventType = a} :: EventParameters)

-- | The IDs of the Amazon Web Services accounts that can trigger policy by
-- sharing snapshots with your account. The policy only runs if one of the
-- specified Amazon Web Services accounts shares a snapshot with your
-- account.
eventParameters_snapshotOwner :: Lens.Lens' EventParameters [Prelude.Text]
eventParameters_snapshotOwner = Lens.lens (\EventParameters' {snapshotOwner} -> snapshotOwner) (\s@EventParameters' {} a -> s {snapshotOwner = a} :: EventParameters) Prelude.. Lens.coerced

-- | The snapshot description that can trigger the policy. The description
-- pattern is specified using a regular expression. The policy runs only if
-- a snapshot with a description that matches the specified pattern is
-- shared with your account.
--
-- For example, specifying
-- @^.*Created for policy: policy-1234567890abcdef0.*$@ configures the
-- policy to run only if snapshots created by policy
-- @policy-1234567890abcdef0@ are shared with your account.
eventParameters_descriptionRegex :: Lens.Lens' EventParameters Prelude.Text
eventParameters_descriptionRegex = Lens.lens (\EventParameters' {descriptionRegex} -> descriptionRegex) (\s@EventParameters' {} a -> s {descriptionRegex = a} :: EventParameters)

instance Core.FromJSON EventParameters where
  parseJSON =
    Core.withObject
      "EventParameters"
      ( \x ->
          EventParameters'
            Prelude.<$> (x Core..: "EventType")
            Prelude.<*> (x Core..:? "SnapshotOwner" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "DescriptionRegex")
      )

instance Prelude.Hashable EventParameters where
  hashWithSalt salt' EventParameters' {..} =
    salt' `Prelude.hashWithSalt` descriptionRegex
      `Prelude.hashWithSalt` snapshotOwner
      `Prelude.hashWithSalt` eventType

instance Prelude.NFData EventParameters where
  rnf EventParameters' {..} =
    Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf descriptionRegex
      `Prelude.seq` Prelude.rnf snapshotOwner

instance Core.ToJSON EventParameters where
  toJSON EventParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("EventType" Core..= eventType),
            Prelude.Just ("SnapshotOwner" Core..= snapshotOwner),
            Prelude.Just
              ("DescriptionRegex" Core..= descriptionRegex)
          ]
      )
