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
-- Module      : Amazonka.Chime.Types.Attendee
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.Attendee where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An Amazon Chime SDK meeting attendee. Includes a unique @AttendeeId@ and
-- @JoinToken@ . The @JoinToken@ allows a client to authenticate and join
-- as the specified attendee. The @JoinToken@ expires when the meeting ends
-- or when DeleteAttendee is called. After that, the attendee is unable to
-- join the meeting.
--
-- We recommend securely transferring each @JoinToken@ from your server
-- application to the client so that no other client has access to the
-- token except for the one authorized to represent the attendee.
--
-- /See:/ 'newAttendee' smart constructor.
data Attendee = Attendee'
  { -- | The Amazon Chime SDK attendee ID.
    attendeeId :: Prelude.Maybe Prelude.Text,
    -- | The join token used by the Amazon Chime SDK attendee.
    joinToken :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Chime SDK external user ID. An idempotency token. Links the
    -- attendee to an identity managed by a builder application.
    externalUserId :: Prelude.Maybe (Core.Sensitive Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Attendee' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attendeeId', 'attendee_attendeeId' - The Amazon Chime SDK attendee ID.
--
-- 'joinToken', 'attendee_joinToken' - The join token used by the Amazon Chime SDK attendee.
--
-- 'externalUserId', 'attendee_externalUserId' - The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
newAttendee ::
  Attendee
newAttendee =
  Attendee'
    { attendeeId = Prelude.Nothing,
      joinToken = Prelude.Nothing,
      externalUserId = Prelude.Nothing
    }

-- | The Amazon Chime SDK attendee ID.
attendee_attendeeId :: Lens.Lens' Attendee (Prelude.Maybe Prelude.Text)
attendee_attendeeId = Lens.lens (\Attendee' {attendeeId} -> attendeeId) (\s@Attendee' {} a -> s {attendeeId = a} :: Attendee)

-- | The join token used by the Amazon Chime SDK attendee.
attendee_joinToken :: Lens.Lens' Attendee (Prelude.Maybe Prelude.Text)
attendee_joinToken = Lens.lens (\Attendee' {joinToken} -> joinToken) (\s@Attendee' {} a -> s {joinToken = a} :: Attendee) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Chime SDK external user ID. An idempotency token. Links the
-- attendee to an identity managed by a builder application.
attendee_externalUserId :: Lens.Lens' Attendee (Prelude.Maybe Prelude.Text)
attendee_externalUserId = Lens.lens (\Attendee' {externalUserId} -> externalUserId) (\s@Attendee' {} a -> s {externalUserId = a} :: Attendee) Prelude.. Lens.mapping Core._Sensitive

instance Core.FromJSON Attendee where
  parseJSON =
    Core.withObject
      "Attendee"
      ( \x ->
          Attendee'
            Prelude.<$> (x Core..:? "AttendeeId")
            Prelude.<*> (x Core..:? "JoinToken")
            Prelude.<*> (x Core..:? "ExternalUserId")
      )

instance Prelude.Hashable Attendee where
  hashWithSalt salt' Attendee' {..} =
    salt' `Prelude.hashWithSalt` externalUserId
      `Prelude.hashWithSalt` joinToken
      `Prelude.hashWithSalt` attendeeId

instance Prelude.NFData Attendee where
  rnf Attendee' {..} =
    Prelude.rnf attendeeId
      `Prelude.seq` Prelude.rnf externalUserId
      `Prelude.seq` Prelude.rnf joinToken
