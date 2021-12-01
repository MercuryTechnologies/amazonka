{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Chime.CreateMeetingWithAttendees
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new Amazon Chime SDK meeting in the specified media Region,
-- with attendees. For more information about specifying media Regions, see
-- <https://docs.aws.amazon.com/chime/latest/dg/chime-sdk-meetings-regions.html Amazon Chime SDK Media Regions>
-- in the /Amazon Chime Developer Guide/ . For more information about the
-- Amazon Chime SDK, see
-- <https://docs.aws.amazon.com/chime/latest/dg/meetings-sdk.html Using the Amazon Chime SDK>
-- in the /Amazon Chime Developer Guide/ .
module Amazonka.Chime.CreateMeetingWithAttendees
  ( -- * Creating a Request
    CreateMeetingWithAttendees (..),
    newCreateMeetingWithAttendees,

    -- * Request Lenses
    createMeetingWithAttendees_mediaRegion,
    createMeetingWithAttendees_meetingHostId,
    createMeetingWithAttendees_attendees,
    createMeetingWithAttendees_notificationsConfiguration,
    createMeetingWithAttendees_externalMeetingId,
    createMeetingWithAttendees_tags,
    createMeetingWithAttendees_clientRequestToken,

    -- * Destructuring the Response
    CreateMeetingWithAttendeesResponse (..),
    newCreateMeetingWithAttendeesResponse,

    -- * Response Lenses
    createMeetingWithAttendeesResponse_attendees,
    createMeetingWithAttendeesResponse_meeting,
    createMeetingWithAttendeesResponse_errors,
    createMeetingWithAttendeesResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMeetingWithAttendees' smart constructor.
data CreateMeetingWithAttendees = CreateMeetingWithAttendees'
  { -- | The Region in which to create the meeting. Default: @us-east-1@ .
    --
    -- Available values: @af-south-1@ , @ap-northeast-1@ , @ap-northeast-2@ ,
    -- @ap-south-1@ , @ap-southeast-1@ , @ap-southeast-2@ , @ca-central-1@ ,
    -- @eu-central-1@ , @eu-north-1@ , @eu-south-1@ , @eu-west-1@ , @eu-west-2@
    -- , @eu-west-3@ , @sa-east-1@ , @us-east-1@ , @us-east-2@ , @us-west-1@ ,
    -- @us-west-2@ .
    mediaRegion :: Prelude.Maybe Prelude.Text,
    -- | Reserved.
    meetingHostId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The request containing the attendees to create.
    attendees :: Prelude.Maybe (Prelude.NonEmpty CreateAttendeeRequestItem),
    notificationsConfiguration :: Prelude.Maybe MeetingNotificationConfiguration,
    -- | The external meeting ID.
    externalMeetingId :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The tag key-value pairs.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The unique identifier for the client request. Use a different token for
    -- different meetings.
    clientRequestToken :: Core.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMeetingWithAttendees' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mediaRegion', 'createMeetingWithAttendees_mediaRegion' - The Region in which to create the meeting. Default: @us-east-1@ .
--
-- Available values: @af-south-1@ , @ap-northeast-1@ , @ap-northeast-2@ ,
-- @ap-south-1@ , @ap-southeast-1@ , @ap-southeast-2@ , @ca-central-1@ ,
-- @eu-central-1@ , @eu-north-1@ , @eu-south-1@ , @eu-west-1@ , @eu-west-2@
-- , @eu-west-3@ , @sa-east-1@ , @us-east-1@ , @us-east-2@ , @us-west-1@ ,
-- @us-west-2@ .
--
-- 'meetingHostId', 'createMeetingWithAttendees_meetingHostId' - Reserved.
--
-- 'attendees', 'createMeetingWithAttendees_attendees' - The request containing the attendees to create.
--
-- 'notificationsConfiguration', 'createMeetingWithAttendees_notificationsConfiguration' - Undocumented member.
--
-- 'externalMeetingId', 'createMeetingWithAttendees_externalMeetingId' - The external meeting ID.
--
-- 'tags', 'createMeetingWithAttendees_tags' - The tag key-value pairs.
--
-- 'clientRequestToken', 'createMeetingWithAttendees_clientRequestToken' - The unique identifier for the client request. Use a different token for
-- different meetings.
newCreateMeetingWithAttendees ::
  -- | 'clientRequestToken'
  Prelude.Text ->
  CreateMeetingWithAttendees
newCreateMeetingWithAttendees pClientRequestToken_ =
  CreateMeetingWithAttendees'
    { mediaRegion =
        Prelude.Nothing,
      meetingHostId = Prelude.Nothing,
      attendees = Prelude.Nothing,
      notificationsConfiguration = Prelude.Nothing,
      externalMeetingId = Prelude.Nothing,
      tags = Prelude.Nothing,
      clientRequestToken =
        Core._Sensitive Lens.# pClientRequestToken_
    }

-- | The Region in which to create the meeting. Default: @us-east-1@ .
--
-- Available values: @af-south-1@ , @ap-northeast-1@ , @ap-northeast-2@ ,
-- @ap-south-1@ , @ap-southeast-1@ , @ap-southeast-2@ , @ca-central-1@ ,
-- @eu-central-1@ , @eu-north-1@ , @eu-south-1@ , @eu-west-1@ , @eu-west-2@
-- , @eu-west-3@ , @sa-east-1@ , @us-east-1@ , @us-east-2@ , @us-west-1@ ,
-- @us-west-2@ .
createMeetingWithAttendees_mediaRegion :: Lens.Lens' CreateMeetingWithAttendees (Prelude.Maybe Prelude.Text)
createMeetingWithAttendees_mediaRegion = Lens.lens (\CreateMeetingWithAttendees' {mediaRegion} -> mediaRegion) (\s@CreateMeetingWithAttendees' {} a -> s {mediaRegion = a} :: CreateMeetingWithAttendees)

-- | Reserved.
createMeetingWithAttendees_meetingHostId :: Lens.Lens' CreateMeetingWithAttendees (Prelude.Maybe Prelude.Text)
createMeetingWithAttendees_meetingHostId = Lens.lens (\CreateMeetingWithAttendees' {meetingHostId} -> meetingHostId) (\s@CreateMeetingWithAttendees' {} a -> s {meetingHostId = a} :: CreateMeetingWithAttendees) Prelude.. Lens.mapping Core._Sensitive

-- | The request containing the attendees to create.
createMeetingWithAttendees_attendees :: Lens.Lens' CreateMeetingWithAttendees (Prelude.Maybe (Prelude.NonEmpty CreateAttendeeRequestItem))
createMeetingWithAttendees_attendees = Lens.lens (\CreateMeetingWithAttendees' {attendees} -> attendees) (\s@CreateMeetingWithAttendees' {} a -> s {attendees = a} :: CreateMeetingWithAttendees) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createMeetingWithAttendees_notificationsConfiguration :: Lens.Lens' CreateMeetingWithAttendees (Prelude.Maybe MeetingNotificationConfiguration)
createMeetingWithAttendees_notificationsConfiguration = Lens.lens (\CreateMeetingWithAttendees' {notificationsConfiguration} -> notificationsConfiguration) (\s@CreateMeetingWithAttendees' {} a -> s {notificationsConfiguration = a} :: CreateMeetingWithAttendees)

-- | The external meeting ID.
createMeetingWithAttendees_externalMeetingId :: Lens.Lens' CreateMeetingWithAttendees (Prelude.Maybe Prelude.Text)
createMeetingWithAttendees_externalMeetingId = Lens.lens (\CreateMeetingWithAttendees' {externalMeetingId} -> externalMeetingId) (\s@CreateMeetingWithAttendees' {} a -> s {externalMeetingId = a} :: CreateMeetingWithAttendees) Prelude.. Lens.mapping Core._Sensitive

-- | The tag key-value pairs.
createMeetingWithAttendees_tags :: Lens.Lens' CreateMeetingWithAttendees (Prelude.Maybe (Prelude.NonEmpty Tag))
createMeetingWithAttendees_tags = Lens.lens (\CreateMeetingWithAttendees' {tags} -> tags) (\s@CreateMeetingWithAttendees' {} a -> s {tags = a} :: CreateMeetingWithAttendees) Prelude.. Lens.mapping Lens.coerced

-- | The unique identifier for the client request. Use a different token for
-- different meetings.
createMeetingWithAttendees_clientRequestToken :: Lens.Lens' CreateMeetingWithAttendees Prelude.Text
createMeetingWithAttendees_clientRequestToken = Lens.lens (\CreateMeetingWithAttendees' {clientRequestToken} -> clientRequestToken) (\s@CreateMeetingWithAttendees' {} a -> s {clientRequestToken = a} :: CreateMeetingWithAttendees) Prelude.. Core._Sensitive

instance Core.AWSRequest CreateMeetingWithAttendees where
  type
    AWSResponse CreateMeetingWithAttendees =
      CreateMeetingWithAttendeesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMeetingWithAttendeesResponse'
            Prelude.<$> (x Core..?> "Attendees" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Meeting")
            Prelude.<*> (x Core..?> "Errors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateMeetingWithAttendees where
  hashWithSalt salt' CreateMeetingWithAttendees' {..} =
    salt' `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` externalMeetingId
      `Prelude.hashWithSalt` notificationsConfiguration
      `Prelude.hashWithSalt` attendees
      `Prelude.hashWithSalt` meetingHostId
      `Prelude.hashWithSalt` mediaRegion

instance Prelude.NFData CreateMeetingWithAttendees where
  rnf CreateMeetingWithAttendees' {..} =
    Prelude.rnf mediaRegion
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf externalMeetingId
      `Prelude.seq` Prelude.rnf notificationsConfiguration
      `Prelude.seq` Prelude.rnf attendees
      `Prelude.seq` Prelude.rnf meetingHostId

instance Core.ToHeaders CreateMeetingWithAttendees where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateMeetingWithAttendees where
  toJSON CreateMeetingWithAttendees' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MediaRegion" Core..=) Prelude.<$> mediaRegion,
            ("MeetingHostId" Core..=) Prelude.<$> meetingHostId,
            ("Attendees" Core..=) Prelude.<$> attendees,
            ("NotificationsConfiguration" Core..=)
              Prelude.<$> notificationsConfiguration,
            ("ExternalMeetingId" Core..=)
              Prelude.<$> externalMeetingId,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("ClientRequestToken" Core..= clientRequestToken)
          ]
      )

instance Core.ToPath CreateMeetingWithAttendees where
  toPath = Prelude.const "/meetings"

instance Core.ToQuery CreateMeetingWithAttendees where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["operation=create-attendees"])

-- | /See:/ 'newCreateMeetingWithAttendeesResponse' smart constructor.
data CreateMeetingWithAttendeesResponse = CreateMeetingWithAttendeesResponse'
  { -- | The attendee information, including attendees IDs and join tokens.
    attendees :: Prelude.Maybe [Attendee],
    meeting :: Prelude.Maybe Meeting,
    -- | If the action fails for one or more of the attendees in the request, a
    -- list of the attendees is returned, along with error codes and error
    -- messages.
    errors :: Prelude.Maybe [CreateAttendeeError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMeetingWithAttendeesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attendees', 'createMeetingWithAttendeesResponse_attendees' - The attendee information, including attendees IDs and join tokens.
--
-- 'meeting', 'createMeetingWithAttendeesResponse_meeting' - Undocumented member.
--
-- 'errors', 'createMeetingWithAttendeesResponse_errors' - If the action fails for one or more of the attendees in the request, a
-- list of the attendees is returned, along with error codes and error
-- messages.
--
-- 'httpStatus', 'createMeetingWithAttendeesResponse_httpStatus' - The response's http status code.
newCreateMeetingWithAttendeesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateMeetingWithAttendeesResponse
newCreateMeetingWithAttendeesResponse pHttpStatus_ =
  CreateMeetingWithAttendeesResponse'
    { attendees =
        Prelude.Nothing,
      meeting = Prelude.Nothing,
      errors = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The attendee information, including attendees IDs and join tokens.
createMeetingWithAttendeesResponse_attendees :: Lens.Lens' CreateMeetingWithAttendeesResponse (Prelude.Maybe [Attendee])
createMeetingWithAttendeesResponse_attendees = Lens.lens (\CreateMeetingWithAttendeesResponse' {attendees} -> attendees) (\s@CreateMeetingWithAttendeesResponse' {} a -> s {attendees = a} :: CreateMeetingWithAttendeesResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createMeetingWithAttendeesResponse_meeting :: Lens.Lens' CreateMeetingWithAttendeesResponse (Prelude.Maybe Meeting)
createMeetingWithAttendeesResponse_meeting = Lens.lens (\CreateMeetingWithAttendeesResponse' {meeting} -> meeting) (\s@CreateMeetingWithAttendeesResponse' {} a -> s {meeting = a} :: CreateMeetingWithAttendeesResponse)

-- | If the action fails for one or more of the attendees in the request, a
-- list of the attendees is returned, along with error codes and error
-- messages.
createMeetingWithAttendeesResponse_errors :: Lens.Lens' CreateMeetingWithAttendeesResponse (Prelude.Maybe [CreateAttendeeError])
createMeetingWithAttendeesResponse_errors = Lens.lens (\CreateMeetingWithAttendeesResponse' {errors} -> errors) (\s@CreateMeetingWithAttendeesResponse' {} a -> s {errors = a} :: CreateMeetingWithAttendeesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createMeetingWithAttendeesResponse_httpStatus :: Lens.Lens' CreateMeetingWithAttendeesResponse Prelude.Int
createMeetingWithAttendeesResponse_httpStatus = Lens.lens (\CreateMeetingWithAttendeesResponse' {httpStatus} -> httpStatus) (\s@CreateMeetingWithAttendeesResponse' {} a -> s {httpStatus = a} :: CreateMeetingWithAttendeesResponse)

instance
  Prelude.NFData
    CreateMeetingWithAttendeesResponse
  where
  rnf CreateMeetingWithAttendeesResponse' {..} =
    Prelude.rnf attendees
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf errors
      `Prelude.seq` Prelude.rnf meeting
